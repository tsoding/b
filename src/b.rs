//! # The B compiler
//!
//! ## Logging
//!
//! Right now there are 3 mechanisms to log anything in the compiler:
//! 1. Just directly output anything to stdout/stderr with (f)printf
//! 2. lexer::diagf()
//! 3. nob::log()
//!
//! Direct printf-ing is used primarily for printing help
//! messages. Flags like `-help`, `-t list`, etc.
//!
//! lexer::diagf() is used for reporting compiler diagnostics that
//! have a specific location within the source code the compiler is
//! analysing.
//!
//! nob::log() is used for reporting things that the compiler is doing
//! outside of direct analysis of the user's source code (like
//! creating files or calling external programs) that are potentially
//! affected by the -q flag.
#![no_main]
#![no_std]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_macros)]

#[macro_use]
pub mod nob;
#[macro_use]
pub mod flag;
#[macro_use]
pub mod crust;
pub mod arena;
pub mod codegen;
pub mod lexer;
pub mod targets;
pub mod ir;
pub mod time;
pub mod shlex;

use core::ffi::*;
use core::mem::zeroed;
use core::ptr;
use core::slice;
use core::cmp;
use nob::*;
use flag::*;
use crust::libc::*;
use crust::assoc_lookup_cstr;
use arena::Arena;
use targets::*;
use lexer::{Lexer, Loc, Token};
use ir::*;
use time::Instant;
use codegen::*;
use shlex::*;

pub unsafe fn expect_tokens(l: *mut Lexer, tokens: *const [Token]) -> Option<()> {
    for i in 0..tokens.len() {
        if (*tokens)[i] == (*l).token {
            return Some(());
        }
    }

    let mut sb: String_Builder = zeroed();
    for i in 0..tokens.len() {
        if i > 0 {
            if i + 1 >= tokens.len() {
                sb_appendf(&mut sb, c!(", or "));
            } else {
                sb_appendf(&mut sb, c!(", "));
            }
        }
        sb_appendf(&mut sb, c!("%s"), lexer::display_token((*tokens)[i]));
    }
    da_append(&mut sb, 0);

    diagf!((*l).loc, c!("ERROR: expected %s, but got %s\n"), sb.items, lexer::display_token((*l).token));

    free(sb.items);
    None
}

pub unsafe fn expect_token(l: *mut Lexer, token: Token) -> Option<()> {
    expect_tokens(l, &[token])
}

pub unsafe fn get_and_expect_token(l: *mut Lexer, token: Token) -> Option<()> {
    lexer::get_token(l)?;
    expect_token(l, token)
}

pub unsafe fn get_and_expect_token_but_continue(l: *mut Lexer, c: *mut Compiler, token: Token) -> Option<()> {
    let saved_point = (*l).parse_point;
    lexer::get_token(l)?;
    if expect_token(l, token).is_none() {
        (*l).parse_point = saved_point;
        bump_error_count(c)
    } else {
        Some(())
    }
}

pub unsafe fn get_and_expect_tokens(l: *mut Lexer, clexes: *const [Token]) -> Option<()> {
    lexer::get_token(l)?;
    expect_tokens(l, clexes)
}

pub unsafe fn expect_token_id(l: *mut Lexer, id: *const c_char) -> Option<()> {
    expect_token(l, Token::ID)?;
    if strcmp((*l).string, id) != 0 {
        diagf!((*l).loc, c!("ERROR: expected `%s`, but got `%s`\n"), id, (*l).string);
        return None;
    }
    Some(())
}

pub unsafe fn get_and_expect_token_id(l: *mut Lexer, id: *const c_char) -> Option<()> {
    lexer::get_token(l)?;
    expect_token_id(l, id)
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum Storage {
    External {name: *const c_char},
    Auto     {index: usize},
}

#[derive(Clone, Copy)]
pub struct Var {
    pub name: *const c_char,
    pub loc: Loc,
    pub storage: Storage,
}

pub unsafe fn scope_push(vars: *mut Array<Array<Var>>) {
    if (*vars).count < (*vars).capacity {
        // Reusing already allocated scopes
        (*vars).count += 1;
        (*da_last_mut(vars).expect("There should be always at least the global scope")).count = 0;
    } else {
        da_append(vars, zeroed());
    }
}

pub unsafe fn scope_pop(vars: *mut Array<Array<Var>>) {
    assert!((*vars).count > 0);
    (*vars).count -= 1;
}

pub unsafe fn find_var_near(vars: *const Array<Var>, name: *const c_char) -> *const Var {
    for i in 0..(*vars).count {
        let var = (*vars).items.add(i);
        if strcmp((*var).name, name) == 0 {
            return var
        }
    }
    ptr::null()
}

pub unsafe fn find_var_deep(vars: *const Array<Array<Var>>, name: *const c_char) -> *const Var {
    let mut i = (*vars).count;
    while i > 0 {
        let var = find_var_near((*vars).items.add(i-1), name);
        if !var.is_null() {
            return var;
        }
        i -= 1;
    }
    ptr::null()
}

pub unsafe fn declare_var(c: *mut Compiler, name: *const c_char, loc: Loc, storage: Storage) -> Option<()> {
    let scope = da_last_mut(&mut (*c).vars).expect("There should be always at least the global scope");
    let existing_var = find_var_near(scope, name);
    if !existing_var.is_null() {
        diagf!(loc, c!("ERROR: redefinition of variable `%s`\n"), name);
        diagf!((*existing_var).loc, c!("NOTE: the first declaration is located here\n"));
        return bump_error_count(c);
    }

    da_append(scope, Var {name, loc, storage});
    Some(())
}

#[derive(Clone, Copy)]
pub struct GotoLabel {
    name: *const c_char,
    loc: Loc,
    label: usize,
}

#[derive(Clone, Copy)]
pub struct Goto {
    name: *const c_char,
    loc: Loc,
    addr: usize,
}

pub unsafe fn find_goto_label(labels: *const Array<GotoLabel>, name: *const c_char) -> *const GotoLabel {
    for i in 0..(*labels).count {
        let label = (*labels).items.add(i);
        if strcmp((*label).name, name) == 0 {
            return label
        }
    }
    ptr::null()
}

pub unsafe fn define_goto_label(c: *mut Compiler, name: *const c_char, loc: Loc, label: usize) -> Option<()> {
    let existing_label = find_goto_label(&(*c).func_goto_labels, name);
    if !existing_label.is_null() {
        diagf!(loc, c!("ERROR: duplicate label `%s`\n"), name);
        diagf!((*existing_label).loc, c!("NOTE: the first definition is located here\n"));
        return bump_error_count(c);
    }

    da_append(&mut (*c).func_goto_labels, GotoLabel {name, loc, label});
    Some(())
}

// The higher the index of the row in this table the higher the precedence of the Binop
pub const PRECEDENCE: *const [*const [Binop]] = &[
    &[Binop::BitOr],
    &[Binop::BitAnd],
    &[Binop::BitShl, Binop::BitShr],
    &[Binop::Equal, Binop::NotEqual],
    &[Binop::Less, Binop::Greater, Binop::GreaterEqual, Binop::LessEqual],
    &[Binop::Plus, Binop::Minus],
    &[Binop::Mult, Binop::Mod, Binop::Div],
];

impl Binop {
    // The outer Option indicates success.
    // The inner Option indicates whether the assign has binop associated with it.
    // It's kinda confusing but I don't know how to make it "prettier"
    pub fn from_assign_token(token: Token) -> Option<Option<Self>> {
        match token {
            Token::Eq      => Some(None),
            Token::PlusEq  => Some(Some(Binop::Plus)),
            Token::MinusEq => Some(Some(Binop::Minus)),
            Token::MulEq   => Some(Some(Binop::Mult)),
            Token::DivEq   => Some(Some(Binop::Div)),
            Token::ModEq   => Some(Some(Binop::Mod)),
            Token::ShlEq   => Some(Some(Binop::BitShl)),
            Token::ShrEq   => Some(Some(Binop::BitShr)),
            Token::OrEq    => Some(Some(Binop::BitOr)),
            Token::AndEq   => Some(Some(Binop::BitAnd)),
            _              => None,
        }
    }

    pub fn from_token(token: Token) -> Option<Self> {
        match token {
            Token::Plus      => Some(Binop::Plus),
            Token::Minus     => Some(Binop::Minus),
            Token::Mul       => Some(Binop::Mult),
            Token::Div       => Some(Binop::Div),
            Token::Mod       => Some(Binop::Mod),
            Token::EqEq      => Some(Binop::Equal),
            Token::NotEq     => Some(Binop::NotEqual),
            Token::Less      => Some(Binop::Less),
            Token::LessEq    => Some(Binop::LessEqual),
            Token::Greater   => Some(Binop::Greater),
            Token::GreaterEq => Some(Binop::GreaterEqual),
            Token::Or        => Some(Binop::BitOr),
            Token::And       => Some(Binop::BitAnd),
            Token::Shl       => Some(Binop::BitShl),
            Token::Shr       => Some(Binop::BitShr),
            _ => None,
        }
    }

    pub const MAX_PRECEDENCE: usize = PRECEDENCE.len();
    pub unsafe fn precedence(self) -> usize {
        for precedence in 0..PRECEDENCE.len() {
            for i in 0..(*PRECEDENCE)[precedence].len() {
                if self == (*(*PRECEDENCE)[precedence])[i] {
                    return precedence
                }
            }
        }
        unreachable!()
    }
}

pub unsafe fn push_opcode(opcode: Op, loc: Loc, c: *mut Compiler) {
    da_append(&mut (*c).func_body, OpWithLocation {opcode, loc});
}

/// Allocator of Auto Vars
#[derive(Clone, Copy)]
pub struct AutoVarsAtor {
    /// How many autovars currently allocated
    pub count: usize,
    /// Maximum allocated autovars throughout the function body
    pub max: usize,
}

pub unsafe fn allocate_label_index(c: *mut Compiler) -> usize {
    let index = (*c).op_label_count;
    (*c).op_label_count += 1;
    index
}

pub unsafe fn allocate_auto_var(t: *mut AutoVarsAtor) -> usize {
    (*t).count += 1;
    if (*t).count > (*t).max {
        (*t).max = (*t).count;
    }
    (*t).count
}


pub unsafe fn compile_string(string: *const c_char, c: *mut Compiler) -> usize {
    let offset = (*c).program.data.count;
    let string_len = strlen(string);
    da_append_many(&mut (*c).program.data, slice::from_raw_parts(string as *const u8, string_len));
    // TODO: Strings in B are not NULL-terminated.
    // They are terminated with symbol '*e' ('*' is escape character akin to '\' in C) which according to the
    // spec is called just "end-of-file" without any elaboration on what its value is. Maybe it had a specific
    // value on PDP that was a common knowledge at the time? In any case that breaks compatibility with
    // libc. While the language is still in development we gonna terminate it with 0. We will make it
    // "spec complaint" later.
    da_append(&mut (*c).program.data, 0); // NULL-terminator
    offset
}

pub unsafe fn compile_primary_expression(l: *mut Lexer, c: *mut Compiler) -> Option<(Arg, bool)> {
    lexer::get_token(l)?;
    let arg = match (*l).token {
        Token::OParen => {
            let result = compile_expression(l, c)?;
            get_and_expect_token_but_continue(l, c, Token::CParen)?;
            Some(result)
        }
        Token::Not => {
            let (arg, _) = compile_primary_expression(l, c)?;
            let result = allocate_auto_var(&mut (*c).auto_vars_ator);
            push_opcode(Op::UnaryNot{result, arg}, (*l).loc, c);
            Some((Arg::AutoVar(result), false))
        }
        Token::Mul => {
            let (arg, _) = compile_primary_expression(l, c)?;
            let index = allocate_auto_var(&mut (*c).auto_vars_ator);
            push_opcode(Op::AutoAssign {index, arg}, (*l).loc, c);
            Some((Arg::Deref(index), true))
        }
        Token::Minus => {
            let (arg, _) = compile_primary_expression(l, c)?;
            if let Arg::Literal(v) = arg {
                Some((Arg::Literal(!v + 1), false))
            } else {
                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                push_opcode(Op::Negate {result: index, arg}, (*l).loc, c);
                Some((Arg::AutoVar(index), false))
            }
        }
        Token::And => {
            let loc = (*l).loc;
            let (arg, is_lvalue) = compile_primary_expression(l, c)?;

            if !is_lvalue {
                diagf!(loc, c!("ERROR: cannot take the address of an rvalue\n"));
                return bump_error_count(c).map(|()| (Arg::Bogus, false));
            }

            match arg {
                Arg::Deref(index)   =>  Some((Arg::AutoVar(index), false)), // "&*x is identically x"
                Arg::External(name) =>  Some((Arg::RefExternal(name), false)),
                Arg::AutoVar(index) =>  Some((Arg::RefAutoVar(index), false)),
                Arg::Bogus          =>  Some((Arg::Bogus, false)), // Reference of a bogus value is a bogus value
                Arg::Literal(_) | Arg::DataOffset(_) | Arg::RefAutoVar(_) | Arg::RefExternal(_) => unreachable!(),
            }
        }
        Token::PlusPlus => {
            let loc = (*l).loc;
            let (arg, is_lvalue) = compile_primary_expression(l, c)?;

            if !is_lvalue {
                diagf!(loc, c!("ERROR: cannot increment an rvalue\n"));
                return bump_error_count(c).map(|()| (Arg::Bogus, false));
            }

            compile_binop(arg, Arg::Literal(1), Binop::Plus, loc, c);
            Some((arg, false))
        }
        Token::MinusMinus => {
            let loc = (*l).loc;
            let (arg, is_lvalue) = compile_primary_expression(l, c)?;

            if !is_lvalue {
                diagf!(loc, c!("ERROR: cannot decrement an rvalue\n"));
                return bump_error_count(c).map(|()| (Arg::Bogus, false));
            }

            compile_binop(arg, Arg::Literal(1), Binop::Minus, loc, c);
            Some((arg, false))
        }
        Token::CharLit | Token::IntLit => Some((Arg::Literal((*l).int_number), false)),
        Token::ID => {
            let name = arena::strdup(&mut (*c).arena, (*l).string);

            let var_def = find_var_deep(&mut (*c).vars, name);
            if var_def.is_null() {
                da_append(&mut (*c).used_funcs, UsedFunc {name, loc: (*l).loc});
                Some((Arg::External(name), true))
            } else {
                match (*var_def).storage {
                    Storage::Auto{index} => Some((Arg::AutoVar(index), true)),
                    Storage::External{name} => Some((Arg::External(name), true)),
                }
            }
        }
        Token::String => {
            let offset = compile_string((*l).string, c);
            Some((Arg::DataOffset(offset), false))
        }
        _ => {
            diagf!((*l).loc, c!("Expected start of a primary expression but got %s\n"), lexer::display_token((*l).token));
            None
        }
    };

    let (mut arg, mut is_lvalue) = arg?;

    loop {
        let saved_point = (*l).parse_point;
        lexer::get_token(l)?;

        (arg, is_lvalue) = match (*l).token {
            Token::OParen => Some((compile_function_call(l, c, arg)?, false)),
            Token::OBracket => {
                let (offset, _) = compile_expression(l, c)?;
                get_and_expect_token_but_continue(l, c, Token::CBracket)?;

                let result = allocate_auto_var(&mut (*c).auto_vars_ator);
                push_opcode(Op::Index {result, arg, offset}, (*l).loc, c);

                Some((Arg::Deref(result), true))
            }
            Token::PlusPlus => {
                let loc = (*l).loc;
                if !is_lvalue {
                    diagf!(loc, c!("ERROR: cannot increment an rvalue\n"));
                    return bump_error_count(c).map(|()| (Arg::Bogus, false));
                }

                let pre = allocate_auto_var(&mut (*c).auto_vars_ator);
                push_opcode(Op::AutoAssign {index: pre, arg}, loc, c);
                compile_binop(arg, Arg::Literal(1), Binop::Plus, loc, c);

                Some((Arg::AutoVar(pre), false))
            }
            Token::MinusMinus => {
                let loc = (*l).loc;
                if !is_lvalue {
                    diagf!(loc, c!("ERROR: cannot decrement an rvalue\n"));
                    return bump_error_count(c).map(|()| (Arg::Bogus, false));
                }

                let pre = allocate_auto_var(&mut (*c).auto_vars_ator);
                push_opcode(Op::AutoAssign {index: pre, arg}, loc, c);
                compile_binop(arg, Arg::Literal(1), Binop::Minus, loc, c);

                Some((Arg::AutoVar(pre), false))
            }
            _ => {
                (*l).parse_point = saved_point;
                return Some((arg, is_lvalue));
            }
        }?;
    }
}

// TODO: communicate to the caller of this function that it expects `lhs` to be an lvalue
pub unsafe fn compile_binop(lhs: Arg, rhs: Arg, binop: Binop, loc: Loc, c: *mut Compiler) {
    match lhs {
        Arg::Deref(index) => {
            let tmp = allocate_auto_var(&mut (*c).auto_vars_ator);
            push_opcode(Op::Binop {binop, index: tmp, lhs, rhs}, loc, c);
            push_opcode(Op::Store {index, arg: Arg::AutoVar(tmp)}, loc, c);
        },
        Arg::External(name) => {
            let tmp = allocate_auto_var(&mut (*c).auto_vars_ator);
            push_opcode(Op::Binop {binop, index: tmp, lhs, rhs}, loc, c);
            push_opcode(Op::ExternalAssign {name, arg: Arg::AutoVar(tmp)}, loc, c)
        }
        Arg::AutoVar(index) => {
            push_opcode(Op::Binop {binop, index, lhs, rhs}, loc, c)
        }
        Arg::Bogus => {
            // Bogus value does not compile to anything
        }
        Arg::Literal(_) | Arg::DataOffset(_) | Arg::RefAutoVar(_) | Arg::RefExternal(_) => unreachable!(),
    }
}

pub unsafe fn compile_binop_expression(l: *mut Lexer, c: *mut Compiler, precedence: usize) -> Option<(Arg, bool)> {
    if precedence >= Binop::MAX_PRECEDENCE {
        return compile_primary_expression(l, c);
    }

    let (mut lhs, mut lvalue) = compile_binop_expression(l, c, precedence + 1)?;

    let mut saved_point = (*l).parse_point;
    lexer::get_token(l)?;

    if let Some(binop) = Binop::from_token((*l).token) {
        if binop.precedence() == precedence {
            while let Some(binop) = Binop::from_token((*l).token) {
                if binop.precedence() != precedence { break; }

                let (rhs, _) = compile_binop_expression(l, c, precedence + 1)?;

                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                push_opcode(Op::Binop {binop, index, lhs, rhs}, (*l).loc, c);
                lhs = Arg::AutoVar(index);

                lvalue = false;

                saved_point = (*l).parse_point;
                lexer::get_token(l)?;
            }
        }
    }

    (*l).parse_point = saved_point;
    Some((lhs, lvalue))
}

pub unsafe fn compile_assign_expression(l: *mut Lexer, c: *mut Compiler) -> Option<(Arg, bool)> {
    let (lhs, mut lvalue) = compile_binop_expression(l, c, 0)?;

    let mut saved_point = (*l).parse_point;
    lexer::get_token(l)?;

    while let Some(binop) = Binop::from_assign_token((*l).token) {
        let binop_loc = (*l).loc;
        let (rhs, _) = compile_assign_expression(l, c)?;

        if !lvalue {
            diagf!(binop_loc, c!("ERROR: cannot assign to rvalue\n"));
            return bump_error_count(c).map(|()| (Arg::Bogus, false));
        }

        if let Some(binop) = binop {
            compile_binop(lhs, rhs, binop, binop_loc, c);
        } else {
            match lhs {
                Arg::Deref(index) => {
                    push_opcode(Op::Store {index, arg: rhs}, binop_loc, c);
                }
                Arg::External(name) => {
                    push_opcode(Op::ExternalAssign {name, arg: rhs}, binop_loc, c);
                }
                Arg::AutoVar(index) => {
                    push_opcode(Op::AutoAssign {index, arg: rhs}, binop_loc, c);
                }
                Arg::Bogus => {
                    // Bogus value does not compile to anything
                }
                Arg::Literal(_) | Arg::DataOffset(_) | Arg::RefAutoVar(_) | Arg::RefExternal(_) => unreachable!(),
            }
        }

        lvalue = false;

        saved_point = (*l).parse_point;
        lexer::get_token(l)?;
    }

    if (*l).token == Token::Question {
        let result = allocate_auto_var(&mut (*c).auto_vars_ator);

        let else_label = allocate_label_index(c);
        push_opcode(Op::JmpIfNotLabel{label: else_label, arg: lhs}, (*l).loc, c);

        let (if_true, _) = compile_expression(l, c)?;
        push_opcode(Op::AutoAssign {index: result, arg: if_true}, (*l).loc, c);
        let out_label = allocate_label_index(c);
        push_opcode(Op::JmpLabel{label: out_label}, (*l).loc, c);

        get_and_expect_token_but_continue(l, c, Token::Colon)?;

        push_opcode(Op::Label{label: else_label}, (*l).loc, c);
        let (if_false, _) = compile_expression(l, c)?;
        push_opcode(Op::AutoAssign {index: result, arg: if_false}, (*l).loc, c);
        push_opcode(Op::Label{label: out_label}, (*l).loc, c);

        Some((Arg::AutoVar(result), false))
    } else {
        (*l).parse_point = saved_point;
        Some((lhs, lvalue))
    }
}

pub unsafe fn compile_expression(l: *mut Lexer, c: *mut Compiler) -> Option<(Arg, bool)> {
    compile_assign_expression(l, c)
}

pub unsafe fn compile_block(l: *mut Lexer, c: *mut Compiler) -> Option<()> {
    loop {
        let saved_point = (*l).parse_point;
        lexer::get_token(l)?;
        if (*l).token == Token::CCurly { return Some(()); }
        (*l).parse_point = saved_point;

        compile_statement(l, c)?
    }
}
 unsafe fn compile_function_call(l: *mut Lexer, c: *mut Compiler, fun: Arg) -> Option<Arg> {
    let mut args: Array<Arg> = zeroed();
    let saved_point = (*l).parse_point;
    lexer::get_token(l)?;
    if (*l).token != Token::CParen {
        (*l).parse_point = saved_point;
        loop {
            let (expr, _) = compile_expression(l, c)?;
            da_append(&mut args, expr);
            get_and_expect_tokens(l, &[Token::CParen, Token::Comma])?;
            match (*l).token {
                Token::CParen => break,
                Token::Comma => continue,
                _ => unreachable!(),
            }
        }
    }

    let result = allocate_auto_var(&mut (*c).auto_vars_ator);
    push_opcode(Op::Funcall {result, fun, args}, (*l).loc, c);
    Some(Arg::AutoVar(result))
}

pub unsafe fn name_declare_if_not_exists(names: *mut Array<*const c_char>, name: *const c_char) {
    for i in 0..(*names).count {
        if strcmp(*(*names).items.add(i), name) == 0 {
            return;
        }
    }
    da_append(names, name)
}

pub unsafe fn compile_asm_stmts(l: *mut Lexer, c: *mut Compiler, stmts: *mut Array<AsmStmt>) -> Option<()> {
    get_and_expect_token_but_continue(l, c, Token::OParen)?;
    let saved_point = (*l).parse_point;
    lexer::get_token(l)?;
    if (*l).token != Token::CParen {
        (*l).parse_point = saved_point;
        loop {
            get_and_expect_token(l, Token::String)?;
            match (*l).token {
                Token::String => {
                    let line = arena::strdup(&mut (*c).arena, (*l).string);
                    let loc = (*l).loc;
                    da_append(stmts, AsmStmt { line, loc });
                }
                _ => unreachable!(),
            }

            get_and_expect_tokens(l, &[Token::Comma, Token::CParen])?;
            match (*l).token {
                Token::Comma  => {}
                Token::CParen => break,
                _             => unreachable!(),
            }
        }
    }
    get_and_expect_token_but_continue(l, c, Token::SemiColon)?;
    Some(())
}

pub unsafe fn compile_statement(l: *mut Lexer, c: *mut Compiler) -> Option<()> {
    let saved_point = (*l).parse_point;
    lexer::get_token(l)?;

    match (*l).token {
        Token::SemiColon => {
            Some(())
        },
        Token::OCurly => {
            scope_push(&mut (*c).vars);
            let saved_auto_vars_count = (*c).auto_vars_ator.count;
            compile_block(l, c)?;
            (*c).auto_vars_ator.count = saved_auto_vars_count;
            scope_pop(&mut (*c).vars);
            Some(())
        }
        Token::Extrn => {
            while (*l).token != Token::SemiColon {
                get_and_expect_token(l, Token::ID)?;
                let name = arena::strdup(&mut (*c).arena, (*l).string);
                name_declare_if_not_exists(&mut (*c).program.extrns, name);
                declare_var(c, name, (*l).loc, Storage::External {name})?;
                get_and_expect_tokens(l, &[Token::SemiColon, Token::Comma])?;
            }
            compile_statement(l, c)
        }
        Token::Auto => {
            while (*l).token != Token::SemiColon {
                get_and_expect_token(l, Token::ID)?;
                let name = arena::strdup(&mut (*c).arena, (*l).string);
                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                declare_var(c, name, (*l).loc, Storage::Auto {index})?;
                get_and_expect_tokens(l, &[Token::SemiColon, Token::Comma, Token::IntLit, Token::CharLit])?;
                if (*l).token == Token::IntLit || (*l).token == Token::CharLit {
                    let size = (*l).int_number as usize;
                    if size == 0 {
                        missingf!((*l).loc, c!("It's unclear how to compile automatic vector of size 0\n"));
                    }
                    for _ in 0..size {
                        allocate_auto_var(&mut (*c).auto_vars_ator);
                    }
                    // TODO: Here we assume the stack grows down. Should we
                    //   instead find a way for the target to decide that?
                    //   See TODO(2025-06-05 17:45:36)
                    let arg = Arg::RefAutoVar(index + size);
                    push_opcode(Op::AutoAssign {index, arg}, (*l).loc, c);
                    get_and_expect_tokens(l, &[Token::SemiColon, Token::Comma])?;
                }
            }
            compile_statement(l, c)
        }
        Token::If => {
            get_and_expect_token_but_continue(l, c, Token::OParen)?;
                let saved_auto_vars_count = (*c).auto_vars_ator.count;
                   let (cond, _) = compile_expression(l, c)?;
                   let else_label = allocate_label_index(c);
                   push_opcode(Op::JmpIfNotLabel{label: else_label, arg: cond}, (*l).loc, c);
                (*c).auto_vars_ator.count = saved_auto_vars_count;
            get_and_expect_token_but_continue(l, c, Token::CParen)?;

            compile_statement(l, c)?;

            let saved_point = (*l).parse_point;
            lexer::get_token(l)?;
            if (*l).token == Token::Else {
                let out_label = allocate_label_index(c);
                push_opcode(Op::JmpLabel{label: out_label}, (*l).loc, c);
                push_opcode(Op::Label{label: else_label}, (*l).loc, c);
                    compile_statement(l, c)?;
                push_opcode(Op::Label{label: out_label}, (*l).loc, c);
            } else {
                (*l).parse_point = saved_point;
                push_opcode(Op::Label{label: else_label}, (*l).loc, c);
            }

            Some(())
        }
        Token::While => {
            let cond_label = allocate_label_index(c);
            push_opcode(Op::Label {label: cond_label}, (*l).loc, c);

            get_and_expect_token_but_continue(l, c, Token::OParen)?;
                let saved_auto_vars_count = (*c).auto_vars_ator.count;
                    let (arg, _) = compile_expression(l, c)?;
                (*c).auto_vars_ator.count = saved_auto_vars_count;
            get_and_expect_token_but_continue(l, c, Token::CParen)?;

            let out_label = allocate_label_index(c);
            push_opcode(Op::JmpIfNotLabel{label: out_label, arg}, (*l).loc, c);

                compile_statement(l, c)?;

            push_opcode(Op::JmpLabel{label: cond_label}, (*l).loc, c);
            push_opcode(Op::Label {label: out_label}, (*l).loc, c);
            Some(())
        }
        Token::Return => {
            get_and_expect_tokens(l, &[Token::SemiColon, Token::OParen])?;
            if (*l).token == Token::SemiColon {
                push_opcode(Op::Return {arg: None}, (*l).loc, c);
            } else if (*l).token == Token::OParen {
                let (arg, _) = compile_expression(l, c)?;
                get_and_expect_token_but_continue(l, c, Token::CParen)?;
                get_and_expect_token_but_continue(l, c, Token::SemiColon)?;
                push_opcode(Op::Return {arg: Some(arg)}, (*l).loc, c);
            } else {
                unreachable!();
            }
            Some(())
        }
        Token::Goto => {
            get_and_expect_token(l, Token::ID)?;
            let name = arena::strdup(&mut (*c).arena, (*l).string);
            let loc = (*l).loc;
            let addr = (*c).func_body.count;
            da_append(&mut (*c).func_gotos, Goto {name, loc, addr});
            get_and_expect_token_but_continue(l, c, Token::SemiColon)?;
            push_opcode(Op::Bogus, (*l).loc, c);
            Some(())
        }
        Token::Asm => {
            let loc = (*l).loc;
            let mut stmts: Array<AsmStmt> = zeroed();
            compile_asm_stmts(l, c, &mut stmts)?;
            push_opcode(Op::Asm {stmts}, loc, c);
            Some(())
        }
        Token::Case => {
            let case_loc = (*l).loc;
            lexer::get_token(l);
            expect_tokens(l, &[Token::IntLit, Token::CharLit])?; // TODO: String ??!
            let case_value = (*l).int_number;
            get_and_expect_token_but_continue(l, c, Token::Colon)?;

            if let Some(switch_frame) = da_last_mut(&mut (*c).switch_stack) {
                let fallthrough_label = allocate_label_index(c);
                push_opcode(Op::JmpLabel{label: fallthrough_label}, case_loc, c);

                push_opcode(Op::Label{
                    label: (*switch_frame).label
                }, case_loc, c);

                push_opcode(Op::Binop{
                    binop: Binop::Equal,
                    index: (*switch_frame).cond,
                    lhs: (*switch_frame).value,
                    rhs: Arg::Literal(case_value)
                }, case_loc, c);

                let next_case_label = allocate_label_index(c);
                push_opcode(Op::JmpIfNotLabel {
                    label: next_case_label,
                    arg: Arg::AutoVar((*switch_frame).cond)
                }, case_loc, c);
                (*switch_frame).label = next_case_label;

                push_opcode(Op::Label{label: fallthrough_label}, case_loc, c);

                Some(())
            } else {
                diagf!(case_loc, c!("ERROR: case label outside of switch\n"));
                bump_error_count(c)
            }
        }
        Token::Switch => {
            let saved_auto_vars_count = (*c).auto_vars_ator.count;

            let switch_loc = (*l).loc;
            let (value, _) = compile_expression(l, c)?;
            let cond = allocate_auto_var(&mut (*c).auto_vars_ator);
            let label = allocate_label_index(c);
            da_append(&mut (*c).switch_stack, Switch {label, value, cond});
            push_opcode(Op::JmpLabel {label}, switch_loc, c);

            compile_statement(l, c)?;

            let switch_frame = da_last_mut(&mut (*c).switch_stack).expect("Switch stack was modified by somebody else");
            push_opcode(Op::Label{label: (*switch_frame).label}, (*l).loc, c);
            (*c).switch_stack.count -= 1;

            (*c).auto_vars_ator.count = saved_auto_vars_count;

            Some(())
        }
        _ => {
            if (*l).token == Token::ID {
                let name = arena::strdup(&mut (*c).arena, (*l).string);
                let name_loc = (*l).loc;
                lexer::get_token(l)?;
                if (*l).token == Token::Colon {
                    let label = allocate_label_index(c);
                    push_opcode(Op::Label{label}, name_loc, c);
                    define_goto_label(c, name, name_loc, label)?;
                    return Some(());
                }
            }
            (*l).parse_point = saved_point;
            let saved_auto_vars_count = (*c).auto_vars_ator.count;
            compile_expression(l, c)?;
            (*c).auto_vars_ator.count = saved_auto_vars_count;
            get_and_expect_token_but_continue(l, c, Token::SemiColon)?;
            Some(())
        }
    }
}

pub unsafe fn usage() {
    fprintf(stderr(), c!("B compiler\n"));
    fprintf(stderr(), c!("Usage: %s [OPTIONS] <inputs...> [--] [run arguments]\n"), flag_program_name());
    fprintf(stderr(), c!("OPTIONS:\n"));
    flag_print_options(stderr());
}

#[derive(Clone, Copy)]
pub struct Switch {
    pub label: usize,
    pub value: Arg,
    pub cond: usize,
}

#[derive(Clone, Copy)]
pub struct Compiler {
    pub program: Program,
    pub vars: Array<Array<Var>>,
    pub auto_vars_ator: AutoVarsAtor,
    pub func_body: Array<OpWithLocation>,
    pub func_goto_labels: Array<GotoLabel>,
    pub func_gotos: Array<Goto>,
    pub used_funcs: Array<UsedFunc>,
    pub op_label_count: usize,
    pub switch_stack: Array<Switch>,
    /// Arena into which the Compiler allocates all the names and
    /// objects that need to live for the duration of the
    /// compilation. Even if some object/names don't need to live that
    /// long (for example, function labels need to live only for the
    /// duration of that function compilation), just letting them live
    /// longer makes the memory management easier.
    ///
    /// Basically just dump everything into this arena and if you ever
    /// need to reset the state of the Compiler, just reset all its
    /// Dynamic Arrays and this Arena.
    pub arena: Arena,
    pub target: Target,
    pub error_count: usize,
    pub historical: bool,
}

#[derive(Clone, Copy)]
pub struct UsedFunc {
    name: *const c_char,
    loc: Loc,
}

pub const MAX_ERROR_COUNT: usize = 100;
/// The point of this function is to indicate that a compilation error happened, but continue the compilation anyway
/// even if the state of the Compiler became bogus. This is needed to report as many compilation errors as possible.
/// After calling this function always continue the compilation like nothing happened.
pub unsafe fn bump_error_count(c: *mut Compiler) -> Option<()> {
    (*c).error_count += 1;
    if (*c).error_count >= MAX_ERROR_COUNT {
        fprintf(stderr(), c!("TOO MANY ERRORS! Fix your program!\n"));
        return None
    }
    Some(())
}

pub unsafe fn compile_program(l: *mut Lexer, c: *mut Compiler) -> Option<()> {
    'def: loop {
        lexer::get_token(l)?;
        match (*l).token {
            Token::EOF => break 'def,
            Token::Variadic => {
                get_and_expect_token_but_continue(l, c, Token::OParen)?;
                get_and_expect_token_but_continue(l, c, Token::ID)?;
                let func = arena::strdup(&mut (*c).arena, (*l).string);
                let func_loc = (*l).loc;
                if let Some(existing_variadic) = assoc_lookup_cstr(da_slice((*c).program.variadics), func) {
                    // TODO: report all the duplicate variadics maybe?
                    diagf!(func_loc, c!("ERROR: duplicate variadic declaration `%s`\n"), func);
                    diagf!((*existing_variadic).loc, c!("NOTE: the first declaration is located here\n"));
                    bump_error_count(c)?;
                }
                get_and_expect_token_but_continue(l, c, Token::Comma)?;
                get_and_expect_token_but_continue(l, c, Token::IntLit)?;
                if (*l).int_number == 0 {
                    diagf!((*l).loc, c!("ERROR: variadic function `%s` cannot have 0 arguments\n"), func);
                    bump_error_count(c)?;
                }
                da_append(&mut (*c).program.variadics, (func, Variadic {
                    loc: func_loc,
                    fixed_args: (*l).int_number as usize,
                }));
                get_and_expect_token_but_continue(l, c, Token::CParen)?;
                get_and_expect_token_but_continue(l, c, Token::SemiColon)?;
            }
            Token::Extrn => {
                while (*l).token != Token::SemiColon {
                    get_and_expect_token(l, Token::ID)?;
                    let name = arena::strdup(&mut (*c).arena, (*l).string);
                    name_declare_if_not_exists(&mut (*c).program.extrns, name);
                    declare_var(c, name, (*l).loc, Storage::External {name})?;
                    get_and_expect_tokens(l, &[Token::SemiColon, Token::Comma])?;
                }
            }
            _ => {
                expect_token(l, Token::ID)?;
                let name = arena::strdup(&mut (*c).arena, (*l).string);
                let name_loc = (*l).loc;
                declare_var(c, name, name_loc, Storage::External{name})?;

                let saved_point = (*l).parse_point;
                lexer::get_token(l)?;

                match (*l).token {
                    Token::OParen => { // Function definition
                        scope_push(&mut (*c).vars); // begin function scope
                        let mut params_count = 0;
                        let saved_point = (*l).parse_point;
                        lexer::get_token(l)?;
                        if (*l).token != Token::CParen {
                            (*l).parse_point = saved_point;
                            'params: loop {
                                get_and_expect_token(l, Token::ID)?;
                                let name = arena::strdup(&mut (*c).arena, (*l).string);
                                let name_loc = (*l).loc;
                                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                                declare_var(c, name, name_loc, Storage::Auto{index})?;
                                params_count += 1;
                                get_and_expect_tokens(l, &[Token::CParen, Token::Comma])?;
                                match (*l).token {
                                    Token::CParen => break 'params,
                                    Token::Comma => continue 'params,
                                    _ => unreachable!(),
                                }
                            }
                        }
                        compile_statement(l, c)?;
                        scope_pop(&mut (*c).vars); // end function scope

                        for i in 0..(*c).func_gotos.count {
                            let used_label = *(*c).func_gotos.items.add(i);
                            let existing_label = find_goto_label(&(*c).func_goto_labels, used_label.name);
                            if existing_label.is_null() {
                                diagf!(used_label.loc, c!("ERROR: label `%s` used but not defined\n"), used_label.name);
                                bump_error_count(c)?;
                                continue;
                            }
                            (*(*c).func_body.items.add(used_label.addr)).opcode = Op::JmpLabel {label: (*existing_label).label};
                        }

                        da_append(&mut (*c).program.funcs, Func {
                            name,
                            name_loc,
                            body: (*c).func_body,
                            params_count,
                            auto_vars_count: (*c).auto_vars_ator.max,
                        });
                        (*c).func_body = zeroed();
                        (*c).func_goto_labels.count = 0;
                        (*c).func_gotos.count = 0;
                        (*c).auto_vars_ator = zeroed();
                        (*c).op_label_count = 0;
                    }
                    Token::Asm => { // Assembly function definition
                        let mut body: Array<AsmStmt> = zeroed();
                        compile_asm_stmts(l, c, &mut body)?;
                        da_append(&mut (*c).program.asm_funcs, AsmFunc {name, name_loc, body});
                    }
                    _ => { // Variable definition
                        (*l).parse_point = saved_point;

                        let mut global = Global {
                            name,
                            values: zeroed(),
                            is_vec: false,
                            minimum_size: 0,
                        };

                        // TODO: This code is ugly
                        // couldn't find a better way to write it while keeping accurate error messages
                        get_and_expect_tokens(l, &[Token::Minus, Token::IntLit, Token::CharLit, Token::String, Token::ID, Token::SemiColon, Token::OBracket])?;

                        if (*l).token == Token::OBracket {
                            global.is_vec = true;
                            get_and_expect_tokens(l, &[Token::IntLit, Token::CBracket])?;
                            if (*l).token == Token::IntLit {
                                global.minimum_size = (*l).int_number as usize;
                                get_and_expect_token_but_continue(l, c, Token::CBracket)?;
                            }
                            get_and_expect_tokens(l, &[Token::Minus, Token::IntLit, Token::CharLit, Token::String, Token::ID, Token::SemiColon])?;
                        }

                        while (*l).token != Token::SemiColon {
                            let value = match (*l).token {
                                Token::Minus => {
                                    get_and_expect_token(l, Token::IntLit)?;
                                    ImmediateValue::Literal(!(*l).int_number + 1)
                                }
                                Token::IntLit | Token::CharLit => ImmediateValue::Literal((*l).int_number),
                                Token::String => ImmediateValue::DataOffset(compile_string((*l).string, c)),
                                Token::ID => {
                                    let name = arena::strdup(&mut (*c).arena, (*l).string);
                                    let scope = da_last_mut(&mut (*c).vars).expect("There should be always at least the global scope");
                                    let var = find_var_near(scope, name);
                                    if var.is_null() {
                                        diagf!((*l).loc, c!("ERROR: could not find name `%s`\n"), name);
                                        bump_error_count(c)?;
                                    }
                                    ImmediateValue::Name(name)
                                }
                                _ => unreachable!()
                            };
                            da_append(&mut global.values, value);

                            get_and_expect_tokens(l, &[Token::SemiColon, Token::Comma])?;
                            if (*l).token == Token::Comma {
                                get_and_expect_tokens(l, &[Token::Minus, Token::IntLit, Token::CharLit, Token::String, Token::ID])?;
                            } else {
                                break;
                            }
                        }

                        if !global.is_vec && global.values.count == 0 {
                            da_append(&mut global.values, ImmediateValue::Literal(0));
                        }
                        da_append(&mut (*c).program.globals, global)
                    }
                }
            }
        }
    }

    Some(())
}

pub unsafe fn include_path_if_exists(input_paths: &mut Array<*const c_char>, path: *const c_char) -> Option<()> {
    if file_exists(path)? {
        da_append(input_paths, path);
    }
    Some(())
}

pub unsafe fn get_file_name(path: *const c_char) -> *const c_char {
    let p = if cfg!(target_os = "windows") {
        let p1 = strrchr(path, '/' as i32);
        let p2 = strrchr(path, '\\' as i32);
        cmp::max(p1, p2)
    } else {
        strrchr(path, '/' as i32)
    };

    if p.is_null() {
        path
    } else {
        p.add(1)
    }
}

pub unsafe fn get_file_ext(path: *const c_char) -> Option<*const c_char> {
    let p = strrchr(get_file_name(path), '.' as i32);
    if p.is_null() { return None; }
    Some(p)
}

pub unsafe fn temp_strip_file_ext(path: *const c_char) -> *const c_char {
    if let Some(ext) = get_file_ext(path) {
        temp_sprintf(c!("%.*s"), strlen(path) - strlen(ext), path)
    } else {
        path
    }
}

pub unsafe fn get_garbage_base(path: *const c_char, target: Target) -> Option<*mut c_char> {
    const GARBAGE_PATH_NAME: *const c_char = c!(".build");

    let filename = get_file_name(path);
    let parent_len = filename.offset_from(path);

    let garbage_dir = if parent_len == 0 {
        // input path is relative and does not begin with "./" or "../", like "main.b"
        GARBAGE_PATH_NAME
    } else {
        temp_sprintf(c!("%.*s%s"), parent_len, path, GARBAGE_PATH_NAME)
    };

    if !mkdir_if_not_exists(garbage_dir) { return None }

    let gitignore_path = temp_sprintf(c!("%s/.gitignore"), garbage_dir);
    if !file_exists(gitignore_path)? {
        write_entire_file(gitignore_path, c!("*") as *const c_void, 1)?;
    }

    Some(temp_sprintf(c!("%s/%s.%s"), garbage_dir, filename, target.name()))
}

pub unsafe fn print_available_targets() {
    fprintf(stderr(), c!("Compilation targets:\n"));
    for i in 0..TARGET_ORDER.len() {
        fprintf(stderr(), c!("    %s\n"), (*TARGET_ORDER)[i].name());
    }
}

pub unsafe fn main(mut argc: i32, mut argv: *mut*mut c_char) -> Option<()> {
    let default_target;
    if cfg!(target_arch = "aarch64") && (cfg!(target_os = "linux") || cfg!(target_os = "android")) {
        default_target = Some(Target::Gas_AArch64_Linux);
    } else if cfg!(target_arch = "aarch64") && cfg!(target_os = "macos") {
        default_target = Some(Target::Gas_AArch64_Darwin);
    } else if cfg!(target_arch = "x86_64") && cfg!(target_os = "linux") {
        default_target = Some(Target::Gas_x86_64_Linux);
    } else if cfg!(target_arch = "x86_64") && cfg!(target_os = "windows") {
        default_target = Some(Target::Gas_x86_64_Windows);
    } else {
        default_target = None;
    }

    let default_target_name = if let Some(default_target) = default_target {
        default_target.name()
    } else {
        ptr::null()
    };

    let target_name = flag_str(c!("t"), default_target_name, c!("Compilation target. Pass \"list\" to get the list of available targets."));
    let output_path = flag_str(c!("o"), ptr::null(), c!("Output path"));
    let run         = flag_bool(c!("run"), false, c!("Run the compiled program (if applicable for the target)"));
    let nobuild  = flag_bool(c!("nobuild"), false, temp_sprintf(c!("Skip the build step. Useful in conjunction with the -%s flag when you already have a built program and just want to run it on the specified target without rebuilding it."), flag_name(run)));
    let help        = flag_bool(c!("help"), false, c!("Print this help message"));
    let codegen_args = flag_list(CODEGEN_FLAG_NAME, temp_sprintf(c!("Pass an argument to the codegen of the current target selected by the -%s flag. Pass argument `-%s help` to learn more about what current codegen provides. All sorts of linker flag parameters are probably there."), flag_name(target_name), CODEGEN_FLAG_NAME));
    let linker = {
        let name = c!("L");
        flag_list(name, temp_sprintf(c!("DEPRECATED! Append a flag to the linker of the target platform. But not every target even has a linker! For backward compatibility we transform `-%s foo -%s bar -%s ...` into `-%s link-args='foo bar ...'` but do not expect every codegen to support that. Use `-%s help` to learn more about what your current codegen supports. Expect -%s to be removed entirely in the future."), name, name, name, CODEGEN_FLAG_NAME, CODEGEN_FLAG_NAME, name))
    };
    let nostdlib    = flag_bool(c!("nostdlib"), false, c!("Do not link with standard libraries like libb and/or libc on some platforms"));
    let ir          = flag_bool(c!("ir"), false, c!("Instead of compiling, dump the IR of the program to stdout"));
    let historical  = flag_bool(c!("hist"), false, c!("Makes the compiler strictly follow the description of the B language from the \"Users' Reference to B\" by Ken Thompson as much as possible"));
    let quiet       = flag_bool(c!("q"), false, c!("Makes the compiler yap less about what it's doing"));
    let debug       = flag_bool(c!("g"), false, c!("Add debug information to the compiled program (if applicable for the target)"));

    let mut input_paths: Array<*const c_char> = zeroed();
    let mut run_args: Array<*const c_char> = zeroed();
    'args: while argc > 0 {
        if !flag_parse(argc, argv) {
            usage();
            flag_print_error(stderr());
            return None;
        }
        argc = flag_rest_argc();
        argv = flag_rest_argv();
        if argc > 0 {
            if strcmp(*argv, c!("--")) == 0 {
                da_append_many(&mut run_args, slice::from_raw_parts(argv.add(1) as *const*const c_char, (argc - 1) as usize));
                break 'args;
            } else {
                da_append(&mut input_paths, shift!(argv, argc));
            }
        }
    }

    if *quiet {
        minimal_log_level = Log_Level::WARNING;
    }

    if *help {
        usage();
        return None;
    }

    if (*target_name).is_null() {
        usage();
        log(Log_Level::ERROR, c!("No value is provided for -%s flag."), flag_name(target_name));
        return None;
    }

    if strcmp(*target_name, c!("list")) == 0 {
        print_available_targets();
        return Some(());
    }

    let Some(target) = Target::by_name(*target_name) else {
        usage();
        print_available_targets();
        log(Log_Level::ERROR, c!("Unknown target `%s`"), *target_name);
        return None;
    };

    let mut c: Compiler = zeroed();
    c.target = target;
    c.historical = *historical;

    if (*linker).count > 0 {
        let mut s: Shlex = zeroed();
        for i in 0..(*linker).count {
            shlex_append_quoted(&mut s, *(*linker).items.add(i));
        }
        let codegen_arg = temp_sprintf(c!("link-args=%s"), shlex_join(&mut s));
        da_append(codegen_args, codegen_arg);
        shlex_free(&mut s);
        log(Log_Level::WARNING, c!("Flag -%s is DEPRECATED! Interpreting it as `-%s %s` instead."), flag_name(linker), CODEGEN_FLAG_NAME, codegen_arg);
    }

    let gen = match target {
        Target::Gas_x86_64_Linux   |
        Target::Gas_x86_64_Windows |
        Target::Gas_x86_64_Darwin  => codegen::gas_x86_64::new(&mut c.arena, da_slice(*codegen_args)),
        Target::Gas_AArch64_Linux  |
        Target::Gas_AArch64_Darwin => codegen::gas_aarch64::new(&mut c.arena, da_slice(*codegen_args)),
        Target::Uxn                => codegen::uxn::new(&mut c.arena, da_slice(*codegen_args)),
        Target::Mos6502_Posix      => codegen::mos6502::new(&mut c.arena, da_slice(*codegen_args)),
        Target::ILasm_Mono         => codegen::ilasm_mono::new(&mut c.arena, da_slice(*codegen_args)),
    }?;

    if input_paths.count == 0 {
        usage();
        log(Log_Level::ERROR, c!("no inputs are provided"));
        return None;
    }

    if !*nobuild {
        if !*nostdlib {
            // TODO: should be probably a list libb paths which we sequentually probe to find which one exists.
            //   And of course we should also enable the user to append additional paths via the command line.
            //   Paths to potentially check by default:
            //   - Current working directory (like right now)
            //   - Directory where the b executable resides
            //   - Some system paths like /usr/include/libb on Linux? (Not 100% sure about this one)
            //   - Some sort of instalation prefix? (Requires making build system more complicated)
            //
            //     - rexim (2025-06-12 20:56:08)
            let libb_path = c!("./libb");
            if !file_exists(libb_path)? {
                log(Log_Level::ERROR, c!("No standard library path %s found. Please run the compiler from the same folder where %s is located. Or if you don't want to use the standard library pass the -%s flag."), libb_path, libb_path, flag_name(nostdlib));
                return None;
            }
            include_path_if_exists(&mut input_paths, arena::sprintf(&mut c.arena, c!("%s/all.b"), libb_path));
            include_path_if_exists(&mut input_paths, arena::sprintf(&mut c.arena, c!("%s/%s.b"), libb_path, *target_name));
        }

        let mut sb: String_Builder = zeroed();
        for i in 0..input_paths.count {
            let input_path = *input_paths.items.add(i);
            if i > 0 { sb_appendf(&mut sb, c!(", ")); }
            sb_appendf(&mut sb, c!("%s"), input_path);
        }
        da_append(&mut sb, 0);
        log(Log_Level::INFO, c!("compiling %zu files: %s"), input_paths.count, sb.items);

        let compilation_start = Instant::now();

        let mut input: String_Builder = zeroed();

        scope_push(&mut c.vars);          // begin global scope

        for i in 0..input_paths.count {
            let input_path = *input_paths.items.add(i);

            input.count = 0;
            read_entire_file(input_path, &mut input)?;

            let mut l: Lexer = lexer::new(input_path, input.items, input.items.add(input.count), *historical);

            compile_program(&mut l, &mut c)?;
        }

        for i in 0..c.used_funcs.count {
            let used_global = *c.used_funcs.items.add(i);

            if find_var_deep(&mut c.vars, used_global.name).is_null() {
                diagf!(used_global.loc, c!("ERROR: could not find name `%s`\n"), used_global.name);
                bump_error_count(&mut c)?;
            }
        }

        scope_pop(&mut c.vars);          // end global scope

        if c.error_count > 0 {
            return None;
        }

        log(Log_Level::INFO, c!("compilation took %.3fs"), compilation_start.elapsed().as_secs_f64());
    }

    let mut output: String_Builder = zeroed();
    let mut cmd: Cmd = zeroed();

    if *ir {
        dump_program(&mut output, &c.program);
        da_append(&mut output, 0);
        printf(c!("%s"), output.items);
        if *nobuild {
            printf(c!("You provided -%s along with -%s. So this is your IR dump of a program that was never built. Enjoy!\n"), flag_name(ir), flag_name(nobuild));
        }
        return Some(())
    }

    let program_path = if (*output_path).is_null() {
        temp_sprintf(c!("%s%s"), temp_strip_file_ext(*input_paths.items), target.file_ext())
    } else {
        if get_file_ext(*output_path).is_some() {
            *output_path
        } else {
            temp_sprintf(c!("%s%s"), *output_path, target.file_ext())
        }
    };

    // Compiler may produce lots of intermediate files (assembly,
    // object, etc) also known collectively as "garbage". We are
    // trying to keep the garbage away from the user in a separate
    // folder. But not delete it, because it's important for
    // transparency! `garbase_base` is a path that has a format
    // "/path/to/garbase/folder/base". It must be concatenated with an
    // approriate suffix to get a file path where the compiler can
    // output a garbage file without worrying about colliding with
    // other garbage files produced by compilations of other programs.
    //
    // Let's say you want to output an object file somewhere. The path
    // to that object should be computed as `temp_sprintf("%s.o", garbase_base)`.
    let garbage_base = get_garbage_base(program_path, target)?;

    match target {
        Target::Gas_AArch64_Linux => {
            let os = targets::Os::Linux;

            if !*nobuild {
                codegen::gas_aarch64::generate_program(
                    gen, &c.program, program_path, garbage_base, os,
                    *nostdlib, *debug,
                )?;
            }

            if *run {
                codegen::gas_aarch64::run_program(
                    gen, program_path, da_slice(run_args), os,
                )?;
            }
        }
        Target::Gas_AArch64_Darwin => {
            let os = targets::Os::Darwin;

            if !*nobuild {
                codegen::gas_aarch64::generate_program(
                    gen, &c.program, program_path, garbage_base, os,
                    *nostdlib, *debug,
                )?;
            }

            if *run {
                codegen::gas_aarch64::run_program(
                    gen, program_path, da_slice(run_args), os,
                )?;
            }
        }
        Target::Gas_x86_64_Linux => {
            let os = targets::Os::Linux;

            if !*nobuild {
                codegen::gas_x86_64::generate_program(
                    // Inputs
                    gen, &c.program, program_path, garbage_base, os,
                    *nostdlib, *debug,
                    // Temporaries
                    &mut output, &mut cmd,
                )?;
            }

            if *run {
                codegen::gas_x86_64::run_program(
                    // Inputs
                    gen, program_path, da_slice(run_args), os,
                    // Temporaries
                    &mut cmd,
                )?;
            }
        }
        Target::Gas_x86_64_Windows => {
            let os = targets::Os::Windows;

            if !*nobuild {
                codegen::gas_x86_64::generate_program(
                    // Inputs
                    gen, &c.program, program_path, garbage_base, os,
                    *nostdlib, *debug,
                    // Temporaries
                    &mut output, &mut cmd,
                )?;
            }

            if *run {
                codegen::gas_x86_64::run_program(
                    // Inputs
                    gen, program_path, da_slice(run_args), os,
                    // Temporaries
                    &mut cmd,
                )?;
            }
        }
        Target::Gas_x86_64_Darwin => {
            let os = targets::Os::Darwin;

            if !*nobuild {
                codegen::gas_x86_64::generate_program(
                    // Inputs
                    gen, &c.program, program_path, garbage_base, os,
                    *nostdlib, *debug,
                    // Temporaries
                    &mut output, &mut cmd,
                )?;
            }

            if *run {
                codegen::gas_x86_64::run_program(
                    // Inputs
                    gen, program_path, da_slice(run_args), os,
                    // Temporaries
                    &mut cmd,
                )?;
            }
        }
        Target::Uxn => {
            if !*nobuild {
                codegen::uxn::generate_program(
                    // Inputs
                    gen, &c.program, program_path, garbage_base,
                    *nostdlib, *debug,
                    // Temporaries
                    &mut output, &mut cmd,
                )?;
            }

            if *run {
                codegen::uxn::run_program(
                    // Inputs
                    gen, program_path, da_slice(run_args),
                    // Temporaries
                    &mut cmd,
                )?;
            }
        }
        Target::Mos6502_Posix => {
            if !*nobuild {
                codegen::mos6502::generate_program(
                    // Inputs
                    gen, &c.program, program_path, garbage_base,
                    *nostdlib, *debug,
                    // Temporaries
                    &mut output, &mut cmd,
                )?;
            }

            if *run {
                codegen::mos6502::run_program(
                    // Inputs
                    gen, program_path, da_slice(run_args),
                    // Temporaries
                    &mut cmd,
                )?;
            }
        }
        Target::ILasm_Mono => {
            if !*nobuild {
                codegen::ilasm_mono::generate_program(
                    // Inputs
                    gen, &c.program, program_path, garbage_base,
                    *nostdlib, *debug,
                    // Temporaries
                    &mut output, &mut cmd,
                )?;
            }

            if *run {
                codegen::ilasm_mono::run_program(
                    // Inputs
                    gen, program_path, da_slice(run_args),
                    // Temporaries
                    &mut cmd,
                )?;
            }
        }
    }
    Some(())
}
