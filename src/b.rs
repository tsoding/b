#![no_main]
#![no_std]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_macros)]

#[macro_use]
pub mod nob;
pub mod stb_c_lexer;
#[macro_use]
pub mod flag;
#[macro_use]
pub mod crust;
pub mod arena;
pub mod codegen;

use core::ffi::*;
use core::mem::zeroed;
use core::ptr;
use core::slice;
use nob::*;
use stb_c_lexer::*;
use flag::*;
use crust::libc::*;
use arena::Arena;
use codegen::{Target, name_of_target, TARGET_NAMES, target_by_name};

macro_rules! diagf {
    ($l:expr, $path:expr, $where:expr, $($args:tt)*) => {{
        let mut loc: stb_lex_location = zeroed();
        stb_c_lexer_get_location($l, $where, &mut loc);
        fprintf(stderr, c!("%s:%d:%d: "), $path, loc.line_number, loc.line_offset + 1);
        fprintf(stderr, $($args)*);
    }};
}

macro_rules! missingf {
    ($l:expr, $path:expr, $where:expr, $($args:tt)*) => {{
        let file = file!();
        let mut loc: stb_lex_location = zeroed();
        stb_c_lexer_get_location($l, $where, &mut loc);
        fprintf(stderr, c!("%s:%d:%d: TODO: "), $path, loc.line_number, loc.line_offset + 1);
        fprintf(stderr, $($args)*);
        fprintf(stderr, c!("%.*s:%d: INFO: implementation should go here\n"), file.len(), file.as_ptr(), line!());
        abort();
    }}
}

unsafe fn display_token_kind_temp(token: c_long) -> *const c_char {
    match token {
        CLEX_id         => c!("identifier"),
        CLEX_eq         => c!("=="),
        CLEX_noteq      => c!("!="),
        CLEX_lesseq     => c!("<="),
        CLEX_greatereq  => c!(">="),
        CLEX_andand     => c!("&&"),
        CLEX_oror       => c!("||"),
        CLEX_shl        => c!("<<"),
        CLEX_shr        => c!(">>"),
        CLEX_plusplus   => c!("++"),
        CLEX_minusminus => c!("--"),
        CLEX_arrow      => c!("->"),
        CLEX_andeq      => c!("&="),
        CLEX_oreq       => c!("|="),
        CLEX_xoreq      => c!("^="),
        CLEX_pluseq     => c!("+="),
        CLEX_minuseq    => c!("-="),
        CLEX_muleq      => c!("*="),
        CLEX_diveq      => c!("/="),
        CLEX_modeq      => c!("%%="),
        CLEX_shleq      => c!("<<="),
        CLEX_shreq      => c!(">>="),
        CLEX_eqarrow    => c!("=>"),
        CLEX_dqstring   => c!("string literal"),
        // NOTE: single quote strings are opt-in in stb_c_lexer.h (see STB_C_LEX_C_SQ_STRINGS)
        CLEX_sqstring   => c!("single quote literal"),
        CLEX_charlit    => c!("character literal"),
        CLEX_intlit     => c!("integer literal"),
        CLEX_floatlit   => c!("floating-point literal"),
        CLEX_eof        => c!("end of file"),
        _ => {
            if token >= 0 && token < 256 {
                temp_sprintf(c!("`%c`"), token)
            } else {
                temp_sprintf(c!("<<<UNKNOWN TOKEN %ld>>>"), token)
            }
        }
    }
}

pub unsafe fn expect_clexes(l: *const stb_lexer, input_path: *const c_char, clexes: *const [i64]) -> Option<()> {
    for i in 0..clexes.len() {
        if (*clexes)[i] == (*l).token {
            return Some(());
        }
    }

    let mut sb: String_Builder = zeroed();
    for i in 0..clexes.len() {
        if i > 0 {
            if i + 1 >= clexes.len() {
                sb_appendf(&mut sb, c!(", or "));
            } else {
                sb_appendf(&mut sb, c!(", "));
            }
        }
        sb_appendf(&mut sb, c!("%s"), display_token_kind_temp((*clexes)[i]));
    }
    da_append(&mut sb, 0);

    diagf!(l, input_path, (*l).where_firstchar, c!("ERROR: expected %s, but got %s\n"), sb.items, display_token_kind_temp((*l).token));

    free(sb.items);
    None
}

pub unsafe fn expect_clex(l: *const stb_lexer, input_path: *const c_char, clex: i64) -> Option<()> {
    expect_clexes(l, input_path, &[clex])
}

pub unsafe fn get_and_expect_clex(l: *mut stb_lexer, input_path: *const c_char, clex: c_long) -> Option<()> {
    stb_c_lexer_get_token(l);
    expect_clex(l, input_path, clex)
}

pub unsafe fn expect_clex_id(l: *const stb_lexer, input_path: *const c_char, id: *const c_char) -> Option<()> {
    expect_clex(l, input_path, CLEX_id)?;
    if strcmp((*l).string, id) != 0 {
        diagf!(l, input_path, (*l).where_firstchar, c!("ERROR: expected `%s`, but got `%s`\n"), id, (*l).string);
        return None;
    }
    Some(())
}

pub unsafe fn get_and_expect_clex_id(l: *mut stb_lexer, input_path: *const c_char, id: *const c_char) -> Option<()> {
    stb_c_lexer_get_token(l);
    expect_clex_id(l, input_path, id)
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
    pub hwere: *const c_char,
    pub storage: Storage,
}

pub unsafe fn scope_push(vars: *mut Array<Array<Var>>) {
    if (*vars).count < (*vars).capacity {
        // Reusing already allocated scopes
        (*vars).count += 1;
        (*da_last_mut(vars)).count = 0;
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

pub unsafe fn declare_var(l: *mut stb_lexer, input_path: *const c_char, vars: *mut Array<Array<Var>>, name: *const c_char, name_where: *const c_char, storage: Storage) -> Option<()> {
    let scope = da_last_mut(vars);
    let existing_var = find_var_near(scope, name);
    if !existing_var.is_null() {
        diagf!(l, input_path, name_where, c!("ERROR: redefinition of variable `%s`\n"), name);
        diagf!(l, input_path, (*existing_var).hwere, c!("NOTE: the first declaration is located here\n"));
        return None;
    }

    da_append(scope, Var {name, storage, hwere: name_where});
    Some(())
}

const B_KEYWORDS: *const [*const c_char] = &[
    c!("auto"),
    c!("extrn"),
    c!("case"),
    c!("if"),
    c!("while"),
    c!("switch"),
    c!("goto"),
    c!("return"),
];

unsafe fn is_keyword(name: *const c_char) -> bool {
    for i in 0..B_KEYWORDS.len() {
        if strcmp((*B_KEYWORDS)[i], name) == 0 {
            return true
        }
    }
    false
}

#[derive(Clone, Copy)]
pub enum Arg {
    AutoVar(usize),
    Ref(usize),
    External(*const c_char),
    Literal(i64),
    DataOffset(usize),
}

// TODO: add support for binary division expression
#[derive(Clone, Copy, PartialEq)]
pub enum Binop {
    Assign,
    AssignPlus,
    AssignMult,
    Plus,
    Minus,
    Mult,
    Mod,
    Less,
    Equal,
    NotEqual,
    GreaterEqual,
    BitOr,
    BitAnd,
    BitShl,
    BitShr,
    AssignBitOr,
    AssignBitAnd,
    AssignBitShl,
}

// The higher the index of the row in this table the higher the precedence of the Binop
pub const PRECEDENCE: *const [*const [Binop]] = &[
    // Precedence 0 is reserved for assignment operators which are always bind right to left.
    // The reset of the operators bind left to right.
    &[Binop::Assign, Binop::AssignPlus, Binop::AssignMult, Binop::AssignBitOr, Binop::AssignBitAnd, Binop::AssignBitShl],
    &[Binop::BitOr],
    &[Binop::BitAnd],
    &[Binop::BitShl, Binop::BitShr],
    &[Binop::Equal, Binop::NotEqual],
    &[Binop::Less, Binop::GreaterEqual],
    &[Binop::Plus, Binop::Minus],
    &[Binop::Mult, Binop::Mod],
];

impl Binop {
    pub fn from_token(token: c_long) -> Option<Self> {
        match token {
            token if token == '+' as i64 => Some(Binop::Plus),
            token if token == '-' as i64 => Some(Binop::Minus),
            token if token == '*' as i64 => Some(Binop::Mult),
            token if token == '%' as i64 => Some(Binop::Mod),
            token if token == '<' as i64 => Some(Binop::Less),
            CLEX_greatereq               => Some(Binop::GreaterEqual),
            token if token == '=' as i64 => Some(Binop::Assign),
            token if token == '|' as i64 => Some(Binop::BitOr),
            token if token == '&' as i64 => Some(Binop::BitAnd),
            CLEX_shleq                   => Some(Binop::AssignBitShl),
            CLEX_oreq                    => Some(Binop::AssignBitOr),
            CLEX_andeq                   => Some(Binop::AssignBitAnd),
            CLEX_pluseq                  => Some(Binop::AssignPlus),
            CLEX_muleq                   => Some(Binop::AssignMult),
            CLEX_shl                     => Some(Binop::BitShl),
            CLEX_shr                     => Some(Binop::BitShr),
            CLEX_eq                      => Some(Binop::Equal),
            CLEX_noteq                   => Some(Binop::NotEqual),
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

// TODO: associate location within the source code with each op
#[derive(Clone, Copy)]
pub enum Op {
    UnaryNot       {result: usize, arg: Arg},
    Negate         {result: usize, arg: Arg},
    Add            {index: usize, lhs: Arg, rhs: Arg},
    Sub            {index: usize, lhs: Arg, rhs: Arg},
    Mul            {index: usize, lhs: Arg, rhs: Arg},
    // TODO: Maybe we should have something like DivMod instruction because many CPUs just do div and mod simultaneously
    Mod            {index: usize, lhs: Arg, rhs: Arg},
    Less           {index: usize, lhs: Arg, rhs: Arg},
    Equal          {index: usize, lhs: Arg, rhs: Arg},
    NotEqual       {index: usize, lhs: Arg, rhs: Arg},
    GreaterEqual   {index: usize, lhs: Arg, rhs: Arg},
    BitOr          {index: usize, lhs: Arg, rhs: Arg},
    BitAnd         {index: usize, lhs: Arg, rhs: Arg},
    BitShl         {index: usize, lhs: Arg, rhs: Arg},
    BitShr         {index: usize, lhs: Arg, rhs: Arg},
    AutoAssign     {index: usize, arg: Arg},
    ExternalAssign {name: *const c_char, arg: Arg},
    Store          {index: usize, arg: Arg},
    Funcall        {result: usize, name: *const c_char, args: Array<Arg>},
    Jmp            {addr: usize},
    JmpIfNot       {addr: usize, arg: Arg},
}

pub unsafe fn align_bytes(bytes: usize, alignment: usize) -> usize {
    let rem = bytes%alignment;
    if rem > 0 {
        bytes + alignment - rem
    } else {
        bytes
    }
}

/// Allocator of Auto Vars
#[derive(Clone, Copy)]
pub struct AutoVarsAtor {
    /// How many autovars currently allocated
    pub count: usize,
    /// Maximum allocated autovars throughout the function body
    pub max: usize,
}

pub unsafe fn allocate_auto_var(t: *mut AutoVarsAtor) -> usize {
    (*t).count += 1;
    if (*t).count > (*t).max {
        (*t).max = (*t).count;
    }
    (*t).count
}

pub unsafe fn compile_primary_expression(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler) -> Option<(Arg, bool)> {
    stb_c_lexer_get_token(l);
    match (*l).token {
        token if token == '(' as i64 => {
            let result = compile_expression(l, input_path, c)?;
            get_and_expect_clex(l, input_path, ')' as i64)?;
            Some(result)
        }
        token if token == '!' as i64 => {
            let (arg, _) = compile_primary_expression(l, input_path, c)?;
            let result = allocate_auto_var(&mut (*c).auto_vars_ator);
            da_append(&mut (*c).func_body, Op::UnaryNot{result, arg});
            Some((Arg::AutoVar(result), false))
        }
        token if token == '*' as i64 => {
            let (arg, _) = compile_primary_expression(l, input_path, c)?;
            let index = allocate_auto_var(&mut (*c).auto_vars_ator);
            da_append(&mut (*c).func_body, Op::AutoAssign {index, arg});
            Some((Arg::Ref(index), true))
        }
        token if token == '-' as i64 => {
            let (arg, _) = compile_primary_expression(l, input_path, c)?;
            let index = allocate_auto_var(&mut (*c).auto_vars_ator);
            da_append(&mut (*c).func_body, Op::Negate {result: index, arg});
            Some((Arg::AutoVar(index), false))
        }
        CLEX_intlit => Some((Arg::Literal((*l).int_number), false)),
        CLEX_id => {
            let name = arena::strdup(&mut (*c).arena, (*l).string);
            let name_where = (*l).where_firstchar;

            let var_def = find_var_deep(&mut (*c).vars, name);
            if var_def.is_null() {
                diagf!(l, input_path, name_where, c!("ERROR: could not find name `%s`\n"), name);
                return None;
            }

            let saved_point = (*l).parse_point;
            stb_c_lexer_get_token(l);

            if (*l).token == '(' as i64 {
                Some((compile_function_call(l, input_path, c, name, name_where)?, false))
            } else {
                (*l).parse_point = saved_point;
                match (*var_def).storage {
                    Storage::Auto{index} => Some((Arg::AutoVar(index), true)),
                    Storage::External{name} => Some((Arg::External(name), true)),
                }
            }
        }
        CLEX_dqstring => {
            let offset = (*c).data.count;
            let string_len = strlen((*l).string);
            da_append_many(&mut (*c).data, slice::from_raw_parts((*l).string as *const u8, string_len));
            // TODO: Strings in B are not NULL-terminated.
            // They are terminated with symbol '*e' ('*' is escape character akin to '\' in C) which according to the
            // spec is called just "end-of-file" without any elaboration on what its value is. Maybe it had a specific
            // value on PDP that was a common knowledge at the time? In any case that breaks compatibility with
            // libc. While the language is still in development we gonna terminate it with 0. We will make it
            // "spec complaint" later.
            da_append(&mut (*c).data, 0); // NULL-terminator
            Some((Arg::DataOffset(offset), false))
        }
        _ => {
            missingf!(l, input_path, (*l).where_firstchar, c!("Unexpected token %s not all expressions are implemented yet\n"), display_token_kind_temp((*l).token));
        }
    }
}

pub unsafe fn compile_binop_expression(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler, precedence: usize) -> Option<(Arg, bool)> {
    if precedence >= Binop::MAX_PRECEDENCE {
        return compile_primary_expression(l, input_path, c);
    }

    let (mut lhs, mut lvalue) = compile_binop_expression(l, input_path, c, precedence + 1)?;

    let mut saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);

    if let Some(binop) = Binop::from_token((*l).token) {
        if binop.precedence() == precedence {
            while let Some(binop) = Binop::from_token((*l).token) {
                if binop.precedence() != precedence { break; }

                let token = (*l).token;
                let binop_where = (*l).where_firstchar;
                let (rhs, _) = if precedence == 0 {
                    // This is an assignment operator. Binding right to left.
                    compile_binop_expression(l, input_path, c, precedence)?
                } else {
                    compile_binop_expression(l, input_path, c, precedence + 1)?
                };

                match Binop::from_token(token).unwrap() {
                    Binop::BitShl => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::BitShl {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::BitShr => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::BitShr {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::BitOr => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::BitOr {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::BitAnd => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::BitAnd {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::Plus  => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::Add  {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::Minus => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::Sub  {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::Mult  => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::Mul  {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::Mod  => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::Mod {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::Less  => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::Less {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::Equal => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::Equal {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::NotEqual => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body, Op::NotEqual {index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::GreaterEqual  => {
                        let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                        da_append(&mut (*c).func_body,  Op::GreaterEqual{index, lhs, rhs});
                        lhs = Arg::AutoVar(index);
                    }
                    Binop::AssignBitOr => {
                        if !lvalue {
                            diagf!(l, input_path, binop_where, c!("ERROR: cannot assign to rvalue\n"));
                            return None;
                        }

                        match lhs {
                            Arg::Ref(index) => {
                                let tmp = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::BitOr {index: tmp, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::Store {index, arg: Arg::AutoVar(tmp)});
                            },
                            Arg::External(name) => {
                                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::BitOr {index, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::ExternalAssign {name, arg: Arg::AutoVar(index)})
                            }
                            Arg::AutoVar(index) => {
                                da_append(&mut (*c).func_body, Op::BitOr {index, lhs, rhs})
                            }
                            Arg::Literal(_) | Arg::DataOffset(_) => unreachable!(),
                        }
                    }
                    Binop::AssignBitAnd => {
                        if !lvalue {
                            diagf!(l, input_path, binop_where, c!("ERROR: cannot assign to rvalue\n"));
                            return None;
                        }

                        match lhs {
                            Arg::Ref(index) => {
                                let tmp = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::BitAnd {index: tmp, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::Store {index, arg: Arg::AutoVar(tmp)});
                            },
                            Arg::External(name) => {
                                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::BitAnd {index, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::ExternalAssign {name, arg: Arg::AutoVar(index)})
                            }
                            Arg::AutoVar(index) => {
                                da_append(&mut (*c).func_body, Op::BitAnd {index, lhs, rhs})
                            }
                            Arg::Literal(_) | Arg::DataOffset(_) => unreachable!(),
                        }
                    }
                    Binop::AssignBitShl => {
                        if !lvalue {
                            diagf!(l, input_path, binop_where, c!("ERROR: cannot assign to rvalue\n"));
                            return None;
                        }

                        match lhs {
                            Arg::Ref(index) => {
                                let tmp = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::BitShl {index: tmp, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::Store {index, arg: Arg::AutoVar(tmp)});
                            },
                            Arg::External(name) => {
                                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::BitShl {index, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::ExternalAssign {name, arg: Arg::AutoVar(index)})
                            }
                            Arg::AutoVar(index) => {
                                da_append(&mut (*c).func_body, Op::BitShl {index, lhs, rhs})
                            }
                            Arg::Literal(_) | Arg::DataOffset(_) => unreachable!(),
                        }
                    }
                    Binop::AssignPlus => {
                        if !lvalue {
                            diagf!(l, input_path, binop_where, c!("ERROR: cannot assign to rvalue\n"));
                            return None;
                        }

                        match lhs {
                            Arg::Ref(index) => {
                                let tmp = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::Add {index: tmp, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::Store {index, arg: Arg::AutoVar(tmp)});
                            },
                            Arg::External(name) => {
                                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::Add {index, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::ExternalAssign {name, arg: Arg::AutoVar(index)})
                            }
                            Arg::AutoVar(index) => {
                                da_append(&mut (*c).func_body, Op::Add {index, lhs, rhs})
                            }
                            Arg::Literal(_) | Arg::DataOffset(_) => unreachable!(),
                        }
                    }
                    Binop::AssignMult => {
                        if !lvalue {
                            diagf!(l, input_path, binop_where, c!("ERROR: cannot assign to rvalue\n"));
                            return None;
                        }

                        match lhs {
                            Arg::Ref(index) => {
                                let tmp = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::Mul {index: tmp, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::Store {index, arg: Arg::AutoVar(tmp)});
                            },
                            Arg::External(name) => {
                                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                                da_append(&mut (*c).func_body, Op::Mul {index, lhs, rhs});
                                da_append(&mut (*c).func_body, Op::ExternalAssign {name, arg: Arg::AutoVar(index)})
                            }
                            Arg::AutoVar(index) => {
                                da_append(&mut (*c).func_body, Op::Mul {index, lhs, rhs})
                            }
                            Arg::Literal(_) | Arg::DataOffset(_) => unreachable!(),
                        }
                    }
                    Binop::Assign => {
                        if !lvalue {
                            diagf!(l, input_path, binop_where, c!("ERROR: cannot assign to rvalue\n"));
                            return None;
                        }

                        match lhs {
                            Arg::Ref(index) => {
                                da_append(&mut (*c).func_body, Op::Store {index, arg: rhs});
                            }
                            Arg::External(name) => {
                                da_append(&mut (*c).func_body, Op::ExternalAssign {name, arg: rhs});
                            }
                            Arg::AutoVar(index) => {
                                da_append(&mut (*c).func_body, Op::AutoAssign {index, arg: rhs});
                            }
                            Arg::Literal(_) | Arg::DataOffset(_) => unreachable!(),
                        }
                    }
                }
                lvalue = false;

                saved_point = (*l).parse_point;
                stb_c_lexer_get_token(l);
            }
        }
    }

    (*l).parse_point = saved_point;
    Some((lhs, lvalue))
}

pub unsafe fn compile_expression(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler) -> Option<(Arg, bool)> {
    compile_binop_expression(l, input_path, c, 0)
}

pub unsafe fn compile_block(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler) -> Option<()> {
    loop {
        let saved_point = (*l).parse_point;
        stb_c_lexer_get_token(l);
        if (*l).token == '}' as c_long { return Some(()); }
        (*l).parse_point = saved_point;

        compile_statement(l, input_path, c)?
    }
}

pub unsafe fn compile_function_call(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler, name: *const c_char, name_where: *const c_char) -> Option<Arg> {
    let var_def = find_var_deep(&(*c).vars, name);
    if var_def.is_null() {
        diagf!(l, input_path, name_where, c!("ERROR: could not find function `%s`\n"), name);
        return None;
    }

    let mut args: Array<Arg> = zeroed();
    let saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);
    if (*l).token != ')' as c_long {
        (*l).parse_point = saved_point;
        loop {
            let (expr, _) = compile_expression(l, input_path, c)?;
            da_append(&mut args, expr);
            stb_c_lexer_get_token(l);
            expect_clexes(l, input_path, &[')' as c_long, ',' as c_long])?;
            if (*l).token == ')' as c_long {
                break;
            } else if (*l).token == ',' as c_long {
                continue;
            } else {
                unreachable!();
            }
        }
    }

    match (*var_def).storage {
        Storage::External{name} => {
            let result = allocate_auto_var(&mut (*c).auto_vars_ator);
            da_append(&mut (*c).func_body, Op::Funcall {result, name, args});
            Some(Arg::AutoVar(result))
        }
        Storage::Auto{..} => {
            missingf!(l, input_path, name_where, c!("calling functions from auto variables\n"));
        }
    }
}

pub unsafe fn name_declare_if_not_exists(names: *mut Array<*const c_char>, name: *const c_char) {
    for i in 0..(*names).count {
        if strcmp(*(*names).items.add(i), name) == 0 {
            return;
        }
    }
    da_append(names, name)
}

pub unsafe fn compile_statement(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler) -> Option<()> {
    let saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);

    if (*l).token == '{' as c_long {
        scope_push(&mut (*c).vars);
            let saved_auto_vars_count = (*c).auto_vars_ator.count;
                compile_block(l, input_path, c)?;
            (*c).auto_vars_ator.count = saved_auto_vars_count;
        scope_pop(&mut (*c).vars);
        Some(())
    } else {
        if (*l).token == CLEX_id && (strcmp((*l).string, c!("extrn")) == 0 || strcmp((*l).string, c!("auto")) == 0) {
            let extrn = strcmp((*l).string, c!("extrn")) == 0;
            'vars: loop {
                get_and_expect_clex(l, input_path, CLEX_id)?;
                let name = arena::strdup(&mut (*c).arena, (*l).string);
                let name_where = (*l).where_firstchar;
                let storage = if extrn {
                    name_declare_if_not_exists(&mut (*c).extrns, name);
                    Storage::External{name}
                } else {
                    let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                    Storage::Auto{index}
                };
                declare_var(l, input_path, &mut (*c).vars, name, name_where, storage)?;
                stb_c_lexer_get_token(l);
                expect_clexes(l, input_path, &[',' as c_long, ';' as c_long])?;
                if (*l).token == ';' as c_long {
                    break 'vars;
                } else if (*l).token == ',' as c_long {
                    continue 'vars;
                } else {
                    unreachable!()
                }
            }

            Some(())
        } else if (*l).token == CLEX_id && strcmp((*l).string, c!("if")) == 0 {
            get_and_expect_clex(l, input_path, '(' as c_long)?;
            let saved_auto_vars_count = (*c).auto_vars_ator.count;
            let (cond, _) = compile_expression(l, input_path, c)?;
            get_and_expect_clex(l, input_path, ')' as c_long)?;

            let addr_condition = (*c).func_body.count;
            da_append(&mut (*c).func_body, Op::JmpIfNot{addr: 0, arg: cond});
            (*c).auto_vars_ator.count = saved_auto_vars_count;

            compile_statement(l, input_path, c)?;

            let saved_point = (*l).parse_point;
            stb_c_lexer_get_token(l);

            if (*l).token == CLEX_id && strcmp((*l).string, c!("else")) == 0 {
                let addr_skips_else = (*c).func_body.count;
                da_append(&mut (*c).func_body, Op::Jmp{addr: 0});
                let addr_else = (*c).func_body.count;
                compile_statement(l, input_path, c)?;
                let addr_after_else = (*c).func_body.count;
                *(*c).func_body.items.add(addr_condition)  = Op::JmpIfNot {addr: addr_else, arg: cond};
                *(*c).func_body.items.add(addr_skips_else) = Op::Jmp      {addr: addr_after_else};
            } else {
                (*l).parse_point = saved_point;
                let addr_after_if = (*c).func_body.count;
                *(*c).func_body.items.add(addr_condition)  = Op::JmpIfNot {addr: addr_after_if , arg: cond};
            }

            Some(())
        } else if (*l).token == CLEX_id && strcmp((*l).string, c!("while")) == 0 {
            let begin = (*c).func_body.count;
            get_and_expect_clex(l, input_path, '(' as c_long)?;
            let saved_auto_vars_count = (*c).auto_vars_ator.count;
            let (arg, _) = compile_expression(l, input_path, c)?;

            get_and_expect_clex(l, input_path, ')' as c_long)?;
            let condition_jump = (*c).func_body.count;
            da_append(&mut (*c).func_body, Op::JmpIfNot{addr: 0, arg});
            (*c).auto_vars_ator.count = saved_auto_vars_count;

            compile_statement(l, input_path, c)?;
            da_append(&mut (*c).func_body, Op::Jmp{addr: begin});
            let end = (*c).func_body.count;
            (*(*c).func_body.items.add(condition_jump)) = Op::JmpIfNot{addr: end, arg};
            Some(())
        } else {
            (*l).parse_point = saved_point;
            let saved_auto_vars_count = (*c).auto_vars_ator.count;
            compile_expression(l, input_path, c)?;
            (*c).auto_vars_ator.count = saved_auto_vars_count;
            get_and_expect_clex(l, input_path, ';' as c_long)?;
            Some(())
        }
    }
}

pub unsafe fn temp_strip_suffix(s: *const c_char, suffix: *const c_char) -> Option<*const c_char> {
    let mut sv = sv_from_cstr(s);
    let suffix_len = strlen(suffix);
    if sv_end_with(sv, suffix) {
        sv.count -= suffix_len;
        Some(temp_sv_to_cstr(sv))
    } else {
        None
    }
}

pub unsafe fn usage() {
    fprintf(stderr, c!("Usage: %s [OPTIONS] <input.b>\n"), flag_program_name());
    fprintf(stderr, c!("OPTIONS:\n"));
    flag_print_options(stderr);
}

#[derive(Clone, Copy)]
pub struct Func {
    name: *const c_char,
    body: Array<Op>,
    params_count: usize,
    auto_vars_count: usize,
}

#[derive(Clone, Copy)]
pub struct Compiler {
    pub vars: Array<Array<Var>>,
    pub auto_vars_ator: AutoVarsAtor,
    pub funcs: Array<Func>,
    pub func_body: Array<Op>,
    pub data: Array<u8>,
    pub extrns: Array<*const c_char>,
    pub globals: Array<*const c_char>,
    pub arena: Arena,
}

pub unsafe fn compile_program(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler) -> Option<()> {
    scope_push(&mut (*c).vars);          // begin global scope
    'def: loop {
        stb_c_lexer_get_token(l);
        if (*l).token == CLEX_eof { break 'def }

        expect_clex(l, input_path, CLEX_id)?;

        let name = arena::strdup(&mut (*c).arena, (*l).string);
        let name_where = (*l).where_firstchar;

        // TODO: maybe the keywords should be identified on the level of lexing
        if is_keyword((*l).string) {
            diagf!(l, input_path, name_where, c!("ERROR: Trying to define a reserved keyword `%s` as a symbol. Please choose a different name.\n"), name);
            diagf!(l, input_path, name_where, c!("NOTE: Reserved keywords are: "));
            for i in 0..B_KEYWORDS.len() {
                if i > 0 {
                    fprintf(stderr, c!(", "));
                }
                fprintf(stderr, c!("`%s`"), (*B_KEYWORDS)[i]);
            }
            fprintf(stderr, c!("\n"));
            return None;
        }

        let saved_point = (*l).parse_point;
        stb_c_lexer_get_token(l);
        if (*l).token == '(' as c_long { // Function definition
            scope_push(&mut (*c).vars); // begin function scope
            let mut params_count = 0;
            let saved_point = (*l).parse_point;
            stb_c_lexer_get_token(l);
            if (*l).token != ')' as c_long {
                (*l).parse_point = saved_point;
                'params: loop {
                    get_and_expect_clex(l, input_path, CLEX_id)?;
                    let name = arena::strdup(&mut (*c).arena, (*l).string);
                    let name_where = (*l).where_firstchar;
                    let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                    declare_var(l, input_path, &mut (*c).vars, name, name_where, Storage::Auto{index})?;
                    params_count += 1;
                    stb_c_lexer_get_token(l);
                    expect_clexes(l, input_path, &[',' as c_long, ')' as c_long])?;
                    if (*l).token == ')' as c_long {
                        break 'params;
                    } else if (*l).token == ',' as c_long {
                        continue 'params;
                    } else {
                        unreachable!()
                    }
                }
            }
            compile_statement(l, input_path, c)?;
            scope_pop(&mut (*c).vars); // end function scope

            declare_var(l, input_path, &mut (*c).vars, name, name_where, Storage::External{name});
            da_append(&mut (*c).funcs, Func {
                name,
                body: (*c).func_body,
                params_count,
                auto_vars_count: (*c).auto_vars_ator.max,
            });
            (*c).func_body = zeroed();
            (*c).auto_vars_ator = zeroed();
        } else { // Variable definition
            (*l).parse_point = saved_point;
            name_declare_if_not_exists(&mut (*c).globals, name);
            declare_var(l, input_path, &mut (*c).vars, name, name_where, Storage::External{name})?;
            get_and_expect_clex(l, input_path, ';' as c_long)?;
        }
    }
    scope_pop(&mut (*c).vars);          // end global scope

    Some(())
}

pub unsafe fn main(mut argc: i32, mut argv: *mut*mut c_char) -> Option<()> {
    let default_target;
    if cfg!(target_arch = "aarch64") {
        default_target = Target::Gas_AArch64_Linux;
    } else {
        default_target = Target::Fasm_x86_64_Linux;
    }
    let default_target_name = name_of_target(default_target).expect("default target name not found");

    let target_name = flag_str(c!("t"), default_target_name, c!("Compilation target. Pass \"list\" to get the list of available targets."));
    let output_path = flag_str(c!("o"), ptr::null(), c!("Output path"));
    let run         = flag_bool(c!("run"), false, c!("Run the compiled program (if applicable for the target)"));
    let help        = flag_bool(c!("help"), false, c!("Print this help message"));
    let linker      = flag_list(c!("L"), c!("Append a flag to the linker of the target platform"));

    let mut input_path: *const c_char = ptr::null();
    while argc > 0 {
        if !flag_parse(argc, argv) {
            usage();
            flag_print_error(stderr);
            return None;
        }
        argc = flag_rest_argc();
        argv = flag_rest_argv();
        if argc > 0 {
            if !input_path.is_null() {
                // TODO: support compiling several files?
                fprintf(stderr, c!("ERROR: Serveral input files is not supported yet\n"));
                return None;
            }
            input_path = shift!(argv, argc);
        }
    }

    if *help {
        usage();
        return Some(());
    }

    if strcmp(*target_name, c!("list")) == 0 {
        fprintf(stderr, c!("Compilation targets:\n"));
        for i in 0..TARGET_NAMES.len() {
            fprintf(stderr, c!("    %s\n"), (*TARGET_NAMES)[i].name);
        }
        return Some(());
    }

    if input_path.is_null() {
        usage();
        fprintf(stderr, c!("ERROR: no input is provided\n"));
        return None;
    }

    let Some(target) = target_by_name(*target_name) else {
        usage();
        fprintf(stderr, c!("ERROR: unknown target `%s`\n"), *target_name);
        return None;
    };

    let mut cmd: Cmd = zeroed();

    let mut input: String_Builder = zeroed();
    if !read_entire_file(input_path, &mut input) { return None; }

    let mut l: stb_lexer = zeroed();
    let mut string_store: [c_char; 1024] = zeroed(); // TODO: size of identifiers and string literals is limited because of stb_c_lexer.h
    stb_c_lexer_init(&mut l, input.items, input.items.add(input.count), string_store.as_mut_ptr(), string_store.len() as i32);

    let mut c: Compiler = zeroed();
    compile_program(&mut l, input_path, &mut c)?;

    let mut output: String_Builder = zeroed();

    match target {
        Target::Gas_AArch64_Linux => {
            codegen::gas_aarch64_linux::generate_program(&mut output, &c);

            let effective_output_path;
            if (*output_path).is_null() {
                if let Some(base_path) = temp_strip_suffix(input_path, c!(".b")) {
                    effective_output_path = base_path;
                } else {
                    effective_output_path = temp_sprintf(c!("%s.out"), input_path);
                }
            } else {
                effective_output_path = *output_path;
            }

            let output_asm_path = temp_sprintf(c!("%s.s"), effective_output_path);
            if !write_entire_file(output_asm_path, output.items as *const c_void, output.count) { return None; }
            printf(c!("Generated %s\n"), output_asm_path);

            if !cfg!(target_arch = "aarch64") {
                // TODO: think how to approach cross-compilation
                fprintf(stderr, c!("ERROR: Cross-compilation of aarch64 is not supported for now\n"));
                return None;
            }

            let output_obj_path = temp_sprintf(c!("%s.o"), effective_output_path);
            cmd_append! {
                &mut cmd,
                c!("as"), c!("-o"), output_obj_path, output_asm_path,
            }
            if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            cmd_append! {
                &mut cmd,
                c!("cc"), c!("-o"), effective_output_path, output_obj_path,
            }
            for i in 0..(*linker).count {
                cmd_append!{
                    &mut cmd,
                    *(*linker).items.add(i),
                }
            }
            if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            if *run {
                // TODO: pass the extra arguments from command line
                // Probably makes sense after we start accepting command line arguments via main after implementing (2025-05-11 15:45:38)

                // if the user does `b program.b -run` the compiler tries to run `program` which is not possible on Linux. It has to be `./program`.
                let run_path: *const c_char;
                if (strchr(effective_output_path, '/' as c_int)).is_null() {
                    run_path = temp_sprintf(c!("./%s"), effective_output_path);
                } else {
                    run_path = effective_output_path;
                }

                cmd_append! {
                    &mut cmd,
                    run_path,
                }
                if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            }
        },
        Target::Fasm_x86_64_Linux => {
            codegen::fasm_x86_64_linux::generate_program(&mut output, &c);

            let effective_output_path;
            if (*output_path).is_null() {
                if let Some(base_path) = temp_strip_suffix(input_path, c!(".b")) {
                    effective_output_path = base_path;
                } else {
                    effective_output_path = temp_sprintf(c!("%s.out"), input_path);
                }
            } else {
                effective_output_path = *output_path;
            }

            let output_asm_path = temp_sprintf(c!("%s.asm"), effective_output_path);
            if !write_entire_file(output_asm_path, output.items as *const c_void, output.count) { return None; }
            printf(c!("Generated %s\n"), output_asm_path);

            if !cfg!(target_arch = "x86_64") {
                // TODO: think how to approach cross-compilation
                fprintf(stderr, c!("ERROR: Cross-compilation of aarch64 is not supported for now\n"));
                return None;
            }

            let output_obj_path = temp_sprintf(c!("%s.o"), effective_output_path);
            cmd_append! {
                &mut cmd,
                c!("fasm"), output_asm_path, output_obj_path,
            }
            if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            cmd_append! {
                &mut cmd,
                c!("cc"), c!("-no-pie"), c!("-o"), effective_output_path, output_obj_path,
            }
            for i in 0..(*linker).count {
                cmd_append!{
                    &mut cmd,
                    *(*linker).items.add(i),
                }
            }
            if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            if *run {
                // TODO: pass the extra arguments from command line
                // Probably makes sense after we start accepting command line arguments via main after implementing (2025-05-11 15:45:38)

                // if the user does `b program.b -run` the compiler tries to run `program` which is not possible on Linux. It has to be `./program`.
                let run_path: *const c_char;
                if (strchr(effective_output_path, '/' as c_int)).is_null() {
                    run_path = temp_sprintf(c!("./%s"), effective_output_path);
                } else {
                    run_path = effective_output_path;
                }

                cmd_append! {
                    &mut cmd,
                    run_path,
                }
                if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            }
        }
        Target::Html_Js => {
            codegen::html_js::generate_program(&mut output, &c);

            let effective_output_path;
            if (*output_path).is_null() {
                let base_path = temp_strip_suffix(input_path, c!(".b")).unwrap_or(input_path);
                effective_output_path = temp_sprintf(c!("%s.html"), base_path);
            } else {
                effective_output_path = *output_path;
            }

            if !write_entire_file(effective_output_path, output.items as *const c_void, output.count) { return None; }
            printf(c!("Generated %s\n"), effective_output_path);
            if *run {
                cmd_append! {
                    &mut cmd,
                    c!("xdg-open"),
                    effective_output_path,
                }
                if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            }
        }
        Target::IR => {
            codegen::ir::generate_program(&mut output, &c);

            let effective_output_path;
            if (*output_path).is_null() {
                let base_path = temp_strip_suffix(input_path, c!(".b")).unwrap_or(input_path);
                effective_output_path = temp_sprintf(c!("%s.ir"), base_path);
            } else {
                effective_output_path = *output_path;
            }

            if !write_entire_file(effective_output_path, output.items as *const c_void, output.count) { return None; }
            printf(c!("Generated %s\n"), effective_output_path);
            if *run {
                todo!("Interpret the IR?");
            }
        }
    }
    Some(())
}

// TODO(2025-05-18 07:06:26): B lexing is different from the C one.
//   Hack stb_c_lexer.h into stb_b_lexer.h
