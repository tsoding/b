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

#[derive(Clone, Copy)]
pub struct Loc {
    pub input_path: *const c_char,
    pub line_number: c_int,
    pub line_offset: c_int,
}

macro_rules! diagf {
    ($loc:expr, $($args:tt)*) => {{
        fprintf(stderr, c!("%s:%d:%d: "), $loc.input_path, $loc.line_number, $loc.line_offset);
        fprintf(stderr, $($args)*);
    }};
}

#[macro_export]
macro_rules! missingf {
    ($loc:expr, $($args:tt)*) => {{
        let file = file!();
        fprintf(stderr, c!("%s:%d:%d: TODO: "), $loc.input_path, $loc.line_number, $loc.line_offset);
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
        CLEX_modeq      => c!("%="),
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

pub unsafe fn lexer_loc(l: *const stb_lexer, input_path: *const c_char) -> Loc {
    Loc {
        input_path,
        line_number: (*l).parse_point.line_number,
        line_offset: (*l).where_firstchar.offset_from((*l).parse_point.line_start) as c_int + 1,
    }
}

pub unsafe fn expect_clexes(l: *const stb_lexer, input_path: *const c_char, clexes: *const [c_long]) -> Option<()> {
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

    diagf!(lexer_loc(l, input_path), c!("ERROR: expected %s, but got %s\n"), sb.items, display_token_kind_temp((*l).token));

    free(sb.items);
    None
}

pub unsafe fn expect_clex(l: *const stb_lexer, input_path: *const c_char, clex: c_long) -> Option<()> {
    expect_clexes(l, input_path, &[clex])
}

pub unsafe fn get_and_expect_clex(l: *mut stb_lexer, input_path: *const c_char, clex: c_long) -> Option<()> {
    stb_c_lexer_get_token(l);
    expect_clex(l, input_path, clex)
}

pub unsafe fn expect_clex_id(l: *const stb_lexer, input_path: *const c_char, id: *const c_char) -> Option<()> {
    expect_clex(l, input_path, CLEX_id)?;
    if strcmp((*l).string, id) != 0 {
        diagf!(lexer_loc(l, input_path), c!("ERROR: expected `%s`, but got `%s`\n"), id, (*l).string);
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
    pub loc: Loc,
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

pub unsafe fn declare_var(l: *mut stb_lexer, input_path: *const c_char, vars: *mut Array<Array<Var>>, name: *const c_char, loc: Loc, storage: Storage) -> Option<()> {
    let scope = da_last_mut(vars);
    let existing_var = find_var_near(scope, name);
    if !existing_var.is_null() {
        diagf!(lexer_loc(l, input_path), c!("ERROR: redefinition of variable `%s`\n"), name);
        diagf!((*existing_var).loc, c!("NOTE: the first declaration is located here\n"));
        return None;
    }

    da_append(scope, Var {name, loc, storage});
    Some(())
}

#[derive(Clone, Copy)]
pub struct Label {
    name: *const c_char,
    loc: Loc,
    addr: usize,
}

pub unsafe fn find_label(labels: *const Array<Label>, name: *const c_char) -> *const Label {
    for i in 0..(*labels).count {
        let label = (*labels).items.add(i);
        if strcmp((*label).name, name) == 0 {
            return label
        }
    }
    ptr::null()
}

pub unsafe fn define_label(labels: *mut Array<Label>, name: *const c_char, loc: Loc, addr: usize) -> Option<()> {
    let existing_label = find_label(labels, name);
    if !existing_label.is_null() {
        diagf!(loc, c!("ERROR: duplicate label `%s`\n"), name);
        diagf!((*existing_label).loc, c!("NOTE: the first definition is located here\n"));
        return None;
    }

    da_append(labels, Label {name, loc, addr});
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
    Deref(usize),
    RefAutoVar(usize),
    RefExternal(*const c_char),
    External(*const c_char),
    Literal(i64),
    DataOffset(usize),
}

#[derive(Clone, Copy, PartialEq)]
pub enum Binop {
    Plus,
    Minus,
    Mult,
    Mod,
    Div,
    Less,
    Greater,
    Equal,
    NotEqual,
    GreaterEqual,
    LessEqual,
    BitOr,
    BitAnd,
    BitShl,
    BitShr,
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
    pub fn from_assign_token(token: c_long) -> Option<Option<Self>> {
        match token {
            token if token == '=' as c_long => Some(None),
            CLEX_shleq                      => Some(Some(Binop::BitShl)),
            CLEX_shreq                      => Some(Some(Binop::BitShr)),
            CLEX_modeq                      => Some(Some(Binop::Mod)),
            CLEX_oreq                       => Some(Some(Binop::BitOr)),
            CLEX_andeq                      => Some(Some(Binop::BitAnd)),
            CLEX_pluseq                     => Some(Some(Binop::Plus)),
            CLEX_minuseq                    => Some(Some(Binop::Minus)),
            CLEX_muleq                      => Some(Some(Binop::Mult)),
            CLEX_diveq                      => Some(Some(Binop::Div)),
            _ => None,
        }
    }

    pub fn from_token(token: c_long) -> Option<Self> {
        match token {
            token if token == '+' as c_long => Some(Binop::Plus),
            token if token == '-' as c_long => Some(Binop::Minus),
            token if token == '*' as c_long => Some(Binop::Mult),
            token if token == '/' as c_long => Some(Binop::Div),
            token if token == '%' as c_long => Some(Binop::Mod),
            token if token == '<' as c_long => Some(Binop::Less),
            token if token == '>' as c_long => Some(Binop::Greater),
            CLEX_greatereq                  => Some(Binop::GreaterEqual),
            CLEX_lesseq                     => Some(Binop::LessEqual),
            token if token == '|' as c_long => Some(Binop::BitOr),
            token if token == '&' as c_long => Some(Binop::BitAnd),
            CLEX_shl                        => Some(Binop::BitShl),
            CLEX_shr                        => Some(Binop::BitShr),
            CLEX_eq                         => Some(Binop::Equal),
            CLEX_noteq                      => Some(Binop::NotEqual),
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

#[derive(Clone, Copy)]
pub enum Op {
    UnaryNot       {result: usize, arg: Arg},
    Negate         {result: usize, arg: Arg},
    Binop          {binop: Binop, index: usize, lhs: Arg, rhs: Arg},
    AutoAssign     {index: usize, arg: Arg},
    ExternalAssign {name: *const c_char, arg: Arg},
    Store          {index: usize, arg: Arg},
    Funcall        {result: usize, name: *const c_char, args: Array<Arg>},
    Jmp            {addr: usize},
    JmpIfNot       {addr: usize, arg: Arg},
    Return         {arg: Option<Arg>},
}

#[derive(Clone, Copy)]
pub struct OpWithLocation {
    pub opcode: Op,
    pub loc: Loc,
}

pub unsafe fn push_opcode(opcode: Op, loc: Loc, c: *mut Compiler) {
    da_append(&mut (*c).func_body, OpWithLocation {opcode, loc});
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
    let arg = match (*l).token {
        token if token == '(' as c_long => {
            let result = compile_expression(l, input_path, c)?;
            get_and_expect_clex(l, input_path, ')' as c_long)?;
            Some(result)
        }
        token if token == '!' as c_long => {
            let (arg, _) = compile_primary_expression(l, input_path, c)?;
            let result = allocate_auto_var(&mut (*c).auto_vars_ator);
            push_opcode(Op::UnaryNot{result, arg}, lexer_loc(l, input_path), c);
            Some((Arg::AutoVar(result), false))
        }
        token if token == '*' as c_long => {
            let (arg, _) = compile_primary_expression(l, input_path, c)?;
            let index = allocate_auto_var(&mut (*c).auto_vars_ator);
            push_opcode(Op::AutoAssign {index, arg}, lexer_loc(l, input_path), c);
            Some((Arg::Deref(index), true))
        }
        token if token == '-' as c_long => {
            let (arg, _) = compile_primary_expression(l, input_path, c)?;
            let index = allocate_auto_var(&mut (*c).auto_vars_ator);
            push_opcode(Op::Negate {result: index, arg}, lexer_loc(l, input_path), c);
            Some((Arg::AutoVar(index), false))
        }
        token if token == '&' as i64 => {
            // TODO: decipher sections 3.0 and 4.2 of the B user manual
            // not sure if this implementation is correct

            let loc = lexer_loc(l, input_path);
            let (arg, is_lvalue) = compile_primary_expression(l, input_path, c)?;

            if !is_lvalue {
                diagf!(loc, c!("ERROR: cannot take the address of an rvalue\n"));
                return None;
            }

            match arg {
                Arg::Deref(index)   =>  Some((Arg::AutoVar(index), false)), // "&*x is identically x"
                Arg::External(name) =>  Some((Arg::RefExternal(name), false)),
                Arg::AutoVar(index) =>  Some((Arg::RefAutoVar(index), false)),
                Arg::Literal(_) | Arg::DataOffset(_) | Arg::RefAutoVar(_) | Arg::RefExternal(_) => unreachable!(),
            }
        }
        CLEX_plusplus => {
            let loc = lexer_loc(l, input_path);
            let (arg, is_lvalue) = compile_primary_expression(l, input_path, c)?;

            if !is_lvalue {
                diagf!(loc, c!("ERROR: cannot increment an rvalue\n"));
                return None;
            }

            compile_binop(arg, Arg::Literal(1), Binop::Plus, loc, c);
            Some((arg, false))
        }
        CLEX_minusminus => {
            let loc = lexer_loc(l, input_path);
            let (arg, is_lvalue) = compile_primary_expression(l, input_path, c)?;

            if !is_lvalue {
                diagf!(loc, c!("ERROR: cannot decrement an rvalue\n"));
                return None;
            }

            compile_binop(arg, Arg::Literal(1), Binop::Minus, loc, c);
            Some((arg, false))
        }
        CLEX_charlit | CLEX_intlit => Some((Arg::Literal((*l).int_number), false)),
        CLEX_id => {
            let name = arena::strdup(&mut (*c).arena_names, (*l).string);
            let name_loc = lexer_loc(l, input_path);

            let var_def = find_var_deep(&mut (*c).vars, name);
            if var_def.is_null() {
                diagf!(lexer_loc(l, input_path), c!("ERROR: could not find name `%s`\n"), name);
                return None;
            }

            let saved_point = (*l).parse_point;
            stb_c_lexer_get_token(l);

            if (*l).token == '(' as c_long {
                Some((compile_function_call(l, input_path, c, name, name_loc)?, false))
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
            diagf!(lexer_loc(l, input_path), c!("Expected start of a primary expression by got %s\n"), display_token_kind_temp((*l).token));
            None
        }
    };

    let (arg, is_lvalue) = arg?;

    let saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);

    if (*l).token == '[' as c_long {
        let (offset, _) = compile_expression(l, input_path, c)?;
        get_and_expect_clex(l, input_path, ']' as c_long)?;

        let result = allocate_auto_var(&mut (*c).auto_vars_ator);
        push_opcode(Op::Binop {binop: Binop::Plus, index: result, lhs: arg, rhs: offset}, lexer_loc(l, input_path), c);

        Some((Arg::Deref(result), true))
    } else if (*l).token == CLEX_plusplus {
        let loc = lexer_loc(l, input_path);
        if !is_lvalue {
            diagf!(loc, c!("ERROR: cannot increment an rvalue\n"));
            return None;
        }

        let pre = allocate_auto_var(&mut (*c).auto_vars_ator);
        push_opcode(Op::AutoAssign {index: pre, arg}, loc, c);
        compile_binop(arg, Arg::Literal(1), Binop::Plus, loc, c);

        Some((Arg::AutoVar(pre), false))
    } else if (*l).token == CLEX_minusminus {
        let loc = lexer_loc(l, input_path);
        if !is_lvalue {
            diagf!(loc, c!("ERROR: cannot decrement an rvalue\n"));
            return None;
        }

        let pre = allocate_auto_var(&mut (*c).auto_vars_ator);
        push_opcode(Op::AutoAssign {index: pre, arg}, loc, c);
        compile_binop(arg, Arg::Literal(1), Binop::Minus, loc, c);

        Some((Arg::AutoVar(pre), false))
    } else {
        (*l).parse_point = saved_point;
        Some((arg, is_lvalue))
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
        Arg::Literal(_) | Arg::DataOffset(_) | Arg::RefAutoVar(_) | Arg::RefExternal(_) => unreachable!(),
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

                let (rhs, _) = compile_binop_expression(l, input_path, c, precedence + 1)?;

                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                push_opcode(Op::Binop {binop, index, lhs, rhs}, lexer_loc(l, input_path), c);
                lhs = Arg::AutoVar(index);

                lvalue = false;

                saved_point = (*l).parse_point;
                stb_c_lexer_get_token(l);
            }
        }
    }

    (*l).parse_point = saved_point;
    Some((lhs, lvalue))
}

#[allow(unused_variables)]
pub unsafe fn compile_assign_expression(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler, precedence: usize) -> Option<(Arg, bool)> {
    let (lhs, mut lvalue) = compile_binop_expression(l, input_path, c, 0)?;

    let mut saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);

    while let Some(binop) = Binop::from_assign_token((*l).token) {
        let binop_loc = lexer_loc(l, input_path);
        let (rhs, _) = compile_assign_expression(l, input_path, c, precedence + 1)?;

        if !lvalue {
            diagf!(binop_loc, c!("ERROR: cannot assign to rvalue\n"));
            return None;
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
                Arg::Literal(_) | Arg::DataOffset(_) | Arg::RefAutoVar(_) | Arg::RefExternal(_) => unreachable!(),
            }
        }

        lvalue = false;

        saved_point = (*l).parse_point;
        stb_c_lexer_get_token(l);
    }

    (*l).parse_point = saved_point;
    Some((lhs, lvalue))
}

pub unsafe fn compile_expression(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler) -> Option<(Arg, bool)> {
    let (arg, is_lvalue) = compile_assign_expression(l, input_path, c, 0)?;

    let saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);

    if (*l).token == '?' as c_long {
        let result = allocate_auto_var(&mut (*c).auto_vars_ator);

        let addr_condition = (*c).func_body.count;
        push_opcode(Op::JmpIfNot{addr: 0, arg}, lexer_loc(l, input_path), c);

        let (if_true, _) = compile_expression(l, input_path, c)?;
        push_opcode(Op::AutoAssign {index: result, arg: if_true}, lexer_loc(l, input_path), c);

        let addr_skips_true = (*c).func_body.count;
        push_opcode(Op::Jmp{addr: 0}, lexer_loc(l, input_path), c);

        let addr_false = (*c).func_body.count;
        get_and_expect_clex(l, input_path, ':' as c_long)?;

        let (if_false, _) = compile_expression(l, input_path, c)?;
        push_opcode(Op::AutoAssign {index: result, arg: if_false}, lexer_loc(l, input_path), c);

        let addr_after_false = (*c).func_body.count;
        (*(*c).func_body.items.add(addr_condition)).opcode  = Op::JmpIfNot {addr: addr_false, arg};
        (*(*c).func_body.items.add(addr_skips_true)).opcode = Op::Jmp      {addr: addr_after_false};

        Some((Arg::AutoVar(result), false))
    } else {
        (*l).parse_point = saved_point;
        Some((arg, is_lvalue))
    }
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

pub unsafe fn compile_function_call(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler, name: *const c_char, loc: Loc) -> Option<Arg> {
    let var_def = find_var_deep(&(*c).vars, name);
    if var_def.is_null() {
        diagf!(loc, c!("ERROR: could not find function `%s`\n"), name);
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
            push_opcode(Op::Funcall {result, name, args}, lexer_loc(l, input_path), c);
            Some(Arg::AutoVar(result))
        }
        Storage::Auto{..} => {
            missingf!(loc, c!("calling functions from auto variables\n"));
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
                let name = arena::strdup(&mut (*c).arena_names, (*l).string);
                let loc = lexer_loc(l, input_path);
                let storage = if extrn {
                    name_declare_if_not_exists(&mut (*c).extrns, name);
                    Storage::External{name}
                } else {
                    let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                    Storage::Auto{index}
                };
                declare_var(l, input_path, &mut (*c).vars, name, loc, storage)?;
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
            push_opcode(Op::JmpIfNot{addr: 0, arg: cond}, lexer_loc(l, input_path), c);
            (*c).auto_vars_ator.count = saved_auto_vars_count;

            compile_statement(l, input_path, c)?;

            let saved_point = (*l).parse_point;
            stb_c_lexer_get_token(l);

            if (*l).token == CLEX_id && strcmp((*l).string, c!("else")) == 0 {
                let addr_skips_else = (*c).func_body.count;
                push_opcode(Op::Jmp{addr: 0}, lexer_loc(l, input_path), c);
                let addr_else = (*c).func_body.count;
                compile_statement(l, input_path, c)?;
                let addr_after_else = (*c).func_body.count;
                (*(*c).func_body.items.add(addr_condition)).opcode  = Op::JmpIfNot {addr: addr_else, arg: cond};
                (*(*c).func_body.items.add(addr_skips_else)).opcode = Op::Jmp      {addr: addr_after_else};
            } else {
                (*l).parse_point = saved_point;
                let addr_after_if = (*c).func_body.count;
                (*(*c).func_body.items.add(addr_condition)).opcode  = Op::JmpIfNot {addr: addr_after_if , arg: cond};
            }

            Some(())
        } else if (*l).token == CLEX_id && strcmp((*l).string, c!("while")) == 0 {
            let begin = (*c).func_body.count;
            get_and_expect_clex(l, input_path, '(' as c_long)?;
            let saved_auto_vars_count = (*c).auto_vars_ator.count;
            let (arg, _) = compile_expression(l, input_path, c)?;

            get_and_expect_clex(l, input_path, ')' as c_long)?;
            let condition_jump = (*c).func_body.count;
            push_opcode(Op::JmpIfNot{addr: 0, arg}, lexer_loc(l, input_path), c);
            (*c).auto_vars_ator.count = saved_auto_vars_count;

            compile_statement(l, input_path, c)?;
            push_opcode(Op::Jmp{addr: begin}, lexer_loc(l, input_path), c);
            let end = (*c).func_body.count;
            (*(*c).func_body.items.add(condition_jump)).opcode = Op::JmpIfNot{addr: end, arg};
            Some(())
        } else if (*l).token == CLEX_id && strcmp((*l).string, c!("return")) == 0 {
            stb_c_lexer_get_token(l);
            expect_clexes(l, input_path, &[';' as c_long, '(' as c_long])?;
            if (*l).token == ';' as c_long {
                push_opcode(Op::Return {arg: None}, lexer_loc(l, input_path), c);
            } else if (*l).token == '(' as c_long {
                let (arg, _) = compile_expression(l, input_path, c)?;
                get_and_expect_clex(l, input_path, ')' as c_long)?;
                get_and_expect_clex(l, input_path, ';' as c_long)?;
                push_opcode(Op::Return {arg: Some(arg)}, lexer_loc(l, input_path), c);
            } else {
                unreachable!();
            }
            Some(())
        } else if (*l).token == CLEX_id && strcmp((*l).string, c!("goto")) == 0 {
            get_and_expect_clex(l, input_path, CLEX_id)?;
            let name = arena::strdup(&mut (*c).arena_labels, (*l).string);
            let loc = lexer_loc(l, input_path);
            let addr = (*c).func_body.count;
            da_append(&mut (*c).func_labels_used, Label {name, loc, addr});
            get_and_expect_clex(l, input_path, ';' as c_long)?;
            push_opcode(Op::Jmp {addr: 0}, lexer_loc(l, input_path), c);
            Some(())
        } else {
            if (*l).token == CLEX_id {
                let name = arena::strdup(&mut (*c).arena_labels, (*l).string);
                let name_loc = lexer_loc(l, input_path);
                let addr = (*c).func_body.count;
                stb_c_lexer_get_token(l);
                if (*l).token == ':' as c_long {
                    define_label(&mut (*c).func_labels, name, name_loc, addr)?;
                    return Some(());
                }
            }
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
    fprintf(stderr, c!("Usage: %s [OPTIONS] <inputs...> [--] [run arguments]\n"), flag_program_name());
    fprintf(stderr, c!("OPTIONS:\n"));
    flag_print_options(stderr);
}

#[derive(Clone, Copy)]
pub struct Func {
    name: *const c_char,
    name_loc: Loc,
    body: Array<OpWithLocation>,
    params_count: usize,
    auto_vars_count: usize,
}

#[derive(Clone, Copy)]
pub struct Compiler {
    pub vars: Array<Array<Var>>,
    pub auto_vars_ator: AutoVarsAtor,
    pub funcs: Array<Func>,
    pub func_body: Array<OpWithLocation>,
    pub func_labels: Array<Label>,
    pub func_labels_used: Array<Label>,
    pub data: Array<u8>,
    pub extrns: Array<*const c_char>,
    pub globals: Array<*const c_char>,
    pub arena_names: Arena,
    pub arena_labels: Arena,
}

pub unsafe fn compile_program(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler) -> Option<()> {
    scope_push(&mut (*c).vars);          // begin global scope
    'def: loop {
        stb_c_lexer_get_token(l);
        if (*l).token == CLEX_eof { break 'def }

        expect_clex(l, input_path, CLEX_id)?;

        let name = arena::strdup(&mut (*c).arena_names, (*l).string);
        let name_loc = lexer_loc(l, input_path);

        // TODO: maybe the keywords should be identified on the level of lexing
        if is_keyword((*l).string) {
            diagf!(name_loc, c!("ERROR: Trying to define a reserved keyword `%s` as a symbol. Please choose a different name.\n"), name);
            diagf!(name_loc, c!("NOTE: Reserved keywords are: "));
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
            declare_var(l, input_path, &mut (*c).vars, name, name_loc, Storage::External{name});
            scope_push(&mut (*c).vars); // begin function scope
            let mut params_count = 0;
            let saved_point = (*l).parse_point;
            stb_c_lexer_get_token(l);
            if (*l).token != ')' as c_long {
                (*l).parse_point = saved_point;
                'params: loop {
                    get_and_expect_clex(l, input_path, CLEX_id)?;
                    let name = arena::strdup(&mut (*c).arena_names, (*l).string);
                    let name_loc = lexer_loc(l, input_path);
                    let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                    declare_var(l, input_path, &mut (*c).vars, name, name_loc, Storage::Auto{index})?;
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

            for i in 0..(*c).func_labels_used.count {
                let used_label = *(*c).func_labels_used.items.add(i);
                let existing_label = find_label(&(*c).func_labels, used_label.name);
                if existing_label.is_null() {
                    diagf!(used_label.loc, c!("ERROR: label `%s` used but not defined\n"), used_label.name);
                    return None;
                }
                (*(*c).func_body.items.add(used_label.addr)).opcode = Op::Jmp {addr: (*existing_label).addr};
            }
            arena::reset(&mut (*c).arena_labels);

            da_append(&mut (*c).funcs, Func {
                name,
                name_loc,
                body: (*c).func_body,
                params_count,
                auto_vars_count: (*c).auto_vars_ator.max,
            });
            (*c).func_body = zeroed();
            (*c).func_labels.count = 0;
            (*c).func_labels_used.count = 0;
            (*c).auto_vars_ator = zeroed();
        } else { // Variable definition
            (*l).parse_point = saved_point;
            name_declare_if_not_exists(&mut (*c).globals, name);
            declare_var(l, input_path, &mut (*c).vars, name, name_loc, Storage::External{name})?;
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

    let mut input_paths: Array<*mut c_char> = zeroed();
    let mut run_args: Array<*mut c_char> = zeroed();
    'args: while argc > 0 {
        if !flag_parse(argc, argv) {
            usage();
            flag_print_error(stderr);
            return None;
        }
        argc = flag_rest_argc();
        argv = flag_rest_argv();
        if argc > 0 {
            if strcmp(*argv, c!("--")) == 0 {
                da_append_many(&mut run_args, slice::from_raw_parts_mut(argv.add(1), (argc - 1) as usize));
                break 'args;
            } else {
                da_append(&mut input_paths, shift!(argv, argc));
            }
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

    if input_paths.count == 0 {
        usage();
        fprintf(stderr, c!("ERROR: no input is provided\n"));
        return None;
    }

    let Some(target) = target_by_name(*target_name) else {
        usage();
        fprintf(stderr, c!("ERROR: unknown target `%s`\n"), *target_name);
        return None;
    };

    let mut c: Compiler = zeroed();
    let mut input: String_Builder = zeroed();
    let mut string_store: [c_char; 1024] = zeroed(); // TODO: size of identifiers and string literals is limited because of stb_c_lexer.h
    for i in 0..input_paths.count {
        let input_path = *input_paths.items.add(i);

        input.count = 0;
        if !read_entire_file(input_path, &mut input) { return None; }

        let mut l: stb_lexer = zeroed();
        stb_c_lexer_init(&mut l, input.items, input.items.add(input.count), string_store.as_mut_ptr(), string_store.len() as i32);

        compile_program(&mut l, input_path, &mut c)?;
    }

    let mut output: String_Builder = zeroed();
    let mut cmd: Cmd = zeroed();
    match target {
        Target::Gas_AArch64_Linux => {
            codegen::gas_aarch64_linux::generate_program(&mut output, &c);

            let effective_output_path;
            if (*output_path).is_null() {
                if let Some(base_path) = temp_strip_suffix(*input_paths.items, c!(".b")) {
                    effective_output_path = base_path;
                } else {
                    effective_output_path = temp_sprintf(c!("%s.out"), *input_paths.items);
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

                for i in 0..run_args.count {
                    cmd_append! {
                        &mut cmd,
                        *(run_args).items.add(i),
                    }
                }

                if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            }
        },
        Target::Fasm_x86_64_Linux => {
            codegen::fasm_x86_64_linux::generate_program(&mut output, &c);

            let effective_output_path;
            if (*output_path).is_null() {
                if let Some(base_path) = temp_strip_suffix(*input_paths.items, c!(".b")) {
                    effective_output_path = base_path;
                } else {
                    effective_output_path = temp_sprintf(c!("%s.out"), *input_paths.items);
                }
            } else {
                effective_output_path = *output_path;
            }

            let output_asm_path = temp_sprintf(c!("%s.asm"), effective_output_path);
            if !write_entire_file(output_asm_path, output.items as *const c_void, output.count) { return None; }
            printf(c!("Generated %s\n"), output_asm_path);

            if !cfg!(target_arch = "x86_64") {
                // TODO: think how to approach cross-compilation
                fprintf(stderr, c!("ERROR: Cross-compilation of x86_64 is not supported for now\n"));
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

                for i in 0..run_args.count {
                    cmd_append! {
                        &mut cmd,
                        *(run_args).items.add(i),
                    }
                }

                if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            }
        }
        Target::Uxn => {
            codegen::uxn::generate_program(&mut output, &c);

            let effective_output_path;
            if (*output_path).is_null() {
                let input_path = *input_paths.items;
                let base_path = temp_strip_suffix(input_path, c!(".b")).unwrap_or(input_path);
                effective_output_path = temp_sprintf(c!("%s.rom"), base_path);
            } else {
                effective_output_path = *output_path;
            }

            if !write_entire_file(effective_output_path, output.items as *const c_void, output.count) { return None; }
            printf(c!("Generated %s\n"), effective_output_path);
            if *run {
                cmd_append! {
                    &mut cmd,
                    c!("uxnemu"), effective_output_path,
                }
                if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            }
        }
        Target::IR => {
            codegen::ir::generate_program(&mut output, &c);

            let effective_output_path;
            if (*output_path).is_null() {
                let input_path = *input_paths.items;
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

// TODO: Continue compilation for as long as possible
//   even if you encounter semantical errors like unknown variables, functions, etc.
//   If we couldn't find some names just report the error, generate some bogus IR, and
//   continue compiling.
//
//   Every time an error occurs during the compilation we should increment some sort of error counter within
//   struct Compiler and at the end of the compilation if the counter is greater than 0, fail the compilation
//   without even trying to generate any assembly. We could also define some maximum error count after which
//   the compilation fails instantaneously because of "too many errors" (similar to how Go compiler does this).
//
//   This idea might be extended to syntactical errors like missing semicolons. We could
//   probably just assume the semicolons where we expect them, report the error and continue
//   parsing and compiling.
