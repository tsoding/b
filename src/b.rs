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

unsafe fn expect_clex(l: *const stb_lexer, input_path: *const c_char, clex: i64) -> Option<()> {
    expect_clexes(l, input_path, &[clex])
}

unsafe fn get_and_expect_clex(l: *mut stb_lexer, input_path: *const c_char, clex: c_long) -> Option<()> {
    stb_c_lexer_get_token(l);
    expect_clex(l, input_path, clex)
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum Storage {
    External{name: *const c_char},
    Auto{index: usize},
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
    Literal(i64),
    DataOffset(usize),
}

// TODO: add support for binary division expression
#[derive(Clone, Copy, PartialEq)]
pub enum Binop {
    Plus,
    Minus,
    Mult,
    Less,
    Assign,
}

// The higher the index of the row in this table the higher the precedence of the Binop
pub const PRECEDENCE: *const [*const [Binop]] = &[
    &[Binop::Assign],
    &[Binop::Less],
    &[Binop::Plus, Binop::Minus],
    &[Binop::Mult],
];

impl Binop {
    pub fn from_token(token: c_long) -> Option<Self> {
        match token {
            token if token == '+' as i64 => Some(Binop::Plus),
            token if token == '-' as i64 => Some(Binop::Minus),
            token if token == '*' as i64 => Some(Binop::Mult),
            token if token == '<' as i64 => Some(Binop::Less),
            token if token == '=' as i64 => Some(Binop::Assign),
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
    UnaryNot   {result: usize, arg: Arg},
    Add        {index: usize, lhs: Arg, rhs: Arg},
    Sub        {index: usize, lhs: Arg, rhs: Arg},
    Mul        {index: usize, lhs: Arg, rhs: Arg},
    Less       {index: usize, lhs: Arg, rhs: Arg},
    AutoAssign {index: usize, arg: Arg},
    Funcall    {result: usize, name: *const c_char, args: Array<Arg>},
    Jmp        {addr: usize},
    JmpIfNot   {addr: usize, arg: Arg},
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

pub unsafe fn compile_lvalue(l: *mut stb_lexer, expr: Expr, input_path: *const c_char, c: *mut Compiler) -> Option<usize> {
    match expr {
        Expr::Binop {..} => todo!(),
        Expr::Not {..} => todo!(),
        Expr::Intlit {..} => todo!(),
        Expr::Name {name, name_where} => {
            let var_def = find_var_deep(&mut (*c).vars, name);
            if var_def.is_null() {
                diagf!(l, input_path, name_where, c!("ERROR: could not find name `%s`\n"), name);
                return None;
            }

            match (*var_def).storage {
                Storage::Auto{index} => Some(index),
                Storage::External{..} => {
                    missingf!(l, input_path, name_where, c!("external variables in lvalues are not supported yet\n"));
                }
            }
        },
        Expr::Dqstring {..} => todo!(),
        Expr::Funcall {..} => todo!(),
    }
}

pub unsafe fn compile_rvalue(l: *mut stb_lexer, expr: Expr, input_path: *const c_char, c: *mut Compiler) -> Option<Arg> {
    match expr {
        Expr::Binop {binop, lhs, rhs} => {
            match binop {
                Binop::Plus => {
                    let lhs = compile_rvalue(l, *lhs, input_path, c)?;
                    let rhs = compile_rvalue(l, *rhs, input_path, c)?;
                    let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                    da_append(&mut (*c).func_body, Op::Add {index, lhs, rhs});
                    Some(Arg::AutoVar(index))
                },
                Binop::Minus => {
                    let lhs = compile_rvalue(l, *lhs, input_path, c)?;
                    let rhs = compile_rvalue(l, *rhs, input_path, c)?;
                    let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                    da_append(&mut (*c).func_body, Op::Sub {index, lhs, rhs});
                    Some(Arg::AutoVar(index))
                },
                Binop::Mult => {
                    let lhs = compile_rvalue(l, *lhs, input_path, c)?;
                    let rhs = compile_rvalue(l, *rhs, input_path, c)?;
                    let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                    da_append(&mut (*c).func_body, Op::Mul {index, lhs, rhs});
                    Some(Arg::AutoVar(index))
                },
                Binop::Less => {
                    let lhs = compile_rvalue(l, *lhs, input_path, c)?;
                    let rhs = compile_rvalue(l, *rhs, input_path, c)?;
                    let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                    da_append(&mut (*c).func_body, Op::Less {index, lhs, rhs});
                    Some(Arg::AutoVar(index))
                },
                Binop::Assign => {
                    let index = compile_lvalue(l, *lhs, input_path, c)?;
                    let arg = compile_rvalue(l, *rhs, input_path, c)?;
                    da_append(&mut (*c).func_body, Op::AutoAssign {index, arg});
                    Some(Arg::AutoVar(index))
                }
            }
        },
        Expr::Not {arg} => {
            let arg = compile_rvalue(l, *arg, input_path, c)?;
            let result = allocate_auto_var(&mut (*c).auto_vars_ator);
            da_append(&mut (*c).func_body, Op::UnaryNot{result, arg});
            Some(Arg::AutoVar(result))
        },
        Expr::Intlit {value} => Some(Arg::Literal(value)),
        Expr::Name {name, name_where} => {
            let var_def = find_var_deep(&mut (*c).vars, name);
            if var_def.is_null() {
                diagf!(l, input_path, name_where, c!("ERROR: could not find name `%s`\n"), name);
                return None;
            }

            match (*var_def).storage {
                Storage::Auto{index} => Some(Arg::AutoVar(index)),
                Storage::External{..} => {
                    missingf!(l, input_path, name_where, c!("external variables in lvalues are not supported yet\n"));
                }
            }
        },
        Expr::Dqstring {data} => {
            let offset = (*c).data.count;
            let string_len = strlen(data);
            da_append_many(&mut (*c).data, slice::from_raw_parts(data as *const u8, string_len));
            // TODO: Strings in B are not NULL-terminated.
            // They are terminated with symbol '*e' ('*' is escape character akin to '\' in C) which according to the
            // spec is called just "end-of-file" without any elaboration on what its value is. Maybe it had a specific
            // value on PDP that was a common knowledge at the time? In any case that breaks compatibility with
            // libc. While the language is still in development we gonna terminate it with 0. We will make it
            // "spec complaint" later.
            da_append(&mut (*c).data, 0); // NULL-terminator
            Some(Arg::DataOffset(offset))
        }
        Expr::Funcall {name, name_where, args} => {
            compile_function_call(l, input_path, c, name, name_where, da_slice(args))
        }
    }
}

pub unsafe fn compile_rvalue_from_lexer(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler) -> Option<Arg> {
    let expr = parse_expression(l, input_path, &mut (*c).arena)?;
    compile_rvalue(l, expr, input_path, c)
}

#[derive(Clone, Copy)]
pub enum Expr {
    Binop    {binop: Binop, lhs: *const Expr, rhs: *const Expr},
    Not      {arg: *const Expr},
    Intlit   {value: c_long},
    Name     {name: *const c_char, name_where: *const c_char},
    Dqstring {data: *const c_char},
    Funcall  {name: *const c_char, name_where: *const c_char, args: Array<Expr> }
}

impl Expr {
    unsafe fn new(a: *mut Arena) -> *mut Expr {
        arena::alloc(a, size_of::<Expr>()) as *mut Expr
    }

    unsafe fn clone(a: *mut Arena, expr: Expr) -> *mut Expr {
        let result = Self::new(a);
        *result = expr;
        result
    }
}

#[allow(unused_variables)]
pub unsafe fn parse_function_call_args(l: *mut stb_lexer, input_path: *const c_char, a: *mut Arena) -> Option<Array<Expr>> {
    let mut args: Array<Expr> = zeroed();
    let saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);
    if (*l).token != ')' as c_long {
        (*l).parse_point = saved_point;
        loop {
            let expr = parse_expression(l, input_path, a)?;
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
    Some(args)
}

pub unsafe fn parse_primary_expression(l: *mut stb_lexer, input_path: *const c_char, a: *mut Arena) -> Option<Expr> {
    stb_c_lexer_get_token(l);
    match (*l).token {
        token if token == '(' as i64 => {
            let result = parse_expression(l, input_path, a)?;
            get_and_expect_clex(l, input_path, ')' as i64)?;
            Some(result)
        }
        token if token == '!' as i64 => {
            let arg = parse_expression(l, input_path, a)?;
            Some(Expr::Not {arg: Expr::clone(a, arg)})
        }
        CLEX_intlit => {
            Some(Expr::Intlit {value: (*l).int_number})
        }
        CLEX_id => {
            let name = arena::strdup(a, (*l).string);
            let name_where = (*l).where_firstchar;

            let saved_point = (*l).parse_point;
            stb_c_lexer_get_token(l);

            if (*l).token == '(' as i64 {
                let args = parse_function_call_args(l, input_path, a)?;
                Some(Expr::Funcall {name, name_where, args})
            } else {
                (*l).parse_point = saved_point;
                Some(Expr::Name {name, name_where})
            }
        }
        CLEX_dqstring => {
            let data = arena::strdup(a, (*l).string);
            Some(Expr::Dqstring{data})
        }
        _ => {
            missingf!(l, input_path, (*l).where_firstchar, c!("Unexpected token %s not all expressions are implemented yet\n"), display_token_kind_temp((*l).token));
        }
    }
}

pub unsafe fn parse_binop_expression(l: *mut stb_lexer, input_path: *const c_char, a: *mut Arena, precedence: usize) -> Option<Expr> {
    if precedence >= Binop::MAX_PRECEDENCE {
        return parse_primary_expression(l, input_path, a);
    }

    let mut lhs = parse_binop_expression(l, input_path, a, precedence + 1)?;

    let mut saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);

    if let Some(binop) = Binop::from_token((*l).token) {
        if binop.precedence() == precedence {
            while let Some(binop) = Binop::from_token((*l).token) {
                if binop.precedence() != precedence { break; }

                let token = (*l).token;
                let rhs = parse_binop_expression(l, input_path, a, precedence)?;
                let binop = Binop::from_token(token).unwrap();
                let binop_expr = Expr::Binop {
                    binop,
                    lhs: Expr::clone(a, lhs),
                    rhs: Expr::clone(a, rhs),
                };
                lhs = binop_expr;

                saved_point = (*l).parse_point;
                stb_c_lexer_get_token(l);
            }
        }
    }

    (*l).parse_point = saved_point;
    Some(lhs)
}

pub unsafe fn parse_expression(l: *mut stb_lexer, input_path: *const c_char, a: *mut Arena) -> Option<Expr> {
    parse_binop_expression(l, input_path, a, 0)
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

pub unsafe fn compile_function_call(l: *mut stb_lexer, input_path: *const c_char, c: *mut Compiler, name: *const c_char, name_where: *const c_char, exprs: *const [Expr]) -> Option<Arg> {
    let var_def = find_var_deep(&(*c).vars, name);
    if var_def.is_null() {
        diagf!(l, input_path, name_where, c!("ERROR: could not find function `%s`\n"), name);
        return None;
    }

    let mut args: Array<Arg> = zeroed();
    for i in 0..exprs.len() {
        let arg = compile_rvalue(l, (*exprs)[i], input_path, c)?;
        da_append(&mut args, arg);
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

pub unsafe fn extrn_declare_if_not_exists(extrns: *mut Array<*const c_char>, name: *const c_char) {
    for i in 0..(*extrns).count {
        if strcmp(*(*extrns).items.add(i), name) == 0 {
            return;
        }
    }
    da_append(extrns, name)
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
                    extrn_declare_if_not_exists(&mut (*c).extrns, name);
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
        } else if  (*l).token == CLEX_id && strcmp((*l).string, c!("while")) == 0 {
            let begin = (*c).func_body.count;
            get_and_expect_clex(l, input_path, '(' as c_long)?;
            let saved_auto_vars_count = (*c).auto_vars_ator.count;
            let arg = compile_rvalue_from_lexer(l, input_path, c)?;

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
            compile_rvalue_from_lexer(l, input_path, c)?;
            get_and_expect_clex(l, input_path, ';' as c_long);
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

        stb_c_lexer_get_token(l);
        if (*l).token == '(' as c_long { // Function definition
            // TODO(2025-05-11 15:45:38): functions with several parameters
            get_and_expect_clex(l, input_path, ')' as c_long)?;

            scope_push(&mut (*c).vars); // begin function scope
            compile_statement(l, input_path, c)?;
            scope_pop(&mut (*c).vars); // end function scope

            declare_var(l, input_path, &mut (*c).vars, name, name_where, Storage::External{name});
            da_append(&mut (*c).funcs, Func {
                name,
                body: (*c).func_body,
                auto_vars_count: (*c).auto_vars_ator.max,
            });
            (*c).func_body = zeroed();
            (*c).auto_vars_ator = zeroed();
        } else { // Variable definition
            missingf!(l, input_path, (*l).where_firstchar, c!("variable definitions\n"));
        }
    }
    scope_pop(&mut (*c).vars);          // end global scope

    Some(())
}

pub unsafe fn dump_expr(expr: Expr, level: usize) {
    printf(c!("%*s"), 2*level, c!(""));
    match expr {
        Expr::Binop {binop, lhs, rhs} => {
            match binop {
                Binop::Plus   => printf(c!("Binop(Plus)\n")),
                Binop::Minus  => printf(c!("Binop(Minus)\n")),
                Binop::Mult   => printf(c!("Binop(Mult)\n")),
                Binop::Less   => printf(c!("Binop(Less)\n")),
                Binop::Assign => printf(c!("Binop(Assign)\n")),
            };
            dump_expr(*lhs, level + 1);
            dump_expr(*rhs, level + 1);
        }
        Expr::Not {arg} => {
            printf(c!("Not\n"));
            dump_expr(*arg, level + 1);
        }
        Expr::Intlit {value} => {
            printf(c!("Intlit(%ld)\n"), value);
        }
        Expr::Name {name, name_where: _} => {
            printf(c!("Name(%s)\n"), name);
        }
        Expr::Dqstring {data} => {
            // TODO: escape the dqstring properly
            printf(c!("Dqstring(\"%s\")\n"), data);
        }
        Expr::Funcall {name, name_where: _, args} => {
            printf(c!("Funcall(%s)\n"), name);
            for i in 0..args.count {
                dump_expr(*args.items.add(i), level + 1);
            }
        }
    };
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
                cmd_append! {
                    &mut cmd,
                    effective_output_path,
                }
                if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            }
        },
        Target::Fasm_x86_64_Linux => {
            codegen::fasm_x86_64_linux::generate_program(&mut output, &c);

            // TODO: if the user does `b program.b -run` the compiler tries to run `program` which is not possible on Linux. It has to be `./program`.
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
                cmd_append! {
                    &mut cmd,
                    effective_output_path,
                }
                if !cmd_run_sync_and_reset(&mut cmd) { return None; }
            }
        }
        Target::JavaScript => {
            codegen::javascript::generate_program(&mut output, &c);

            let effective_output_path;
            if (*output_path).is_null() {
                let base_path = temp_strip_suffix(input_path, c!(".b")).unwrap_or(input_path);
                effective_output_path = temp_sprintf(c!("%s.js"), base_path);
            } else {
                effective_output_path = *output_path;
            }

            // TODO(2025-05-08 07:18:15): make the js target automatically generate the html file
            if !write_entire_file(effective_output_path, output.items as *const c_void, output.count) { return None; }
            printf(c!("Generated %s\n"), effective_output_path);
            if *run {
                // TODO: when (2025-05-08 07:18:15) is done, open the html file in browser with html
                todo!("Run the JavaScript program");
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
                todo!("Interpret the IR");
            }
        }
    }
    Some(())
}

// TODO: B lexing is different from the C one.
//   Hack stb_c_lexer.h into stb_b_lexer.h
// TODO: Looks like B does not have hex literals, which means we will have to remove the from stb_c_lexer.h
