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
pub mod crust;

use core::ffi::*;
use core::mem::zeroed;
use core::ptr;
use core::slice;
use nob::*;
use stb_c_lexer::*;
use flag::*;
use crust::libc::*;

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
        _ => {
            if token >= 0 && token < 256 {
                temp_sprintf(c!("`%c`"), token)
            } else {
                temp_sprintf(c!("<<<UNKNOWN TOKEN %ld>>>"), token)
            }
        }
    }
}

unsafe fn expect_clexes(l: *const stb_lexer, input_path: *const c_char, clexes: *const [i64]) -> bool {
    for i in 0..clexes.len() {
        if (*clexes)[i] == (*l).token {
            return true;
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
    false
}

unsafe fn expect_clex(l: *const stb_lexer, input_path: *const c_char, clex: i64) -> bool {
    expect_clexes(l, input_path, &[clex])
}

unsafe fn get_and_expect_clex(l: *mut stb_lexer, input_path: *const c_char, clex: c_long) -> bool {
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

pub unsafe fn declare_var(l: *mut stb_lexer, input_path: *const c_char, vars: *mut Array<Array<Var>>, name: *const c_char, name_where: *const c_char, storage: Storage) -> bool {
    let scope = da_last_mut(vars);
    let existing_var = find_var_near(scope, name);
    if !existing_var.is_null() {
        diagf!(l, input_path, name_where, c!("ERROR: redefinition of variable `%s`\n"), name);
        diagf!(l, input_path, (*existing_var).hwere, c!("NOTE: the first declaration is located here\n"));
        return false;
    }

    da_append(scope, Var {name, storage, hwere: name_where});
    true
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
#[derive(Clone, Copy)]
pub enum Binop {
    Plus,
    Minus,
    Mult,
    Less,
}

impl Binop {
    pub fn from_token(token: c_long) -> Option<Self> {
        match token {
            token if token == '+' as i64 => Some(Binop::Plus),
            token if token == '-' as i64 => Some(Binop::Minus),
            token if token == '*' as i64 => Some(Binop::Mult),
            token if token == '<' as i64 => Some(Binop::Less),
            _ => None,
        }
    }

    pub const MAX_PRECEDENCE: usize = 3;
    pub fn precedence(self) -> usize {
        let x = match self {
            Binop::Less => 0,
            Binop::Plus | Binop::Minus => 1,
            Binop::Mult => 2,
        };
        assert!(x < Self::MAX_PRECEDENCE);
        x
    }
}

// TODO: associate location within the source code with each op
#[derive(Clone, Copy)]
pub enum Op {
    ExtrnVar(*const c_char),
    AutoBinop  {binop: Binop, index: usize, lhs: Arg, rhs: Arg},
    AutoAssign {index: usize, arg: Arg},
    Funcall    {name: *const c_char, args: Array<Arg>},
    Jmp        {addr: usize},
    JmpIfNot   {addr: usize, arg: Arg},
}

pub unsafe fn dump_arg(output: *mut String_Builder, arg: Arg) {
    match arg {
        Arg::Literal(value) => sb_appendf(output, c!("Literal(%ld)"), value),
        Arg::AutoVar(index) => sb_appendf(output, c!("AutoVar(%zu)"), index),
        Arg::DataOffset(offset) => sb_appendf(output, c!("DataOffset(%zu)"), offset),
    };
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Target {
    Fasm_x86_64_Linux,
    JavaScript,
    IR,
}

#[derive(Clone, Copy)]
struct Target_Name {
    name: *const c_char,
    target: Target,
}

// TODO: How do we make this place fail compiling when you add a new target above?
//   Maybe we can introduce some sort of macro that generates all of this from a single list of targets
const TARGET_NAMES: *const [Target_Name] = &[
    Target_Name { name: c!("fasm_x86_64_linux"), target: Target::Fasm_x86_64_Linux },
    Target_Name { name: c!("js"),                target: Target::JavaScript        },
    Target_Name { name: c!("ir"),                target: Target::IR                },
];

unsafe fn name_of_target(target: Target) -> Option<*const c_char> {
    for i in 0..(*TARGET_NAMES).len() {
        if target == (*TARGET_NAMES)[i].target {
            return Some((*TARGET_NAMES)[i].name);
        }
    }
    None
}

unsafe fn target_by_name(name: *const c_char) -> Option<Target> {
    for i in 0..(*TARGET_NAMES).len() {
        if strcmp(name, (*TARGET_NAMES)[i].name) == 0 {
            return Some((*TARGET_NAMES)[i].target);
        }
    }
    None
}

unsafe fn generate_fasm_x86_64_linux_executable(output: *mut String_Builder) {
    sb_appendf(output, c!("format ELF64\n"));
    sb_appendf(output, c!("section \".text\" executable\n"));
}

unsafe fn generate_javascript_executable(output: *mut String_Builder) {
    sb_appendf(output, c!("\"use strict\"\n"));
}

unsafe fn generate_executable(output: *mut String_Builder, target: Target) {
    match target {
        Target::Fasm_x86_64_Linux => generate_fasm_x86_64_linux_executable(output),
        Target::JavaScript        => generate_javascript_executable(output),
        Target::IR                => {}
    }
}

unsafe fn generate_fasm_x86_64_linux_function(name: *const c_char, auto_vars_count: usize, body: *const [Op], output: *mut String_Builder) {
    sb_appendf(output, c!("public %s\n"), name);
    sb_appendf(output, c!("%s:\n"), name);
    sb_appendf(output, c!("    push rbp\n"));
    sb_appendf(output, c!("    mov rbp, rsp\n"));
    if auto_vars_count > 0 {
        sb_appendf(output, c!("    sub rsp, %zu\n"), auto_vars_count*8);
    }
    for i in 0..body.len() {
        sb_appendf(output, c!(".op_%zu:\n"), i);
        match (*body)[i] {
            Op::ExtrnVar(name) => {
                sb_appendf(output, c!("    extrn %s\n"), name);
            },
            Op::AutoAssign{index, arg} => {
                match arg {
                    Arg::AutoVar(other_index) => {
                        sb_appendf(output, c!("    mov rax, [rbp-%zu]\n"), other_index*8);
                        sb_appendf(output, c!("    mov QWORD [rbp-%zu], rax\n"), index*8);
                    }
                    Arg::Literal(value) => {
                        sb_appendf(output, c!("    mov QWORD [rbp-%zu], %ld\n"), index*8, value);
                    }
                    Arg::DataOffset(offset) => {
                        sb_appendf(output, c!("    mov rax, dat+%zu\n"), offset);
                        sb_appendf(output, c!("    mov QWORD [rbp-%zu], rax\n"), index*8);
                    }
                }
            },
            Op::AutoBinop{binop, index, lhs, rhs} => {
                match lhs {
                    Arg::AutoVar(index)     => sb_appendf(output, c!("    mov rax, [rbp-%zu]\n"), index*8),
                    Arg::Literal(value)     => sb_appendf(output, c!("    mov rax, %ld\n"), value),
                    Arg::DataOffset(offset) => sb_appendf(output, c!("    mov rax, dat+%zu"), offset),
                };
                match binop {
                    Binop::Plus => {
                        match rhs {
                            Arg::AutoVar(index)     => sb_appendf(output, c!("    add rax, [rbp-%zu]\n"), index*8),
                            Arg::Literal(value)     => sb_appendf(output, c!("    add rax, %ld\n"), value),
                            Arg::DataOffset(offset) => sb_appendf(output, c!("    add rax, dat+%zu"), offset),
                        };
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::Minus => {
                        match rhs {
                            Arg::AutoVar(index)     => sb_appendf(output, c!("    sub rax, [rbp-%zu]\n"), index*8),
                            Arg::Literal(value)     => sb_appendf(output, c!("    sub rax, %ld\n"), value),
                            Arg::DataOffset(offset) => sb_appendf(output, c!("    sub rax, dat+%zu\n"), offset),
                        };
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::Mult => {
                        sb_appendf(output, c!("    xor rdx, rdx\n"));
                        match rhs {
                            Arg::AutoVar(index) => {
                                // TODO: how do we even distinguish signed and unsigned mul in B?
                                sb_appendf(output, c!("    mul QWORD [rbp-%zu]\n"), index*8);
                            }
                            Arg::Literal(value) => {
                                sb_appendf(output, c!("    mov rbx, %ld\n"), value);
                                sb_appendf(output, c!("    mul rbx\n"));
                            }
                            Arg::DataOffset(offset) => {
                                // TODO: should this be even allowed? We are potentially multiplying by a string address in here.
                                sb_appendf(output, c!("    mov rbx, dat+%zu\n"), offset);
                                sb_appendf(output, c!("    mul rbx\n"));
                            }
                        };
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::Less => {
                        sb_appendf(output, c!("    xor rbx, rbx\n"), index*8);
                        match rhs {
                            Arg::AutoVar(index) => sb_appendf(output, c!("    cmp rax, [rbp-%zu]\n"), index*8),
                            Arg::Literal(value) => sb_appendf(output, c!("    cmp rax, %ld\n"), value),
                            Arg::DataOffset(_offset) => todo!(),
                        };
                        sb_appendf(output, c!("    setl bl\n"));
                        sb_appendf(output, c!("    mov QWORD [rbp-%zu], rbx\n"), index*8);
                    }
                }
            }
            Op::Funcall{name, args} => {
                const REGISTERS: *const[*const c_char] = &[c!("rdi"), c!("rsi"), c!("rdx")];
                if args.count > REGISTERS.len() {
                    todo!("Too many function call arguments. We support only {} but {} were provided", REGISTERS.len(), args.count);
                }
                for i in 0..args.count {
                    let reg = (*REGISTERS)[i];
                    match *args.items.add(i) {
                        Arg::AutoVar(index)     => sb_appendf(output, c!("    mov %s, [rbp-%zu]\n"), reg, index*8),
                        Arg::Literal(value)     => sb_appendf(output, c!("    mov %s, %ld\n"),       reg, value),
                        Arg::DataOffset(offset) => sb_appendf(output, c!("    mov %s, dat+%zu\n"),   reg, offset),
                    };
                }
                sb_appendf(output, c!("    call %s\n"), name);
            },
            Op::JmpIfNot{addr, arg} => {
                match arg {
                    Arg::AutoVar(index)     => sb_appendf(output, c!("    mov rax, [rbp-%zu]\n"), index*8),
                    Arg::Literal(value)     => sb_appendf(output, c!("    mov rax, %ld\n"), value),
                    Arg::DataOffset(offset) => sb_appendf(output, c!("    mov rax, dat+%zu\n"), offset),
                };
                sb_appendf(output, c!("    test rax, rax\n"));
                sb_appendf(output, c!("    jz .op_%zu\n"), addr);
            },
            Op::Jmp{addr} => {
                sb_appendf(output, c!("    jmp .op_%zu\n"), addr);
            },
        }
    }
    sb_appendf(output, c!(".op_%zu:\n"), body.len());
    sb_appendf(output, c!("    mov rsp, rbp\n"));
    sb_appendf(output, c!("    pop rbp\n"));
    sb_appendf(output, c!("    mov rax, 0\n"));
    sb_appendf(output, c!("    ret\n"));
}

unsafe fn generate_javascript_function(name: *const c_char, auto_vars_count: usize, body: *const [Op], output: *mut String_Builder) {
    sb_appendf(output, c!("function %s() {\n"), name);
    sb_appendf(output, c!("    let vars = Array(%zu).fill(0);\n"), auto_vars_count);
    for i in 0..body.len() {
        match (*body)[i] {
            Op::ExtrnVar(_name) => {},
            Op::AutoAssign{index, arg} => {
                match arg {
                    Arg::AutoVar(other_index) => {
                        sb_appendf(output, c!("    vars[%zu] = vars[%zu];\n"), index - 1, other_index - 1);
                    }
                    Arg::Literal(value) => {
                        sb_appendf(output, c!("    vars[%zu] = %ld;\n"), index - 1, value);
                    }
                    Arg::DataOffset(_offset) => todo!("DataOffset in js target"),
                }
            },
            Op::AutoBinop{binop, index, lhs, rhs} => {
                sb_appendf(output, c!("    vars[%zu] = "), index - 1);
                match lhs {
                    Arg::AutoVar(index) => sb_appendf(output, c!("vars[%zu]"), index - 1),
                    Arg::Literal(value) => sb_appendf(output, c!("%ld"), value),
                    Arg::DataOffset(_offset) => todo!("DataOffset in js target"),
                };
                match binop {
                    Binop::Plus  => sb_appendf(output, c!(" + ")),
                    Binop::Minus => sb_appendf(output, c!(" - ")),
                    Binop::Mult  => sb_appendf(output, c!(" * ")),
                    Binop::Less  => sb_appendf(output, c!(" < ")),
                };
                match rhs {
                    Arg::AutoVar(index) => sb_appendf(output, c!("vars[%zu]"), index - 1),
                    Arg::Literal(value) => sb_appendf(output, c!("%ld"), value),
                    Arg::DataOffset(_offset) => todo!("DataOffset in js target"),
                };
                sb_appendf(output, c!(";\n"));
            }
            Op::Funcall{name, args} => {
                sb_appendf(output, c!("    %s("), name);
                for i in 0..args.count {
                    if i > 0 { sb_appendf(output, c!(", ")); }
                    match *args.items.add(i) {
                        Arg::AutoVar(index) => sb_appendf(output, c!("vars[%zu]"), index - 1),
                        Arg::Literal(value) => sb_appendf(output, c!("%ld"), value),
                        Arg::DataOffset(_offset) => todo!("DataOffset in js target"),
                    };
                }
                sb_appendf(output, c!(");\n"));
            },
            Op::JmpIfNot{..} => todo!(),
            Op::Jmp{..} => todo!(),
        }
    }
    sb_appendf(output, c!("}\n"));
}

pub unsafe fn generate_function(name: *const c_char, auto_vars_count: usize, body: *const [Op], output: *mut String_Builder, target: Target) {
    match target {
        Target::Fasm_x86_64_Linux => generate_fasm_x86_64_linux_function(name, auto_vars_count, body, output),
        Target::JavaScript        => generate_javascript_function(name, auto_vars_count, body, output),
        Target::IR => {
            sb_appendf(output, c!("%s:\n"), name);
            for i in 0..body.len() {
                sb_appendf(output, c!("%8zu"), i);
                match (*body)[i] {
                    Op::ExtrnVar(name) => {
                        sb_appendf(output, c!("    ExtrnVar(\"%s\")\n"), name);
                    }
                    Op::AutoAssign{index, arg} => {
                        sb_appendf(output, c!("    AutoAssign(%zu, "), index);
                        dump_arg(output, arg);
                        sb_appendf(output, c!(")\n"));
                    }
                    Op::AutoBinop{binop, index, lhs, rhs} => {
                        sb_appendf(output, c!("    AutoBinop("));
                        match binop {
                            Binop::Plus  => sb_appendf(output, c!("Plus")),
                            Binop::Minus => sb_appendf(output, c!("Minus")),
                            Binop::Mult  => sb_appendf(output, c!("Mult")),
                            Binop::Less  => sb_appendf(output, c!("Less")),
                        };
                        sb_appendf(output, c!(", %zu, "), index);
                        dump_arg(output, lhs);
                        sb_appendf(output, c!(", "));
                        dump_arg(output, rhs);
                        sb_appendf(output, c!(")\n"));
                    }
                    Op::Funcall{name, args} => {
                        sb_appendf(output, c!("    Funcall(\"%s\""), name);
                        for i in 0..args.count {
                            sb_appendf(output, c!(", "));
                            dump_arg(output, *args.items.add(i));
                        }
                        sb_appendf(output, c!(")\n"));
                    }
                    Op::JmpIfNot{addr, arg} => {
                        sb_appendf(output, c!("    JmpIfNot(%zu, "), addr);
                        dump_arg(output, arg);
                        sb_appendf(output, c!(")\n"));
                    }
                    Op::Jmp{addr} => {
                        sb_appendf(output, c!("    Jmp(%zu)\n"), addr);
                    }
                }
            }
        }
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, target: Target, data: *const [u8]) {
    match target {
        Target::Fasm_x86_64_Linux => {
            if data.len() > 0 {
                sb_appendf(output, c!("section \".data\"\n"));
                sb_appendf(output, c!("dat: db "));
                for i in 0..data.len() {
                    if i > 0 {
                        sb_appendf(output, c!(","));
                    }
                    sb_appendf(output, c!("0x%02X"), (*data)[i] as c_uint);
                }
                sb_appendf(output, c!("\n"));
            }
        }
        Target::JavaScript => {
            // TODO: Generate data section for js target
        }
        Target::IR => {
            if data.len() > 0 {
                sb_appendf(output, c!("\n"));
                // TODO: display the IR Data Section in hex editor style
                sb_appendf(output, c!("-- Data Section --\n"));
                sb_appendf(output, c!("    "));
                for i in 0..data.len() {
                    if i > 0 {
                        sb_appendf(output, c!(","));
                    }
                    sb_appendf(output, c!("0x%02X"), (*data)[i] as c_uint);
                }
                sb_appendf(output, c!("\n"));
            }
        }
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

pub unsafe fn compile_primary_expression(l: *mut stb_lexer, input_path: *const c_char, vars: *const Array<Array<Var>>, auto_vars_ator: *mut AutoVarsAtor, func_body: *mut Array<Op>, data: *mut Array<u8>) -> Option<Arg> {
    stb_c_lexer_get_token(l);
    match (*l).token {
        token if token == '(' as i64 => {
            let expr = compile_expression(l, input_path, vars, auto_vars_ator, func_body, data)?;
            if !get_and_expect_clex(l, input_path, ')' as i64) { return None }
            Some(expr)
        }
        CLEX_intlit => return Some(Arg::Literal((*l).int_number)),
        CLEX_id => {
            let name = (*l).string;
            let name_where = (*l).where_firstchar;
            let var_def = find_var_deep(vars, name);
            if var_def.is_null() {
                diagf!(l, input_path, name_where, c!("ERROR: could not find variable `%s`\n"), name);
                return None;
            }
            match (*var_def).storage {
                Storage::Auto{index} => return Some(Arg::AutoVar(index)),
                Storage::External{..} => {
                    missingf!(l, input_path, name_where, c!("external variables in lvalues are not supported yet\n"));
                }
            }
        }
        CLEX_dqstring => {
            let offset = (*data).count;
            let string_len = strlen((*l).string);
            da_append_many(data, slice::from_raw_parts((*l).string as *const u8, string_len));
            // TODO: Strings in B are not NULL-terminated.
            // They are terminated with symbol '*e' ('*' is escape character akin to '\' in C) which according to the
            // spec is called just "end-of-file" without any elaboration on what its value is. Maybe it had a specific
            // value on PDP that was a common knowledge at the time? In any case that breaks compatibility with
            // libc. While the language is still in development we gonna terminate it with 0. We will make it
            // "spec complaint" later.
            da_append(data, 0); // NULL-terminator
            Some(Arg::DataOffset(offset))
        }
        _ => {
            missingf!(l, input_path, (*l).where_firstchar, c!("Unexpected token %s not all expressions are implemented yet\n"), display_token_kind_temp((*l).token));
        }
    }
}

pub unsafe fn compile_binop_expression(l: *mut stb_lexer, input_path: *const c_char, vars: *const Array<Array<Var>>, auto_vars_ator: *mut AutoVarsAtor, func_body: *mut Array<Op>, data: *mut Array<u8>, precedence: usize) -> Option<Arg> {
    if precedence >= Binop::MAX_PRECEDENCE {
        return compile_primary_expression(l, input_path, vars, auto_vars_ator, func_body, data);
    }

    let mut lhs = compile_binop_expression(l, input_path, vars, auto_vars_ator, func_body, data, precedence + 1)?;

    let mut saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);

    if let Some(binop) = Binop::from_token((*l).token) {
        if binop.precedence() == precedence {
            let index = allocate_auto_var(auto_vars_ator);
            while let Some(binop) = Binop::from_token((*l).token) {
                if binop.precedence() != precedence { break; }

                let token = (*l).token;
                let rhs = compile_binop_expression(l, input_path, vars, auto_vars_ator, func_body, data, precedence)?;
                da_append(func_body, Op::AutoBinop {binop: Binop::from_token(token).unwrap(), index, lhs, rhs});
                lhs = Arg::AutoVar(index);

                saved_point = (*l).parse_point;
                stb_c_lexer_get_token(l);
            }
        }
    }

    (*l).parse_point = saved_point;
    Some(lhs)
}

pub unsafe fn compile_expression(l: *mut stb_lexer, input_path: *const c_char, vars: *const Array<Array<Var>>, auto_vars_ator: *mut AutoVarsAtor, func_body: *mut Array<Op>, data: *mut Array<u8>) -> Option<Arg> {
    compile_binop_expression(l, input_path, vars, auto_vars_ator, func_body, data, 0)
}

pub unsafe fn compile_block(l: *mut stb_lexer, input_path: *const c_char, vars: *mut Array<Array<Var>>, auto_vars_ator: *mut AutoVarsAtor, func_body: *mut Array<Op>, data: *mut Array<u8>) -> bool {
    loop {
        let saved_point = (*l).parse_point;
        stb_c_lexer_get_token(l);
        if (*l).token == '}' as c_long { return true; }
        (*l).parse_point = saved_point;

        if !compile_stmt(l, input_path, vars, auto_vars_ator, func_body, data) { return false; }
    }
}

pub unsafe fn compile_function_call(l: *mut stb_lexer, input_path: *const c_char, vars: *mut Array<Array<Var>>, auto_vars_ator: *mut AutoVarsAtor, func_body: *mut Array<Op>, data: *mut Array<u8>, name: *const c_char, name_where: *const c_char) -> bool {
    let var_def = find_var_deep(vars, name);
    if var_def.is_null() {
        diagf!(l, input_path, name_where, c!("ERROR: could not find function `%s`\n"), name);
        return false;
    }

    let mut args: Array<Arg> = zeroed();
    let saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);
    if (*l).token != ')' as c_long {
        (*l).parse_point = saved_point;
        loop {
            if let Some(expr) = compile_expression(l, input_path, vars, auto_vars_ator, func_body, data) {
                da_append(&mut args, expr)
            } else {
                return false;
            }
            stb_c_lexer_get_token(l);
            if !expect_clexes(l, input_path, &[')' as c_long, ',' as c_long]) { return false; }
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
            da_append(func_body, Op::Funcall {name, args});
        }
        Storage::Auto{..} => {
            missingf!(l, input_path, name_where, c!("calling functions from auto variables\n"));
        }
    }

    true
}

pub unsafe fn compile_stmt(l: *mut stb_lexer, input_path: *const c_char, vars: *mut Array<Array<Var>>, auto_vars_ator: *mut AutoVarsAtor, func_body: *mut Array<Op>, data: *mut Array<u8>) -> bool {
    stb_c_lexer_get_token(l);

    if (*l).token == '{' as c_long {
        scope_push(vars);
            let saved_auto_vars_count = (*auto_vars_ator).count;
                if !compile_block(l, input_path, vars, auto_vars_ator, func_body, data) { return false; }
            (*auto_vars_ator).count = saved_auto_vars_count;
        scope_pop(vars);
        true
    } else {
        if !expect_clex(l, input_path, CLEX_id) { return false; }
        if strcmp((*l).string, c!("extrn")) == 0 {
            // TODO: support multiple extrn declarations
            if !get_and_expect_clex(l, input_path, CLEX_id) { return false; }
            let name = strdup((*l).string);
            let name_where = (*l).where_firstchar;
            if !declare_var(l, input_path, vars, name, name_where, Storage::External{name}) { return false; }
            da_append(func_body, Op::ExtrnVar(name));
            get_and_expect_clex(l, input_path, ';' as c_long)
        } else if strcmp((*l).string, c!("auto")) == 0 {
            'vars: loop {
                if !get_and_expect_clex(l, input_path, CLEX_id) { return false; }
                let name = strdup((*l).string);
                let name_where = (*l).where_firstchar;
                let index = allocate_auto_var(auto_vars_ator);
                if !declare_var(l, input_path, vars, name, name_where, Storage::Auto{index}) { return false; }
                stb_c_lexer_get_token(l);
                if !expect_clexes(l, input_path, &[',' as c_long, ';' as c_long]) { return false }
                if (*l).token == ';' as c_long {
                    break 'vars;
                } else if (*l).token == ',' as c_long {
                    continue 'vars;
                } else {
                    unreachable!()
                }
            }

            true
        } else if strcmp((*l).string, c!("while")) == 0 {
            let begin = (*func_body).count;
            if !get_and_expect_clex(l, input_path, '(' as c_long) { return false; }
            let saved_auto_vars_count = (*auto_vars_ator).count;
            if let Some(arg) = compile_expression(l, input_path, vars, auto_vars_ator, func_body, data) {
                if !get_and_expect_clex(l, input_path, ')' as c_long) { return false; }
                let condition_jump = (*func_body).count;
                da_append(func_body, Op::JmpIfNot{addr: 0, arg});
                (*auto_vars_ator).count = saved_auto_vars_count;

                if !compile_stmt(l, input_path, vars, auto_vars_ator, func_body, data) { return false; }
                da_append(func_body, Op::Jmp{addr: begin});
                let end = (*func_body).count;
                (*(*func_body).items.add(condition_jump)) = Op::JmpIfNot{addr: end, arg};
            } else {
                return false;
            }

            true
        } else {
            let name = strdup((*l).string);
            let name_where = (*l).where_firstchar;

            stb_c_lexer_get_token(l);
            // TODO: assignment and function call must be expressions
            if (*l).token == '=' as c_long {
                let var_def = find_var_deep(vars, name);
                if var_def.is_null() {
                    diagf!(l, input_path, name_where, c!("ERROR: could not find variable `%s`\n"), name);
                    return false;
                }

                let saved_auto_vars_count = (*auto_vars_ator).count;
                match (*var_def).storage {
                    Storage::Auto{index} => {
                        if let Some(arg) = compile_expression(l, input_path, vars, auto_vars_ator, func_body, data) {
                            da_append(func_body, Op::AutoAssign{index, arg})
                        } else {
                            return false;
                        }
                    }
                    Storage::External{..} => {
                        missingf!(l, input_path, name_where, c!("assignment to external variables\n"));
                    }
                }
                (*auto_vars_ator).count = saved_auto_vars_count;

                get_and_expect_clex(l, input_path, ';' as c_long)
            } else if (*l).token == '(' as c_long {
                let saved_auto_vars_count = (*auto_vars_ator).count;
                if !compile_function_call(l, input_path, vars, auto_vars_ator, func_body, data, name, name_where) { return false; }
                (*auto_vars_ator).count = saved_auto_vars_count;
                get_and_expect_clex(l, input_path, ';' as c_long)
            } else {
                diagf!(l, input_path, (*l).where_firstchar, c!("ERROR: unexpected token %s\n"), display_token_kind_temp((*l).token));
                false
            }
        }
    }
}

pub unsafe fn usage(target_name_flag: *mut*mut c_char) {
    fprintf(stderr, c!("Usage: %s [OPTIONS] <input.b>\n"), flag_program_name());
    fprintf(stderr, c!("OPTIONS:\n"));
    flag_print_options(stderr);
    fprintf(stderr, c!("Compilation targets:\n"));
    for i in 0..TARGET_NAMES.len() {
        fprintf(stderr, c!("    -%s %s\n"), flag_name(target_name_flag), (*TARGET_NAMES)[i].name);
    }
}

pub unsafe fn temp_default_output_path(input_path: *const c_char) -> *const c_char {
    let mut input_path_sv = sv_from_cstr(input_path);
    let b_ext = c!(".b");
    let b_ext_len = strlen(b_ext);
    if sv_end_with(input_path_sv, b_ext) {
        input_path_sv.count -= b_ext_len;
        temp_sv_to_cstr(input_path_sv)
    } else {
        temp_sprintf(c!("%s.out"), input_path)
    }
}

pub unsafe fn main(mut argc: i32, mut argv: *mut*mut c_char) -> i32 {
    let default_target_name = name_of_target(Target::Fasm_x86_64_Linux).expect("default target name not found");

    // TODO: some sort of a -run flag that automatically runs the executable
    let target_name = flag_str(c!("target"), default_target_name, c!("Compilation target"));
    let output_path_flag = flag_str(c!("o"), ptr::null(), c!("Output path (MANDATORY)"));
    let help        = flag_bool(c!("help"), false, c!("Print this help message"));
    // TODO: pass user flags to the linker

    let mut input_path: *const c_char = ptr::null();
    while argc > 0 {
        if !flag_parse(argc, argv) {
            usage(target_name);
            flag_print_error(stderr);
            return 1;
        }
        argc = flag_rest_argc();
        argv = flag_rest_argv();
        if argc > 0 {
            if !input_path.is_null() {
                // TODO: support compiling several files?
                fprintf(stderr, c!("ERROR: Serveral input files is not supported yet\n"));
                return 1;
            }
            input_path = shift!(argv, argc);
        }
    }

    if *help {
        usage(target_name);
        return 0;
    }

    if input_path.is_null() {
        usage(target_name);
        fprintf(stderr, c!("ERROR: no input is provided\n"));
        return 1;
    }

    let output_path;
    if (*output_path_flag).is_null() {
        output_path = temp_default_output_path(input_path);
    } else {
        output_path = *output_path_flag;
    }

    let Some(target) = target_by_name(*target_name) else {
        usage(target_name);
        fprintf(stderr, c!("ERROR: unknown target `%s`\n"), *target_name);
        return 1;
    };

    let mut cmd: Cmd = zeroed();
    let mut vars: Array<Array<Var>> = zeroed();
    let mut func_body: Array<Op> = zeroed();
    let mut data: Array<u8> = zeroed();

    let mut input: String_Builder = zeroed();
    if !read_entire_file(input_path, &mut input) { return 1; }

    let mut l: stb_lexer = zeroed();
    let mut string_store: [c_char; 1024] = zeroed(); // TODO: size of identifiers and string literals is limited because of stb_c_lexer.h
    stb_c_lexer_init(&mut l, input.items, input.items.add(input.count), string_store.as_mut_ptr(), string_store.len() as i32);

    let mut output: String_Builder = zeroed();
    generate_executable(&mut output, target);

    scope_push(&mut vars);          // global scope

    'def: loop {
        stb_c_lexer_get_token(&mut l);
        if l.token == CLEX_eof { break 'def }

        if !expect_clex(&mut l, input_path, CLEX_id) { return 1; }

        let symbol_name = strdup(l.string);
        let symbol_name_where = l.where_firstchar;

        // TODO: maybe the keywords should be identified on the level of lexing
        if is_keyword(l.string) {
            diagf!(&l, input_path, symbol_name_where, c!("ERROR: Trying to define a reserved keyword `%s` as a symbol. Please choose a different name.\n"), symbol_name);
            diagf!(&l, input_path, symbol_name_where, c!("NOTE: Reserved keywords are: "));
            for i in 0..B_KEYWORDS.len() {
                if i > 0 {
                    fprintf(stderr, c!(", "));
                }
                fprintf(stderr, c!("`%s`"), (*B_KEYWORDS)[i]);
            }
            fprintf(stderr, c!("\n"));
            return 69;
        }

        stb_c_lexer_get_token(&mut l);
        if l.token == '(' as c_long { // Function definition
            // TODO: functions with several parameters
            if !get_and_expect_clex(&mut l, input_path, ')' as c_long) { return 1; }

            scope_push(&mut vars);          // begin function scope
                let mut auto_vars_ator: AutoVarsAtor = zeroed();
                if !compile_stmt(&mut l, input_path, &mut vars, &mut auto_vars_ator, &mut func_body, &mut data) { return 1; }
                generate_function(symbol_name, auto_vars_ator.max, da_slice(func_body), &mut output, target);
            scope_pop(&mut vars);          // end function scope

            declare_var(&mut l, input_path, &mut vars, symbol_name, symbol_name_where, Storage::External{name: symbol_name});

            func_body.count = 0;
        } else { // Variable definition
            missingf!(&l, input_path, l.where_firstchar, c!("variable definitions\n"));
        }
    }

    generate_data_section(&mut output, target, da_slice(data));

    match target {
        Target::Fasm_x86_64_Linux => {
            let output_asm_path = temp_sprintf(c!("%s.asm"), output_path);
            let output_obj_path = temp_sprintf(c!("%s.o"), output_path);
            if !write_entire_file(output_asm_path, output.items as *const c_void, output.count) { return 69 }
            cmd_append! {
                &mut cmd,
                c!("fasm"), output_asm_path, output_obj_path,
            }
            if !cmd_run_sync_and_reset(&mut cmd) { return 1 }
            cmd_append! {
                &mut cmd,
                c!("cc"), c!("-no-pie"), c!("-o"), output_path, output_obj_path,
            }
            if !cmd_run_sync_and_reset(&mut cmd) { return 1 }
        }
        Target::JavaScript => {
            // TODO: make the js target automatically generate the html file
            if !write_entire_file(output_path, output.items as *const c_void, output.count) { return 69 }
        }
        Target::IR => {
            if !write_entire_file(output_path, output.items as *const c_void, output.count) { return 69 }
        }
    }
    0
}

// TODO: B lexing is different from the C one.
//   Hack stb_c_lexer.h into stb_b_lexer.h
