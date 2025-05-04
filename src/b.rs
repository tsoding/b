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
    ($l:expr, $path:expr, $where:expr, $fmt:literal $($args:tt)*) => {{
        let mut loc: stb_lex_location = zeroed();
        stb_c_lexer_get_location($l, $where, &mut loc);
        fprintf!(stderr, c"%s:%d:%d: ", $path, loc.line_number, loc.line_offset + 1);
        fprintf!(stderr, $fmt $($args)*);
    }};
}

macro_rules! missingf {
    ($l:expr, $path:expr, $where:expr, $fmt:literal $($args:tt)*) => {{
        let file = file!();
        let mut loc: stb_lex_location = zeroed();
        stb_c_lexer_get_location($l, $where, &mut loc);
        fprintf!(stderr, c"%s:%d:%d: TODO: ", $path, loc.line_number, loc.line_offset + 1);
        fprintf!(stderr, $fmt $($args)*);
        fprintf!(stderr, c"%.*s:%d: INFO: implementation should go here\n", file.len(), file.as_ptr(), line!());
        abort();
    }}
}

unsafe fn display_token_kind_temp(token: c_long) -> *const c_char {
    match token {
        CLEX_id         => c"identifier".as_ptr(),
        CLEX_eq         => c"==".as_ptr(),
        CLEX_noteq      => c"!=".as_ptr(),
        CLEX_lesseq     => c"<=".as_ptr(),
        CLEX_greatereq  => c">=".as_ptr(),
        CLEX_andand     => c"&&".as_ptr(),
        CLEX_oror       => c"||".as_ptr(),
        CLEX_shl        => c"<<".as_ptr(),
        CLEX_shr        => c">>".as_ptr(),
        CLEX_plusplus   => c"++".as_ptr(),
        CLEX_minusminus => c"--".as_ptr(),
        CLEX_arrow      => c"->".as_ptr(),
        CLEX_andeq      => c"&=".as_ptr(),
        CLEX_oreq       => c"|=".as_ptr(),
        CLEX_xoreq      => c"^=".as_ptr(),
        CLEX_pluseq     => c"+=".as_ptr(),
        CLEX_minuseq    => c"-=".as_ptr(),
        CLEX_muleq      => c"*=".as_ptr(),
        CLEX_diveq      => c"/=".as_ptr(),
        CLEX_modeq      => c"%%=".as_ptr(),
        CLEX_shleq      => c"<<=".as_ptr(),
        CLEX_shreq      => c">>=".as_ptr(),
        CLEX_eqarrow    => c"=>".as_ptr(),
        CLEX_dqstring   => c"string literal".as_ptr(),
        // NOTE: single quote strings are opt-in in stb_c_lexer.h (see STB_C_LEX_C_SQ_STRINGS)
        CLEX_sqstring   => c"single quote literal".as_ptr(),
        CLEX_charlit    => c"character literal".as_ptr(),
        CLEX_intlit     => c"integer literal".as_ptr(),
        CLEX_floatlit   => c"floating-point literal".as_ptr(),
        _ => {
            if token >= 0 && token < 256 {
                temp_sprintf(c"`%c`".as_ptr(), token)
            } else {
                temp_sprintf(c"<<<UNKNOWN TOKEN %ld>>>".as_ptr(), token)
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

    diagf!(l, input_path, (*l).where_firstchar, c"ERROR: expected %s, but got %s\n", sb.items, display_token_kind_temp((*l).token));

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
        diagf!(l, input_path, name_where, c"ERROR: redefinition of variable `%s`\n", name);
        diagf!(l, input_path, (*existing_var).hwere, c"NOTE: the first declaration is located here\n");
        return false;
    }

    da_append(scope, Var {name, storage, hwere: name_where});
    true
}

const B_KEYWORDS: *const [*const c_char] = &[
    c"auto".as_ptr(),
    c"extrn".as_ptr(),
    c"case".as_ptr(),
    c"if".as_ptr(),
    c"while".as_ptr(),
    c"switch".as_ptr(),
    c"goto".as_ptr(),
    c"return".as_ptr(),
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

#[derive(Clone, Copy)]
pub enum Binop {
    Plus,
    Minus,
    Mult,
}

impl Binop {
    pub fn from_token(token: c_long) -> Option<Self> {
        match token {
            token if token == '+' as i64 => Some(Binop::Plus),
            token if token == '-' as i64 => Some(Binop::Minus),
            token if token == '*' as i64 => Some(Binop::Mult),
            _ => None,
        }
    }
}


// TODO: associate location within the source code with each op
#[derive(Clone, Copy)]
pub enum Op {
    AutoAlloc(usize),
    ExtrnVar(*const c_char),
    AutoBinop  {binop: Binop, index: usize, lhs: Arg, rhs: Arg},
    AutoAssign {index: usize, arg: Arg},
    Funcall    {name: *const c_char, arg: Option<Arg>},
}

pub unsafe fn dump_arg(output: *mut String_Builder, arg: Arg) {
    match arg {
        Arg::Literal(value) => sb_appendf(output, c"Literal(%ld)".as_ptr(), value),
        Arg::AutoVar(index) => sb_appendf(output, c"AutoVar(%zu)".as_ptr(), index),
        Arg::DataOffset(offset) => sb_appendf(output, c"DataOffset(%zu)".as_ptr(), offset),
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
    Target_Name { name: c"fasm_x86_64_linux".as_ptr(), target: Target::Fasm_x86_64_Linux },
    Target_Name { name: c"js".as_ptr(),                target: Target::JavaScript        },
    Target_Name { name: c"ir".as_ptr(),                target: Target::IR                },
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
    sb_appendf(output, c"format ELF64\n".as_ptr());
    sb_appendf(output, c"section \".text\" executable\n".as_ptr());
}

unsafe fn generate_javascript_executable(output: *mut String_Builder) {
    sb_appendf(output, c"\"use strict\"\n".as_ptr());
}

unsafe fn generate_executable(output: *mut String_Builder, target: Target) {
    match target {
        Target::Fasm_x86_64_Linux => generate_fasm_x86_64_linux_executable(output),
        Target::JavaScript        => generate_javascript_executable(output),
        Target::IR                => {}
    }
}

unsafe fn generate_fasm_x86_64_linux_func_prolog(name: *const c_char, output: *mut String_Builder) {
    sb_appendf(output, c"public %s\n".as_ptr(), name);
    sb_appendf(output, c"%s:\n".as_ptr(), name);
    sb_appendf(output, c"    push rbp\n".as_ptr());
    sb_appendf(output, c"    mov rbp, rsp\n".as_ptr());
}

unsafe fn generate_javascript_func_prolog(name: *const c_char, output: *mut String_Builder) {
    sb_appendf(output, c"function %s() {\n".as_ptr(), name);
    sb_appendf(output, c"    let vars = [];\n".as_ptr(), name);
}

unsafe fn generate_func_prolog(name: *const c_char, output: *mut String_Builder, target: Target) {
    match target {
        Target::Fasm_x86_64_Linux => generate_fasm_x86_64_linux_func_prolog(name, output),
        Target::JavaScript => generate_javascript_func_prolog(name, output),
        Target::IR => {
            sb_appendf(output, c"%s:\n".as_ptr(), name);
        }
    }
}

unsafe fn generate_fasm_x86_64_linux_func_epilog(output: *mut String_Builder) {
    sb_appendf(output, c"    mov rsp, rbp\n".as_ptr());
    sb_appendf(output, c"    pop rbp\n".as_ptr());
    sb_appendf(output, c"    mov rax, 0\n".as_ptr());
    sb_appendf(output, c"    ret\n".as_ptr());
}

unsafe fn generate_javascript_func_epilog(output: *mut String_Builder) {
    sb_appendf(output, c"}\n".as_ptr());
}

unsafe fn generate_func_epilog(output: *mut String_Builder, target: Target) {
    match target {
        Target::Fasm_x86_64_Linux => generate_fasm_x86_64_linux_func_epilog(output),
        Target::JavaScript => generate_javascript_func_epilog(output),
        Target::IR => {}
    }
}

unsafe fn generate_fasm_x86_64_linux_func_body(body: *const [Op], output: *mut String_Builder) {
    for i in 0..body.len() {
        match (*body)[i] {
            Op::AutoAlloc(count) => {
                sb_appendf(output, c"    sub rsp, %zu\n".as_ptr(), count*8);
            },
            Op::ExtrnVar(name) => {
                sb_appendf(output, c"    extrn %s\n".as_ptr(), name);
            },
            Op::AutoAssign{index, arg} => {
                match arg {
                    Arg::AutoVar(other_index) => {
                        sb_appendf(output, c"    mov rax, [rbp-%zu]\n".as_ptr(), other_index*8);
                        sb_appendf(output, c"    mov QWORD [rbp-%zu], rax\n".as_ptr(), index*8);
                    }
                    Arg::Literal(value) => {
                        sb_appendf(output, c"    mov QWORD [rbp-%zu], %ld\n".as_ptr(), index*8, value);
                    }
                    Arg::DataOffset(offset) => {
                        sb_appendf(output, c"    mov rax, dat+%zu\n".as_ptr(), offset);
                        sb_appendf(output, c"    mov QWORD [rbp-%zu], rax\n".as_ptr(), index*8);
                    }
                }
            },
            Op::AutoBinop{binop, index, lhs, rhs} => {
                match lhs {
                    Arg::AutoVar(index)     => sb_appendf(output, c"    mov rax, [rbp-%zu]\n".as_ptr(), index*8),
                    Arg::Literal(value)     => sb_appendf(output, c"    mov rax, %ld\n".as_ptr(), value),
                    Arg::DataOffset(offset) => sb_appendf(output, c"    mov rax, dat+%zu".as_ptr(), offset),
                };
                match binop {
                    Binop::Plus => {
                        match rhs {
                            Arg::AutoVar(index)     => sb_appendf(output, c"    add rax, [rbp-%zu]\n".as_ptr(), index*8),
                            Arg::Literal(value)     => sb_appendf(output, c"    add rax, %ld\n".as_ptr(), value),
                            Arg::DataOffset(offset) => sb_appendf(output, c"    add rax, dat+%zu".as_ptr(), offset),
                        };
                        sb_appendf(output, c"    mov [rbp-%zu], rax\n".as_ptr(), index*8);
                    }
                    Binop::Minus => {
                        match rhs {
                            Arg::AutoVar(index)     => sb_appendf(output, c"    sub rax, [rbp-%zu]\n".as_ptr(), index*8),
                            Arg::Literal(value)     => sb_appendf(output, c"    sub rax, %ld\n".as_ptr(), value),
                            Arg::DataOffset(offset) => sb_appendf(output, c"    sub rax, dat+%zu\n".as_ptr(), offset),
                        };
                        sb_appendf(output, c"    mov [rbp-%zu], rax\n".as_ptr(), index*8);
                    }
                    Binop::Mult => {
                        sb_appendf(output, c"    xor rdx, rdx\n".as_ptr());
                        match rhs {
                            Arg::AutoVar(index) => {
                                // TODO: how do we even distinguish signed and unsigned mul in B?
                                sb_appendf(output, c"    mul QWORD [rbp-%zu]\n".as_ptr(), index*8);
                            }
                            Arg::Literal(value) => {
                                sb_appendf(output, c"    mov rbx, %ld\n".as_ptr(), value);
                                sb_appendf(output, c"    mul rbx\n".as_ptr());
                            }
                            Arg::DataOffset(offset) => {
                                // TODO: should this be even allowed? We are potentially multiplying by a string address in here.
                                sb_appendf(output, c"    mov rbx, dat+%zu\n".as_ptr(), offset);
                                sb_appendf(output, c"    mul rbx\n".as_ptr());
                            }
                        };
                        sb_appendf(output, c"    mov [rbp-%zu], rax\n".as_ptr(), index*8);
                    }
                }
            }
            Op::Funcall{name, arg} => {
                if let Some(arg) = arg {
                    match arg {
                        Arg::AutoVar(index)     => sb_appendf(output, c"    mov rdi, [rbp-%zu]\n".as_ptr(), index*8),
                        Arg::Literal(value)     => sb_appendf(output, c"    mov rdi, %ld\n".as_ptr(), value),
                        Arg::DataOffset(offset) => sb_appendf(output, c"    mov rdi, dat+%zu\n".as_ptr(), offset),
                    };
                }
                sb_appendf(output, c"    call %s\n".as_ptr(), name);
            },
        }
    }
}

unsafe fn generate_javascript_func_body(body: *const [Op], output: *mut String_Builder) {
    for i in 0..body.len() {
        match (*body)[i] {
            Op::AutoAlloc(count) => {
                for _ in 0..count {
                    sb_appendf(output, c"    vars.push(0);\n".as_ptr());
                }
            },
            Op::ExtrnVar(_name) => {},
            Op::AutoAssign{index, arg} => {
                match arg {
                    Arg::AutoVar(other_index) => {
                        sb_appendf(output, c"    vars[%zu] = vars[%zu];\n".as_ptr(), index - 1, other_index - 1);
                    }
                    Arg::Literal(value) => {
                        sb_appendf(output, c"    vars[%zu] = %ld;\n".as_ptr(), index - 1, value);
                    }
                    Arg::DataOffset(_offset) => todo!("DataOffset in js target"),
                }
            },
            Op::AutoBinop{binop, index, lhs, rhs} => {
                sb_appendf(output, c"    vars[%zu] = ".as_ptr(), index - 1);
                match lhs {
                    Arg::AutoVar(index) => sb_appendf(output, c"vars[%zu]".as_ptr(), index - 1),
                    Arg::Literal(value) => sb_appendf(output, c"%ld".as_ptr(), value),
                    Arg::DataOffset(_offset) => todo!("DataOffset in js target"),
                };
                match binop {
                    Binop::Plus  => sb_appendf(output, c" + ".as_ptr()),
                    Binop::Minus => sb_appendf(output, c" - ".as_ptr()),
                    Binop::Mult  => sb_appendf(output, c" * ".as_ptr()),
                };
                match rhs {
                    Arg::AutoVar(index) => sb_appendf(output, c"vars[%zu]".as_ptr(), index - 1),
                    Arg::Literal(value) => sb_appendf(output, c"%ld".as_ptr(), value),
                    Arg::DataOffset(_offset) => todo!("DataOffset in js target"),
                };
                sb_appendf(output, c";\n".as_ptr());
            }
            Op::Funcall{name, arg} => {
                if let Some(arg) = arg {
                    match arg {
                        Arg::AutoVar(index) => sb_appendf(output, c"    %s(vars[%zu]);\n".as_ptr(), name, index - 1),
                        Arg::Literal(value) => sb_appendf(output, c"    %s(%ld);\n".as_ptr(), name, value),
                        Arg::DataOffset(_offset) => todo!("DataOffset in js target"),
                    };
                } else {
                    sb_appendf(output, c"    %s();\n".as_ptr(), name);
                }
            },
        }
    }
}

pub unsafe fn generate_func_body(body: *const [Op], output: *mut String_Builder, target: Target) {
    match target {
        Target::Fasm_x86_64_Linux => generate_fasm_x86_64_linux_func_body(body, output),
        Target::JavaScript => generate_javascript_func_body(body, output),
        Target::IR => {
            for i in 0..body.len() {
                match (*body)[i] {
                    Op::AutoAlloc(index) => {
                        sb_appendf(output, c"    AutoAlloc(%zu)\n".as_ptr(), index);
                    }
                    Op::ExtrnVar(name) => {
                        sb_appendf(output, c"    ExtrnVar(\"%s\")\n".as_ptr(), name);
                    }
                    Op::AutoAssign{index, arg} => {
                        sb_appendf(output, c"    AutoAssign(%zu, ".as_ptr(), index);
                        dump_arg(output, arg);
                        sb_appendf(output, c")\n".as_ptr());
                    }
                    Op::AutoBinop{binop, index, lhs, rhs} => {
                        sb_appendf(output, c"    AutoBinop(".as_ptr());
                        match binop {
                            Binop::Plus  => sb_appendf(output, c"Plus".as_ptr()),
                            Binop::Minus => sb_appendf(output, c"Minus".as_ptr()),
                            Binop::Mult  => sb_appendf(output, c"Mult".as_ptr()),
                        };
                        sb_appendf(output, c", %zu, ".as_ptr(), index);
                        dump_arg(output, lhs);
                        sb_appendf(output, c", ".as_ptr());
                        dump_arg(output, rhs);
                        sb_appendf(output, c")\n".as_ptr());
                    }
                    Op::Funcall{name, arg} => {
                        match arg {
                            Some(arg) => {
                                sb_appendf(output, c"    Funcall(\"%s\", ".as_ptr(), name);
                                dump_arg(output, arg);
                                sb_appendf(output, c")\n".as_ptr());
                            }
                            None => {
                                sb_appendf(output, c"    Funcall(\"%s\")\n".as_ptr(), name);
                            }
                        }
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
                sb_appendf(output, c"section \".data\"\n".as_ptr());
                sb_appendf(output, c"dat: db ".as_ptr());
                for i in 0..data.len() {
                    if i > 0 {
                        sb_appendf(output, c",".as_ptr());
                    }
                    sb_appendf(output, c"0x%02X".as_ptr(), (*data)[i] as c_uint);
                }
                sb_appendf(output, c"\n".as_ptr());
            }
        }
        Target::JavaScript => {
            // TODO: Generate data section for js target
        }
        Target::IR => {
            if data.len() > 0 {
                sb_appendf(output, c"\n".as_ptr());
                sb_appendf(output, c"-- Data Section --\n".as_ptr());
                sb_appendf(output, c"    ".as_ptr());
                for i in 0..data.len() {
                    if i > 0 {
                        sb_appendf(output, c",".as_ptr());
                    }
                    sb_appendf(output, c"0x%02X".as_ptr(), (*data)[i] as c_uint);
                }
                sb_appendf(output, c"\n".as_ptr());
            }
        }
    }
}

pub unsafe fn compile_primary_expression(l: *mut stb_lexer, input_path: *const c_char, vars: *const Array<Array<Var>>, auto_vars_count: *mut usize, func_body: *mut Array<Op>, data: *mut Array<u8>) -> Option<Arg> {
    stb_c_lexer_get_token(l);
    match (*l).token {
        token if token == '(' as i64 => {
            let expr = compile_expression(l, input_path, vars, auto_vars_count, func_body, data)?;
            if !get_and_expect_clex(l, input_path, ')' as i64) { return None }
            Some(expr)
        }
        CLEX_intlit => return Some(Arg::Literal((*l).int_number)),
        CLEX_id => {
            let name = (*l).string;
            let name_where = (*l).where_firstchar;
            let var_def = find_var_deep(vars, name);
            if var_def.is_null() {
                diagf!(l, input_path, name_where, c"ERROR: could not find variable `%s`\n", name);
                return None;
            }
            match (*var_def).storage {
                Storage::Auto{index} => return Some(Arg::AutoVar(index)),
                Storage::External{..} => {
                    missingf!(l, input_path, name_where, c"external variables in lvalues are not supported yet\n");
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
            missingf!(l, input_path, (*l).where_firstchar, c"Unexpected token %s not all expressions are implemented yet\n", display_token_kind_temp((*l).token));
        }
    }
}

// TODO: add support for binary division expression
pub unsafe fn compile_multiply_expression(l: *mut stb_lexer, input_path: *const c_char, vars: *const Array<Array<Var>>, auto_vars_count: *mut usize, func_body: *mut Array<Op>, data: *mut Array<u8>) -> Option<Arg> {
    let mut lhs = compile_primary_expression(l, input_path, vars, auto_vars_count, func_body, data)?;

    let mut saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);
    if (*l).token == '*' as i64 {
        (*auto_vars_count) += 1;
        let index = *auto_vars_count;
        da_append(func_body, Op::AutoAlloc(1));
        while (*l).token == '*' as i64 {
            let rhs = compile_primary_expression(l, input_path, vars, auto_vars_count, func_body, data)?;
            da_append(func_body, Op::AutoBinop {binop: Binop::Mult, index, lhs, rhs});
            lhs = Arg::AutoVar(index);

            saved_point = (*l).parse_point;
            stb_c_lexer_get_token(l);
        }
    }

    (*l).parse_point = saved_point;
    Some(lhs)
}

// TODO: add support for binary minus expression
pub unsafe fn compile_plus_or_minus_expression(l: *mut stb_lexer, input_path: *const c_char, vars: *const Array<Array<Var>>, auto_vars_count: *mut usize, func_body: *mut Array<Op>, data: *mut Array<u8>) -> Option<Arg> {
    let mut lhs = compile_multiply_expression(l, input_path, vars, auto_vars_count, func_body, data)?;

    let mut saved_point = (*l).parse_point;
    stb_c_lexer_get_token(l);
    // TODO: adding more binops of the same precedence requires modification in 3 places here.
    if (*l).token == '+' as i64 || (*l).token == '-' as i64 {
        (*auto_vars_count) += 1;
        let index = *auto_vars_count;
        da_append(func_body, Op::AutoAlloc(1));
        while (*l).token == '+' as i64 || (*l).token == '-' as i64{
            let token = (*l).token;
            let rhs = compile_multiply_expression(l, input_path, vars, auto_vars_count, func_body, data)?;
            da_append(func_body, Op::AutoBinop {binop: Binop::from_token(token).unwrap(), index, lhs, rhs});
            lhs = Arg::AutoVar(index);

            saved_point = (*l).parse_point;
            stb_c_lexer_get_token(l);
        }
    }

    (*l).parse_point = saved_point;
    Some(lhs)
}

// TODO: the expression compilation leaks a lot of temporary variables
//   Maybe deallocate all of them at the end of a statement.
pub unsafe fn compile_expression(l: *mut stb_lexer, input_path: *const c_char, vars: *const Array<Array<Var>>, auto_vars_count: *mut usize, func_body: *mut Array<Op>, data: *mut Array<u8>) -> Option<Arg> {
    compile_plus_or_minus_expression(l, input_path, vars, auto_vars_count, func_body, data)
}

pub unsafe fn compile_func_body(l: *mut stb_lexer, input_path: *const c_char, vars: *mut Array<Array<Var>>, auto_vars_count: *mut usize, func_body: *mut Array<Op>, data: *mut Array<u8>) -> bool {
    loop {
        // Statement
        stb_c_lexer_get_token(l);
        if (*l).token == '}' as c_long {
            return true;
        }
        if !expect_clex(l, input_path, CLEX_id) { return false; }
        if strcmp((*l).string, c"extrn".as_ptr()) == 0 {
            // TODO: support multiple extrn declarations
            if !get_and_expect_clex(l, input_path, CLEX_id) { return false; }
            let name = strdup((*l).string);
            let name_where = (*l).where_firstchar;
            if !declare_var(l, input_path, vars, name, name_where, Storage::External{name}) { return false; }
            da_append(func_body, Op::ExtrnVar(name));
            if !get_and_expect_clex(l, input_path, ';' as c_long) { return false; }
        } else if strcmp((*l).string, c"auto".as_ptr()) == 0 {
            let mut count = 0;
            'vars: loop {
                if !get_and_expect_clex(l, input_path, CLEX_id) { return false; }
                let name = strdup((*l).string);
                let name_where = (*l).where_firstchar;
                (*auto_vars_count) += 1;
                if !declare_var(l, input_path, vars, name, name_where, Storage::Auto{index: *auto_vars_count}) { return false; }
                count += 1;
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

            da_append(func_body, Op::AutoAlloc(count));
        } else {
            let name = strdup((*l).string);
            let name_where = (*l).where_firstchar;

            stb_c_lexer_get_token(l);
            // TODO: assignment and function call must be expressions
            if (*l).token == '=' as c_long {
                let var_def = find_var_deep(vars, name);
                if var_def.is_null() {
                    diagf!(l, input_path, name_where, c"ERROR: could not find variable `%s`\n", name);
                    return false;
                }

                match (*var_def).storage {
                    Storage::Auto{index} => {
                        if let Some(arg) = compile_expression(l, input_path, vars, auto_vars_count, func_body, data) {
                            da_append(func_body, Op::AutoAssign{index, arg})
                        } else {
                            return false;
                        }
                    }
                    Storage::External{..} => {
                        missingf!(l, input_path, name_where, c"assignment to external variables\n");
                    }
                }

                if !get_and_expect_clex(l, input_path, ';' as c_long) { return false; }
            } else if (*l).token == '(' as c_long {
                let var_def = find_var_deep(vars, name);
                if var_def.is_null() {
                    diagf!(l, input_path, name_where, c"ERROR: could not find function `%s`\n", name);
                    return false;
                }

                let saved_point = (*l).parse_point;
                stb_c_lexer_get_token(l);
                let mut arg = None;
                if (*l).token != ')' as c_long {
                    (*l).parse_point = saved_point;
                    // TODO: function calls with multiple arguments
                    if let Some(expr) = compile_expression(l, input_path, vars, auto_vars_count, func_body, data) {
                        arg = Some(expr)
                    } else {
                        return false;
                    }
                    if !get_and_expect_clex(l, input_path, ')' as c_long) { return false; }
                }

                match (*var_def).storage {
                    Storage::External{name} => {
                        da_append(func_body, Op::Funcall {name, arg});
                    }
                    Storage::Auto{..} => {
                        missingf!(l, input_path, name_where, c"calling functions from auto variables\n");
                    }
                }

                if !get_and_expect_clex(l, input_path, ';' as c_long) { return false; }
            } else {
                diagf!(l, input_path, (*l).where_firstchar, c"ERROR: unexpected token %s\n", display_token_kind_temp((*l).token));
                return false;
            }
        }
    }
}

pub unsafe fn usage(target_name_flag: *mut*mut c_char) {
    fprintf!(stderr, c"Usage: %s [OPTIONS] <input.b>\n", flag_program_name());
    fprintf!(stderr, c"OPTIONS:\n");
    flag_print_options(stderr);
    fprintf!(stderr, c"Compilation targets:\n");
    for i in 0..TARGET_NAMES.len() {
        fprintf!(stderr, c"    -%s %s\n", flag_name(target_name_flag), (*TARGET_NAMES)[i].name);
    }
}

pub unsafe fn temp_default_output_path(input_path: *const c_char) -> *const c_char {
    let mut input_path_sv = sv_from_cstr(input_path);
    let b_ext = c".b".as_ptr();
    let b_ext_len = strlen(b_ext);
    if sv_end_with(input_path_sv, b_ext) {
        input_path_sv.count -= b_ext_len;
        temp_sv_to_cstr(input_path_sv)
    } else {
        temp_sprintf(c"%s.out".as_ptr(), input_path)
    }
}

pub unsafe fn main(mut argc: i32, mut argv: *mut*mut c_char) -> i32 {
    let default_target_name = name_of_target(Target::Fasm_x86_64_Linux).expect("default target name not found");

    // TODO: some sort of a -run flag that automatically runs the executable
    let target_name = flag_str(c"target".as_ptr(), default_target_name, c"Compilation target".as_ptr());
    let output_path_flag = flag_str(c"o".as_ptr(), ptr::null(), c"Output path (MANDATORY)".as_ptr());
    let help        = flag_bool(c"help".as_ptr(), false, c"Print this help message".as_ptr());

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
                fprintf!(stderr, c"ERROR: Serveral input files is not supported yet\n");
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
        fprintf!(stderr, c"ERROR: no input is provided\n");
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
        fprintf!(stderr, c"ERROR: unknown target `%s`\n", *target_name);
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
            diagf!(&l, input_path, symbol_name_where, c"ERROR: Trying to define a reserved keyword `%s` as a symbol. Please choose a different name.\n", symbol_name);
            diagf!(&l, input_path, symbol_name_where, c"NOTE: Reserved keywords are: ");
            for i in 0..B_KEYWORDS.len() {
                if i > 0 {
                    fprintf!(stderr, c", ");
                }
                fprintf!(stderr, c"`%s`", (*B_KEYWORDS)[i]);
            }
            fprintf!(stderr, c"\n");
            return 69;
        }

        stb_c_lexer_get_token(&mut l);
        if l.token == '(' as c_long { // Function definition
            // TODO: functions with several parameters
            if !get_and_expect_clex(&mut l, input_path, ')' as c_long) { return 1; }
            if !get_and_expect_clex(&mut l, input_path, '{' as c_long) { return 1; }

            scope_push(&mut vars);          // begin function scope
                generate_func_prolog(symbol_name, &mut output, target);
                let mut auto_vars_count: usize = 0;
                if !compile_func_body(&mut l, input_path, &mut vars, &mut auto_vars_count, &mut func_body, &mut data) { return 1; }
                generate_func_body(da_slice(func_body), &mut output, target);
                generate_func_epilog(&mut output, target);
            scope_pop(&mut vars);          // end function scope

            declare_var(&mut l, input_path, &mut vars, symbol_name, symbol_name_where, Storage::External{name: symbol_name});

            func_body.count = 0;
        } else { // Variable definition
            missingf!(&l, input_path, l.where_firstchar, c"variable definitions\n");
        }
    }

    generate_data_section(&mut output, target, da_slice(data));

    match target {
        Target::Fasm_x86_64_Linux => {
            let output_asm_path = temp_sprintf(c"%s.asm".as_ptr(), output_path);
            let output_obj_path = temp_sprintf(c"%s.o".as_ptr(), output_path);
            if !write_entire_file(output_asm_path, output.items as *const c_void, output.count) { return 69 }
            cmd_append! {
                &mut cmd,
                c"fasm".as_ptr(), output_asm_path, output_obj_path,
            }
            if !cmd_run_sync_and_reset(&mut cmd) { return 1 }
            cmd_append! {
                &mut cmd,
                c"cc".as_ptr(), c"-no-pie".as_ptr(), c"-o".as_ptr(), output_path, output_obj_path,
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
// TODO: Create a roadmap based on the spec.
