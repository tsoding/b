use core::ffi::*;
use crate::{Op, Arg, Func, Binop, Compiler};
use crate::nob::*;

pub unsafe fn generate_function(name: *const c_char, auto_vars_count: usize, body: *const [Op], output: *mut String_Builder) {
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
            Op::UnaryNot{result, arg} => {
                sb_appendf(output, c!("    xor rbx, rbx\n"));
                match arg {
                    Arg::AutoVar(index) => sb_appendf(output, c!("    mov rax, [rbp-%zu]\n"), index*8),
                    Arg::Literal(value) => sb_appendf(output, c!("    mov rax, %ld\n"), value),
                    Arg::DataOffset(_offset) => todo!(),
                };
                sb_appendf(output, c!("    test rax, rax\n"));
                sb_appendf(output, c!("    setz bl\n"));
                sb_appendf(output, c!("    mov [rbp-%zu], rbx\n"), result*8);
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
            Op::Funcall{result, name, args} => {
                const REGISTERS: *const[*const c_char] = &[c!("rdi"), c!("rsi"), c!("rdx"), c!("rcx"), c!("r8")];
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
                sb_appendf(output, c!("    mov al, 0\n")); // x86_64 Linux ABI passes the amount of
                                                           // floating point args via al. Since B
                                                           // does not distinguish regular and
                                                           // variadic functions we set al to 0 just
                                                           // in case.
                sb_appendf(output, c!("    call %s\n"), name);
                sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), result*8);
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

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func]) {
    sb_appendf(output, c!("section \".text\" executable\n"));
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
    }
}

pub unsafe fn generate_extrns(output: *mut String_Builder, extrns: *const [*const c_char]) {
    for i in 0..extrns.len() {
        sb_appendf(output, c!("extrn %s\n"), (*extrns)[i]);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
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

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    sb_appendf(output, c!("format ELF64\n"));
    generate_funcs(output, da_slice((*c).funcs));
    generate_extrns(output, da_slice((*c).extrns));
    generate_data_section(output, da_slice((*c).data));
}
