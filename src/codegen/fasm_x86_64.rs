use core::ffi::*;
use core::cmp;
use crate::{Op, Binop, OpWithLocation, Arg, Func, Global, ImmediateValue, AsmFunc, Compiler, align_bytes};
use crate::nob::*;
use crate::crust::libc::*;
use crate::targets::Os;

pub unsafe fn call_arg(arg: Arg, output: *mut String_Builder) {
    match arg {
        Arg::RefExternal(name) | Arg::External(name) => sb_appendf(output, c!("    call _%s\n"), name),
        arg => {
            load_arg_to_reg(arg, c!("rax"), output);
            sb_appendf(output, c!("    call rax\n"))
        },
    };
}

pub unsafe fn load_arg_to_reg(arg: Arg, reg: *const c_char, output: *mut String_Builder) {
    match arg {
        Arg::Deref(index) => {
            sb_appendf(output, c!("    mov %s, [rbp-%zu]\n"), reg, index*8);
            sb_appendf(output, c!("    mov %s, [%s]\n"), reg, reg)
        }
        Arg::RefAutoVar(index)  => sb_appendf(output, c!("    lea %s, [rbp-%zu]\n"), reg, index*8),
        Arg::RefExternal(name)  => sb_appendf(output, c!("    lea %s, [_%s]\n"), reg, name),
        Arg::External(name)     => sb_appendf(output, c!("    mov %s, [_%s]\n"), reg, name),
        Arg::AutoVar(index)     => sb_appendf(output, c!("    mov %s, [rbp-%zu]\n"), reg, index*8),
        Arg::Literal(value)     => sb_appendf(output, c!("    mov %s, %ld\n"), reg, value),
        Arg::DataOffset(offset) => sb_appendf(output, c!("    mov %s, dat+%zu\n"), reg, offset),
        Arg::Bogus => unreachable!("bogus-amogus")
    };
}

pub unsafe fn generate_function(name: *const c_char, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], output: *mut String_Builder, os: Os) {
    let stack_size = align_bytes(auto_vars_count*8, 16);
    sb_appendf(output, c!("public _%s as '%s'\n"), name, name);
    sb_appendf(output, c!("_%s:\n"), name);
    sb_appendf(output, c!("    push rbp\n"));
    sb_appendf(output, c!("    mov rbp, rsp\n"));
    if stack_size > 0 {
        sb_appendf(output, c!("    sub rsp, %zu\n"), stack_size);
    }
    assert!(auto_vars_count >= params_count);
    let registers: *const[*const c_char] = match os {
        Os::Linux   => &[c!("rdi"), c!("rsi"), c!("rdx"), c!("rcx"), c!("r8"), c!("r9")],
        Os::Windows => &[c!("rcx"), c!("rdx"), c!("r8"), c!("r9")], // https://en.wikipedia.org/wiki/X86_calling_conventions#Microsoft_x64_calling_convention
    };

    let mut i = 0;
    while i < cmp::min(params_count, registers.len()) {
        let reg = (*registers)[i];
        sb_appendf(output, c!("    mov QWORD [rbp-%zu], %s\n"), (i + 1)*8, reg);
        i += 1;
    }
    for j in i..params_count {
        match os {
            Os::Linux   => sb_appendf(output, c!("    mov QWORD rax, [rbp+%zu]\n"), ((j - i) + 2)*8),
            Os::Windows => sb_appendf(output, c!("    mov QWORD rax, [rbp+%zu]\n"), ((j - i) + 6)*8),
        };
        sb_appendf(output, c!("    mov QWORD [rbp-%zu], rax\n"), (j + 1)*8);
    }

    for i in 0..body.len() {
        let op = (*body)[i];
        match op.opcode {
            Op::Bogus => unreachable!("bogus-amogus"),
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg_to_reg(arg, c!("rax"), output);
                }
                sb_appendf(output, c!("    mov rsp, rbp\n"));
                sb_appendf(output, c!("    pop rbp\n"));
                sb_appendf(output, c!("    ret\n"));
            }
            Op::Store {index, arg} => {
                sb_appendf(output, c!("    mov rax, [rbp-%zu]\n"), index*8);
                load_arg_to_reg(arg, c!("rbx"), output);
                sb_appendf(output, c!("    mov [rax], rbx\n"));
            }
            Op::ExternalAssign{name, arg} => {
                load_arg_to_reg(arg, c!("rax"), output);
                sb_appendf(output, c!("    mov [_%s], rax\n"), name);
            },
            Op::AutoAssign{index, arg} => {
                load_arg_to_reg(arg, c!("rax"), output);
                sb_appendf(output, c!("    mov QWORD [rbp-%zu], rax\n"), index*8);
            },
            Op::Negate {result, arg} => {
                load_arg_to_reg(arg, c!("rax"), output);
                sb_appendf(output, c!("    neg rax\n"));
                sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), result*8);
            }
            Op::UnaryNot{result, arg} => {
                sb_appendf(output, c!("    xor rbx, rbx\n"));
                load_arg_to_reg(arg, c!("rax"), output);
                sb_appendf(output, c!("    test rax, rax\n"));
                sb_appendf(output, c!("    setz bl\n"));
                sb_appendf(output, c!("    mov [rbp-%zu], rbx\n"), result*8);
            },
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    or rax, rbx\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::BitAnd => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    and rax, rbx\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::BitShl => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rcx"), output);
                        sb_appendf(output, c!("    shl rax, cl\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::BitShr => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rcx"), output);
                        sb_appendf(output, c!("    shr rax, cl\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::Plus => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    add rax, rbx\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::Minus  => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    sub rax, rbx\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::Mod => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    cqo\n"));
                        sb_appendf(output, c!("    idiv rbx\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rdx\n"), index*8);
                    }
                    Binop::Div => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    cqo\n"));
                        sb_appendf(output, c!("    idiv rbx\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::Mult => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    xor rdx, rdx\n"));
                        sb_appendf(output, c!("    imul rbx\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), index*8);
                    }
                    Binop::Less => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    xor rdx, rdx\n"));
                        sb_appendf(output, c!("    cmp rax, rbx\n"));
                        sb_appendf(output, c!("    setl dl\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rdx\n"), index*8);
                    }
                    Binop::Greater => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    xor rdx, rdx\n"));
                        sb_appendf(output, c!("    cmp rax, rbx\n"));
                        sb_appendf(output, c!("    setg dl\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rdx\n"), index*8);
                    }
                    Binop::Equal => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    xor rdx, rdx\n"));
                        sb_appendf(output, c!("    cmp rax, rbx\n"));
                        sb_appendf(output, c!("    sete dl\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rdx\n"), index*8);
                    }
                    Binop::NotEqual => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    xor rdx, rdx\n"));
                        sb_appendf(output, c!("    cmp rax, rbx\n"));
                        sb_appendf(output, c!("    setne dl\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rdx\n"), index*8);
                    }
                    Binop::GreaterEqual => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    xor rdx, rdx\n"));
                        sb_appendf(output, c!("    cmp rax, rbx\n"));
                        sb_appendf(output, c!("    setge dl\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rdx\n"), index*8);
                    }
                    Binop::LessEqual => {
                        load_arg_to_reg(lhs, c!("rax"), output);
                        load_arg_to_reg(rhs, c!("rbx"), output);
                        sb_appendf(output, c!("    xor rdx, rdx\n"));
                        sb_appendf(output, c!("    cmp rax, rbx\n"));
                        sb_appendf(output, c!("    setle dl\n"));
                        sb_appendf(output, c!("    mov [rbp-%zu], rdx\n"), index*8);
                    }
                }
            }
            Op::Funcall{result, fun, args} => {
                let reg_args_count = cmp::min(args.count, registers.len());
                for i in 0..reg_args_count {
                    let reg = (*registers)[i];
                    load_arg_to_reg(*args.items.add(i), reg, output);
                }

                let stack_args_count = args.count - reg_args_count;
                let stack_args_size = align_bytes(stack_args_count*8, 16);
                if stack_args_count > 0 {
                    sb_appendf(output, c!("    sub rsp, %zu\n"), stack_args_size);
                    for i in 0..stack_args_count {
                        load_arg_to_reg(*args.items.add(reg_args_count + i), c!("rax"), output);
                        sb_appendf(output, c!("    mov [rsp+%zu], rax\n"), i*8);
                    }
                }

                match os {
                    Os::Linux => {
                        sb_appendf(output, c!("    mov al, 0\n")); // x86_64 Linux ABI passes the amount of
                                                                   // floating point args via al. Since B
                                                                   // does not distinguish regular and
                                                                   // variadic functions we set al to 0 just
                                                                   // in case.
                        call_arg(fun, output);
                    }
                    Os::Windows => {
                        // allocate 32 bytes for "shadow space"
                        // it must be allocated at the top of the stack after all arguments are pushed
                        // so we can't allocate it at function prologue
                        sb_appendf(output, c!("    sub rsp, 32\n"));
                        call_arg(fun, output);
                        sb_appendf(output, c!("    add rsp, 32\n")); // deallocate "shadow space"
                    }
                }
                if stack_args_count > 0 {
                    sb_appendf(output, c!("    add rsp, %zu\n"), stack_args_size);
                }
                sb_appendf(output, c!("    mov [rbp-%zu], rax\n"), result*8);
            },
            Op::Asm {args} => {
                for i in 0..args.count {
                    let arg = *args.items.add(i);
                    sb_appendf(output, c!("    %s\n"), arg);
                }
            }
            Op::Label {label} => {
                sb_appendf(output, c!(".label_%zu:\n"), label);
            }
            Op::JmpLabel {label} => {
                sb_appendf(output, c!("    jmp .label_%zu\n"), label);
            }
            Op::JmpUnlessLabel {label, arg} => {
                load_arg_to_reg(arg, c!("rax"), output);
                sb_appendf(output, c!("    test rax, rax\n"));
                sb_appendf(output, c!("    jz .label_%zu\n"), label);
            }
        }
    }
    sb_appendf(output, c!("    mov rax, 0\n"));
    sb_appendf(output, c!("    mov rsp, rbp\n"));
    sb_appendf(output, c!("    pop rbp\n"));
    sb_appendf(output, c!("    ret\n"));
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func], os: Os) {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output, os);
    }
}

pub unsafe fn generate_extrns(output: *mut String_Builder, extrns: *const [*const c_char], funcs: *const [Func], globals: *const [Global], asm_funcs: *const [AsmFunc]) {
    'skip: for i in 0..extrns.len() {
        let name = (*extrns)[i];

        for j in 0..funcs.len() {
            let func = (*funcs)[j];
            if strcmp(func.name, name) == 0 {
                continue 'skip
            }
        }

        for j in 0..globals.len() {
            let global = (*globals)[j].name;
            if strcmp(global, name) == 0 {
                continue 'skip
            }
        }

        for j in 0..asm_funcs.len() {
            let asm_func = (*asm_funcs)[j].name;
            if strcmp(asm_func, name) == 0 {
                continue 'skip
            }
        }

        sb_appendf(output, c!("extrn '%s' as _%s\n"), name, name);
    }
}

pub unsafe fn generate_asm_funcs(output: *mut String_Builder, asm_funcs: *const [AsmFunc]) {
    for i in 0..asm_funcs.len() {
        let asm_func = (*asm_funcs)[i];
        sb_appendf(output, c!("public _%s as '%s'\n"), asm_func.name, asm_func.name);
        sb_appendf(output, c!("_%s:\n"), asm_func.name);
        for j in 0..asm_func.body.count {
            sb_appendf(output, c!("    %s\n"), *asm_func.body.items.add(j));
        }
    }
}

pub unsafe fn generate_globals(output: *mut String_Builder, globals: *const [Global]) {
    for i in 0..globals.len() {
        let global = (*globals)[i];
        sb_appendf(output, c!("public _%s as '%s'\n"), global.name, global.name);
        sb_appendf(output, c!("_%s: "), global.name);

        if global.is_vec {
            sb_appendf(output, c!("dq $+8\n"));
        }

        if global.values.count > 0 {
            sb_appendf(output, c!("dq "), global.name);
            for j in 0..global.values.count {
                if j > 0 {
                    sb_appendf(output, c!(","));
                }
                match *global.values.items.add(j) {
                    ImmediateValue::Literal(lit) => sb_appendf(output, c!("0x%llX"), lit),
                    ImmediateValue::Name(name) => sb_appendf(output, c!("_%s"), name),
                    ImmediateValue::DataOffset(offset) => sb_appendf(output, c!("dat+%zu"), offset),
                };
            }
        }

        if global.values.count < global.minimum_size {
            sb_appendf(output, c!("\nrq %zu"), global.minimum_size - global.values.count);
        }

        sb_appendf(output, c!("\n"));
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    if data.len() > 0 {
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

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler, os: Os) {
    match os {
        Os::Linux   => sb_appendf(output, c!("format ELF64\n")),
        Os::Windows => sb_appendf(output, c!("format MS64 COFF\n")),
    };
    sb_appendf(output, c!("section \".text\" executable\n"));
    generate_funcs(output, da_slice((*c).funcs), os);
    generate_asm_funcs(output, da_slice((*c).asm_funcs));
    generate_extrns(output, da_slice((*c).extrns), da_slice((*c).funcs), da_slice((*c).globals), da_slice((*c).asm_funcs));
    sb_appendf(output, c!("section \".data\"\n"));
    generate_data_section(output, da_slice((*c).data));
    generate_globals(output, da_slice((*c).globals));
}
