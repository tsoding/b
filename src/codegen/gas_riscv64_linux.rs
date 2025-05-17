use core::ffi::*;
use crate::nob::*;
use crate::{Compiler, Func, Op, align_bytes, Arg};

pub unsafe fn load_arg_to_reg(arg: Arg, reg: *const c_char, output: *mut String_Builder) {
    match arg {
        Arg::AutoVar(index) => {
            // ld = load
            sb_appendf(output, c!("    ld %s,%zu(sp)\n"), reg, (index+1)*8);
        },
        Arg::Literal(value) => {
            // li = load immediate
            sb_appendf(output, c!("    li %s, %zu\n"), reg, value);
        }
        Arg::DataOffset(offset) => {
            // lla = load local address
            sb_appendf(output, c!("    lla %s, .dat+%zu\n"), reg, offset);
        }
    }
}
pub unsafe fn generate_function(name : *const c_char, auto_vars_count: usize, body: *const [Op], output: *mut String_Builder) {
    // 2 more to stock ra (return address) and s0 (saved register, here used as "previous stack value")
    let stack_size = align_bytes(8*(2+auto_vars_count), 16);

    // decl
    sb_appendf(output, c!(".global %s\n"), name);
    sb_appendf(output, c!("%s:\n"), name);
    sb_appendf(output, c!(".cfi_startproc\n"));
            
    // alloc memory on stack
    sb_appendf(output, c!("    addi sp, sp, -%zu\n"), stack_size);
    // store ra and s0
    sb_appendf(output, c!("    sd ra,%zu(sp)\n"), stack_size-8);
    sb_appendf(output, c!("    sd s0,%zu(sp)\n"), stack_size-16);
    // put s0 as top of stack
    sb_appendf(output, c!("    addi s0,sp,%zu\n"), stack_size);

    for i in 0..body.len() {
        sb_appendf(output, c!("%s.op_%zu:\n"), name, i);
        match (*body)[i] {
            // TODO: rename result to index ?
            Op::UnaryNot {result, arg} => {
                load_arg_to_reg(arg, c!("t0"), output);
                sb_appendf(output, c!("    snez t0, x0\n"));
                sb_appendf(output, c!("    sd t0, %zu(sp)\n"), 8*(1+result));
            },
            Op::Add {index, lhs, rhs} => {
                load_arg_to_reg(lhs, c!("t0"), output);
                load_arg_to_reg(rhs, c!("t1"), output);
                sb_appendf(output, c!("    add t0, t0, t1\n"));
                sb_appendf(output, c!("    sd t0, %zu(sp)\n"), 8*(1+index));
            },
            Op::Sub {index, lhs, rhs} => {
                load_arg_to_reg(lhs, c!("t0"), output);
                load_arg_to_reg(rhs, c!("t1"), output);
                sb_appendf(output, c!("    sub t0, t0, t1\n"));
                sb_appendf(output, c!("    sd t0, %zu(sp)\n"), 8*(1+index));
            },
            Op::Mul {index, lhs, rhs} => {
                load_arg_to_reg(lhs, c!("t0"), output);
                load_arg_to_reg(rhs, c!("t1"), output);
                sb_appendf(output, c!("    mul t0, t0, t1\n"));
                sb_appendf(output, c!("    sd t0, %zu(sp)\n"), 8*(1+index));
            },
            Op::Less {index, lhs, rhs} => {
                load_arg_to_reg(lhs, c!("t0"), output);
                load_arg_to_reg(rhs, c!("t1"), output);
                sb_appendf(output, c!("    slt t0, t1, t0\n"));
                sb_appendf(output, c!("    sd t0, %zu(sp)\n"), 8*(1+index));
            },
            Op::Funcall {result: _, name, args} => {
                const REGISTERS: *const[*const c_char] = &[c!("a0"), c!("a1"), c!("a2"), c!("a3"), c!("a4"), c!("a5"), c!("a6"), c!("a7")];
                if args.count >= REGISTERS.len() {
                    todo!("Too many function call arguments. We support only {} but {} were provided", REGISTERS.len(), args.count);
                }
                for i in 0..args.count {
                    let reg = (*REGISTERS)[i];
                    load_arg_to_reg(*args.items.add(i), reg, output);
                }
                sb_appendf(output, c!("    call %s\n"), name);

            },
            Op::AutoAssign {index, arg} => {
                load_arg_to_reg(arg, c!("t0"), output);
                sb_appendf(output, c!("    sd t0, %zu(sp)\n"), (index+1)*8);
            },
            Op::Jmp {addr} => {
                sb_appendf(output, c!("    j %s.op_%zu\n"), name, addr);
            },
            Op::JmpIfNot {addr, arg} => {
                load_arg_to_reg(arg, c!("t0"), output);
                sb_appendf(output, c!("    bnez t0, %s.op_%zu\n"), name, addr);
            },
        }
    }
    // allows jump to end of function
    sb_appendf(output, c!("%s.op_%zu:\n"), name, body.len());

    // retrieve ra and s0
    sb_appendf(output, c!("    ld s0,%zu(sp)\n"), stack_size-16);
    sb_appendf(output, c!("    ld ra,%zu(sp)\n"), stack_size-8);
    // "free" alloc-ed stack memory
    sb_appendf(output, c!("    addi sp, sp, %zu\n"), stack_size);

    // return with 0 by default
    sb_appendf(output, c!("    li a0, 0\n"));
    sb_appendf(output, c!("    ret\n")); // == jr ra == jalr x0, x1, 0
    sb_appendf(output, c!(".cfi_endproc\n"));
}
pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func]) {
    for i in 0..funcs.len() {
        let fun = (*funcs)[i];
        generate_function(fun.name, fun.auto_vars_count, da_slice(fun.body), output);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    if data.len() > 1 {
        sb_appendf(output, c!("    .section .data\n"));
        sb_appendf(output, c!(".dat:\n"));
        sb_appendf(output, c!(".byte "));
        for i in 0..data.len() {
            if i != 0 {sb_appendf(output, c!(", "));}
            sb_appendf(output, c!("%zu"), (*data)[i] as c_uint);
        }
        sb_appendf(output, c!("\n"));
    }
}

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    generate_funcs(output, da_slice((*c).funcs));
    generate_data_section(output, da_slice((*c).data));
}
