use core::ffi::*;
use core::mem::zeroed;
use crate::nob::*;
use crate::crust::libc::*;
use crate::{Compiler, Binop, Op, OpWithLocation, Arg, Func, align_bytes};
use crate::missingf;

pub unsafe fn load_literal_to_reg(output: *mut String_Builder, reg: *const c_char, literal: i64) {
    if literal < 0 {
        todo!("Loading negative numbers is not supported yet");
    }

    let mut literal = literal as u64;

    if literal == 0 {
        sb_appendf(output, c!("    mov %s, 0\n"), reg);
        return;
    }

    let mut chunks: [u16; 4] = zeroed();
    let mut chunks_len = 0;

    while literal > 0 {
        chunks[chunks_len] = (literal&0xFFFF) as u16;
        chunks_len += 1;
        literal >>= 16;
    }

    assert!(chunks_len > 0);

    let mut i = 0;

    sb_appendf(output, c!("    mov %s, %d\n"), reg, chunks[i] as u64);
    i += 1;

    while i < chunks_len {
        sb_appendf(output, c!("    movk %s, %d, lsl %d\n"), reg, chunks[i] as u64, 16 * i);
        i += 1;
    }
}

pub unsafe fn load_arg_to_reg(arg: Arg, reg: *const c_char, output: *mut String_Builder) {
    match arg {
        Arg::External(name) => {
            sb_appendf(output, c!("    adrp %s, %s\n"), reg, name);
            sb_appendf(output, c!("    add  %s, %s, :lo12:%s\n"), reg, reg, name);
            sb_appendf(output, c!("    ldr %s, [%s]\n"), reg, reg);
        }
        Arg::Ref(index) => {
            sb_appendf(output, c!("    ldr %s, [sp, %zu]\n"), reg, (index + 1)*8);
            sb_appendf(output, c!("    ldr %s, [%s]\n"), reg, reg);
        },
        Arg::AutoVar(index) => {
            sb_appendf(output, c!("    ldr %s, [sp, %zu]\n"), reg, (index + 1)*8);
        }
        Arg::Literal(value) => {
            load_literal_to_reg(output, reg, value);
        }
        Arg::DataOffset(offset) => {
            sb_appendf(output, c!("    adrp %s, .dat\n"), reg);
            sb_appendf(output, c!("    add  %s, %s, :lo12:.dat\n"), reg, reg);
            if offset >= 4095 {
                todo!("Data offsets bigger than 4095 are not supported yet");
            } else if offset > 0 {
                sb_appendf(output, c!("    add %s, %s, %zu\n"), reg, reg, offset);
            }
        }
    };
}

pub unsafe fn generate_function(name: *const c_char, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], output: *mut String_Builder) {
    let stack_size = align_bytes((2 + auto_vars_count)*8, 16);
    sb_appendf(output, c!(".global %s\n"), name);
    sb_appendf(output, c!("%s:\n"), name);
    sb_appendf(output, c!("    stp x29, x30, [sp, -%zu]!\n"), stack_size);
    sb_appendf(output, c!("    mov x29, sp\n"), name);
    assert!(auto_vars_count >= params_count);
    // TODO: add the rest of the registers.
    // The first 8 args go to x0-x7
    const REGISTERS: *const[*const c_char] = &[c!("x0"), c!("x1"), c!("x2"), c!("x3"), c!("x4")];
    if params_count > REGISTERS.len() {
        todo!("Too many parameters in function definition. We support only {} but {} were provided", REGISTERS.len(), params_count);
    }
    for i in 0..params_count {
        let reg = (*REGISTERS)[i];
        sb_appendf(output, c!("    str %s, [sp, %zu]\n"), reg, (2 + i)*8);
    }
    for i in 0..body.len() {
        sb_appendf(output, c!("%s.op_%zu:\n"), name, i);
        let op = (*body)[i];
        match op.opcode {
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg_to_reg(arg, c!("x0"), output);
                }
                sb_appendf(output, c!("    ldp x29, x30, [sp], %zu\n"), stack_size);
                sb_appendf(output, c!("    ret\n"));
            }
            Op::Negate {result, arg} => {
                load_arg_to_reg(arg, c!("x0"), output);
                sb_appendf(output, c!("    mov x1, 1\n")); // TODO: is it possible to somehow
                                                           // supply 1 to the 3rd argument of mneg
                                                           // as an immediate value without moving
                                                           // it to a separate register?
                sb_appendf(output, c!("    mneg x2, x0, x1\n"));
                sb_appendf(output, c!("    str x2, [sp, %zu]\n"), (result + 1)*8);
            }
            Op::UnaryNot {result, arg} => {
                load_arg_to_reg(arg, c!("x0"), output);
                sb_appendf(output, c!("    cmp x0, 0\n"));
                sb_appendf(output, c!("    cset x0, eq\n"));
                sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (result + 1)*8);
            },
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    orr x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Binop::BitAnd => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    and x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Binop::BitShl => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    lsl x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Binop::BitShr => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    lsr x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Binop::Plus => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    add x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    }
                    Binop::Minus => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    sub x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Binop::Mod => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        // https://stackoverflow.com/questions/35351470/obtaining-remainder-using-single-aarch64-instruction
                        sb_appendf(output, c!("    sdiv x2, x0, x1\n"));
                        sb_appendf(output, c!("    msub x2, x2, x1, x0\n"));
                        sb_appendf(output, c!("    str x2, [sp, %zu]\n"), (index + 1)*8);
                    }
                    Binop::Mult => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    mul x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Binop::Less => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, lt\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    }
                    Binop::Equal => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, eq\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    }
                    Binop::NotEqual => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, ne\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    }
                    Binop::GreaterEqual => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, ge\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Binop::LessEqual => {
                        load_arg_to_reg(lhs, c!("x0"), output);
                        load_arg_to_reg(rhs, c!("x1"), output);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, le\n"));
                        sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                }
            }
            Op::ExternalAssign{name, arg} => {
                load_arg_to_reg(arg, c!("x0"), output);
                sb_appendf(output, c!("    adrp x1, %s\n"), name);
                sb_appendf(output, c!("    add  x1, x1, :lo12:%s\n"), name);
                sb_appendf(output, c!("    str x0, [x1]\n"), name);
            }
            Op::AutoAssign {index, arg} => {
                load_arg_to_reg(arg, c!("x0"), output);
                sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
            },
            Op::Store {index, arg} => {
                sb_appendf(output, c!("    ldr x0, [sp, %zu]\n"), (index + 1)*8);
                load_arg_to_reg(arg, c!("x1"), output);
                sb_appendf(output, c!("    str x1, [x0]\n"));
            },
            Op::Funcall {result, name, args} => {
                if args.count > REGISTERS.len() {
                    missingf!(op.input_stream, op.input_path, op.location, c!("Too many function call arguments. We support only %zu but %zu were provided"), REGISTERS.len(), args.count);
                }
                for i in 0..args.count {
                    let reg = (*REGISTERS)[i];
                    load_arg_to_reg(*args.items.add(i), reg, output);
                }
                sb_appendf(output, c!("    bl %s\n"), name);
                sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (result + 1)*8);
            },
            Op::Jmp {addr} => {
                sb_appendf(output, c!("    b %s.op_%zu\n"), name, addr);
            },
            Op::JmpIfNot {addr, arg} => {
                load_arg_to_reg(arg, c!("x0"), output);
                sb_appendf(output, c!("    cmp x0, 0\n"));
                sb_appendf(output, c!("    beq %s.op_%zu\n"), name, addr);
            },
        }
    }
    sb_appendf(output, c!("%s.op_%zu:\n"), name, body.len());
    sb_appendf(output, c!("    mov x0, 0\n"));
    sb_appendf(output, c!("    ldp x29, x30, [sp], %zu\n"), stack_size);
    sb_appendf(output, c!("    ret\n"));
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func]) {
    sb_appendf(output, c!(".text\n"));
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    if data.len() > 0 {
        sb_appendf(output, c!(".data\n"));
        sb_appendf(output, c!(".dat: .byte "));
        for i in 0..data.len() {
            if i > 0 {
                sb_appendf(output, c!(","));
            }
            sb_appendf(output, c!("0x%02X"), (*data)[i] as c_uint);
        }
        sb_appendf(output, c!("\n"));
    }
}

pub unsafe fn generate_globals(output: *mut String_Builder, globals: *const [*const c_char]) {
    if globals.len() > 0 {
        sb_appendf(output, c!(".bss\n"));
        for i in 0..globals.len() {
            let name = (*globals)[i];
            sb_appendf(output, c!(".global %s\n"), name);
            sb_appendf(output, c!("%s: .zero 8\n"), name);
        }
    }
}

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    generate_funcs(output, da_slice((*c).funcs));
    generate_globals(output, da_slice((*c). globals));
    generate_data_section(output, da_slice((*c).data));
}
