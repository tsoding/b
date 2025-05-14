use core::ffi::*;
use core::mem::zeroed;
use crate::nob::*;
use crate::{Compiler, Op, Arg, Binop, Func, align_bytes};

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

pub unsafe fn generate_function(name: *const c_char, auto_vars_count: usize, body: *const [Op], output: *mut String_Builder) {
    let stack_size = align_bytes((2 + auto_vars_count)*8, 16);
    sb_appendf(output, c!(".global %s\n"), name);
    sb_appendf(output, c!("%s:\n"), name);
    sb_appendf(output, c!("    stp x29, x30, [sp, -%zu]!\n"), stack_size);
    sb_appendf(output, c!("    mov x29, sp\n"), name);
    for i in 0..body.len() {
        sb_appendf(output, c!("%s.op_%zu:\n"), name, i);
        match (*body)[i] {
            Op::UnaryNot   {..} => todo!(),
            Op::AutoBinop  {binop, index, lhs, rhs} => {
                match lhs {
                    Arg::AutoVar(index) => {
                        sb_appendf(output, c!("    ldr x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Arg::Literal(value) => {
                        load_literal_to_reg(output, c!("x0"), value);
                    },
                    Arg::DataOffset(_) => todo!(),
                };
                match rhs {
                    Arg::AutoVar(index) => {
                        sb_appendf(output, c!("    ldr x1, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Arg::Literal(value) => {
                        load_literal_to_reg(output, c!("x1"), value);
                    },
                    Arg::DataOffset(_) => todo!(),
                };

                match binop {
                    Binop::Plus => {
                        sb_appendf(output, c!("    add x0, x1, x0\n"));
                    }
                    Binop::Minus => todo!(),
                    Binop::Mult  => todo!(),
                    Binop::Less  => {
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, ls\n"));
                    },
                }

                sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
            },
            Op::AutoAssign {index, arg} => {
                match arg {
                    Arg::AutoVar(index) => {
                        sb_appendf(output, c!("    ldr x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Arg::Literal(value) => {
                        load_literal_to_reg(output, c!("x0"), value);
                    }
                    Arg::DataOffset(offset) => {
                        if offset == 0 {
                            sb_appendf(output, c!("    adrp x0, .dat\n"));
                            sb_appendf(output, c!("    add  x0, x0, :lo12:.dat\n"));
                        } else {
                            todo!();
                        }
                    },
                }
                sb_appendf(output, c!("    str x0, [sp, %zu]\n"), (index + 1)*8);
            },
            Op::Funcall {result: _, name, args} => {
                // TODO: add the rest of the registers.
                // The first 8 args go to x0-x7
                const REGISTERS: *const[*const c_char] = &[c!("x0"), c!("x1"), c!("x2"), c!("x3"), c!("x4")];
                if args.count > REGISTERS.len() {
                    todo!("Too many function call arguments. We support only {} but {} were provided", REGISTERS.len(), args.count);
                }
                for i in 0..args.count {
                    let reg = (*REGISTERS)[i];
                    match *args.items.add(i) {
                        Arg::AutoVar(index) => {
                            sb_appendf(output, c!("    ldr %s, [sp, %zu]\n"), reg, (index + 1)*8);
                        },
                        Arg::Literal(value)  => {
                            load_literal_to_reg(output, reg, value)
                        }
                        Arg::DataOffset(offset)  => {
                            if offset == 0 {
                                sb_appendf(output, c!("    adrp %s, .dat\n"), reg);
                                sb_appendf(output, c!("    add  %s, %s, :lo12:.dat\n"), reg, reg);
                            } else {
                                todo!();
                            }
                        },
                    };
                }
                sb_appendf(output, c!("    bl %s\n"), name);
                // TODO: save the result of the function call to the auto var
            },
            Op::Jmp {addr} => {
                sb_appendf(output, c!("    b %s.op_%zu\n"), name, addr);
            },
            Op::JmpIfNot {addr, arg} => {
                match arg {
                    Arg::AutoVar(index) => {
                        sb_appendf(output, c!("    ldr x0, [sp, %zu]\n"), (index + 1)*8);
                    },
                    Arg::Literal(value) => {
                        load_literal_to_reg(output, c!("x0"), value);
                    },
                    Arg::DataOffset(_) => todo!(),
                };
                sb_appendf(output, c!("    cmp x0, 0\n"));
                sb_appendf(output, c!("    beq %s.op_%zu\n"), name, addr);
            },
        }
    }
    sb_appendf(output, c!("%s.op_%zu:\n"), name, body.len());
    sb_appendf(output, c!("    mov w0, 0\n"));
    sb_appendf(output, c!("    ldp x29, x30, [sp], %zu\n"), stack_size);
    sb_appendf(output, c!("    ret\n"));

}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func]) {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    if data.len() > 0 {
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

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    generate_funcs(output, da_slice((*c).funcs));
    generate_data_section(output, da_slice((*c).data));
}
