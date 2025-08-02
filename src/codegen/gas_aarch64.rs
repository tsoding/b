use core::ffi::*;
use core::mem::zeroed;
use crate::hashtable::HashTable;
use crate::nob::*;
use crate::crust::libc::*;
use crate::ir::*;
use crate::lexer::*;
use crate::missingf;
use crate::targets::Os;
use crate::codegen::*;
use crate::shlex::*;
use crate::arena;

pub unsafe fn align_bytes(bytes: usize, alignment: usize) -> usize {
    let rem = bytes%alignment;
    if rem > 0 {
        bytes + alignment - rem
    } else {
        bytes
    }
}

pub unsafe fn call_arg(arg: Arg, loc: Loc, output: *mut String_Builder, os: Os) {
    match arg {
        Arg::RefExternal(name) | Arg::External(name) => match os {
            Os::Linux   => sb_appendf(output, c!("    bl %s\n"), name),
            Os::Darwin  => sb_appendf(output, c!("    bl _%s\n"), name),
            Os::Windows => missingf!(loc, c!("AArch64 is not supported on windows\n")),
        }
        arg => {
            load_arg_to_reg(arg, c!("x16"), output, loc, os);
            sb_appendf(output, c!("    blr x16\n"))
        },
    };
}

pub unsafe fn load_literal_to_reg(output: *mut String_Builder, reg: *const c_char, literal: u64) {
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

pub unsafe fn load_arg_to_reg(arg: Arg, reg: *const c_char, output: *mut String_Builder, loc: Loc, os: Os) {
    match arg {
        Arg::External(name) => {
            match os {
                Os::Linux => {
                    sb_appendf(output, c!("    adrp %s, %s\n"), reg, name);
                    sb_appendf(output, c!("    add  %s, %s, :lo12:%s\n"), reg, reg, name);
                }
                Os::Darwin => {
                    sb_appendf(output, c!("    adrp %s, _%s@GOTPAGE\n"), reg, name);
                    sb_appendf(output, c!("    ldr  %s, [%s, _%s@GOTPAGEOFF]\n"), reg, reg, name);
                }
                Os::Windows => missingf!(loc, c!("AArch64 is not supported on windows\n")),
            }
            sb_appendf(output, c!("    ldr %s, [%s]\n"), reg, reg);
        }
        Arg::Deref(index) => {
            sb_appendf(output, c!("    ldr %s, [x29, -%zu]\n"), reg, index*8);
            sb_appendf(output, c!("    ldr %s, [%s]\n"), reg, reg);
        },
        Arg::RefAutoVar(index) => {
            sb_appendf(output, c!("    sub %s, x29, %zu\n"), reg, index*8);
        }
        Arg::RefExternal(name) => match os {
            Os::Linux => {
                sb_appendf(output, c!("    adrp %s, %s\n"), reg, name);
                sb_appendf(output, c!("    add  %s, %s, :lo12:%s\n"), reg, reg, name);
            }
            Os::Darwin => {
                sb_appendf(output, c!("    adrp %s, _%s@GOTPAGE\n"), reg, name);
                sb_appendf(output, c!("    ldr  %s, [%s, _%s@GOTPAGEOFF]\n"), reg, reg, name);
            }
            Os::Windows => missingf!(loc, c!("AArch64 is not supported on windows\n")),
        },
        Arg::AutoVar(index) => {
            sb_appendf(output, c!("    ldr %s, [x29, -%zu]\n"), reg, index*8);
        }
        Arg::Literal(value) => {
            load_literal_to_reg(output, reg, value);
        }
        Arg::DataOffset(offset) => {
            match os {
                Os::Linux => {
                    sb_appendf(output, c!("    adrp %s, .dat\n"), reg);
                    sb_appendf(output, c!("    add  %s, %s, :lo12:.dat\n"), reg, reg);
                }
                Os::Darwin => {
                    sb_appendf(output, c!("    adrp %s, .dat@PAGE\n"), reg);
                    sb_appendf(output, c!("    add  %s, %s, .dat@PAGEOFF\n"), reg, reg);
                }
                Os::Windows => missingf!(loc, c!("AArch64 is not supported on windows\n")),
            }

            if offset >= 4095 {
                missingf!(loc, c!("Data offsets bigger than 4095 are not supported yet\n"));
            } else if offset > 0 {
                sb_appendf(output, c!("    add %s, %s, %zu\n"), reg, reg, offset);
            }
        }
        Arg::Bogus => unreachable!("bogus-amogus")
    };
}

pub unsafe fn generate_function(name: *const c_char, _name_loc: Loc, params_count: usize, auto_vars_count: usize, os: Os, variadics: *const HashTable<*const c_char, Variadic>, body: *const [OpWithLocation], output: *mut String_Builder) {
    let stack_size = align_bytes(auto_vars_count*8, 16);
    match os {
        Os::Linux => {
            sb_appendf(output, c!(".global %s\n"), name);
            sb_appendf(output, c!(".p2align 4\n"));
            sb_appendf(output, c!("%s:\n"), name);
        }
        Os::Darwin => {
            sb_appendf(output, c!(".global _%s\n"), name);
            sb_appendf(output, c!(".p2align 4\n"));
            sb_appendf(output, c!("_%s:\n"), name);
        }
        Os::Windows => {
            todo!("AArch64 is not supported on windows\n")
        }
    }
    //sb_appendf(output, c!("    stp x29, x30, [sp, -%zu]!\n"), stack_size);
    sb_appendf(output, c!("    stp x29, x30, [sp, -2*8]!\n"));
    sb_appendf(output, c!("    mov x29, sp\n"), name);
    sb_appendf(output, c!("    sub sp, sp, %zu\n"), stack_size);
    assert!(auto_vars_count >= params_count);

    const REGISTERS: *const[*const c_char] = &[c!("x0"), c!("x1"), c!("x2"), c!("x3"), c!("x4"), c!("x5"), c!("x6"), c!("x7")];
    for i in 0..params_count {
        let below_index = i + 1;
        let reg = if i < REGISTERS.len() { (*REGISTERS)[i] } else { c!("x8") };

        if i >= REGISTERS.len() {
            let above_index = i - REGISTERS.len();
            sb_appendf(output, c!("    ldr x8, [x29, 2*8 + %zu]\n"), above_index*8);
        }

        sb_appendf(output, c!("    str %s, [x29, -%zu]\n"), reg, below_index*8);
    }

    for i in 0..body.len() {
        let op = (*body)[i];
        match op.opcode {
            Op::Bogus => unreachable!("bogus-amogus"),
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg_to_reg(arg, c!("x0"), output, op.loc, os);
                }
                sb_appendf(output, c!("    add sp, sp, %zu\n"), stack_size);
                sb_appendf(output, c!("    ldp x29, x30, [sp], 2*8\n"));
                sb_appendf(output, c!("    ret\n"));
            }
            Op::Negate {result, arg} => {
                load_arg_to_reg(arg, c!("x0"), output, op.loc, os);
                sb_appendf(output, c!("    neg x0, x0\n"));
                sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), result*8);
            }
            Op::UnaryNot {result, arg} => {
                load_arg_to_reg(arg, c!("x0"), output, op.loc, os);
                sb_appendf(output, c!("    cmp x0, 0\n"));
                sb_appendf(output, c!("    cset x0, eq\n"));
                sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), result*8);
            },
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    orr x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    },
                    Binop::BitAnd => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    and x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    },
                    Binop::BitShl => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    lsl x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    },
                    Binop::BitShr => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    lsr x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    },
                    Binop::Plus => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    add x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    }
                    Binop::Minus => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    sub x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    },
                    Binop::Mod => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        // https://stackoverflow.com/questions/35351470/obtaining-remainder-using-single-aarch64-instruction
                        sb_appendf(output, c!("    sdiv x2, x0, x1\n"));
                        sb_appendf(output, c!("    msub x2, x2, x1, x0\n"));
                        sb_appendf(output, c!("    str x2, [x29, -%zu]\n"), index*8);
                    }
                    Binop::Div => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    sdiv x2, x0, x1\n"));
                        sb_appendf(output, c!("    str x2, [x29, -%zu]\n"), index*8);
                    }
                    Binop::Mult => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    mul x0, x0, x1\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    },
                    Binop::Less => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, lt\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    }
                    Binop::Greater => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, gt\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    }
                    Binop::Equal => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, eq\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    }
                    Binop::NotEqual => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, ne\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    }
                    Binop::GreaterEqual => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, ge\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    },
                    Binop::LessEqual => {
                        load_arg_to_reg(lhs, c!("x0"), output, op.loc, os);
                        load_arg_to_reg(rhs, c!("x1"), output, op.loc, os);
                        sb_appendf(output, c!("    cmp x0, x1\n"));
                        sb_appendf(output, c!("    cset x0, le\n"));
                        sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
                    },
                }
            }
            Op::ExternalAssign{name, arg} => {
                load_arg_to_reg(arg, c!("x0"), output, op.loc, os);
                match os {
                    Os::Linux => {
                        sb_appendf(output, c!("    adrp x1, %s\n"), name);
                        sb_appendf(output, c!("    add  x1, x1, :lo12:%s\n"), name);
                    }
                    Os::Darwin => {
                        sb_appendf(output, c!("    adrp x1, _%s@GOTPAGE\n"), name);
                        sb_appendf(output, c!("    ldr  x1, [x1, _%s@GOTPAGEOFF]\n"), name);
                    }
                    Os::Windows => missingf!(op.loc, c!("AArch64 is not supported on windows\n")),
                }
                sb_appendf(output, c!("    str x0, [x1]\n"), name);
            }
            Op::AutoAssign {index, arg} => {
                load_arg_to_reg(arg, c!("x0"), output, op.loc, os);
                sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), index*8);
            },
            Op::Store {index, arg} => {
                sb_appendf(output, c!("    ldr x0, [x29, -%zu]\n"), index*8);
                load_arg_to_reg(arg, c!("x1"), output, op.loc, os);
                sb_appendf(output, c!("    str x1, [x0]\n"));
            },
            Op::Funcall {result, fun, args} => {
                let mut fixed_args = 0;
                match fun {
                    Arg::External(name) | Arg::RefExternal(name) => {
                        if let Some(variadic) = HashTable::get(variadics, name) {
                            fixed_args = (*variadic).fixed_args;
                        }
                    }
                    _ => {}
                }

                // Apple's AArch64 ABI is slightly different from standard AAPCS64
                // and specifies that all varargs are passed on the stack:
                // https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms#Update-code-that-passes-arguments-to-variadic-functions
                let reg_args_count = if fixed_args != 0 && os == Os::Darwin {
                    fixed_args
                } else if args.count <= REGISTERS.len() {
                    args.count
                } else {
                    REGISTERS.len()
                };
                for i in 0..reg_args_count {
                    let reg = (*REGISTERS)[i];
                    load_arg_to_reg(*args.items.add(i), reg, output, op.loc, os);
                }

                let stack_args_count = args.count - reg_args_count;
                let stack_args_size = align_bytes(stack_args_count*8, 16);
                sb_appendf(output, c!("    sub sp, sp, %zu\n"), stack_args_size);
                for i in reg_args_count..args.count {
                    let above_index = i - reg_args_count;
                    load_arg_to_reg(*args.items.add(i), c!("x8"), output, op.loc, os);
                    sb_appendf(output, c!("    str x8, [sp, %zu]\n"), above_index*8);
                }

                call_arg(fun, op.loc, output, os);

                sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), result*8);
                sb_appendf(output, c!("    add sp, sp, %zu\n"), stack_args_size);
            },
            Op::Asm {stmts} => {
                for i in 0..stmts.count {
                    let stmt = *stmts.items.add(i);
                    sb_appendf(output, c!("    %s\n"), stmt.line);
                }
            }
            Op::Label {label} => {
                match os {
                    Os::Linux => sb_appendf(output, c!(".L%s.label_%zu:\n"), name, label),
                    Os::Darwin => sb_appendf(output, c!("L%s.label_%zu:\n"), name, label),
                    Os::Windows => missingf!(op.loc, c!("AArch64 is not supported on windows\n")),
                };
            }
            Op::JmpLabel {label} => {
                match os {
                    Os::Linux => sb_appendf(output, c!("b .L%s.label_%zu\n"), name, label),
                    Os::Darwin => sb_appendf(output, c!("b L%s.label_%zu\n"), name, label),
                    Os::Windows => missingf!(op.loc, c!("AArch64 is not supported on windows\n")),
                };
            }
            Op::JmpIfNotLabel {label, arg} => {
                load_arg_to_reg(arg, c!("x0"), output, op.loc, os);
                sb_appendf(output, c!("    cmp x0, 0\n"));
                match os {
                    Os::Linux => sb_appendf(output, c!("    beq .L%s.label_%zu\n"), name, label),
                    Os::Darwin => sb_appendf(output, c!("    beq L%s.label_%zu\n"), name, label),
                    Os::Windows => missingf!(op.loc, c!("AArch64 is not supported on windows\n")),
                };
            }
            Op::Index {result, arg, offset} => {
                load_arg_to_reg(arg, c!("x0"), output, op.loc, os);
                load_arg_to_reg(offset, c!("x1"), output, op.loc, os);
                sb_appendf(output, c!("    add x0, x0, x1, lsl 3\n"));
                sb_appendf(output, c!("    str x0, [x29, -%zu]\n"), result*8);
            },
        }
    }
    sb_appendf(output, c!("    mov x0, 0\n"));
    sb_appendf(output, c!("    add sp, sp, %zu\n"), stack_size);
    sb_appendf(output, c!("    ldp x29, x30, [sp], 2*8\n"));
    sb_appendf(output, c!("    ret\n"));
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func], variadics: *const HashTable<*const c_char, Variadic>, os: Os) {
    sb_appendf(output, c!(".text\n"));
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].name_loc, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, os, variadics, da_slice((*funcs)[i].body), output);
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

pub unsafe fn generate_globals(output: *mut String_Builder, globals: *const [Global], os: Os) {
    if globals.len() > 0 {
        // TODO: consider splitting globals into bss and data sections,
        // depending on whether it's zero
        sb_appendf(output, c!(".data\n"));
        for i in 0..globals.len() {
            let global = (*globals)[i];
            match os {
                Os::Linux => {
                    sb_appendf(output, c!(".global %s\n"), global.name);
                    sb_appendf(output, c!(".p2align 3\n"));
                    sb_appendf(output, c!("%s:\n"), global.name);
                }
                Os::Darwin => {
                    sb_appendf(output, c!(".global _%s\n"), global.name);
                    sb_appendf(output, c!(".p2align 3\n"));
                    sb_appendf(output, c!("_%s:\n"), global.name);
                }
                Os::Windows => {
                    todo!("AArch64 is not supported on windows\n")
                }
            }
            if global.is_vec {
                sb_appendf(output, c!("    .quad .+8\n"), global.name);
            }
            for j in 0..global.values.count {
                match *global.values.items.add(j) {
                    ImmediateValue::Literal(lit) => {
                        sb_appendf(output, c!("    .quad %zu\n"), lit);
                    }
                    ImmediateValue::Name(name) => match os {
                        Os::Linux => {
                            sb_appendf(output, c!("    .quad %s\n"), name);
                        }
                        Os::Darwin => {
                            sb_appendf(output, c!("    .quad _%s\n"), name);
                        }
                        Os::Windows => {
                            todo!("AArch64 is not supported on windows\n");
                        }
                    },
                    ImmediateValue::DataOffset(offset) => {
                        sb_appendf(output, c!("    .quad .dat+%zu\n"), offset);
                    }
                }
            }
            if global.values.count < global.minimum_size {
                sb_appendf(output, c!("    .zero %zu\n"), 8*(global.minimum_size - global.values.count));
            }
        }
    }
}

pub unsafe fn generate_asm_funcs(output: *mut String_Builder, asm_funcs: *const [AsmFunc], os: Os) {
    for i in 0..asm_funcs.len() {
        let asm_func = (*asm_funcs)[i];
        match os {
            Os::Linux => {
                sb_appendf(output, c!(".global %s\n"), asm_func.name);
                sb_appendf(output, c!(".p2align 4\n"));
                sb_appendf(output, c!("%s:\n"), asm_func.name);
            }
            Os::Darwin => {
                sb_appendf(output, c!(".global _%s\n"), asm_func.name);
                sb_appendf(output, c!(".p2align 4\n"));
                sb_appendf(output, c!("_%s:\n"), asm_func.name);
            }
            Os::Windows => missingf!(asm_func.name_loc, c!("AArch64 is not supported on windows\n")),
        }
        for j in 0..asm_func.body.count {
            let stmt = *asm_func.body.items.add(j);
            sb_appendf(output, c!("    %s\n"), stmt.line);
        }
    }
}

pub unsafe fn usage(params: *const [Param]) {
    fprintf(stderr(), c!("gas_aarch64 codegen for the B compiler\n"));
    fprintf(stderr(), c!("OPTIONS:\n"));
    print_params_help(params);
}

struct Gas_AArch64 {
    link_args: *const c_char,
    output: String_Builder,
    cmd: Cmd,
}

pub unsafe fn new(a: *mut arena::Arena, args: *const [*const c_char]) -> Option<*mut c_void> {
    let gen = arena::alloc_type::<Gas_AArch64>(a);
    memset(gen as _ , 0, size_of::<Gas_AArch64>());

    let mut help = false;
    let params = &[
        Param {
            name:        c!("help"),
            description: c!("Print this help message"),
            value:       ParamValue::Flag { var: &mut help },
        },
        Param {
            name:        c!("link-args"),
            description: c!("Additional linker arguments"),
            value:       ParamValue::String { var: &mut (*gen).link_args, default: c!("") },
        },
    ];

    if let Err(message) = parse_args(params, args) {
        usage(params);
        log(Log_Level::ERROR, c!("%s"), message);
        return None;
    }

    if help {
        usage(params);
        return None;
    }

    Some(gen as *mut c_void)
}

pub unsafe fn generate_program(
    gen: *mut c_void, program: *const Program, program_path: *const c_char, garbage_base: *const c_char, os: Os,
    nostdlib: bool, debug: bool,
) -> Option<()> {
    let gen = gen as *mut Gas_AArch64;
    let output = &mut (*gen).output;
    let cmd = &mut (*gen).cmd;

    if debug { todo!("Debug information for aarch64") }

    generate_funcs(output, da_slice((*program).funcs), &(*program).variadics, os);
    generate_asm_funcs(output, da_slice((*program).asm_funcs), os);
    generate_globals(output, da_slice((*program). globals), os);
    generate_data_section(output, da_slice((*program).data));

    let output_asm_path = temp_sprintf(c!("%s.s"), garbage_base);
    write_entire_file(output_asm_path, (*output).items as *const c_void, (*output).count)?;
    log(Log_Level::INFO, c!("generated %s"), output_asm_path);

    match os {
        Os::Linux => {
            let (gas, cc) = if cfg!(target_arch = "aarch64") && (cfg!(target_os = "linux") || cfg!(target_os = "android")) {
                (c!("as"), c!("cc"))
            } else {
                // TODO: document somewhere the additional packages you may require to cross compile gas-aarch64-linux
                //   The packages include qemu-user and some variant of the aarch64 gcc compiler (different distros call it differently)
                (c!("aarch64-linux-gnu-as"), c!("aarch64-linux-gnu-gcc"))
            };

            let output_obj_path = temp_sprintf(c!("%s.o"), garbage_base);
            cmd_append! {
                cmd,
                gas, c!("-o"), output_obj_path, output_asm_path,
            }
            if !cmd_run_sync_and_reset(cmd) { return None; }

            cmd_append! {
                cmd,
                cc, if cfg!(target_os = "android") {
                    c!("-fPIC")
                } else {
                    c!("-no-pie")
                },
                c!("-o"), program_path, output_obj_path,
            }
            if nostdlib {
                cmd_append!(cmd, c!("-nostdlib"));
            }
            let mut s: Shlex = zeroed();
            let link_args = (*gen).link_args;
            shlex_init(&mut s, link_args, link_args.add(strlen(link_args)));
            while !shlex_next(&mut s).is_null() {
                da_append(cmd, temp_strdup(s.string));
            }
            shlex_free(&mut s);
            if !cmd_run_sync_and_reset(cmd) { return None; }
        }
        Os::Darwin => {
            let (gas, cc) = (c!("as"), c!("cc"));

            if !(cfg!(target_os = "macos")) {
                log(Log_Level::ERROR, c!("Cross-compilation of darwin is not supported"));
                return None;
            }

            let output_obj_path = temp_sprintf(c!("%s.o"), garbage_base);
            cmd_append! {
                cmd,
                gas, c!("-arch"), c!("arm64"), c!("-o"), output_obj_path, output_asm_path,
            }
            if !cmd_run_sync_and_reset(cmd) { return None; }
            cmd_append! {
                cmd,
                cc, c!("-arch"), c!("arm64"), c!("-o"), program_path, output_obj_path,
            }
            if nostdlib {
                cmd_append! {
                    cmd,
                    c!("-nostdlib"),
                }
            }
            let mut s: Shlex = zeroed();
            let link_args = (*gen).link_args;
            shlex_init(&mut s, link_args, link_args.add(strlen(link_args)));
            while !shlex_next(&mut s).is_null() {
                da_append(cmd, temp_strdup(s.string));
            }
            shlex_free(&mut s);
            if !cmd_run_sync_and_reset(cmd) { return None; }
        }
        Os::Windows => todo!(),
    }

    Some(())
}

pub unsafe fn run_program(
    gen: *mut c_void, program_path: *const c_char, run_args: *const [*const c_char], os: Os,
) -> Option<()> {
    let gen = gen as *mut Gas_AArch64;
    let cmd = &mut (*gen).cmd;

    match os {
        Os::Linux => {
            if !(cfg!(target_arch = "aarch64") && cfg!(target_os = "linux")) {
                cmd_append! {
                    cmd,
                    c!("qemu-aarch64"), c!("-L"), c!("/usr/aarch64-linux-gnu"),
                }
            }

            // if the user does `b program.b -run` the compiler tries to run `program` which is not possible on Linux. It has to be `./program`.
            let run_path: *const c_char;
            if (strchr(program_path, '/' as c_int)).is_null() {
                run_path = temp_sprintf(c!("./%s"), program_path);
            } else {
                run_path = program_path;
            }

            cmd_append! {
                cmd,
                run_path,
            }

            da_append_many(cmd, run_args);
            if !cmd_run_sync_and_reset(cmd) { return None; }
        }
        Os::Darwin => {
            if !cfg!(target_arch = "aarch64") {
                log(Log_Level::ERROR, c!("This runner is only for aarch64 Darwin, but the current target is not aarch64 Darwin."));
                return None;
            }

            // if the user does `b program.b -run` the compiler tries to run `program` which is not possible on Darwin. It has to be `./program`.
            let run_path: *const c_char;
            if (strchr(program_path, '/' as c_int)).is_null() {
                run_path = temp_sprintf(c!("./%s"), program_path);
            } else {
                run_path = program_path;
            }

            cmd_append! {
                cmd,
                run_path,
            }

            da_append_many(cmd, run_args);
            if !cmd_run_sync_and_reset(cmd) { return None; }
        }
        Os::Windows => todo!(),
    }
    Some(())
}
