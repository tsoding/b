use core::ffi::*;
use core::cmp;
use core::mem::zeroed;
use crate::nob::*;
use crate::crust::libc::*;
use crate::{Compiler, Binop, Op, OpWithLocation, Arg, Func, Global, ImmediateValue, AsmFunc};
use crate::{missingf, Loc};

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Littset {
    pub cntr: u16,
    pub data: String_Builder,
}

pub unsafe fn call_arg(arg: Arg, loc: Loc, output: *mut String_Builder, consts: *mut Array<u64>, litts: *mut Littset, funcname: *const c_char) {
    load_arg_to_reg(arg, c!("r0"), output, loc, consts, litts, funcname);
    sb_appendf(output, c!("    jsr @r0\n"));
    sb_appendf(output, c!("    nop\n"));
}

pub unsafe fn load_literal_to_reg(output: *mut String_Builder, reg: *const c_char, literal: u64, consts: *mut Array<u64>, name: *const c_char) {
    let sliteral = literal as i64;

    // Literals that can fit in a byte are well-supported on SH-4    
    if (sliteral >= -128) & (sliteral < 127) {
        sb_appendf(output, c!("    mov #%d, %s\n"), literal, reg);
        return;
    }
    // Now, we have to deal with literals fitting in a 32-bit word.
    // The problem is that the generally accepted method is to declare a new symbol.
    //
    // TODO: Worry about very problematic PCrel stuff (probably by splitting functions into 
    // chunks (which are joined together by branches)
    let index = (*consts).count as u8;
    da_append(consts, literal);
    sb_appendf(output, c!("    mov.l .L_%sC%d, %s\n"), name, index as u32, reg);
}
// let mut littset: Array<*const i8> = zeroed();
pub unsafe fn load_arg_to_reg(arg: Arg, reg: *const c_char, output: *mut String_Builder, loc: Loc, consts: *mut Array<u64>, litts: *mut Littset, funcname: *const c_char) {
    match arg {
        Arg::External(name) => {
            let index = (*litts).cntr;
            sb_appendf(&mut (*litts).data, c!(".L_%sCL%d: .long %s\n"), funcname, index as u32, name);
            sb_appendf(output, c!("    mov.l .L_%sCL%d, %s\n"), funcname, index as u32, reg);
            (*litts).cntr += 1;
            // TODO: Littset
        },
        Arg::Deref(index) => {
            // TODO
            sb_appendf(output, c!("    mov r14, r2\n"));
            sb_appendf(output, c!("    add #-%d, r2\n"), index*4);
            sb_appendf(output, c!("    mov.l @r2, %s\n"), reg);
            sb_appendf(output, c!("    mov.l @%s, %s\n"), reg);
        },
        Arg::RefAutoVar(index) => {
            sb_appendf(output, c!("    mov r14, %s\n"), reg);
            sb_appendf(output, c!("    add #-%d, %s\n"), index*4, reg);
        },
        Arg::RefExternal(name) => {
            let index = (*litts).cntr;
            sb_appendf(&mut (*litts).data, c!(".L_%sCL%d: .long %s\n"), funcname, index as u32, name);
            sb_appendf(output, c!("    mov.l .L_%sCL%d, %s\n"), funcname, index as u32, reg);
            sb_appendf(output, c!("    mov.l @%s, %s\n"), reg, reg);
            (*litts).cntr += 1;
        },
        Arg::AutoVar(index) => {
            sb_appendf(output, c!("    mov r14, r2\n"));
            sb_appendf(output, c!("    add #-%d, r2\n"), index*4);
            sb_appendf(output, c!("    mov.l @r2, %s\n"), reg);
        },
        Arg::Literal(value) => {
            load_literal_to_reg(output, reg, value, consts, funcname);
        },
        Arg::DataOffset(offset) => {
            if offset >= 255 {
                missingf!(loc, c!("Data offsets bigger than 255 are not supported yet\n"));
            } else {
                sb_appendf(output, c!("    mov.l .L_%sCL0, %s\n"), funcname, reg);
                if offset != 0 { sb_appendf(output, c!("    add #%d, %s\n"), offset, reg); }
            }
        },
        Arg::Bogus => unreachable!("bogus-amogus")
    };
}

pub unsafe fn write_r0(output: *mut String_Builder, argument: usize) {
    sb_appendf(output, c!("    ! Loading argument[%zu]\n"), argument);
    if argument > 0 {
        sb_appendf(output, c!("    mov r0, r3\n"));
        sb_appendf(output, c!("    mov #-%d, r0\n"), argument*4);
        sb_appendf(output, c!("    mov.l r3, @(r0,r13)\n"));
    } else {
        // ?!
        sb_appendf(output, c!("    mov.l r0, @r13\n"));
    }
    sb_appendf(output, c!("    \n"));
}
pub unsafe fn generate_function(name: *const c_char, _name_loc: Loc, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], output: *mut String_Builder, _c: *const Compiler) {
    // TODO: Get a &mut constset
    let mut constset: Array<u64> = zeroed();
    let mut littset: Littset = Littset { data: zeroed(), cntr: 0 };

    // Put each symbol in its own section
    if strcmp(name, c!("start")) == 0 {
        sb_appendf(output, c!(".section .text.start\n"), name);
    }
    sb_appendf(output, c!(".global %s\n"), name);
    sb_appendf(output, c!("%s:\n"), name);

    // TODO: Manage the data section properly. (CL0 is reserved for it at the moment)
    let index = littset.cntr;
    sb_appendf(&mut littset.data, c!(".L_%sCL%d: .long %s\n"), name, index as u32, ".dat");
    (littset).cntr += 1;
    // Save Everything
    
    // KRIS, Get The PR System Register
    // TODO: Saving every callee register like that is WASTEFUL
    sb_appendf(output, c!("    mov.l r8, @-r15\n"));
    sb_appendf(output, c!("    mov.l r9, @-r15\n"));
    sb_appendf(output, c!("    mov.l r10, @-r15\n"));
    sb_appendf(output, c!("    mov.l r11, @-r15\n"));
    sb_appendf(output, c!("    mov.l r12, @-r15\n"));
    sb_appendf(output, c!("    mov.l r13, @-r15\n"));
    sb_appendf(output, c!("    mov.l r14, @-r15\n"));
    sb_appendf(output, c!("    sts.l pr, @-r15\n"));

    // Prepare r14/r13 to hold our important stuff
    sb_appendf(output, c!("    mov r15, r14\n"));
    sb_appendf(output, c!("    mov r15, r13\n"));

    // TODO: Frame pointers? what frame pointers?
    assert!(auto_vars_count >= params_count);

    // TODO: Deal with arguments
    const REGISTERS: *const[*const c_char] = &[c!("r4"), c!("r5"), c!("r6"), c!("r7")];
    // Now that r13 is ready to hold the argument space, we can just push them into the stack, as
    // regular.
    for i in 0..params_count {
        // ???
        let reg = if i < REGISTERS.len() { (*REGISTERS)[i] } else { c!("r0") };

        if i >= REGISTERS.len() {
            // TODO: We should probably get a register ready for those occasions 
            // (reading from another zone of the stack)
            unreachable!("TODO: Implement more than 4 arguments");
        }
        
        // Push that argument (from a register) onto the stack
        sb_appendf(output, c!("    mov.l %s, @-r15\n"), reg);
    }
    let diff = auto_vars_count - params_count;
    sb_appendf(output, c!("    add #-%d, r15\n"), diff * 4);
    sb_appendf(output, c!("    ! PROLOGUE END\n"));

    for i in 0..body.len() {
        let op = (*body)[i];
        match op.opcode {
            Op::Bogus => unreachable!("bogus-amogus"),
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                }
                sb_appendf(output, c!("    mov r14, r15\n"));
                sb_appendf(output, c!("    lds.l @r15+, pr\n"));
                sb_appendf(output, c!("    mov.l @r15+, r14\n"));
                sb_appendf(output, c!("    mov.l @r15+, r13\n"));
                sb_appendf(output, c!("    mov.l @r15+, r12\n"));
                sb_appendf(output, c!("    mov.l @r15+, r11\n"));
                sb_appendf(output, c!("    mov.l @r15+, r10\n"));
                sb_appendf(output, c!("    mov.l @r15+, r9\n"));
                sb_appendf(output, c!("    mov.l @r15+, r8\n"));
                // Restore every register, but backwards

                // Delay slot jumpscare
                sb_appendf(output, c!("    rts\n"));
                sb_appendf(output, c!("    nop\n"));
            }
            Op::Negate {result, arg} => {
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                sb_appendf(output, c!("    neg r0, r0\n"));
                write_r0(output, result);
            }
            Op::UnaryNot {result, arg} => {
                // This is probably not the most efficient way to do things, and it 
                // also thrashes T, so yeah. but then again the B compiler doesn't seem 
                // to be something that cares about processor flags all that much.
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                sb_appendf(output, c!("    cmp/eq #0, r0\n"));
                sb_appendf(output, c!("    movt r0\n"));
                sb_appendf(output, c!("    xor #1, r0\n"));
                write_r0(output, result);
            },
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    or r1, r0\n"));
                        write_r0(output, index);
                    },
                    Binop::BitAnd => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    and r1, r0\n"));
                        write_r0(output, index);
                    },

                    // TODO: SH4 is a very funny architecture when it comes to bitshifting.
                    // Part of me just wants to do a little trolling by using DSP loops and moving
                    // on to something better with my life instead of having a builtin routine for
                    // this.
                    Binop::BitShl => {
                        // TODO
                    },
                    Binop::BitShr => {
                        // TODO
                    },
                    Binop::Plus => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    add r1, r0\n"));
                        write_r0(output, index);
                    }
                    Binop::Minus => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    add r1, r0\n"));
                        write_r0(output, index);
                    },

                    // Oh god. God no. Don't let me put the 32 div1 jumpscare.
                    Binop::Mod => {
                        // TODO
                    }
                    Binop::Div => {
                        // TODO
                    }
                    
                    // This sounds more reasonable to implement
                    Binop::Mult => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    mul.l r0, r1\n"));
                        sb_appendf(output, c!("    sts macl, r0\n"));
                        write_r0(output, index);
                    },
                    Binop::Less => {
                        // a < b <=> not (b >= a)
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    cmp/ge r0, r1\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        sb_appendf(output, c!("    xor #1, r0\n"));
                        write_r0(output, index);
                    }
                    Binop::Greater => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    cmp/ge r1, r0\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        write_r0(output, index);
                    }
                    Binop::Equal => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    cmp/eq r1, r0\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        write_r0(output, index);
                    }
                    Binop::NotEqual => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    cmp/eq r1, r0\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        sb_appendf(output, c!("    xor #1, r0\n"));
                        write_r0(output, index);
                    }
                    Binop::GreaterEqual => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut constset, &mut littset, name);
                        sb_appendf(output, c!("    cmp/ge r1, r0\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        write_r0(output, index);
                    },
                    Binop::LessEqual => {
                        // TODO
                    },
                }
            }
            Op::ExternalAssign{name, arg} => {
                // I'm just going to add it to the littset to make sure everything's okay
                let index = littset.cntr;
                sb_appendf(&mut littset.data, c!(".L_CL%d: .long %s\n"), index as u32, name);

                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                sb_appendf(output, c!("    mov.l .L_CL%d, r1\n"), name);
                sb_appendf(output, c!("    mov.l r0, @r1\n"), name);
                littset.cntr += 1;
            }
            Op::AutoAssign {index, arg} => {
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                write_r0(output, index);
            },
            Op::Store {index, arg} => {
                // TODO
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                write_r0(output, index);
            },
            Op::Funcall {result, fun, args} => {
                // TODO: Prepare everything
                let registers: *const[*const c_char] = &[ c!("r4"), c!("r5"), c!("r6"), c!("r7") ];
                let reg_args_count = cmp::min(args.count, registers.len());
                let stack_args_count = args.count - reg_args_count;

                sb_appendf(output, c!("    \n"));
                for i in 0..reg_args_count {
                    let reg = (*registers)[i];

                    // why is it called add???
                    sb_appendf(output, c!("    ! loading %d\n"), i);
                    //sb_appendf(output, c!("    mov #0, %s\n"), reg);
                    load_arg_to_reg(*args.items.add(i), reg, output, op.loc, &mut constset, &mut littset, name);
                }
                for i in 0..stack_args_count {
                    sb_appendf(output, c!("    ! loading stack %d\n"), i);
                    load_arg_to_reg(*args.items.add(i), c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                    sb_appendf(output, c!("    mov.l r0, @-r15\n"));
                    sb_appendf(output, c!("    \n"));
                }
                call_arg(fun, op.loc, output, &mut constset, &mut littset, name);
                write_r0(output, result);
                if stack_args_count > 0 {
                    sb_appendf(output, c!("    add #%d, r15\n"), 4*stack_args_count);
                }
                sb_appendf(output, c!("    \n"));
            },
            Op::Asm {args} => {
                for i in 0..args.count {
                    let arg = *args.items.add(i);
                    sb_appendf(output, c!("    %s\n"), arg);
                }
            }
            Op::Label {label} => {
                sb_appendf(output, c!("%s.label_%zu:\n"), name, label);
            }
            Op::JmpLabel {label} => {
                // TODO: Deal with PCrel nonsense
                sb_appendf(output, c!("    bra %s.label_%zu\n"), name, label);
                sb_appendf(output, c!("    nop\n"));
            }
            Op::JmpIfNotLabel {label, arg} => {
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut constset, &mut littset, name);
                sb_appendf(output, c!("    tst r0, r0\n"));
                sb_appendf(output, c!("    bt %s.label_%zu\n"), name, label);
            }
        }
    }

    // TODO: Function epilogue + Constzone

    // KRIS, RESTORE THE PR REGISTER
    // TODO: Do not do it if we have a tail function that doesn't call anything.
    sb_appendf(output, c!("    ! EPILOGUE START\n"));
    sb_appendf(output, c!("    mov r14, r15\n"));
    sb_appendf(output, c!("    lds.l @r15+, pr\n"));
    sb_appendf(output, c!("    mov.l @r15+, r14\n"));
    sb_appendf(output, c!("    mov.l @r15+, r13\n"));
    sb_appendf(output, c!("    mov.l @r15+, r12\n"));
    sb_appendf(output, c!("    mov.l @r15+, r11\n"));
    sb_appendf(output, c!("    mov.l @r15+, r10\n"));
    sb_appendf(output, c!("    mov.l @r15+, r9\n"));
    sb_appendf(output, c!("    mov.l @r15+, r8\n"));
    // Restore every register, but backwards

    // Delay slot jumpscare
    sb_appendf(output, c!("    rts\n"));
    sb_appendf(output, c!("    nop\n"));
    // Restore every register, but backwards

    sb_appendf(output, c!(".align 4\n"));
    for i in 0..constset.count {
        sb_appendf(output, c!(".L_%sC%d:  .long %d\n"), i, name, constset.items.add(i));
    }
    if littset.cntr != 0 {
        sb_appendf(output, c!("%s\n"), littset.data.items);
    }
    if strcmp(name, c!("start")) == 0 {
        sb_appendf(output, c!(".section .text\n"));
    }
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func], c: *const Compiler) {
    sb_appendf(output, c!(".text\n"));
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].name_loc, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output, c);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    // TODO: Setting it as the data segment causes many problems as of now.
    //sb_appendf(output, c!(".data\n"));
    sb_appendf(output, c!(".dat:"));
    if data.len() > 0 {
        sb_appendf(output, c!(" .byte "));
        for i in 0..data.len() {
            if i > 0 {
                sb_appendf(output, c!(","));
            }
            sb_appendf(output, c!("0x%02X"), (*data)[i] as c_uint);
        }
        sb_appendf(output, c!("\n"));
    }
}

pub unsafe fn generate_globals(output: *mut String_Builder, globals: *const [Global]) {
    if globals.len() > 0 {
        // TODO: consider splitting globals into bss and data sections,
        // depending on whether it's zero
        sb_appendf(output, c!(".data\n"));
        for i in 0..globals.len() {
            let global = (*globals)[i];
            sb_appendf(output, c!(".global %s\n"), global.name);
            sb_appendf(output, c!("%s:\n"), global.name);
            if global.is_vec {
                sb_appendf(output, c!("    .quad .+8\n"), global.name);
            }
            for j in 0..global.values.count {
                match *global.values.items.add(j) {
                    ImmediateValue::Literal(lit) => {
                        sb_appendf(output, c!("    .quad %zu\n"), lit);
                    }
                    ImmediateValue::Name(name) => {
                        sb_appendf(output, c!("    .quad %s\n"), name);
                    }
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

pub unsafe fn generate_asm_funcs(output: *mut String_Builder, asm_funcs: *const [AsmFunc]) {
    for i in 0..asm_funcs.len() {
        let asm_func = (*asm_funcs)[i];
        sb_appendf(output, c!(".global %s ! ASM\n"), asm_func.name);
        sb_appendf(output, c!("%s:\n"), asm_func.name);
        for j in 0..asm_func.body.count {
            sb_appendf(output, c!("    %s\n"), *asm_func.body.items.add(j));
        }
    }
}

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    // TODO: Different ABIs (I could think Renesas v. GCC instead of the custom one here)
    generate_funcs(output, da_slice((*c).funcs), c);
    generate_asm_funcs(output, da_slice((*c).asm_funcs));
    generate_globals(output, da_slice((*c). globals));
    generate_data_section(output, da_slice((*c).data));
}
