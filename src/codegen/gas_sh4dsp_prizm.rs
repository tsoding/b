use core::ffi::*;
use core::cmp;
use crate::nob::*;
use crate::crust::libc::*;
use crate::{Compiler, Binop, Op, OpWithLocation, Arg, Func, Global, ImmediateValue, AsmFunc};
use crate::{Loc};

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Assembler {
    pub funcname: *const c_char,
    pub output: *mut String_Builder,
    pub next_trampoline : usize,
    pub next_litteral : usize
}

pub unsafe fn write_nextsym(assembler: *mut Assembler) {
    let index = (*assembler).next_litteral;
    sb_appendf((*assembler).output, c!(".L_%sCL%d"), (*assembler).funcname, index as c_int);
}
pub unsafe fn next_litteral(assembler: *mut Assembler, lit: u32) -> usize {
    let index = (*assembler).next_litteral;
    let next_trampoline = (*assembler).next_trampoline;
    // Write a dirty "trampoline"
    sb_appendf((*assembler).output, c!("    nop ! Safety NOP for delay slot issues\n"));
    sb_appendf((*assembler).output, c!("    bra .L%sTrampoline%d\n"), (*assembler).funcname, next_trampoline);
    sb_appendf((*assembler).output, c!("    nop ! Safety NOP for delay slot issues\n"));
    sb_appendf((*assembler).output, c!("    .align 4\n"), (*assembler).funcname, next_trampoline);
    sb_appendf((*assembler).output, c!(".L_%sCL%d: .long 0x%x\n"), (*assembler).funcname, index as c_int, lit as c_uint);
    sb_appendf((*assembler).output, c!("    .align 2\n"), (*assembler).funcname, next_trampoline);
    sb_appendf((*assembler).output, c!(".L%sTrampoline%d:\n"), (*assembler).funcname, next_trampoline);

    (*assembler).next_litteral += 1;
    (*assembler).next_trampoline += 1;
    index
}
pub unsafe fn next_symbol(assembler: *mut Assembler, sym: *const c_char) -> usize {
    let index = (*assembler).next_litteral;
    let next_trampoline = (*assembler).next_trampoline;
    // Write a dirty "trampoline"
    sb_appendf((*assembler).output, c!("    nop ! Safety NOP for delay slot issues\n"));
    sb_appendf((*assembler).output, c!("    bra .L%sTrampoline%d\n"), (*assembler).funcname, next_trampoline);
    sb_appendf((*assembler).output, c!("    nop ! Safety NOP for delay slot issues\n"));
    sb_appendf((*assembler).output, c!("    .align 4\n"), (*assembler).funcname, next_trampoline);
    sb_appendf((*assembler).output, c!(".L_%sCL%d: .long %s\n"), (*assembler).funcname, index as c_int, sym);
    sb_appendf((*assembler).output, c!("    .align 2\n"), (*assembler).funcname, next_trampoline);
    sb_appendf((*assembler).output, c!(".L%sTrampoline%d:\n"), (*assembler).funcname, next_trampoline);

    (*assembler).next_litteral += 1;
    (*assembler).next_trampoline += 1;
    index
}

pub unsafe fn call_arg(arg: Arg, loc: Loc, output: *mut String_Builder, assembler: *mut Assembler) {
    // The extra flag is because arg doesnt use references to a symbol but the symbol itself,
    // which then causes some fun issues, as load_arg_to_reg will try to dereference a value that 
    // should NOT be dereferenced... this should be fixed in another PR >_>
    //                  - LDA: June 25th, 2025
    load_arg_to_reg(arg, c!("r0"), output, loc, assembler, true);
    sb_appendf(output, c!("    jsr @r0\n"));
    sb_appendf(output, c!("    nop\n"));
}

pub unsafe fn load_literal_to_reg(output: *mut String_Builder, reg: *const c_char, literal: u32, assembler: *mut Assembler) {
    let sliteral = literal as i64;

    // Literals that can fit in a byte are well-supported on SH-4    
    if (sliteral >= -128) & (sliteral <= 127) {
        sb_appendf(output, c!("    mov #%d, %s\n"), literal, reg);
        return;
    }
    sb_appendf(output, c!("    mov.l "));
    write_nextsym(assembler);
    sb_appendf(output, c!(", %s\n"), reg);

    next_litteral(assembler, literal);
}
pub unsafe fn load_arg_to_reg(arg: Arg, reg: *const c_char, output: *mut String_Builder, _loc: Loc, assembler: *mut Assembler, is_call: bool) {
    match arg {
        Arg::External(name) => {
            sb_appendf(output, c!("    mov.l "));
            write_nextsym(assembler);
            sb_appendf(output, c!(", %s ! %s\n"), reg, name);
            if !is_call {
                sb_appendf(output, c!("    mov.l @%s, %s\n"), reg, reg);
            }
            next_symbol(assembler, name);
        },
        Arg::Deref(index) => {
            // TODO
            sb_appendf(output, c!("    ! dereference of argument[%d]\n"), index);
            sb_appendf(output, c!("    mov r14, r8\n"));
            sb_appendf(output, c!("    add #-%d, r8\n"), index*4);
            sb_appendf(output, c!("    mov.l @r8, %s\n"), reg);
            sb_appendf(output, c!("    mov.l @%s, %s\n"), reg, reg);
        },
        Arg::RefAutoVar(index) => {
            sb_appendf(output, c!("    mov r14, %s\n"), reg);
            sb_appendf(output, c!("    add #-%d, %s\n"), index*4, reg);
        },
        Arg::RefExternal(name) => {
            sb_appendf(output, c!("    mov.l "));
            write_nextsym(assembler);
            sb_appendf(output, c!(", %s ! %s\n"), reg, name);
            next_symbol(assembler, name);
        },
        Arg::AutoVar(index) => {
            sb_appendf(output, c!("    mov r14, r8\n"));
            sb_appendf(output, c!("    add #-%d, r8\n"), index*4);
            sb_appendf(output, c!("    mov.l @r8, %s\n"), reg);
        },
        Arg::Literal(value) => {
            load_literal_to_reg(output, reg, value as u32, assembler);
        },
        Arg::DataOffset(offset) => {
            sb_appendf(output, c!("    mov.l "));
            write_nextsym(assembler);
            sb_appendf(output, c!(", %s ! %s\n"), reg, c!("<data zone>"));
            next_symbol(assembler, c!(".dat"));
            if offset >= 127 {
                load_literal_to_reg(output, c!("r8"), offset as u32, assembler);
                sb_appendf(output, c!("    add r8, %s\n"), reg);
            } else {
                // TODO: Manage this through loading a literal into a register and adding it if
                // possible
                if offset != 0 { sb_appendf(output, c!("    add #%d, %s\n"), offset, reg); }
            }
        },
        Arg::Bogus => unreachable!("bogus-amogus")
    };
}

pub unsafe fn write_r0(output: *mut String_Builder, argument: usize) {
    sb_appendf(output, c!("    ! Storing into argument[%zu]\n"), argument);
    if argument > 0 {
        sb_appendf(output, c!("    mov r0, r8\n"));
        sb_appendf(output, c!("    mov #-%d, r0\n"), argument*4);
        sb_appendf(output, c!("    mov.l r8, @(r0,r14)\n"));
    } else {
        // ?!
        sb_appendf(output, c!("    mov.l r0, @r14\n"));
    }
    sb_appendf(output, c!("    \n"));
}
pub unsafe fn generate_function(name: *const c_char, _name_loc: Loc, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], output: *mut String_Builder, _c: *const Compiler) {
    let mut assembler: Assembler = Assembler { 
        funcname: name,
        output: output,
        next_trampoline: 0, next_litteral: 0
    };

    // Put each symbol in its own section
    sb_appendf(output, c!(".text\n"));

    sb_appendf(output, c!(".section .text.%s,\"ax\",@progbits\n"), name);
    sb_appendf(output, c!(".align 4\n"));
    sb_appendf(output, c!(".global %s\n"), name);
    sb_appendf(output, c!(".type %s, @function\n"), name);
    sb_appendf(output, c!("%s:\n"), name);

    sb_appendf(output, c!("    mov.l r8, @-r15\n"));
    sb_appendf(output, c!("    mov.l r9, @-r15\n"));
    sb_appendf(output, c!("    mov.l r10, @-r15\n"));
    sb_appendf(output, c!("    mov.l r11, @-r15\n"));
    sb_appendf(output, c!("    mov.l r12, @-r15\n"));
    sb_appendf(output, c!("    mov.l r13, @-r15\n"));
    sb_appendf(output, c!("    mov.l r14, @-r15\n"));
    sb_appendf(output, c!("    sts.l pr, @-r15\n"));

    // Prepare r14 to hold our important stuff
    sb_appendf(output, c!("    mov r15, r14\n"));

    assert!(auto_vars_count >= params_count);

    // TODO: Deal with arguments
    const REGISTERS: *const[*const c_char] = &[c!("r4"), c!("r5"), c!("r6"), c!("r7")];

    // To address the arguments backwards, simply remove from r13
    sb_appendf(output, c!("    mov r15, r13\n"));
    sb_appendf(output, c!("    add #%d, r13\n"), 8*4);
    for i in 0..params_count {
        // ???
        let reg = if i < REGISTERS.len() { (*REGISTERS)[i] } else { c!("r7") };

        if i >= REGISTERS.len() {
            // TODO: We should probably get a register ready for those occasions 
            // (reading from another zone of the stack)
            sb_appendf(output, c!("    mov.l @r13+, %s\n"), reg);
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
                    load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut assembler, false);
                } else {
                    sb_appendf(output, c!("    mov #0, r0\n"));
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
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut assembler, false);
                sb_appendf(output, c!("    neg r0, r0\n"));
                write_r0(output, result);
            }
            Op::UnaryNot {result, arg} => {
                // This is probably not the most efficient way to do things, and it 
                // also thrashes T, so yeah. but then again the B compiler doesn't seem 
                // to be something that cares about processor flags all that much.
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut assembler, false);
                sb_appendf(output, c!("    cmp/eq #0, r0\n"));
                sb_appendf(output, c!("    movt r0\n"));
                //sb_appendf(output, c!("    xor #1, r0\n"));
                write_r0(output, result);
            },
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    or r1, r0\n"));
                        write_r0(output, index);
                    },
                    Binop::BitAnd => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    and r1, r0\n"));
                        write_r0(output, index);
                    },

                    Binop::BitShl => {
                        // TODO: There is an opportunity for optimisation for shifts by certain
                        // literals (16, 8, 4, 2, 1)
                        match rhs {
                            Arg::Literal(amount) => {
                                if amount >= 32 {
                                    // You may as well just clear the output.
                                    sb_appendf(output, c!("    mov #0, r0\n"));
                                } else {
                                    const SHIFTS: [u64;4] = [16,8,2,1];
                                    let mut remaining = amount;
                                    let mut index = 0;
                                    load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                                    while remaining > 0 {
                                        let off = SHIFTS[index];
                                        if remaining >= off {
                                            if off != 1 {
                                                sb_appendf(output, c!("    shll%d r0\n"), off as c_int);
                                            } else {
                                                sb_appendf(output, c!("    shll r0\n"));
                                            }
                                            remaining -= off;
                                            continue;
                                        }
                                        index += 1;
                                    }
                                }
                            }
                            _ => {
                                load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                                load_arg_to_reg(rhs, c!("r5"), output, op.loc, &mut assembler, false);
                                // TODO: How should shifting by negative amounts be handled?
                                // Should the absolute value be taken, or should it be interpreted as a
                                // right shift?
                                sb_appendf(output, c!("    shld r5, r0\n"));
                            }
                        }
                        write_r0(output, index);
                    },
                    Binop::BitShr => {
                        // TODO: There is an opportunity for optimisation for shifts by certain
                        // literals (16, 8, 4, 2, 1)
                        match rhs {
                            Arg::Literal(amount) => {
                                if amount >= 32 {
                                    // You may as well just clear the output.
                                    sb_appendf(output, c!("    mov #0, r0\n"));
                                } else {
                                    const SHIFTS: [u64;4] = [16,8,2,1];
                                    let mut remaining = amount;
                                    let mut index = 0;
                                    load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                                    while remaining > 0 {
                                        let off = SHIFTS[index];
                                        if remaining >= off {
                                            if off != 1 {
                                                sb_appendf(output, c!("    shll%d r0\n"), off as c_int);
                                            } else {
                                                sb_appendf(output, c!("    shll r0\n"));
                                            }
                                            remaining -= off;
                                            continue;
                                        }
                                        index += 1;
                                    }
                                }
                            }
                            _ => {
                                load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                                load_arg_to_reg(rhs, c!("r5"), output, op.loc, &mut assembler, false);
                                // TODO: How should shifting by negative amounts be handled?
                                // Should the absolute value be taken, or should it be interpreted as a
                                // right shift?
                                sb_appendf(output, c!("    neg r5, r5\n"));
                                sb_appendf(output, c!("    shld r5, r0\n"));
                            }
                        }
                        write_r0(output, index);
                    },
                    Binop::Plus => {
                        sb_appendf(output, c!("    ! +\n"));
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    add r1, r0\n"));
                        write_r0(output, index);
                    }
                    Binop::Minus => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    sub r1, r0\n"));
                        write_r0(output, index);
                    },

                    // Oh god. God no. Don't let me put the 32 div1 jumpscare.
                    Binop::Mod => {
                        // TODO
                        load_arg_to_reg(lhs, c!("r4"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r5"), output, op.loc, &mut assembler, false);
                        call_arg(Arg::External(c!("intrisic_mod")), op.loc, output, &mut assembler);
                        write_r0(output, index);
                    }
                    Binop::Div => {
                        // TODO
                        load_arg_to_reg(lhs, c!("r4"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r5"), output, op.loc, &mut assembler, false);
                        call_arg(Arg::External(c!("intrisic_div")), op.loc, output, &mut assembler);
                        write_r0(output, index);
                    }
                    
                    // This sounds more reasonable to implement
                    Binop::Mult => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    mul.l r0, r1\n"));
                        sb_appendf(output, c!("    sts macl, r0\n"));
                        write_r0(output, index);
                    },

                    // TODO: Test these out
                    Binop::Less => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    cmp/gt r0, r1\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        write_r0(output, index);
                    }
                    Binop::Greater => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    cmp/gt r1, r0\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        write_r0(output, index);
                    }
                    Binop::Equal => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    cmp/eq r1, r0\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        write_r0(output, index);
                    }
                    Binop::NotEqual => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    cmp/eq r1, r0\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        sb_appendf(output, c!("    xor #1, r0\n"));
                        write_r0(output, index);
                    }
                    Binop::GreaterEqual => {
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    cmp/ge r1, r0\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        write_r0(output, index);
                    },
                    Binop::LessEqual => {
                        // TODO: Make sure these work well
                        load_arg_to_reg(lhs, c!("r0"), output, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, c!("r1"), output, op.loc, &mut assembler, false);
                        sb_appendf(output, c!("    cmp/ge r0, r1\n"));      // Signed
                        sb_appendf(output, c!("    movt r0\n"));
                        write_r0(output, index);
                    },
                }
            }
            Op::ExternalAssign{name, arg} => {
                // I'm just going to add it to the assembler.littset to make sure everything's okay
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut assembler, false);
                sb_appendf(output, c!("    mov.l "));
                write_nextsym(&mut assembler);
                sb_appendf(output, c!(", r1 ! %s\n"), name);
                sb_appendf(output, c!("    mov.l r0, @r1\n"));

                next_symbol(&mut assembler, name);
            }
            Op::AutoAssign {index, arg} => {
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut assembler, false);
                write_r0(output, index);
            },
            Op::Store {index, arg} => {
                sb_appendf(output, c!("    ! STORE ARGUMENT INTO INDEX\n"));
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut assembler, false);
                sb_appendf(output, c!("    mov r14, r1\n"));
                sb_appendf(output, c!("    add #-%d, r1\n"), index * 4);
                sb_appendf(output, c!("    mov.l @r1, r1\n"));
                sb_appendf(output, c!("    mov.l r0, @r1\n"));
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
                    load_arg_to_reg(*args.items.add(i), reg, output, op.loc, &mut assembler, false);
                }
                for i in 0..stack_args_count {
                    let j = stack_args_count - i - 1;
                    sb_appendf(output, c!("    ! loading stack %d\n"), i);
                    load_arg_to_reg(*args.items.add(reg_args_count+j), c!("r0"), output, op.loc, &mut assembler, false);
                    sb_appendf(output, c!("    mov.l r0, @-r15\n"));
                    sb_appendf(output, c!("    \n"));
                }
                call_arg(fun, op.loc, output, &mut assembler);
                write_r0(output, result);
                if stack_args_count > 0 {
                    sb_appendf(output, c!("    add #%d, r15\n"), 4*stack_args_count);
                }
                sb_appendf(output, c!("    \n"));
            },
            Op::Asm { stmts } => {
                for i in 0..stmts.count {
                    let stmt = *stmts.items.add(i);
                    sb_appendf(output, c!("    %s\n"), stmt.line);
                }
            }
            Op::Label {label} => {
                sb_appendf(output, c!("%s.label_%zu:\n"), name, label);
            }
            Op::JmpLabel {label} => {
                // TODO: Deal with __more__ PCrel nonsense
                // (well, to be frank, GNU as, by default, does some "trampoline" magic 
                // to ensure those jumps are always addressible (but I still think they should 
                // be managed properly :3)
                sb_appendf(output, c!("    nop ! Safety NOP for delay slot issues\n"));
                sb_appendf(output, c!("    bra %s.label_%zu\n"), name, label);
                sb_appendf(output, c!("    nop ! Safety NOP for delay slot issues\n"));
            }
            Op::JmpIfNotLabel {label, arg} => {
                load_arg_to_reg(arg, c!("r0"), output, op.loc, &mut assembler, false);
                sb_appendf(output, c!("    tst r0, r0\n"));
                sb_appendf(output, c!("    bt %s.label_%zu\n"), name, label);
            }
        }
    }

    // TODO: Function epilogue + Constzone

    // KRIS, RESTORE THE PR REGISTER
    // TODO: Do not do it if we have a tail function that doesn't call anything.
    sb_appendf(output, c!("    ! EPILOGUE START\n"));
    sb_appendf(output, c!("    mov #0, r0\n"));
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
    sb_appendf(output, c!(".data\n"));
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
                sb_appendf(output, c!("    .long .+4 ! (address into veczone)\n"), global.name);
            }
            for j in 0..global.values.count {
                match *global.values.items.add(j) {
                    ImmediateValue::Literal(lit) => {
                        sb_appendf(output, c!("    .long %zu\n"), lit);
                    }
                    ImmediateValue::Name(name) => {
                        sb_appendf(output, c!("    .long %s\n"), name);
                    }
                    ImmediateValue::DataOffset(offset) => {
                        sb_appendf(output, c!("    .long .dat+%zu\n"), offset);
                    }
                }
            }
            if global.values.count < global.minimum_size {
                sb_appendf(output, c!("    .zero %zu\n"), 4*(global.minimum_size - global.values.count));
            }
        }
    }
}

pub unsafe fn generate_asm_funcs(output: *mut String_Builder, asm_funcs: *const [AsmFunc]) {
    for i in 0..asm_funcs.len() {
        let asm_func = (*asm_funcs)[i];
        sb_appendf(output, c!(".global %s\n"), asm_func.name);
        sb_appendf(output, c!("%s:\n"), asm_func.name);
        for j in 0..asm_func.body.count {
            let stmt = *asm_func.body.items.add(j);
            sb_appendf(output, c!("    %s\n"), stmt.line);
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
