// This uses 16-bit words, because addresses in 6502 are 16bits, so otherwise pointers would not work.
// To emulate 16-bit words using 8-bit registers, we use Y to hold the high byte and A to hold the low byte.

// As 6502 has a fixed stack at $0100-$01FF, we only have 255 bytes available. Machine code is loaded at $E000 by default, but can be reconfigured via LOAD_OFFSET=<offset> "linker flag".

// "Calling convention": first argument in Y:A, remaining args on the stack.

use core::ffi::*;
use core::mem::zeroed;
use core::ptr;
use crate::{Func, OpWithLocation, Global, Op, Compiler, Binop, Arg, AsmFunc, Loc};
use crate::nob::*;
use crate::{missingf, diagf};
use crate::crust::libc::*;
use crate::runner::mos6502::{Config, DEFAULT_LOAD_OFFSET};

const ADC_IMM:   u8 = 0x69;
const ADC_X:     u8 = 0x7D;
const ADC_ZP:    u8 = 0x65;
const AND_ZP:    u8 = 0x25;
const ASL_ZP:    u8 = 0x06;
const BCC:       u8 = 0x90;
const BMI:       u8 = 0x30;
const BNE:       u8 = 0xD0;
const BPL:       u8 = 0x10;
const CLC:       u8 = 0x18;
const CMP_IMM:   u8 = 0xC9;
const CMP_ZP:    u8 = 0xC5;
const CPY_IMM:   u8 = 0xC0;
const CPY_ZP:    u8 = 0xC4;
const DEX:       u8 = 0xCA;
const DEY:       u8 = 0x88;
const INX:       u8 = 0xE8;
const JMP_ABS:   u8 = 0x4C;
const JMP_IND:   u8 = 0x6C;
const JSR:       u8 = 0x20;
const LDA_IMM:   u8 = 0xA9;
const LDA_IND_X: u8 = 0xA1;
const LDA_IND_Y: u8 = 0xB1;
const LDA_X:     u8 = 0xBD;
const LDA_ZP:    u8 = 0xA5;
const LDX_IMM:   u8 = 0xA2;
const LDY_IMM:   u8 = 0xA0;
const LDY_X:     u8 = 0xBC;
const LDY_ZP:    u8 = 0xA4;
const PHA:       u8 = 0x48;
const PLA:       u8 = 0x68;
const RTS:       u8 = 0x60;
const ROL_ZP:    u8 = 0x26;
const SBC_ZP:    u8 = 0xE5;
const SEC:       u8 = 0x38;
const STA_IND_Y: u8 = 0x91;
const STA_X:     u8 = 0x9D;
const STA_ZP:    u8 = 0x85;
const STY_ZP:    u8 = 0x84;
const TAX:       u8 = 0xAA;
const TAY:       u8 = 0xA8;
const TSX:       u8 = 0xBA;
const TXA:       u8 = 0x8A;
const TXS:       u8 = 0x9A;
const TYA:       u8 = 0x98;
const NOP:       u8 = 0xEA;

// zero page addresses
// TODO: Do we really have to use
// zero page for indirect function calls
// or derefs?
const ZP_DEREF_0:       u8 = 0;
const ZP_DEREF_1:       u8 = 1;
const ZP_DEREF_STORE_0: u8 = 2;
const ZP_DEREF_STORE_1: u8 = 3;
const ZP_RHS_L:         u8 = 4;
const ZP_RHS_H:         u8 = 5;
const ZP_TMP_0:         u8 = 6;
const ZP_TMP_1:         u8 = 7;
const ZP_TMP_2:         u8 = 8;
const ZP_TMP_3:         u8 = 9;
const ZP_DEREF_FUN_0:   u8 = 10; // can't be the same as ZP_DEREF,
const ZP_DEREF_FUN_1:   u8 = 11; // as we use this before argument loading

const STACK_PAGE: u16 = 0x0100;

#[derive(Clone, Copy)]
pub enum RelocationKind {
    AddressAbs {
        idx: usize
    }, // address from Assembler.addresses
    AddressRel {
        idx: usize,
    }, // address from Assembler.addresses
    DataOffset {
        off: u16,
        low: bool
    },
    Function {
        name: *const c_char
    },
    Label {
        func_name: *const c_char,
        label: usize
    },
}
impl RelocationKind {
    pub fn is16(self) -> bool {
        match self {
            RelocationKind::DataOffset{..} => false,
            RelocationKind::Function{..}   => true,
            RelocationKind::Label{..}      => true,
            RelocationKind::AddressRel{..} => false,
            RelocationKind::AddressAbs{..} => true,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Relocation {
    pub kind: RelocationKind,
    pub addr: u16,
}

#[derive(Clone, Copy)]
pub struct Function {
    pub name: *const c_char,
    pub addr: u16,
}

#[derive(Clone, Copy)]
pub struct Label {
    pub func_name: *const c_char,
    pub label: usize,
    pub addr: u16,
}

#[derive(Clone, Copy)]
pub struct Assembler {
    pub relocs: Array<Relocation>,
    pub functions: Array<Function>,
    pub op_labels: Array<Label>,
    pub addresses: Array<u16>,
    pub code_start: u16, // load address of code section
    pub frame_sz: u8, // current stack frame size in bytes, because 6502 has no base register
}

pub unsafe fn write_byte(output: *mut String_Builder, byte: u8) {
    da_append(output, byte as c_char);
}
pub unsafe fn write_word(output: *mut String_Builder, word: u16) {
    write_byte(output, word as u8);
    write_byte(output, (word >> 8) as u8);
}
pub unsafe fn write_byte_at(output: *mut String_Builder, byte: u8, addr: u16) {
    *((*output).items.add(addr as usize)) = byte as c_char;
}
pub unsafe fn write_word_at(output: *mut String_Builder, word: u16, addr: u16) {
    write_byte_at(output, word as u8, addr);
    write_byte_at(output, (word>>8) as u8, addr+1);
}

pub unsafe fn add_reloc(output: *mut String_Builder, kind: RelocationKind, asm: *mut Assembler) {
    da_append(&mut (*asm).relocs, Relocation {
        kind,
        addr: (*output).count as u16
    });
    if kind.is16() {
        write_word(output, 0);
    } else {
        write_byte(output, 0);
    }
}

pub unsafe fn create_address_label(asm: *mut Assembler) -> usize {
    let idx = (*asm).addresses.count;
    da_append(&mut (*asm).addresses, 0);
    idx
}
pub unsafe fn create_address_label_here(output: *const String_Builder, asm: *mut Assembler) -> usize {
    let label = create_address_label(asm);
    link_address_label_here(label, output, asm);
    label
}

// TODO: inform the caller, that `addr' is relative to code_start
pub unsafe fn link_address_label(label: usize, addr: u16, asm: *mut Assembler) {
    *(*asm).addresses.items.add(label) = addr;
}
pub unsafe fn link_address_label_here(label: usize, output: *const String_Builder, asm: *mut Assembler) {
    *(*asm).addresses.items.add(label) = (*output).count as u16;
}

pub unsafe fn load_auto_var(output: *mut String_Builder, index: usize, asm: *mut Assembler) {
    // save current stack pointer
    write_byte(output, TSX);

    // load low byte
    write_byte(output, LDA_X);
    write_word(output, STACK_PAGE + (*asm).frame_sz as u16 - (index-1) as u16 * 2 - 1);

    // load high byte
    write_byte(output, LDY_X);
    write_word(output, STACK_PAGE + (*asm).frame_sz as u16 - (index-1) as u16 * 2);
}

pub unsafe fn load_arg(arg: Arg, loc: Loc, output: *mut String_Builder, asm: *mut Assembler) {
    match arg {
        Arg::Deref(index) => {
            load_auto_var(output, index, asm);

            // load address to buffer in ZP to dereference, because registers
            // only 8 bits
            write_byte(output, STA_ZP);
            write_byte(output, ZP_DEREF_0);
            write_byte(output, STY_ZP);
            write_byte(output, ZP_DEREF_1);

            // Y = ((0),1)
            write_byte(output, LDY_IMM);
            write_byte(output, 1);

            write_byte(output, LDA_IND_Y);
            write_byte(output, ZP_DEREF_0);
            write_byte(output, TAY);

            // A = ((0,0))
            write_byte(output, LDX_IMM);
            write_byte(output, 0);

            write_byte(output, LDA_IND_X);
            write_byte(output, ZP_DEREF_0);
        },
        Arg::RefAutoVar(_index)  => missingf!(loc, c!("RefAutoVar\n")),
        Arg::RefExternal(_name)  => missingf!(loc, c!("RefExternal\n")),
        Arg::External(_name)     => missingf!(loc, c!("External\n")),
        Arg::AutoVar(index)     => {
            load_auto_var(output, index, asm);
        },
        Arg::Literal(value) => {
            if value >= 65536 {
                diagf!(loc, c!("WARNING: contant `%d` out of range for 16 bits\n"), value);
            }
            write_byte(output, LDA_IMM);
            write_byte(output, value as u8);
            write_byte(output, LDY_IMM);
            write_byte(output, (value >> 8) as u8);
        },
        Arg::DataOffset(offset) => {
            assert!(offset < 65536, "data offset out of range");
            write_byte(output, LDA_IMM);
            add_reloc(output, RelocationKind::DataOffset{off: offset as u16, low: true}, asm);
            write_byte(output, LDY_IMM);
            add_reloc(output, RelocationKind::DataOffset{off: (offset + 1) as u16, low: false}, asm);
        },
        Arg::Bogus => unreachable!(),
    };
}

pub unsafe fn store_auto(output: *mut String_Builder, index: usize, asm: *mut Assembler) {
    // save current stack pointer
    write_byte(output, TSX);

    // save low byte
    write_byte(output, STA_X);
    write_word(output, STACK_PAGE + (*asm).frame_sz as u16 - (index-1) as u16 * 2 - 1);

    // save high byte
    write_byte(output, TYA);
    write_byte(output, STA_X);
    write_word(output, STACK_PAGE + (*asm).frame_sz as u16 - (index-1) as u16 * 2);
}

// TODO: can this be done better?
pub unsafe fn add_sp(output: *mut String_Builder, bytes: u8, asm: *mut Assembler) {
    (*asm).frame_sz -= bytes;
    if bytes < 8 {
        for _ in 0 .. bytes {
            write_byte(output, PLA);
        }
    } else {
        write_byte(output, TSX);
        write_byte(output, TXA);
        write_byte(output, CLC);
        write_byte(output, ADC_IMM);
        write_byte(output, bytes);
        write_byte(output, TAX);
        write_byte(output, TXS);
    }
}
// cannot modify Y:A here, as they hold first argument
// TODO: look, if this can be done without a loop, like in `add_sp` without modifying
// Y:A. Either save them temporarily or write the first arg to stack before decrementing
// SP
pub unsafe fn sub_sp(output: *mut String_Builder, bytes: u8, asm: *mut Assembler) {
    (*asm).frame_sz += bytes;
    for _ in 0 .. bytes {
        write_byte(output, PHA);
    }
}
pub unsafe fn push16(output: *mut String_Builder, asm: *mut Assembler) {
    (*asm).frame_sz += 2;

    write_byte(output, TAX);
    write_byte(output, TYA);
    // push high byte first
    write_byte(output, PHA);
    write_byte(output, TXA);
    // then low
    write_byte(output, PHA);
}
pub unsafe fn pop16_discard(output: *mut String_Builder, asm: *mut Assembler) {
    (*asm).frame_sz -= 2;

    write_byte(output, PLA);
    write_byte(output, PLA);
}

// load lhs in Y:A, rhs in RHS_L:RHS_H
pub unsafe fn load_two_args(output: *mut String_Builder, lhs: Arg, rhs: Arg, op: OpWithLocation, asm: *mut Assembler) {
    load_arg(rhs, op.loc, output, asm);
    write_byte(output, STA_ZP);
    write_byte(output, ZP_RHS_L);
    write_byte(output, STY_ZP);
    write_byte(output, ZP_RHS_H);
    load_arg(lhs, op.loc, output, asm);
}

pub unsafe fn generate_function(name: *const c_char, params_count: usize, auto_vars_count: usize,
                                body: *const [OpWithLocation], output: *mut String_Builder,
                                asm: *mut Assembler) {
    (*asm).frame_sz = 0;
    let fun_addr = (*output).count as u16;
    da_append(&mut (*asm).functions, Function {
        name,
        addr: fun_addr,
    });

    // prepare function labels for each op and the end of the function
    let mut op_addresses: Array<usize> = zeroed();
    for _ in 0..=body.len() {
        let idx = (*asm).addresses.count;
        da_append(&mut op_addresses, idx);

        da_append(&mut (*asm).addresses, 0);
    }

    // TODO: use params_count, auto_vars_count
    assert!(auto_vars_count*2 < 256);
    let stack_size = (auto_vars_count * 2) as u8;
    sub_sp(output, stack_size, asm);

    for i in 0..(params_count as u16) {
        write_byte(output, TSX);
        if i == 0 {
            // low
            write_byte(output, STA_X);
            write_word(output, STACK_PAGE + stack_size as u16 - 2*i - 1);

            // high
            write_byte(output, TYA);
            write_byte(output, STA_X);
            write_word(output, STACK_PAGE + stack_size as u16 - 2*i);
            continue;
        }

        // low
        write_byte(output, LDA_X);
        write_word(output, STACK_PAGE + stack_size as u16 + 2*i + 1);
        write_byte(output, STA_X);
        write_word(output, STACK_PAGE + stack_size as u16 - 2*i - 1);

        // high
        write_byte(output, LDA_X);
        write_word(output, STACK_PAGE + stack_size as u16 + 2*i + 2);
        write_byte(output, STA_X);
        write_word(output, STACK_PAGE + stack_size as u16 - 2*i);
    }

    for i in 0..body.len() {
        let addr_idx = *op_addresses.items.add(i);
        *(*asm).addresses.items.add(addr_idx) = (*output).count as u16; // update op address

        let op = (*body)[i];
        match op.opcode {
            Op::Bogus => unreachable!("bogus-amogus"),
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg(arg, op.loc, output, asm);
                }

                // jump to ret statement
                write_byte(output, JMP_ABS);
                add_reloc(output, RelocationKind::AddressAbs
                          {idx: *op_addresses.items.add(body.len())}, asm);
            },
            Op::Store {index, arg} => {
                load_auto_var(output, index, asm);
                write_byte(output, STA_ZP);
                write_byte(output, ZP_DEREF_STORE_0);
                write_byte(output, STY_ZP);
                write_byte(output, ZP_DEREF_STORE_1);

                load_arg(arg, op.loc, output, asm);
                write_byte(output, TAX);
                write_byte(output, TYA);

                write_byte(output, LDY_IMM);
                write_byte(output, 1);
                write_byte(output, STA_IND_Y); // high
                write_byte(output, ZP_DEREF_STORE_0);
                write_byte(output, DEY);
                write_byte(output, TXA);
                write_byte(output, STA_IND_Y); // low
                write_byte(output, ZP_DEREF_STORE_0);
            },
            Op::ExternalAssign{name: _, arg: _} => missingf!(op.loc, c!("implement ExternalAssign\n")),
            Op::AutoAssign{index, arg} => {
                load_arg(arg, op.loc, output, asm);
                store_auto(output, index, asm);
            },
            Op::Negate {result: _, arg: _} => missingf!(op.loc, c!("implement Negate\n")),
            Op::UnaryNot{result: _, arg: _} => missingf!(op.loc, c!("implement UnaryNot\n")),
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => missingf!(op.loc, c!("implement BitOr\n")),
                    Binop::BitAnd => {
                        load_two_args(output, lhs, rhs, op, asm);

                        write_byte(output, AND_ZP);
                        write_byte(output, ZP_RHS_L);
                        write_byte(output, TAX);
                        write_byte(output, TYA);
                        write_byte(output, AND_ZP);
                        write_byte(output, ZP_RHS_H);
                        write_byte(output, TAY);
                        write_byte(output, TXA);
                    },
                    Binop::BitShl => missingf!(op.loc, c!("implement BitShl\n")),
                    Binop::BitShr => missingf!(op.loc, c!("implement BitShr\n")),
                    Binop::Plus => {
                        load_two_args(output, lhs, rhs, op, asm);

                        write_byte(output, CLC);
                        write_byte(output, ADC_ZP);
                        write_byte(output, ZP_RHS_L);
                        write_byte(output, TAX);
                        write_byte(output, TYA);
                        write_byte(output, ADC_ZP);
                        write_byte(output, ZP_RHS_H);
                        write_byte(output, TAY);
                        write_byte(output, TXA);
                    },
                    Binop::Minus  => missingf!(op.loc, c!("implement Minus\n")),
                    Binop::Mod => missingf!(op.loc, c!("implement Mod\n")),
                    Binop::Div => missingf!(op.loc, c!("implement Div\n")),
                    Binop::Mult => {
                        load_two_args(output, lhs, rhs, op, asm);

                        // TODO: maybe move this to an intrinsic function,
                        // because it is rather long. Consider this, if we run
                        // out of memory at some point.

                        // shift-and-add/long multiplication
                        // see: https://en.wikipedia.org/wiki/Multiplication_algorithm

                        // TODO: this should be signed, save and restore signes before.

                        // from here on: unsigned multiplication
                        // store lhs
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_TMP_0);
                        write_byte(output, STY_ZP);
                        write_byte(output, ZP_TMP_1);

                        // store Y:A in ZP, because shifting and adding is easier
                        // without all the register switching
                        write_byte(output, LDA_IMM);
                        write_byte(output, 0);
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_TMP_2);
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_TMP_3);

                        let loop_start = create_address_label_here(output, asm);
                        let cont = create_address_label(asm);
                        let finished = create_address_label(asm);

                        // if both zero [-> A = 0], we are finished
                        write_byte(output, LDA_ZP);
                        write_byte(output, ZP_RHS_L);
                        write_byte(output, BNE);
                        add_reloc(output, RelocationKind::AddressRel{idx: cont}, asm);
                        write_byte(output, LDA_ZP);
                        write_byte(output, ZP_RHS_H);
                        write_byte(output, BNE);
                        add_reloc(output, RelocationKind::AddressRel{idx: cont}, asm);

                        write_byte(output, JMP_ABS);
                        add_reloc(output, RelocationKind::AddressAbs{idx: finished}, asm);

                        link_address_label_here(cont, output, asm);

                        // shift left current accumulater between single adds
                        write_byte(output, ASL_ZP);
                        write_byte(output, ZP_TMP_2);
                        write_byte(output, ROL_ZP);
                        write_byte(output, ZP_TMP_3);

                        write_byte(output, ASL_ZP);
                        write_byte(output, ZP_RHS_L);
                        write_byte(output, ROL_ZP);
                        write_byte(output, ZP_RHS_H);

                        // if bit is 0, do not add anything
                        write_byte(output, BCC);
                        add_reloc(output, RelocationKind::AddressRel{idx: loop_start}, asm);

                        // bit is 1 here, we have to add entire lhs to acc
                        write_byte(output, CLC);
                        write_byte(output, LDA_ZP);
                        write_byte(output, ZP_TMP_2); // acc, low
                        write_byte(output, ADC_ZP);
                        write_byte(output, ZP_TMP_0); // lhs, low
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_TMP_2); // acc, low

                        write_byte(output, LDA_ZP);
                        write_byte(output, ZP_TMP_3); // acc, high
                        write_byte(output, ADC_ZP);
                        write_byte(output, ZP_TMP_1); // lhs, high
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_TMP_3); // acc, high

                        // continue loop
                        write_byte(output, JMP_ABS);
                        add_reloc(output, RelocationKind::AddressAbs{idx: loop_start}, asm);
                        link_address_label_here(finished, output, asm);

                        // move back in Y:A
                        write_byte(output, LDA_ZP);
                        write_byte(output, ZP_TMP_2);
                        write_byte(output, LDY_ZP);
                        write_byte(output, ZP_TMP_3);

                        // missingf!(op.loc, c!("implement Mult\n"))
                    },
                    Binop::Less => {
                        load_two_args(output, lhs, rhs, op, asm);
                        // we subtract, then check sign

                        write_byte(output, LDX_IMM);
                        write_byte(output, 1);

                        write_byte(output, SEC); // set carry
                        // sub low byte
                        write_byte(output, SBC_ZP);
                        write_byte(output, ZP_RHS_L);
                        // sub high byte
                        write_byte(output, TYA);
                        write_byte(output, SBC_ZP);
                        write_byte(output, ZP_RHS_H);
                        // high result in A, N flag if less.

                        // if less skip, we already have X=1
                        write_byte(output, BMI);
                        write_byte(output, 1);

                        write_byte(output, DEX);

                        write_byte(output, TXA);
                        // zero extend result
                        write_byte(output, LDY_IMM);
                        write_byte(output, 0);
                    },
                    Binop::Greater => missingf!(op.loc, c!("implement Greater\n")),
                    Binop::Equal => {
                        load_two_args(output, lhs, rhs, op, asm);

                        write_byte(output, LDX_IMM);
                        write_byte(output, 0);

                        write_byte(output, CMP_ZP);
                        write_byte(output, ZP_RHS_L);
                        write_byte(output, BNE);
                        write_byte(output, 5);

                        write_byte(output, CPY_ZP);
                        write_byte(output, ZP_RHS_H);
                        write_byte(output, BNE);
                        write_byte(output, 1);

                        write_byte(output, INX);
                        write_byte(output, TXA);
                        write_byte(output, LDY_IMM);
                        write_byte(output, 0);
                    },
                    Binop::NotEqual => missingf!(op.loc, c!("implement NotEqual\n")),
                    Binop::GreaterEqual => missingf!(op.loc, c!("implement GreaterEqual\n")),
                    Binop::LessEqual => missingf!(op.loc, c!("implement LessEqual\n")),
                }
                store_auto(output, index, asm);
            },
            Op::Funcall{result, fun, args} => {
                match fun {
                    Arg::RefExternal(_) | Arg::External(_) => {},
                    arg => {
                        load_arg(arg, op.loc, output, asm);
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_DEREF_FUN_0);
                        write_byte(output, STY_ZP);
                        write_byte(output, ZP_DEREF_FUN_1);
                    }
                }

                for i in (0..args.count).rev() {
                    load_arg(*args.items.add(i), op.loc, output, asm);
                    // first arg in Y:A to be compatible with wozmon routines
                    if i != 0 {
                        push16(output, asm);
                    }
                }
                match fun {
                    Arg::RefExternal(name) | Arg::External(name) => {
                        write_byte(output, JSR);
                        add_reloc(output, RelocationKind::Function{name}, asm);
                    },
                    _ => { // function pointer already loaded in ZP_DEREF_FUN
                        // there is no jsr (indirect), so emulate using jsr and jmp (indirect).
                        write_byte(output, JSR);
                        write_word(output, (*asm).code_start + (*output).count as u16 + 5);
                        write_byte(output, JMP_ABS);
                        write_word(output, (*asm).code_start + (*output).count as u16 + 5);
                        write_byte(output, JMP_IND);
                        write_word(output, ZP_DEREF_FUN_0 as u16);
                    },
                }
                if args.count > 1 {
                    write_byte(output, TAX);
                    // clear stack
                    for i in 0 .. args.count {
                        if i == 0 {
                            continue;
                        }
                        pop16_discard(output, asm);
                    }
                    write_byte(output, TXA);
                }
                store_auto(output, result, asm);
            },
            Op::Asm {args: _} => unreachable!(),
            Op::Label{label} => {
                // TODO: RE: https://github.com/tsoding/b/pull/147#issue-3154667157
                // > For this thing I introduces a new NOP instruction because it would be a bit too
                // > risky to just blindly jump on an address that could possibly be unused.
                //
                // Assess the risk and potentially remove this NOP
                write_byte(output, NOP);
                da_append(&mut (*asm).op_labels, Label {
                    func_name: name,
                    label,
                    addr: (*output).count as u16,
                });
            },
            Op::JmpLabel{label} => {
                write_byte(output, JMP_ABS);
                add_reloc(output, RelocationKind::Label{func_name: name, label}, asm);
            },
            Op::JmpUnlessLabel{label, arg} => {
                load_arg(arg, op.loc, output, asm);

                write_byte(output, CMP_IMM);
                write_byte(output, 0);

                // if !=0, skip next check and branch
                write_byte(output, BNE);
                write_byte(output, 7); // skip next 4 instructions

                write_byte(output, CPY_IMM);
                write_byte(output, 0);

                write_byte(output, BNE);
                write_byte(output, 3);

                write_byte(output, JMP_ABS);
                add_reloc(output, RelocationKind::Label{func_name: name, label}, asm);
            },
        }
    }
    let addr_idx = *op_addresses.items.add(body.len());
    *(*asm).addresses.items.add(addr_idx) = (*output).count as u16; // update op address

    if stack_size > 0 {
        // seriously... we don't have enough registers to save A to...
        write_byte(output, STA_ZP);
        write_byte(output, ZP_RHS_L);
        add_sp(output, stack_size, asm);
        write_byte(output, LDA_ZP);
        write_byte(output, ZP_RHS_L);
    }
    write_byte(output, RTS);
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func], asm: *mut Assembler) {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output, asm);
    }
}

pub unsafe fn apply_relocations(output: *mut String_Builder, data_start: u16, asm: *mut Assembler) {
    'reloc_loop: for i in 0..(*asm).relocs.count {
        let reloc = *(*asm).relocs.items.add(i);
        let caddr = reloc.addr;
        match reloc.kind {
            RelocationKind::DataOffset{off, low} => {
                if low {
                    write_byte_at(output, (data_start + off) as u8, caddr);
                } else {
                    write_byte_at(output, ((data_start + off) >> 8) as u8, caddr);
                }
            },
            RelocationKind::Function{name} => {
                for i in 0..(*asm).functions.count {
                    let label = *(*asm).functions.items.add(i);
                    if strcmp(label.name, name) == 0 {
                        write_word_at(output, (*asm).code_start + label.addr, caddr);
                        continue 'reloc_loop;
                    }
                }
                printf(c!("linking failed. could not find function `%s'\n"), name);
                unreachable!();
            },
            RelocationKind::Label{func_name: name, label} => {
                for i in 0..(*asm).op_labels.count {
                    let op_label = *(*asm).op_labels.items.add(i);
                    if strcmp(op_label.func_name, name) == 0 && op_label.label == label {
                        write_word_at(output, (*asm).code_start + op_label.addr, caddr);
                        continue 'reloc_loop;
                    }
                }
                printf(c!("linking failed. could not find label `%s.%u'\n"), name, label);
                unreachable!();
            },
            RelocationKind::AddressRel{idx} => {
                let jaddr = *(*asm).addresses.items.add(idx);
                let rel: i16 = jaddr as i16 - (caddr + 1) as i16;
                assert!(rel < 128 && rel >= -128);
                write_byte_at(output, rel as u8, caddr);
            },
            RelocationKind::AddressAbs{idx} => {
                let saddr = *(*asm).addresses.items.add(idx) + (*asm).code_start;
                write_word_at(output, saddr, caddr);
            },
        }
    }
}

pub unsafe fn generate_extrns(output: *mut String_Builder, extrns: *const [*const c_char],
                              funcs: *const [Func], globals: *const [Global], asm: *mut Assembler) {
    'skip_function_or_global: for i in 0..extrns.len() {
        // assemble a few "stdlib" functions which can't be programmed in B
        let name = (*extrns)[i];
        for j in 0..funcs.len() {
            let func = (*funcs)[j];
            if strcmp(func.name, name) == 0 {
                continue 'skip_function_or_global
            }
        }
        for j in 0..globals.len() {
            let global = (*globals)[j].name;
            if strcmp(global, name) == 0 {
                continue 'skip_function_or_global
            }
        }
        // TODO: consider introducing target-specific inline assembly and implementing all these intrinsics in it
        if strcmp(name, c!("char")) == 0 {
            // ch = char(string, i);
            // returns the ith character in a string pointed to by string, 0 based

            let fun_addr = (*output).count as u16;
            da_append(&mut (*asm).functions, Function {
                name,
                addr: fun_addr,
            });

            write_byte(output, TSX);
            write_byte(output, CLC);
            write_byte(output, ADC_X);
            write_word(output, STACK_PAGE + 2 + 1); // low

            // load address to buffer in ZP to dereference, because registers
            // only 8 bits
            write_byte(output, STA_ZP);
            write_byte(output, ZP_DEREF_0);

            write_byte(output, TYA);
            write_byte(output, ADC_X);
            write_word(output, STACK_PAGE + 2 + 2); // high
            write_byte(output, STA_ZP);
            write_byte(output, ZP_DEREF_1);

            write_byte(output, LDX_IMM);
            write_byte(output, 0);

            // A = ((0))
            write_byte(output, LDA_IND_X);
            write_byte(output, ZP_DEREF_0);

            // sign extend Y
            write_byte(output, LDY_IMM);
            write_byte(output, 0);

            write_byte(output, CMP_IMM);
            write_byte(output, 0);
            write_byte(output, BPL);
            write_byte(output, 1);
            write_byte(output, DEY);

            write_byte(output, RTS);
        } else {
            fprintf(stderr(), c!("Unknown extrn: `%s`, can not link\n"), name);
            abort();
        }
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    for i in 0..data.len() {
        write_byte(output, (*data)[i]);
    }
}

pub unsafe fn generate_entry(output: *mut String_Builder, asm: *mut Assembler) {
    write_byte(output, JSR);
    add_reloc(output, RelocationKind::Function{name: c!("main")}, asm);

    // exit code 0
    write_byte(output, LDA_IMM);
    write_byte(output, 0);

    write_byte(output, JMP_IND);
    write_word(output, 0xFFFC);
}

pub unsafe fn parse_config_from_link_flags(link_flags: *const[*const c_char]) -> Option<Config> {
    let mut config = Config {
        load_offset: DEFAULT_LOAD_OFFSET,
    };

    // TODO: some sort of help flag to list all these "linker" flags for mos6502
    for i in 0..link_flags.len() {
        let flag = (*link_flags)[i];
        let mut flag_sv = sv_from_cstr(flag);
        let load_offset_prefix = sv_from_cstr(c!("LOAD_OFFSET="));
        if sv_starts_with(flag_sv, load_offset_prefix) {
            flag_sv.data = flag_sv.data.add(load_offset_prefix.count);
            flag_sv.count += load_offset_prefix.count;
            config.load_offset = strtoull(flag_sv.data, ptr::null_mut(), 16) as u16;
        } else {
            fprintf(stderr(), c!("Unknown linker flag: %s\n"), flag);
            return None
        }
    }

    Some(config)
}

pub unsafe fn generate_asm_funcs(_output: *mut String_Builder, asm_funcs: *const [AsmFunc]) {
    for i in 0..asm_funcs.len() {
        let asm_func = (*asm_funcs)[i];
        missingf!(asm_func.name_loc, c!("__asm__ functions for 6502"));
    }
}

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler, config: Config) -> Option<()> {
    let mut asm: Assembler = zeroed();
    generate_entry(output, &mut asm);
    asm.code_start = config.load_offset;

    generate_funcs(output, da_slice((*c).funcs), &mut asm);
    generate_asm_funcs(output, da_slice((*c).asm_funcs));
    generate_extrns(output, da_slice((*c).extrns), da_slice((*c).funcs), da_slice((*c).globals), &mut asm);

    let data_start = config.load_offset + (*output).count as u16;
    generate_data_section(output, da_slice((*c).data));

    apply_relocations(output, data_start, &mut asm);
    Some(())
}
