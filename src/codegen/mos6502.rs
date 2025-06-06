use core::ffi::*;
use core::mem::zeroed;
use crate::{Func, OpWithLocation, Op, Compiler, Binop, Arg};
use crate::nob::*;
use crate::missingf;
use crate::crust::libc::*;
use crate::Loc;

const ADC_IMM:   u8 = 0x69;
const ADC_ZP:    u8 = 0x65;
const BEQ:       u8 = 0xF0;
const BNE:       u8 = 0xD0;
const CLC:       u8 = 0x18;
const CMP_IMM:   u8 = 0xC9;
const CPY_IMM:   u8 = 0xC0;
const DEY:       u8 = 0x88;
const INY:       u8 = 0xC8;
const TAX:       u8 = 0xAA;
const TXA:       u8 = 0x8A;
const TAY:       u8 = 0xA8;
const TYA:       u8 = 0x98;
const TSX:       u8 = 0xBA;
const TXS:       u8 = 0x9A;
const JMP_ABS:   u8 = 0x4C;
const JMP_IND:   u8 = 0x6C;
const JSR:       u8 = 0x20;
const LDA_ABS:   u8 = 0xAD;
const LDA_IND_X: u8 = 0xA1;
const LDA_IND_Y: u8 = 0xB1;
const LDA_IMM:   u8 = 0xA9;
const LDA_X:     u8 = 0xBD;
const LDX_ABS:   u8 = 0xAE;
const LDX_IMM:   u8 = 0xA2;
const LDY_ABS:   u8 = 0xAC;
const LDY_IMM:   u8 = 0xA0;
const LDY_X:     u8 = 0xBC;
const SBC_IMM:   u8 = 0xE9;
const STA_X:     u8 = 0x9D;
const STA_IND_Y: u8 = 0x91;
const STA_ZP:    u8 = 0x85;
const STX_ZP:    u8 = 0x86;
const STY_ZP:    u8 = 0x84;
const PHA:       u8 = 0x48;
const RTS:       u8 = 0x60;

// zero page addresses
const ZP_DEREF_0: u8 = 0;
const ZP_DEREF_1: u8 = 1;
const ZP_OP_TMP_0: u8 = 3;
const ZP_OP_TMP_1: u8 = 4;
const ZP_STACK_0: u8 = 5;
const ZP_STACK_1: u8 = 6;

#[derive(Clone, Copy)]
pub enum RelocationKind {
    AddressAbs {
        idx: usize
    }, // address from Assembler.addresses
    AddressRel {
        idx: usize,
        add: u16
    }, // address from Assembler.addresses
    DataOffset {
        off: u16,
        low: bool
    },
    Label {
        name: *const c_char
    },
}
impl RelocationKind {
    pub fn is16(self) -> bool {
        match self {
            RelocationKind::DataOffset{..} => false,
            RelocationKind::Label{..} => true,
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
pub struct Label {
    pub name: *const c_char,
    pub addr: u16,
}

#[derive(Clone, Copy)]
pub struct Assembler {
    pub relocs: Array<Relocation>,
    pub labels: Array<Label>,
    pub addresses: Array<u16>,
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

pub unsafe fn load_auto_var(output: *mut String_Builder, index: usize, asm: *mut Assembler) {
    // save current stack pointer
    write_byte(output, TSX);
    write_byte(output, STX_ZP);
    write_byte(output, ZP_STACK_0);

    // index relative to stack pointer
    write_byte(output, LDY_IMM);
    write_byte(output, (*asm).frame_sz - index as u8 * 2 - 1);

    // load low byte
    write_byte(output, LDA_IND_Y);
    write_byte(output, ZP_STACK_0);

    write_byte(output, TAX);

    // load high byte
    write_byte(output, INY);
    write_byte(output, LDA_IND_Y);
    write_byte(output, ZP_STACK_0);

    write_byte(output, TAY);
    write_byte(output, TXA);
}

pub unsafe fn load_arg(arg: Arg, output: *mut String_Builder, asm: *mut Assembler) {
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
        Arg::RefAutoVar(index)  => unimplemented!("RefAutoVar"),
        Arg::RefExternal(name)  => unimplemented!("RefExternal"),
        Arg::External(name)     => unimplemented!("External"),
        Arg::AutoVar(index)     => {
            load_auto_var(output, index, asm);
        },
        Arg::Literal(value) => {
            assert!(value < 65536);
            write_byte(output, LDA_IMM);
            write_byte(output, value as u8);
            write_byte(output, LDY_IMM);
            write_byte(output, (value >> 8) as u8);
        },
        Arg::DataOffset(offset) => {
            assert!(offset < 65536);
            write_byte(output, LDA_IMM);
            add_reloc(output, RelocationKind::DataOffset{off: offset as u16, low: true}, asm);
            write_byte(output, LDY_IMM);
            add_reloc(output, RelocationKind::DataOffset{off: (offset + 1) as u16, low: false}, asm);
        },
    };
}

pub unsafe fn store_auto(output: *mut String_Builder, index: usize, asm: *mut Assembler) {
    // save current stack pointer
    write_byte(output, TSX);
    write_byte(output, STX_ZP);
    write_byte(output, ZP_STACK_0);

    // [A;Y] -> [X;A]
    write_byte(output, TAX);
    write_byte(output, TYA);

    // index relative to stack pointer
    write_byte(output, LDY_IMM);
    write_byte(output, (*asm).frame_sz - index as u8 * 2);

    // save high byte
    write_byte(output, STA_IND_Y);
    write_byte(output, ZP_STACK_0);

    // save low byte
    write_byte(output, TXA);
    write_byte(output, DEY);
    write_byte(output, STA_IND_Y);
    write_byte(output, ZP_STACK_0);
}

pub unsafe fn add_sp(output: *mut String_Builder, bytes: u8) {
    write_byte(output, TSX);
    write_byte(output, TXA);
    write_byte(output, CLC);
    write_byte(output, ADC_IMM);
    write_byte(output, bytes);
    write_byte(output, TAX);
    write_byte(output, TXS);
}
pub unsafe fn sub_sp(output: *mut String_Builder, bytes: u8) {
    write_byte(output, TSX);
    write_byte(output, TXA);
    write_byte(output, CLC);
    write_byte(output, SBC_IMM);
    write_byte(output, bytes);
    write_byte(output, TAX);
    write_byte(output, TXS);
}
pub unsafe fn push16(output: *mut String_Builder) {
    write_byte(output, TAX);
    write_byte(output, TYA);
    write_byte(output, PHA);
    write_byte(output, TXA);
    write_byte(output, PHA);
}

pub unsafe fn generate_function(name: *const c_char, name_loc: Loc, code_start: u16,
                                params_count: usize, auto_vars_count: usize,
                                body: *const [OpWithLocation], output: *mut String_Builder,
                                asm: *mut Assembler) {
    let fun_addr = (*output).count as u16;
    da_append(&mut (*asm).labels, Label {
        name,
        addr: fun_addr,
    });

    printf(c!("TODO: passing arguments not implemented yet."));
    // for i in 0..params_count {
    // }

    // prepare labels for each op and the end of the function
    let mut op_addresses: Array<usize> = zeroed();
    for _ in 0..=body.len() {
        let idx = (*asm).addresses.count;
        da_append(&mut op_addresses, idx);

        da_append(&mut (*asm).addresses, 0);
    }

    // TODO: use params_count, auto_vars_count
    assert!(auto_vars_count*2 < 256);
    let stack_size = (auto_vars_count * 2) as u8;
    sub_sp(output, stack_size);
    (*asm).frame_sz = stack_size as u8;

    for i in 0..body.len() {
        let addr_idx = *op_addresses.items.add(i);
        *(*asm).addresses.items.add(addr_idx) = (*output).count as u16; // update op address
        
        let op = (*body)[i];
        match op.opcode {
            Op::Return {arg: _} => missingf!(name_loc, c!("implement Return\n\n")),
            Op::Store {index: _, arg: _} => missingf!(name_loc, c!("implement Store")),
            Op::ExternalAssign{name: _, arg: _} => missingf!(name_loc, c!("implement ExternalAssign\n")),
            Op::AutoAssign{index, arg} => {
                load_arg(arg, output, asm);
                store_auto(output, index, asm);
            },
            Op::Negate {result: _, arg: _} => missingf!(name_loc, c!("implement Negate\n")),
            Op::UnaryNot{result: _, arg: _} => missingf!(name_loc, c!("implement UnaryNot\n")),
            Op::Binop {binop, index: _, lhs, rhs} => {
                match binop {
                    Binop::BitOr => missingf!(name_loc, c!("implement BitOr\n")),
                    Binop::BitAnd => missingf!(name_loc, c!("implement BitAnd\n")),
                    Binop::BitShl => missingf!(name_loc, c!("implement BitShl\n")),
                    Binop::BitShr => missingf!(name_loc, c!("implement BitShr\n")),
                    Binop::Plus => {
                        load_arg(rhs, output, asm);
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_OP_TMP_0);
                        write_byte(output, STY_ZP);
                        write_byte(output, ZP_OP_TMP_1);
                        load_arg(lhs, output, asm);

                        write_byte(output, CLC);
                        write_byte(output, ADC_ZP);
                        write_byte(output, ZP_OP_TMP_0);
                        write_byte(output, TAX);
                        write_byte(output, TYA);
                        write_byte(output, ADC_ZP);
                        write_byte(output, ZP_OP_TMP_1);
                        write_byte(output, TAY);
                        write_byte(output, TXA);
                    },
                    Binop::Minus  => missingf!(name_loc, c!("implement Minus\n")),
                    Binop::Mod => missingf!(name_loc, c!("implement Mod\n")),
                    Binop::Div => missingf!(name_loc, c!("implement Div\n")),
                    Binop::Mult => missingf!(name_loc, c!("implement Mult\n")),
                    Binop::Less => missingf!(name_loc, c!("implement Less\n")),
                    Binop::Greater => missingf!(name_loc, c!("implement Greater\n")),
                    Binop::Equal => missingf!(name_loc, c!("implement Equal\n")),
                    Binop::NotEqual => missingf!(name_loc, c!("implement NotEqual\n")),
                    Binop::GreaterEqual => missingf!(name_loc, c!("implement GreaterEqual\n")),
                    Binop::LessEqual => missingf!(name_loc, c!("implement LessEqual\n")),
                }
            },
            Op::Funcall{result: _, fun, args} => {
                for i in (0..args.count).rev() {
                    load_arg(*args.items.add(i), output, asm);
                    push16(output);
                }
                match fun {
                    Arg::RefExternal(name) | Arg::External(name) => {
                        write_byte(output, JSR);
                        add_reloc(output, RelocationKind::Label{name}, asm);
                    },
                    arg => {
                        load_arg(arg, output, asm);
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_DEREF_0);
                        write_byte(output, STY_ZP);
                        write_byte(output, ZP_DEREF_1);

                        // there is no jsr (indirect), so emulate using jsr and jmp (indirect).
                        write_byte(output, JSR);
                        write_word(output, code_start + (*output).count as u16 + 5);
                        write_byte(output, JMP_ABS);
                        write_word(output, code_start + (*output).count as u16 + 5);
                        write_byte(output, JMP_IND);
                        write_word(output, 0);
                    },
                }
            },
            Op::Asm {args: _} => unreachable!(),
            Op::JmpIfNot{addr, arg} => {
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
                add_reloc(output, RelocationKind::AddressAbs{idx: *op_addresses.items.add(addr)}, asm);
            },
            Op::Jmp{addr} => {
                write_byte(output, JMP_ABS);
                add_reloc(output, RelocationKind::AddressAbs{idx: *op_addresses.items.add(addr)}, asm);
            },
        }
    }
    let addr_idx = *op_addresses.items.add(body.len());
    *(*asm).addresses.items.add(addr_idx) = (*output).count as u16; // update op address

    add_sp(output, stack_size);
    write_byte(output, RTS);
}

pub unsafe fn generate_funcs(output: *mut String_Builder, code_start: u16,
                             funcs: *const [Func], asm: *mut Assembler) {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].name_loc, code_start, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output, asm);
    }
}

pub unsafe fn apply_relocations(output: *mut String_Builder, code_start: u16,
                                data_start: u16, asm: *mut Assembler) {
    'reloc_loop: for i in 0..(*asm).relocs.count {
        let reloc = *(*asm).relocs.items.add(i);
        let taddr = reloc.addr;
        match reloc.kind {
            RelocationKind::DataOffset{off, low} => {
                if low {
                    write_byte_at(output, (data_start + off) as u8, taddr);
                } else {
                    write_byte_at(output, ((data_start + off) >> 8) as u8, taddr);
                }
            },
            RelocationKind::Label{name} => {
                for i in 0..(*asm).labels.count {
                    let label = *(*asm).labels.items.add(i);
                    if strcmp(label.name, name) == 0 {
                        write_word_at(output, code_start + label.addr, taddr);
                        continue 'reloc_loop;
                    }
                }
                printf(c!("linking failed. could not find label `%s'\n"), name);
                unreachable!();
            },
            RelocationKind::AddressRel{idx, add} => {
                let jaddr = *(*asm).addresses.items.add(idx);
                printf(c!("idx=%d,jaddr=%d\n"), idx as c_int, jaddr as c_int);
                let rel: i16 = jaddr as i16 - (taddr + add) as i16;
                assert!(rel < 128 && rel >= -128);
                write_byte_at(output, rel as u8, taddr);
            },
            RelocationKind::AddressAbs{idx} => {
                let saddr = *(*asm).addresses.items.add(idx) + code_start;
                write_word_at(output, saddr, taddr);
            },
        }
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    for i in 0..data.len() {
        write_byte(output, (*data)[i]);
    }
}

pub unsafe fn generate_entry(output: *mut String_Builder, asm: *mut Assembler) {
    write_byte(output, LDA_IMM);
    write_byte(output, 0x01); // stack page
    write_byte(output, STA_ZP);
    write_byte(output, ZP_STACK_1);

    write_byte(output, JSR);
    add_reloc(output, RelocationKind::Label{name: c!("main")}, asm);
    write_byte(output, JMP_IND);
    write_word(output, 0xFFFC);
}

pub const LOAD_OFFSET: u16 = 0xE000;

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    let mut asm: Assembler = zeroed();
    let code_start = (*output).count as u16 + LOAD_OFFSET;
    generate_entry(output, &mut asm);

    generate_funcs(output, code_start, da_slice((*c).funcs), &mut asm);

    let data_start = code_start + (*output).count as u16;
    generate_data_section(output, da_slice((*c).data));

    // generate_globals(output, da_slice((*c).globals), &mut assembler);
    apply_relocations(output, code_start, data_start, &mut asm);
}
