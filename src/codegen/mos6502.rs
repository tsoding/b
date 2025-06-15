// This uses 16-bit words, because addresses in 6502 are 16bits, so otherwise pointers would not work.
// To emulate 16-bit words using 8-bit registers, we use Y to hold the high byte and A to hold the low byte.

// As 6502 has a fixed stack at $0100-$01FF, we only have 255 bytes available. Machine code is loaded at $E000 by default, but can be reconfigured via LOAD_OFFSET=<offset> "linker flag".

// "Calling convention": first argument in Y:A, remaining args on the stack.

use core::ffi::*;
use core::mem::zeroed;
use core::ptr;
use crate::{Func, OpWithLocation, Global, Op, Compiler, Binop, Arg};
use crate::nob::*;
use crate::missingf;
use crate::crust::libc::*;
use crate::Loc;

const ADC_IMM:   u8 = 0x69;
const ADC_X:     u8 = 0x7D;
const ADC_ZP:    u8 = 0x65;
const BNE:       u8 = 0xD0;
const CLC:       u8 = 0x18;
const CMP_IMM:   u8 = 0xC9;
const CMP_ZP:    u8 = 0xC5;
const CPY_IMM:   u8 = 0xC0;
const CPY_ZP:    u8 = 0xC4;
const INX:       u8 = 0xE8;
const JMP_ABS:   u8 = 0x4C;
const JMP_IND:   u8 = 0x6C;
const JSR:       u8 = 0x20;
const LDA_IMM:   u8 = 0xA9;
const LDA_IND_X: u8 = 0xA1;
const LDA_IND_Y: u8 = 0xB1;
const LDA_X:     u8 = 0xBD;
const LDX_IMM:   u8 = 0xA2;
const LDY_IMM:   u8 = 0xA0;
const LDY_X:     u8 = 0xBC;
const PHA:       u8 = 0x48;
const PLA:       u8 = 0x68;
const RTS:       u8 = 0x60;
const STA_X:     u8 = 0x9D;
const STA_ZP:    u8 = 0x85;
const STY_ZP:    u8 = 0x84;
const TAX:       u8 = 0xAA;
const TAY:       u8 = 0xA8;
const TSX:       u8 = 0xBA;
const TXA:       u8 = 0x8A;
const TXS:       u8 = 0x9A;
const TYA:       u8 = 0x98;

// zero page addresses
// TODO: Do we really have to use
// zero page for indirect function calls
// or derefs?
const ZP_DEREF_0:     u8 = 0;
const ZP_DEREF_1:     u8 = 1;
const ZP_OP_TMP_0:    u8 = 3;
const ZP_OP_TMP_1:    u8 = 4;
const ZP_DEREF_FUN_0: u8 = 5;
const ZP_DEREF_FUN_1: u8 = 6;

const STACK_PAGE: u16 = 0x0100;

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
pub unsafe fn sub_sp(output: *mut String_Builder, bytes: u8, asm: *mut Assembler) {
    (*asm).frame_sz += bytes;
    // if bytes < 8 {
    for _ in 0 .. bytes {
        write_byte(output, PHA);
    }
    // } else {
    //     write_byte(output, TSX);
    //     write_byte(output, TXA);
    //     write_byte(output, CLC);
    //     write_byte(output, SBC_IMM);
    //     write_byte(output, bytes);
    //     write_byte(output, TAX);
    //     write_byte(output, TXS);
    // }
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

pub unsafe fn generate_function(name: *const c_char, name_loc: Loc, code_start: u16,
                                params_count: usize, auto_vars_count: usize,
                                body: *const [OpWithLocation], output: *mut String_Builder,
                                asm: *mut Assembler) {
    (*asm).frame_sz = 0;
    let fun_addr = (*output).count as u16;
    da_append(&mut (*asm).labels, Label {
        name,
        addr: fun_addr,
    });

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
        write_word(output, STACK_PAGE + stack_size as u16 + 2 + 2*i + 1);
        write_byte(output, STA_X);
        write_word(output, STACK_PAGE + stack_size as u16 - 2*i - 1);

        // high
        write_byte(output, LDA_X);
        write_word(output, STACK_PAGE + stack_size as u16 + 2 + 2*i + 2);
        write_byte(output, STA_X);
        write_word(output, STACK_PAGE + stack_size as u16 - 2*i);
    }

    for i in 0..body.len() {
        let addr_idx = *op_addresses.items.add(i);
        *(*asm).addresses.items.add(addr_idx) = (*output).count as u16; // update op address

        let op = (*body)[i];
        match op.opcode {
            Op::Bogus => unreachable!("bogus-amogus"),
            Op::Return {arg: _} => missingf!(name_loc, c!("implement Return\n\n")),
            Op::Store {index: _, arg: _} => missingf!(name_loc, c!("implement Store")),
            Op::ExternalAssign{name: _, arg: _} => missingf!(name_loc, c!("implement ExternalAssign\n")),
            Op::AutoAssign{index, arg} => {
                load_arg(arg, op.loc, output, asm);
                store_auto(output, index, asm);
            },
            Op::Negate {result: _, arg: _} => missingf!(name_loc, c!("implement Negate\n")),
            Op::UnaryNot{result: _, arg: _} => missingf!(name_loc, c!("implement UnaryNot\n")),
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => missingf!(name_loc, c!("implement BitOr\n")),
                    Binop::BitAnd => missingf!(name_loc, c!("implement BitAnd\n")),
                    Binop::BitShl => missingf!(name_loc, c!("implement BitShl\n")),
                    Binop::BitShr => missingf!(name_loc, c!("implement BitShr\n")),
                    Binop::Plus => {
                        load_arg(rhs, op.loc, output, asm);
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_OP_TMP_0);
                        write_byte(output, STY_ZP);
                        write_byte(output, ZP_OP_TMP_1);
                        load_arg(lhs, op.loc, output, asm);

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
                    Binop::Equal => {
                        load_arg(rhs, op.loc, output, asm);
                        write_byte(output, STA_ZP);
                        write_byte(output, ZP_OP_TMP_0);
                        write_byte(output, STY_ZP);
                        write_byte(output, ZP_OP_TMP_1);
                        load_arg(lhs, op.loc, output, asm);

                        write_byte(output, LDX_IMM);
                        write_byte(output, 0);

                        write_byte(output, CMP_ZP);
                        write_byte(output, ZP_OP_TMP_0);
                        write_byte(output, BNE);
                        write_byte(output, 5);

                        write_byte(output, CPY_ZP);
                        write_byte(output, ZP_OP_TMP_1);
                        write_byte(output, BNE);
                        write_byte(output, 1);

                        write_byte(output, INX);
                        write_byte(output, TXA);
                        write_byte(output, LDY_IMM);
                        write_byte(output, 0);
                    },
                    Binop::NotEqual => missingf!(name_loc, c!("implement NotEqual\n")),
                    Binop::GreaterEqual => missingf!(name_loc, c!("implement GreaterEqual\n")),
                    Binop::LessEqual => missingf!(name_loc, c!("implement LessEqual\n")),
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
                        add_reloc(output, RelocationKind::Label{name}, asm);
                    },
                    _ => { // arg already in (*)
                        // there is no jsr (indirect), so emulate using jsr and jmp (indirect).
                        write_byte(output, JSR);
                        write_word(output, code_start + (*output).count as u16 + 5);
                        write_byte(output, JMP_ABS);
                        write_word(output, code_start + (*output).count as u16 + 5);
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
            // Op::JmpIfNot{addr, arg} => {
            //     load_arg(arg, op.loc, output, asm);

            //     write_byte(output, CMP_IMM);
            //     write_byte(output, 0);

            //     // if !=0, skip next check and branch
            //     write_byte(output, BNE);
            //     write_byte(output, 7); // skip next 4 instructions

            //     write_byte(output, CPY_IMM);
            //     write_byte(output, 0);

            //     write_byte(output, BNE);
            //     write_byte(output, 3);

            //     write_byte(output, JMP_ABS);
            //     add_reloc(output, RelocationKind::AddressAbs{idx: *op_addresses.items.add(addr)}, asm);
            // },
            // Op::Jmp{addr} => {
            //     write_byte(output, JMP_ABS);
            //     add_reloc(output, RelocationKind::AddressAbs{idx: *op_addresses.items.add(addr)}, asm);
            // },
            Op::Label          {..} => missingf!(op.loc, c!("Label-style IR\n")),
            Op::JmpLabel       {..} => missingf!(op.loc, c!("Label-style IR\n")),
            Op::JmpIfNotLabel  {..} => missingf!(op.loc, c!("Label-style IR\n")),
        }
    }
    let addr_idx = *op_addresses.items.add(body.len());
    *(*asm).addresses.items.add(addr_idx) = (*output).count as u16; // update op address

    add_sp(output, stack_size, asm);
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
            da_append(&mut (*asm).labels, Label {
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

            // clear Y
            write_byte(output, LDY_IMM);
            write_byte(output, 0);
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
    add_reloc(output, RelocationKind::Label{name: c!("main")}, asm);
    write_byte(output, JMP_IND);
    write_word(output, 0xFFFC);
}

pub const DEFAULT_LOAD_OFFSET: u16 = 0xE000;

#[derive(Clone, Copy)]
pub struct Config {
    pub load_offset: u16,
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

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler, config: Config) -> Option<()> {
    let mut asm: Assembler = zeroed();
    generate_entry(output, &mut asm);

    generate_funcs(output, config.load_offset, da_slice((*c).funcs), &mut asm);
    generate_extrns(output, da_slice((*c).extrns), da_slice((*c).funcs), da_slice((*c).globals), &mut asm);

    let data_start = config.load_offset + (*output).count as u16;
    generate_data_section(output, da_slice((*c).data));

    apply_relocations(output, config.load_offset, data_start, &mut asm);
    Some(())
}
