// This uses 16-bit words, because addresses in 6502 are 16bits, so otherwise pointers would not work.
// To emulate 16-bit words using 8-bit registers, we use Y to hold the high byte and A to hold the low byte.

// As 6502 has a fixed stack at $0100-$01FF, we only have 255 bytes available. Machine code is loaded at $E000 by default, but can be reconfigured via LOAD_OFFSET=<offset> "linker flag".

// "Calling convention": first argument in Y:A, remaining args on the stack.

use core::ffi::*;
use core::mem::zeroed;
use core::ptr;
use crate::lexer::*;
use crate::ir::*;
use crate::nob::*;
use crate::diagf;
use crate::crust::libc::*;
use crate::runner::mos6502::{Config, DEFAULT_LOAD_OFFSET};
use crate::lexer::{is_identifier_start, is_identifier};
use crate::arena::{self, Arena};

// TODO: does this have to be a macro?
macro_rules! instr_enum {
    (enum $n:ident { $($instr:ident),* }) => {
        #[derive(Clone, Copy)]
        #[repr(u8)]
        pub enum $n {
            $($instr),*,
            COUNT
        }

        // TODO: maybe not search linearly, if this is too slow
        pub unsafe fn instr_from_string(s: *const c_char) -> Option<Instr> {
            $(
                let curr = c!(stringify!($instr));
                if (strcmp(s, curr) == 0) {
                    return Some($n::$instr);
                }
            )*
            return None;
        }
    }
}

instr_enum! {
    enum Instr {
        ADC,
        AND,
        ASL,
        BCC, BCS, BEQ, BIT,
        BMI,
        BNE, BPL, BRK, BVC,
        BVS,
        CLC, CLD, CLI, CLV,
        CMP, CPX, CPY,
        DEC, DEX, DEY,
        EOR,
        INC, INX, INY,
        JMP, JSR,
        LDA, LDX, LDY,
        LSR,
        NOP,
        ORA,
        PHA, PHP, PLA, PLP,
        ROL, ROR,
        RTI, RTS,
        SBC,
        SEC, SED, SEI,
        STA, STX, STY,
        TAX, TAY, TSX, TXA, TXS, TYA
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AddrMode {
    IMM = 0,
    ZP,
    ZP_X,
    ZP_Y,
    ABS,
    ABS_X,
    ABS_Y,
    IND_X,
    IND_Y,

    ACC,
    REL,
    IND,
    IMPL, // implied, no arg

    COUNT
}
use Instr::*;
use AddrMode::*;

// TODO: we currently use 0xFF for invalid opcode, because Some() and None
// make this table way too big/hard to read
const INVL: u8 = 0xFF;
const OPCODES: [[u8; AddrMode::COUNT as usize]; Instr::COUNT as usize] =
       [// IMM    ZP    ZP_X   ZP_Y,  ABS   ABS_X  ABS_Y  IND_X  IND_Y   ACC    REL   IND, IMPL
/*ADC*/[  0x69,  0x65,  0x75,  INVL, 0x6D,  0x7D,  0x79,  0x61,  0x71,  INVL,  INVL, INVL, INVL],
/*AND*/[  0x29,  0x25,  0x35,  INVL, 0x2D,  0x3D,  0x39,  0x21,  0x31,  INVL,  INVL, INVL, INVL],
/*ASL*/[  INVL,  0x06,  0x16,  INVL, 0x0E,  0x1E,  INVL,  INVL,  INVL,  0x0A,  INVL, INVL, INVL],
/*BCC*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  0x90, INVL, INVL],
/*BCS*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  0xB0, INVL, INVL],
/*BEQ*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  0xF0, INVL, INVL],
/*BIT*/[  INVL,  0x24,  INVL,  INVL, 0x2C,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*BMI*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  0x30, INVL, INVL],
/*BNE*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  0xD0, INVL, INVL],
/*BPL*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  0x10, INVL, INVL],
/*BRK*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x00],
/*BVC*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  0x50, INVL, INVL],
/*BVS*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  0x70, INVL, INVL],
/*CLC*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x18],
/*CLD*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xD8],
/*CLI*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x58],
/*CLV*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xB8],
/*CMP*/[  0xC9,  0xC5,  0xD5,  INVL, 0xCD,  0xDD,  0xD9,  0xC1,  0xD1,  INVL,  INVL, INVL, INVL],
/*CPX*/[  0xE0,  0xE4,  INVL,  INVL, 0xEC,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*CPY*/[  0xC0,  0xC4,  INVL,  INVL, 0xCC,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*DEC*/[  INVL,  0xC6,  0xD6,  INVL, 0xCE,  0xDE,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*DEX*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xCA],
/*DEY*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x88],
/*EOR*/[  0x49,  0x45,  0x55,  INVL, 0x4D,  0x5D,  0x59,  0x41,  0x51,  INVL,  INVL, INVL, INVL],
/*INC*/[  INVL,  0xE6,  0xF6,  INVL, 0xEE,  0xFE,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*INX*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xE8],
/*INY*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xC8],
/*JMP*/[  INVL,  INVL,  INVL,  INVL, 0x4C,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, 0x6C, INVL],
/*JSR*/[  INVL,  INVL,  INVL,  INVL, 0x20,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*LDA*/[  0xA9,  0xA5,  0xB5,  INVL, 0xAD,  0xBD,  0xB9,  0xA1,  0xB1,  INVL,  INVL, INVL, INVL],
/*LDX*/[  0xA2,  0xA6,  INVL,  0xB6, 0xAE,  INVL,  0xBE,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*LDY*/[  0xA0,  0xA4,  0xB4,  INVL, 0xAC,  0xBC,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*LSR*/[  INVL,  0x46,  0x56,  INVL, 0x4E,  0x5E,  INVL,  INVL,  INVL,  0x4A,  INVL, INVL, INVL],
/*NOP*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xEA],
/*ORA*/[  0x09,  0x05,  0x15,  INVL, 0x0D,  0x1D,  0x19,  0x01,  0x11,  INVL,  INVL, INVL, INVL],
/*PHA*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x48],
/*PHP*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x08],
/*PLA*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x68],
/*PLP*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x28],
/*ROL*/[  INVL,  0x26,  0x36,  INVL, 0x2E,  0x3E,  INVL,  INVL,  INVL,  0x2A,  INVL, INVL, INVL],
/*ROR*/[  INVL,  0x66,  0x76,  INVL, 0x6E,  0x7E,  INVL,  INVL,  INVL,  0x6A,  INVL, INVL, INVL],
/*RTI*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x40],
/*RTS*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x60],
/*SBC*/[  0xE9,  0xE5,  0xF5,  INVL, 0xED,  0xFD,  0xF9,  0xE1,  0xF1,  INVL,  INVL, INVL, INVL],
/*SEC*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x38],
/*SED*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xF8],
/*SEI*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x78],
/*STA*/[  INVL,  0x85,  0x95,  INVL, 0x8D,  0x9D,  0x99,  0x81,  0x91,  INVL,  INVL, INVL, INVL],
/*STX*/[  INVL,  0x86,  INVL,  0x96, 0x8E,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*STY*/[  INVL,  0x84,  0x94,  INVL, 0x8C,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, INVL],
/*TAX*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xAA],
/*TAY*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xA8],
/*TSX*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0xBA],
/*TXA*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x8A],
/*TXS*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x9A],
/*TYA*/[  INVL,  INVL,  INVL,  INVL, INVL,  INVL,  INVL,  INVL,  INVL,  INVL,  INVL, INVL, 0x98],
       ]// IMM    ZP    ZP_X   ZP_Y,  ABS   ABS_X  ABS_Y  IND_X  IND_Y   ACC    REL   IND, IMPL
    ;

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
const ZP_TMP_4:         u8 = 10;
const ZP_TMP_5:         u8 = 11;
const ZP_DEREF_FUN_0:   u8 = 12; // can't be the same as ZP_DEREF,
const ZP_DEREF_FUN_1:   u8 = 13; // as we use this before argument loading

const STACK_PAGE: u16 = 0x0100;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Byte {
    Low,
    High,
    Both
}

#[derive(Clone, Copy)]
pub enum RelocationKind {
    Address {
        idx: usize,
        relative: bool,
    }, // address from Assembler.addresses
    DataOffset {
        off: u16,
        byte: Byte,
    },
    External {
        name: *const c_char,
        offset: usize,
        byte: Byte,
        relative: bool,
    },
    Label {
        func_name: *const c_char,
        label: usize
    },
}
impl RelocationKind {
    pub fn is16(self) -> bool {
        match self {
            RelocationKind::DataOffset{byte, ..}  => byte == Byte::Both,
            RelocationKind::External{byte, relative, ..} => byte == Byte::Both && !relative,
            RelocationKind::Label{..}             => true,
            RelocationKind::Address{relative, ..} => !relative,
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
    pub func_name: *const c_char,
    pub label: usize,
    pub addr: u16,
}

#[derive(Clone, Copy)]
pub struct External {
    pub name: *const c_char,
    pub addr: u16,
    pub loc: Loc,
}

pub unsafe fn add_external(name: *const c_char, addr: u16, loc: Loc, asm: *mut Assembler) -> Option<()> {
    for i in 0..(*asm).externals.count {
        let ext = *(*asm).externals.items.add(i);
        if strcmp(ext.name, name) == 0 {
            diagf!(loc,     c!("ERROR: redefinition of name `%s`\n"), name);
            diagf!(ext.loc, c!("INFO: previously defined here\n"));
            return None;
        }
    }

    da_append(&mut (*asm).externals, External {
        name, addr, loc
    });

    Some(())
}

#[derive(Clone, Copy)]
pub struct Assembler {
    pub relocs: Array<Relocation>,
    pub op_labels: Array<Label>,
    pub externals: Array<External>,
    pub addresses: Array<u16>,
    pub code_start: u16, // load address of code section
    pub frame_sz: u8, // current stack frame size in bytes, because 6502 has no base register
    pub string_arena: Arena, // used for inline assembly labels
}

pub unsafe fn write_byte(out: *mut String_Builder, byte: u8) {
    da_append(out, byte as c_char);
}
pub unsafe fn write_word(out: *mut String_Builder, word: u16) {
    write_byte(out, word as u8);
    write_byte(out, (word >> 8) as u8);
}
pub unsafe fn write_byte_at(out: *mut String_Builder, byte: u8, addr: u16) {
    *((*out).items.add(addr as usize)) = byte as c_char;
}
pub unsafe fn write_word_at(out: *mut String_Builder, word: u16, addr: u16) {
    write_byte_at(out, word as u8, addr);
    write_byte_at(out, (word>>8) as u8, addr+1);
}

pub unsafe fn instr0(out: *mut String_Builder, inst: Instr, mode: AddrMode) {
    let opcode = OPCODES[inst as usize][mode as usize];
    if opcode == INVL {
        log(Log_Level::ERROR, c!("6502: Invalid combination of opcode and operand %u and %u"), inst as usize, mode as usize);
        abort();
    }
    write_byte(out, opcode);
}
// IMPL (implied) addressing mode
pub unsafe fn instr(out: *mut String_Builder, inst: Instr) {
    instr0(out, inst, IMPL);
}
pub unsafe fn instr8(out: *mut String_Builder, inst: Instr, mode: AddrMode, v: u8) {
    instr0(out, inst, mode);
    write_byte(out, v);
}
pub unsafe fn instr16(out: *mut String_Builder, inst: Instr, mode: AddrMode, v: u16) {
    instr0(out, inst, mode);
    write_word(out, v);
}

pub unsafe fn add_reloc(out: *mut String_Builder, kind: RelocationKind, asm: *mut Assembler) {
    da_append(&mut (*asm).relocs, Relocation {
        kind,
        addr: (*out).count as u16
    });
    if kind.is16() {
        write_word(out, 0);
    } else {
        write_byte(out, 0);
    }
}

pub unsafe fn create_address_label(asm: *mut Assembler) -> usize {
    let idx = (*asm).addresses.count;
    da_append(&mut (*asm).addresses, 0);
    idx
}
pub unsafe fn create_address_label_here(out: *const String_Builder, asm: *mut Assembler) -> usize {
    let label = create_address_label(asm);
    link_address_label_here(label, out, asm);
    label
}

// TODO: inform the caller, that `addr' is relative to code_start
pub unsafe fn link_address_label(label: usize, addr: u16, asm: *mut Assembler) {
    *(*asm).addresses.items.add(label) = addr;
}
pub unsafe fn link_address_label_here(label: usize, out: *const String_Builder, asm: *mut Assembler) {
    *(*asm).addresses.items.add(label) = (*out).count as u16;
}

pub unsafe fn load_auto_var(out: *mut String_Builder, index: usize, asm: *mut Assembler) {
    // save current stack pointer
    instr(out, TSX);
    // load low byte
    instr16(out, LDA, ABS_X, STACK_PAGE + (*asm).frame_sz as u16 - (index-1) as u16 * 2 - 1);
    // load high byte
    instr16(out, LDY, ABS_X, STACK_PAGE + (*asm).frame_sz as u16 - (index-1) as u16 * 2);
}
pub unsafe fn load_auto_var_ref(out: *mut String_Builder, index: usize, asm: *mut Assembler) {
    // save current stack pointer
    instr(out, TSX);
    instr(out, TXA);
    instr(out, CLC);
    instr8(out, ADC, IMM, (*asm).frame_sz as u8 - (index-1) as u8 * 2 - 1);
    instr8(out, LDY, IMM, (STACK_PAGE >> 8) as u8);
}

pub unsafe fn load_arg(arg: Arg, loc: Loc, out: *mut String_Builder, asm: *mut Assembler) {
    match arg {
        Arg::Deref(index) => {
            load_auto_var(out, index, asm);

            // load address to buffer in ZP to dereference, because registers
            // only 8 bits
            instr8(out, STA, ZP, ZP_DEREF_0);
            instr8(out, STY, ZP, ZP_DEREF_1);

            // Y = ((0),1)
            instr8(out, LDY, IMM, 1);
            instr8(out, LDA, IND_Y, ZP_DEREF_0);
            instr(out, TAY);

            // A = ((0,0))
            instr8(out, LDX, IMM, 0);
            instr8(out, LDA, IND_X, ZP_DEREF_0);
        },
        Arg::RefExternal(name) => {
            instr0(out, LDA, IMM);
            add_reloc(out, RelocationKind::External {name, offset: 0, byte: Byte::Low, relative: false}, asm);
            instr0(out, LDY, IMM);
            add_reloc(out, RelocationKind::External {name, offset: 0, byte: Byte::High, relative: false}, asm);
        },
        Arg::External(name) => {
            instr0(out, LDA, ABS);
            add_reloc(out, RelocationKind::External {name, offset: 0, byte: Byte::Both, relative: false}, asm);
            instr0(out, LDY, ABS);
            add_reloc(out, RelocationKind::External {name, offset: 1, byte: Byte::Both, relative: false}, asm);
        },
        Arg::AutoVar(index) => load_auto_var(out, index, asm),
        Arg::RefAutoVar(index) => load_auto_var_ref(out, index, asm),
        Arg::Literal(value) => {
            if value >= 65536 {
                diagf!(loc, c!("WARNING: contant $%llX out of range for 16 bits\n"), value);
            }
            instr8(out, LDA, IMM, value as u8);
            instr8(out, LDY, IMM, (value >> 8) as u8);
        },
        Arg::DataOffset(offset) => {
            assert!(offset < 65536, "data offset out of range");
            instr0(out, LDA, IMM);
            add_reloc(out, RelocationKind::DataOffset{off: offset as u16, byte: Byte::Low}, asm);
            instr0(out, LDY, IMM);
            add_reloc(out, RelocationKind::DataOffset{off: offset as u16, byte: Byte::High}, asm);
        },
        Arg::Bogus => unreachable!("bogus-amogus"),
    };
}

pub unsafe fn store_auto(out: *mut String_Builder, index: usize, asm: *mut Assembler) {
    // save current stack pointer
    instr(out, TSX);
    // save low byte
    instr16(out, STA, ABS_X, STACK_PAGE + (*asm).frame_sz as u16 - (index-1) as u16 * 2 - 1);

    // save high byte
    instr(out, TYA);
    instr16(out, STA, ABS_X, STACK_PAGE + (*asm).frame_sz as u16 - (index-1) as u16 * 2);
}

// TODO: can this be done better?
pub unsafe fn add_sp(out: *mut String_Builder, bytes: u8, asm: *mut Assembler) {
    (*asm).frame_sz -= bytes;
    if bytes < 8 {
        for _ in 0 .. bytes {
            instr(out, PLA);
        }
    } else {
        instr(out, TSX);
        instr(out, TXA);
        instr(out, CLC);
        instr8(out, ADC, IMM, bytes);
        instr(out, TAX);
        instr(out, TXS);
    }
}
// cannot modify Y:A here, as they hold first argument
// TODO: look, if this can be done without a loop, like in `add_sp` without modifying
// Y:A. Either save them temporarily or write the first arg to stack before decrementing
// SP
pub unsafe fn sub_sp(out: *mut String_Builder, bytes: u8, asm: *mut Assembler) {
    (*asm).frame_sz += bytes;
    for _ in 0 .. bytes {
        instr(out, PHA);
    }
}
pub unsafe fn push16(out: *mut String_Builder, asm: *mut Assembler) {
    (*asm).frame_sz += 2;

    instr(out, TAX);
    instr(out, TYA);
    // push high byte first
    instr(out, PHA);
    instr(out, TXA);
    // then low
    instr(out, PHA);
}
pub unsafe fn pop16_discard(out: *mut String_Builder, asm: *mut Assembler) {
    (*asm).frame_sz -= 2;

    instr(out, PLA);
    instr(out, PLA);
}

// load lhs in Y:A, rhs in RHS_L:RHS_H
pub unsafe fn load_two_args(out: *mut String_Builder, lhs: Arg, rhs: Arg, op: OpWithLocation, asm: *mut Assembler) {
    load_arg(rhs, op.loc, out, asm);
    instr8(out, STA, ZP, ZP_RHS_L);
    instr8(out, STY, ZP, ZP_RHS_H);
    load_arg(lhs, op.loc, out, asm);
}

#[derive(Clone, Copy)]
#[repr(C)]
pub enum Address {
    Literal(u16),
    Label(*const c_char),
}


pub unsafe fn parse_num(line_begin: *const c_char, mut line: *const c_char, mut loc: Loc) -> (u16, *const c_char) {
    while isspace(*line as i32) != 0 {line = line.add(1);}

    let (v, mut end) = match *line as u8 {
        b'$' => {
            let mut end = ptr::null_mut();
            let v = strtoull(line.add(1), &mut end, 16);
            (v, end as *const c_char)
        }
        b'0'..=b'9' => {
            let mut end = ptr::null_mut();
            let v = strtoull(line, &mut end, 10);
            (v, end as *const c_char)
        },
        c => {
            loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
            diagf!(loc, c!("ERROR: unexpected character `%c` in numberic literal\n"),
                   c as c_int);
            abort();
        }
    };
    if v > 0xFFFF {
        loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
        diagf!(loc, c!("ERROR: contant $%X out of range for 16 bits\n"), v);
        abort();
    }
    while isspace(*end as i32) != 0 {end = end.add(1);}
    (v as u16, end)
}

pub unsafe fn parse_addr_or_label(line_begin: *const c_char, mut line: *const c_char, loc: Loc,
                                  asm: *mut Assembler) -> (Address, *const c_char) {
    while isspace(*line as i32) != 0 {line = line.add(1);}

    let (v, mut end) = match *line as u8 {
        c if is_identifier_start(c as i8) => {
            let start = line;
            while is_identifier(*line as i8) {line = line.add(1);}
            let len = line as isize - start as isize;

            let label = arena::sprintf(&mut (*asm).string_arena, c!("%.*s"), len, start);
            (Address::Label(label), line)
        },
        _ => {
            let (v, line) = parse_num(line_begin, line, loc);
            (Address::Literal(v), line)
        }
    };
    while isspace(*end as i32) != 0 {end = end.add(1);}
    (v, end)
}

pub unsafe fn assemble_statement(out: *mut String_Builder,
                                 mut line: *const c_char, mut loc: Loc,
                                 asm: *mut Assembler) {

    let line_begin = line;
    // TODO: IMPORTANT! What we are doing in here is basically lexing.
    // Consider maybe reusing and adapting our B lexer in here?
    while isspace(*line as i32) != 0 {
        line = line.add(1);
    }

    let inst_start = line;
    while *line != 0 && isspace(*line as i32) == 0 {
        line = line.add(1);
    }
    let len = line as usize - inst_start as usize;
    let name = arena::sprintf(&mut (*asm).string_arena, c!("%.*s"), len, inst_start);

    if len > 0 && *name.add(len-1) as u8 == b':' {
        *name.add(len-1) = 0;
        let label_addr = (*out).count as u16;
        let mut lloc = loc;
        lloc.line_offset += (line as isize - line_begin as isize + 1) as i32;

        add_external(name, label_addr, lloc, asm);

        if *line != 0 {
            diagf!(lloc, c!("ERROR: trailing garbage after label: `%s`\n"), line);
            abort();
        }
        return;
    }

    for i in 0..len {
        *name.add(i) = toupper(*name.add(i) as i32) as i8;
    }
    let instr = match instr_from_string(name) {
        Some(v) => v,
        None => {
            loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
            diagf!(loc, c!("ERROR: invalid instruction mnemonic `%s`\n"), name);
            abort();
        }
    };

    while isspace(*line as i32) != 0 {line = line.add(1);}

    let operand = line;
    let mut arg8 = None;
    let mut arg16 = None;
    let mut arg_label = None;

    let mut mode = match *line as u8 {
        0    => IMPL,
        b'*' => {
            line = line.add(1);

            let rel = match *line as u8 {
                b'+' | b'-' => {
                    let mut end = ptr::null_mut();
                    let num = strtoull(line, &mut end, 10) as u16 as i16;
                    line = end;
                    num as i8
                },
                _ => 0
            };
            arg8 = Some((rel - 2) as u8);
            REL
        },
        b'#' => {
            line = line.add(1);

            let num;
            (num, line) = parse_num(line_begin, line, loc);
            if num > 0xFF {
                loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
                diagf!(loc, c!("ERROR: constant $%X out of range for 8 bit immediate\n"),
                       num as c_uint);
                abort();
            }
            arg8 = Some(num as u8);
            IMM
        },
        b'(' => {
            line = line.add(1);
            let addr;
            (addr, line) = parse_addr_or_label(line_begin, line, loc, asm);

            if *line as u8 == b',' {
                let num = match addr {
                    Address::Literal(l) => l,
                    Address::Label(_) => {
                        loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
                        diagf!(loc, c!("ERROR: cannot use 16-bit label address for X-inderect addressing\n"));
                        abort();
                    },
                };

                if num > 0xFF {
                    loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
                    diagf!(loc, c!("ERROR: constant $%X out of 8-bit range for indirect X\n"),
                           num as c_uint);
                    abort();
                }
                arg8 = Some(num as u8);

                line = line.add(1);
                while isspace(*line as i32) != 0 {line = line.add(1);}
                if toupper(*line as i32) as u8 != b'X' {
                    loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
                    diagf!(loc, c!("ERROR: X expected for indirect addressing mode\n"), *line as c_int);
                    abort();
                }
                line = line.add(1);
                while isspace(*line as i32) != 0 {line = line.add(1);}
                if toupper(*line as i32) as u8 != b')' {
                    loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
                    diagf!(loc, c!("ERROR: ) expected after X-indirect address\n"), *line as c_int);
                    abort();
                }
                line = line.add(1);
                IND_X
            } else {
                if *line as u8 != b')' {
                    loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
                    diagf!(loc, c!("ERROR: expected ',' or ')' after indirect address\n"), *line as c_int);
                    abort();
                }
                line = line.add(1);
                while isspace(*line as i32) != 0 {line = line.add(1);}

                if *line as u8 == b',' {
                    let num = match addr {
                        Address::Literal(l) => l,
                        Address::Label(_) => {
                            loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
                            diagf!(loc, c!("ERROR: cannot use 16-bit label address for Y-inderect addressing\n"));
                            abort();
                        },
                    };

                    if num > 0xFF {
                        loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
                        diagf!(loc, c!("ERROR: constant $%X out of 8-bit range for indirect Y\n"),
                               num as c_uint);
                        abort();
                    }
                    arg8 = Some(num as u8);

                    line = line.add(1);
                    while isspace(*line as i32) != 0 {line = line.add(1);}
                    if toupper(*line as i32) as u8 != b'Y' {
                        loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
                        diagf!(loc, c!("ERROR: Y expected for Y-indirect addressing mode\n"), *line as c_int);
                        abort();
                    }
                    line = line.add(1);
                    IND_Y
                } else {
                    match addr {
                        Address::Literal(l) => arg16 = Some(l),
                        Address::Label(s) => arg_label = Some(s),
                    }
                    IND
                }
            }
        },
        _  => {
            let addr;
            (addr, line) = parse_addr_or_label(line_begin, line, loc, asm);
            match addr {
                Address::Literal(l) => arg16 = Some(l),
                Address::Label(s) => arg_label = Some(s),
            }

            if *line as u8 == b',' {
                line = line.add(1);
                while isspace(*line as i32) != 0 {line = line.add(1);}

                if toupper(*line as i32) as u8 == b'X' {
                    line = line.add(1);
                    ABS_X
                } else if toupper(*line as i32) as u8 == b'Y' {
                    line = line.add(1);
                    ABS_Y
                } else {
                    ABS
                }
            } else {
                ABS
            }
        },
    };

    // prefer zeropage instructions, if they exist
    if let Some(v) = arg16 {
        if mode == ABS && v <= 0xFF && OPCODES[instr as usize][ZP as usize] != INVL {
            mode = ZP;
            arg8 = Some(v as u8);
            arg16 = None;
        } else if mode == ABS_X && v <= 0xFF && OPCODES[instr as usize][ZP_X as usize] != INVL {
            mode = ZP_X;
            arg8 = Some(v as u8);
            arg16 = None;
        } else if mode == ABS_Y && v <= 0xFF && OPCODES[instr as usize][ZP_Y as usize] != INVL {
            mode = ZP_Y;
            arg8 = Some(v as u8);
            arg16 = None;
        }
    }

    // labels for REL-only instructions should use REL
    if let Some(_) = arg_label {
        if mode == ABS && OPCODES[instr as usize][ABS as usize] == INVL &&
            OPCODES[instr as usize][REL as usize] != INVL {
            mode = REL;
        }
    }

    let opcode = OPCODES[instr as usize][mode as usize];
    if opcode == INVL {
        loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
        diagf!(loc, c!("ERROR: invalid combination of instruction `%s` and operand `%s`\n"),
               name, operand);
        abort();
    }

    write_byte(out, opcode);
    if let Some(a) = arg8 {
        write_byte(out, a);
    } else if let Some(a) = arg16 {
        write_word(out, a);
    } else if let Some(name) = arg_label {
        add_reloc(out, RelocationKind::External {name, offset: 0, byte: Byte::Both,
                                                 relative: mode == REL}, asm);
    }

    if *line != 0 {
        loc.line_offset += (line as isize - line_begin as isize + 1) as i32;
        diagf!(loc, c!("ERROR: trailing garbage: `%s`\n"), line);
        abort();
    }
}

// repetitve code for emulating 16bit instructions
// TODO: most of these could probably be converted
// to intrinsic functions
mod ops {
    use super::*;
    pub unsafe fn save_and_remove_signs(out: *mut String_Builder, asm: *mut Assembler) {
        let if0_end = create_address_label(asm);
        let if1_end = create_address_label(asm);
        // if (lhs < 0) {
        instr8(out, CPY, IMM, 0);
        instr0(out, BPL, REL);
        add_reloc(out, RelocationKind::Address{idx: if0_end, relative: true}, asm);

        // lhs = -lhs;
        instr8(out, LDA, IMM, 0);
        instr(out, SEC);
        instr8(out, SBC, ZP, ZP_TMP_0);
        instr8(out, STA, ZP, ZP_TMP_0);
        instr8(out, LDA, IMM, 0);
        instr8(out, SBC, ZP, ZP_TMP_1);
        instr8(out, STA, ZP, ZP_TMP_1);

        // tmp4 = 1;
        instr8(out, LDA, IMM, 1);
        instr8(out, STA, ZP, ZP_TMP_4);
        // }
        link_address_label_here(if0_end, out, asm);

        // if (rhs < 0) {
        instr8(out, CPY, IMM, 0);
        instr0(out, BPL, REL);
        add_reloc(out, RelocationKind::Address{idx: if1_end, relative: true}, asm);

        // lhs = -lhs;
        instr8(out, LDA, IMM,  0);
        instr(out, SEC);
        instr8(out, SBC, ZP, ZP_TMP_0);
        instr8(out, STA, ZP, ZP_TMP_0);
        instr8(out, LDA, IMM, 0);
        instr8(out, SBC, ZP, ZP_TMP_1);
        instr8(out, STA, ZP, ZP_TMP_1);

        // tmp4 ^= 1;
        instr8(out, LDA, ZP, ZP_TMP_4);
        instr8(out, EOR, IMM, 1);
        instr8(out, STA, ZP, ZP_TMP_4);
        // }
        link_address_label_here(if1_end, out, asm);
    }
}

pub unsafe fn generate_function(name: *const c_char, loc: Loc, params_count: usize, auto_vars_count: usize,
                                body: *const [OpWithLocation], out: *mut String_Builder,
                                asm: *mut Assembler) {
    (*asm).frame_sz = 0;
    let fun_addr = (*out).count as u16;
    add_external(name, fun_addr, loc, asm);

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
    sub_sp(out, stack_size, asm);

    for i in 0..(params_count as u16) {
        instr(out, TSX);
        if i == 0 {
            // low
            instr16(out, STA, ABS_X, STACK_PAGE + stack_size as u16 - 2*i - 1);

            // high
            instr(out, TYA);
            instr16(out, STA, ABS_X, STACK_PAGE + stack_size as u16 - 2*i);
            continue;
        }

        // low
        instr16(out, LDA, ABS_X, STACK_PAGE + stack_size as u16 + 2*i + 1);
        instr16(out, STA, ABS_X, STACK_PAGE + stack_size as u16 - 2*i - 1);

        // high
        instr16(out, LDA, ABS_X, STACK_PAGE + stack_size as u16 + 2*i + 2);
        instr16(out, STA, ABS_X, STACK_PAGE + stack_size as u16 - 2*i);
    }

    for i in 0..body.len() {
        let addr_idx = *op_addresses.items.add(i);
        *(*asm).addresses.items.add(addr_idx) = (*out).count as u16; // update op address

        let op = (*body)[i];
        match op.opcode {
            Op::Bogus => unreachable!("bogus-amogus"),
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg(arg, op.loc, out, asm);
                }

                // jump to ret statement
                instr0(out, JMP, ABS);
                add_reloc(out, RelocationKind::Address{idx: *op_addresses.items.add(body.len()),
                                                       relative: false}, asm);
            },
            Op::Store {index, arg} => {
                load_auto_var(out, index, asm);
                instr8(out, STA, ZP, ZP_DEREF_STORE_0);
                instr8(out, STY, ZP, ZP_DEREF_STORE_1);

                load_arg(arg, op.loc, out, asm);
                instr(out, TAX);
                instr(out, TYA);

                instr8(out, LDY, IMM, 1);
                instr8(out, STA, IND_Y, ZP_DEREF_STORE_0); // high
                instr(out, DEY);
                instr(out, TXA);
                instr8(out, STA, IND_Y, ZP_DEREF_STORE_0); // low
            },
            Op::ExternalAssign{name, arg} => {
                load_arg(arg, op.loc, out, asm);
                instr0(out, STA, ABS);
                add_reloc(out, RelocationKind::External {name, offset: 0, byte: Byte::Both, relative: false}, asm);
                instr0(out, STY, ABS);
                add_reloc(out, RelocationKind::External {name, offset: 1, byte: Byte::Both, relative: false}, asm);
            },
            Op::AutoAssign{index, arg} => {
                load_arg(arg, op.loc, out, asm);
                store_auto(out, index, asm);
            },
            Op::Negate {result, arg} => { // Y:A -> 0 - Y:A
                load_arg(arg, op.loc, out, asm);

                instr8(out, STA, ZP, ZP_TMP_0);
                instr8(out, STY, ZP, ZP_TMP_1);

                instr8(out, LDA, IMM, 0);
                instr(out, TAY);

                instr(out, SEC);
                instr8(out, SBC, ZP, ZP_TMP_0);
                instr(out, TAX);
                instr(out, TYA);
                instr8(out, SBC, ZP, ZP_TMP_1);
                instr(out, TAY);
                instr(out, TXA);

                store_auto(out, result, asm);
            },
            Op::UnaryNot{result, arg} => {
                load_arg(arg, op.loc, out, asm);

                instr8(out, LDX, IMM, 0);

                instr8(out, CMP, IMM, 0);
                instr8(out, BNE, REL, 5);

                instr(out, TYA);
                instr8(out, CMP, IMM, 0);
                instr8(out, BNE, REL, 1);

                instr(out, INX);

                instr(out, TXA);
                instr8(out, LDY, IMM, 0);

                store_auto(out, result, asm);
            },
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => {
                        load_two_args(out, lhs, rhs, op, asm);

                        instr8(out, ORA, ZP, ZP_RHS_L);
                        instr(out, TAX);
                        instr(out, TYA);
                        instr8(out, ORA, ZP, ZP_RHS_H);
                        instr(out, TAY);
                        instr(out, TXA);
                    },
                    Binop::BitAnd => {
                        load_two_args(out, lhs, rhs, op, asm);

                        instr8(out, AND, ZP, ZP_RHS_L);
                        instr(out, TAX);
                        instr(out, TYA);
                        instr8(out, AND, ZP, ZP_RHS_H);
                        instr(out, TAY);
                        instr(out, TXA);
                    },
                    Binop::BitShl => {
                        load_two_args(out, lhs, rhs, op, asm);

                        instr8(out, STA, ZP, ZP_TMP_0);
                        instr8(out, STY, ZP, ZP_TMP_1);

                        // as maximum shift is 16, Y can be ignored.
                        // TODO: only shift 16 times if value > 16 provided
                        // TODO: do we have to handle negative shifts?
                        instr8(out, LDX, ZP, ZP_RHS_L);

                        let loop_start = create_address_label_here(out, asm);
                        instr8(out, BEQ, REL, 8);

                        instr8(out, ASL, ZP, ZP_TMP_0);
                        instr8(out, ROL, ZP, ZP_TMP_1);

                        instr(out, DEX);
                        instr0(out, JMP, ABS);
                        add_reloc(out, RelocationKind::Address{idx: loop_start, relative: false}, asm);

                        instr8(out, LDA, ZP, ZP_TMP_0);
                        instr8(out, LDY, ZP, ZP_TMP_1);
                    },
                    Binop::BitShr => {
                        load_two_args(out, lhs, rhs, op, asm);

                        instr8(out, STA, ZP, ZP_TMP_0);
                        instr8(out, STY, ZP, ZP_TMP_1);

                        // as maximum shift is 16, Y can be ignored.
                        // TODO: only shift 16 times if value > 16 provided
                        // TODO: do we have to handle negative shifts?
                        instr8(out, LDX, ZP, ZP_RHS_L);

                        let loop_start = create_address_label_here(out, asm);
                        instr8(out, BEQ, REL, 8);

                        instr8(out, LSR, ZP, ZP_TMP_1);
                        instr8(out, ROR, ZP, ZP_TMP_0);

                        instr(out, DEX);
                        instr0(out, JMP, ABS);
                        add_reloc(out, RelocationKind::Address{idx: loop_start, relative: false}, asm);
                        instr8(out, LDA, ZP, ZP_TMP_0);
                        instr8(out, LDY, ZP, ZP_TMP_1);
                    },
                    Binop::Plus => {
                        load_two_args(out, lhs, rhs, op, asm);

                        instr(out, CLC);
                        instr8(out, ADC, ZP, ZP_RHS_L);
                        instr(out, TAX);
                        instr(out, TYA);
                        instr8(out, ADC, ZP, ZP_RHS_H);
                        instr(out, TAY);
                        instr(out, TXA);
                    },
                    Binop::Minus  => {
                        load_two_args(out, lhs, rhs, op, asm);

                        instr(out, SEC);
                        instr8(out, SBC, ZP, ZP_RHS_L);
                        instr(out, TAX);
                        instr(out, TYA);
                        instr8(out, SBC, ZP, ZP_RHS_H);
                        instr(out, TAY);
                        instr(out, TXA);
                    },
                    Binop::Mod => {
                        // !! TODO !! this should be implemented here and not as a B functions.
                        // TODO: current mod implementation is linear, we can do better.
                        load_arg(rhs, op.loc, out, asm);
                        push16(out, asm);
                        load_arg(lhs, op.loc, out, asm);

                        instr0(out, JSR, ABS);
                        add_reloc(out, RelocationKind::External{name: c!("_rem"), offset: 0,
                                                                   byte: Byte::Both, relative: false}, asm);
                        instr(out, TAX);
                        pop16_discard(out, asm);
                        instr(out, TXA);
                    },
                    Binop::Div => {
                        // !! TODO !! this should be implemented here and not as a B functions.
                        // TODO: current div implementation is linear, we can do better.
                        load_arg(rhs, op.loc, out, asm);
                        push16(out, asm);
                        load_arg(lhs, op.loc, out, asm);

                        instr0(out, JSR, ABS);
                        add_reloc(out, RelocationKind::External{name: c!("_div"), offset: 0,
                                                                   byte: Byte::Both, relative: false}, asm);
                        instr(out, TAX);
                        pop16_discard(out, asm);
                        instr(out, TXA);
                    },
                    Binop::Mult => {
                        load_two_args(out, lhs, rhs, op, asm);

                        // TODO: maybe move this to an intrinsic function,
                        // because it is rather long. Consider this, if we run
                        // out of memory at some point.

                        // shift-and-add/long multiplication
                        // see: https://en.wikipedia.org/wiki/Multiplication_algorithm

                        // store lhs
                        instr8(out, STA, ZP, ZP_TMP_0);
                        instr8(out, STY, ZP, ZP_TMP_1);

                        // shift 16 times
                        instr8(out, LDA, IMM, 16);
                        instr8(out, STA, ZP, ZP_TMP_5);

                        ops::save_and_remove_signs(out, asm);

                        // from here on: unsigned multiplication
                        // store Y:A in ZP, because shifting and adding is easier
                        // without all the register switching
                        instr8(out, LDA, IMM, 0);
                        instr8(out, STA, ZP, ZP_TMP_2);
                        instr8(out, STA, ZP, ZP_TMP_3);

                        let loop_start = create_address_label_here(out, asm);
                        let cont = create_address_label(asm);
                        let finished = create_address_label(asm);

                        // if shifted 16 times, we are finished
                        instr8(out, LDA, ZP, ZP_TMP_5);
                        instr0(out, BNE, REL);
                        add_reloc(out, RelocationKind::Address{idx: cont, relative: true}, asm);

                        instr0(out, JMP, ABS);
                        add_reloc(out, RelocationKind::Address{idx: finished, relative: false}, asm);

                        link_address_label_here(cont, out, asm);

                        instr8(out, DEC, ZP, ZP_TMP_5);

                        // shift left current accumulater between single adds
                        instr8(out, ASL, ZP, ZP_TMP_2);
                        instr8(out, ROL, ZP, ZP_TMP_3);

                        instr8(out, ASL, ZP, ZP_RHS_L);
                        instr8(out, ROL, ZP, ZP_RHS_H);

                        // if bit is 0, do not add anything
                        instr0(out, BCC, REL);
                        add_reloc(out, RelocationKind::Address{idx: loop_start, relative: true}, asm);

                        // bit is 1 here, we have to add entire lhs to acc
                        instr(out, CLC);
                        instr8(out, LDA, ZP, ZP_TMP_2); // acc, low
                        instr8(out, ADC, ZP, ZP_TMP_0); // lhs, low
                        instr8(out, STA, ZP, ZP_TMP_2); // acc, low

                        instr8(out, LDA, ZP, ZP_TMP_3); // acc, high
                        instr8(out, ADC, ZP, ZP_TMP_1); // lhs, high
                        instr8(out, STA, ZP, ZP_TMP_3); // acc, high

                        // continue loop
                        instr0(out, JMP, ABS);
                        add_reloc(out, RelocationKind::Address{idx: loop_start, relative: false}, asm);
                        link_address_label_here(finished, out, asm);

                        // move back in Y:A
                        instr8(out, LDA, ZP, ZP_TMP_2);
                        instr8(out, LDY, ZP, ZP_TMP_3);

                        instr8(out, LDX, ZP, ZP_TMP_4);
                        // if (negative == 1) {
                        instr8(out, BEQ, REL, 12);

                        instr8(out, LDA, IMM, 0);
                        instr(out, TAY);

                        // Y:A = -Y:A
                        instr(out, SEC);
                        instr8(out, SBC, ZP, ZP_TMP_2);
                        instr(out, TAX);
                        instr(out, TYA);
                        instr8(out, SBC, ZP, ZP_TMP_3);
                        instr(out, TXA);
                        instr(out, TAY);
                        // }

                        // missingf!(op.loc, c!("implement Mult\n"))
                    },

                    // TODO: use same less code everywhere without duplication
                    Binop::Less => {
                        load_two_args(out, lhs, rhs, op, asm);
                        // we subtract, then check sign

                        instr8(out, LDX, IMM, 1);

                        instr(out, SEC); // set carry
                        // sub low byte
                        instr8(out, SBC, ZP, ZP_RHS_L);
                        // sub high byte
                        instr(out, TYA);
                        instr8(out, SBC, ZP, ZP_RHS_H);
                        // high result in A, N flag if less.

                        // if less skip, we already have X=1
                        instr8(out, BMI, REL, 1);
                        instr(out, DEX);
                        instr(out, TXA);
                        // zero extend result
                        instr8(out, LDY, IMM, 0);
                    },
                    Binop::Greater => { // A > B <=> B < A
                        load_two_args(out, rhs, lhs, op, asm);
                        // we subtract, then check sign

                        instr8(out, LDX, IMM, 1);

                        instr(out, SEC); // set carry
                        // sub low byte
                        instr8(out, SBC, ZP, ZP_RHS_L);
                        // sub high byte
                        instr(out, TYA);
                        instr8(out, SBC, ZP, ZP_RHS_H);
                        // high result in A, N flag if less.

                        // if less skip, we already have X=1
                        instr8(out, BMI, REL, 1);

                        instr(out, DEX);
                        instr(out, TXA);
                        // zero extend result
                        instr8(out, LDY, IMM, 0);
                    },
                    Binop::Equal => {
                        load_two_args(out, lhs, rhs, op, asm);

                        instr8(out, LDX, IMM, 0);

                        instr8(out, CMP, ZP, ZP_RHS_L);
                        instr8(out, BNE, REL, 5);

                        instr8(out, CPY, ZP, ZP_RHS_H);
                        instr8(out, BNE, REL, 1);

                        instr(out, INX);
                        instr(out, TXA);
                        instr8(out, LDY, IMM, 0);
                    },
                    Binop::NotEqual => {
                        load_two_args(out, lhs, rhs, op, asm);

                        instr8(out, LDX, IMM, 1);

                        instr8(out, CMP, ZP, ZP_RHS_L);
                        instr8(out, BNE, REL, 5);

                        instr8(out, CPY, ZP, ZP_RHS_H);
                        instr8(out, BNE, REL, 1);

                        instr(out, DEX);
                        instr(out, TXA);
                        instr8(out, LDY, IMM, 0);
                    },
                    Binop::GreaterEqual => { // A >= B <=> !(A < B)
                        load_two_args(out, lhs, rhs, op, asm);
                        // we subtract, then check sign

                        instr8(out, LDX, IMM, 0);

                        instr(out, SEC); // set carry
                        // sub low byte
                        instr8(out, SBC, ZP, ZP_RHS_L);
                        // sub high byte
                        instr(out, TYA);
                        instr8(out, SBC, ZP, ZP_RHS_H);
                        // high result in A, N flag if less.

                        // if less skip, we already have X=0
                        instr8(out, BMI, REL, 1);
                        instr(out, INX);
                        instr(out, TXA);
                        // zero extend result
                        instr8(out, LDY, IMM, 0);
                    },
                    Binop::LessEqual => { // X <= Y <=> Y >= X <=> !(Y < X)
                        load_two_args(out, rhs, lhs, op, asm);
                        // we subtract, then check sign

                        instr8(out, LDX, IMM, 0);

                        instr(out, SEC); // set carry
                        // sub low byte
                        instr8(out, SBC, ZP, ZP_RHS_L);
                        // sub high byte
                        instr(out, TYA);
                        instr8(out, SBC, ZP, ZP_RHS_H);
                        // high result in A, N flag if less.

                        // if greater skip, we already have X=0
                        instr8(out, BMI, REL, 1);
                        instr(out, INX);

                        instr(out, TXA);
                        // zero extend result
                        instr8(out, LDY, IMM, 0);
                    },
                }
                store_auto(out, index, asm);
            },
            Op::Funcall{result, fun, args} => {
                match fun {
                    Arg::RefExternal(_) | Arg::External(_) | Arg::Literal(_) => {},
                    arg => {
                        load_arg(arg, op.loc, out, asm);
                        instr8(out, STA, ZP, ZP_DEREF_FUN_0);
                        instr8(out, STY, ZP, ZP_DEREF_FUN_1);
                    }
                }

                for i in (0..args.count).rev() {
                    load_arg(*args.items.add(i), op.loc, out, asm);
                    // first arg in Y:A to be compatible with wozmon routines
                    if i != 0 {
                        push16(out, asm);
                    }
                }
                match fun {
                    Arg::RefExternal(name) | Arg::External(name) => {
                        instr0(out, JSR, ABS);
                        add_reloc(out, RelocationKind::External{name, offset: 0, byte: Byte::Both, relative: false}, asm);
                    },
                    Arg::Literal(lit) => {
                        if lit >= 65536 {
                            diagf!(op.loc, c!("ERROR: function address $%X out of range for 16 bits\n"), lit);
                            abort();
                        }
                        instr16(out, JSR, ABS, lit as u16);
                    },
                    _ => { // function pointer already loaded in ZP_DEREF_FUN
                        // there is no jsr (indirect), so emulate using jsr and jmp (indirect).
                        instr16(out, JSR, ABS, (*asm).code_start + (*out).count as u16 + 6);
                        instr16(out, JMP, ABS, (*asm).code_start + (*out).count as u16 + 6);
                        instr16(out, JMP, IND, ZP_DEREF_FUN_0 as u16);
                    },
                }
                if args.count > 1 {
                    instr(out, TAX);
                    // clear stack
                    for i in 0 .. args.count {
                        if i == 0 {
                            continue;
                        }
                        pop16_discard(out, asm);
                    }
                    instr(out, TXA);
                }
                store_auto(out, result, asm);
            },
            Op::Asm {stmts} => {
                for i in 0..stmts.count {
                    let stmt = *stmts.items.add(i);
                    assemble_statement(out, stmt.line, stmt.loc, asm);
                }
            },
            Op::Label{label} => {
                // RE: https://github.com/tsoding/b/pull/147#issue-3154667157
                // > For this thing I introduces a new NOP instruction because it would be a bit too
                // > risky to just blindly jump on an address that could possibly be unused.
                //
                // We now, this label will always be followed by an instruction: either the next
                // generated OP, or the return code at the end of a function. No NOP needed.
                da_append(&mut (*asm).op_labels, Label {
                    func_name: name,
                    label,
                    addr: (*out).count as u16,
                });
            },
            Op::JmpLabel{label} => {
                instr0(out, JMP, ABS);
                add_reloc(out, RelocationKind::Label{func_name: name, label}, asm);
            },
            Op::JmpIfNotLabel{label, arg} => {
                load_arg(arg, op.loc, out, asm);

                instr8(out, CMP, IMM, 0);

                // if !=0, skip next check and branch
                instr8(out, BNE, REL, 7); // skip next 4 instructions
                instr8(out, CPY, IMM, 0);
                instr8(out, BNE, REL, 3);

                instr0(out, JMP, ABS);
                add_reloc(out, RelocationKind::Label{func_name: name, label}, asm);
            },
            Op::Index {result, arg, offset} => {
                load_two_args(out, arg, offset, op, asm);

                // shift offset to the left by one bit
                instr8(out, ASL, ZP, ZP_RHS_L);
                instr8(out, ROL, ZP, ZP_RHS_H);

                // add offset and arg
                instr(out, CLC);
                instr8(out, ADC, ZP, ZP_RHS_L);
                instr(out, TAX);
                instr(out, TYA);
                instr8(out, ADC, ZP, ZP_RHS_H);
                instr(out, TAY);
                instr(out, TXA);

                store_auto(out, result, asm);
            },
        }
    }

    instr8(out, LDA, IMM, 0);
    instr(out, TAY);

    let addr_idx = *op_addresses.items.add(body.len());
    *(*asm).addresses.items.add(addr_idx) = (*out).count as u16;

    if stack_size > 0 {
        // seriously... we don't have enough registers to save A to...
        instr8(out, STA, ZP, ZP_TMP_0);
        add_sp(out, stack_size, asm);
        instr8(out, LDA, ZP, ZP_TMP_0);
    }
    instr(out, RTS);
}

pub unsafe fn generate_funcs(out: *mut String_Builder, funcs: *const [Func], asm: *mut Assembler) {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].name_loc, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), out, asm);
    }
}

pub unsafe fn apply_relocations(out: *mut String_Builder, data_start: u16, asm: *mut Assembler) {
    'reloc_loop: for i in 0..(*asm).relocs.count {
        let reloc = *(*asm).relocs.items.add(i);
        let caddr = reloc.addr;
        match reloc.kind {
            RelocationKind::DataOffset{off, byte} => {
                let faddr = data_start + off;
                match byte {
                    Byte::Low  => write_byte_at(out, faddr as u8, caddr),
                    Byte::High => write_byte_at(out, (faddr >> 8) as u8, caddr),
                    Byte::Both => write_word_at(out, faddr, caddr),
                }
            },
            RelocationKind::Label{func_name: name, label} => {
                for i in 0..(*asm).op_labels.count {
                    let op_label = *(*asm).op_labels.items.add(i);
                    if strcmp(op_label.func_name, name) == 0 && op_label.label == label {
                        write_word_at(out, (*asm).code_start + op_label.addr, caddr);
                        continue 'reloc_loop;
                    }
                }
                log(Log_Level::ERROR, c!("6502: Linking failed. Could not find label `%s.%u'"), name, label);
                unreachable!();
            },
            RelocationKind::External{name, offset, byte, relative} => {
                for i in 0..(*asm).externals.count {
                    let label = *(*asm).externals.items.add(i);
                    if strcmp(label.name, name) == 0 {
                        let faddr = (*asm).code_start + label.addr + offset as u16;
                        if relative {
                            let rel = (faddr as i64) - ((caddr + 1) as i64);
                            write_byte_at(out, rel as i8 as u8, caddr);
                        } else {
                            match byte {
                                Byte::Low  => write_byte_at(out, faddr as u8, caddr),
                                Byte::High => write_byte_at(out, (faddr >> 8) as u8, caddr),
                                Byte::Both => write_word_at(out, faddr, caddr)
                            }
                        }
                        continue 'reloc_loop;
                    }
                }
                log(Log_Level::ERROR, c!("6502: Linking failed. Could not find extrn `%s'"), name);
                unreachable!();
            },
            RelocationKind::Address{idx, relative: true} => {
                let jaddr = *(*asm).addresses.items.add(idx);
                let rel: i16 = jaddr as i16 - (caddr + 1) as i16;
                assert!(rel < 128 && rel >= -128);
                write_byte_at(out, rel as u8, caddr);
            },
            RelocationKind::Address{idx, relative: false} => {
                let saddr = *(*asm).addresses.items.add(idx) + (*asm).code_start;
                write_word_at(out, saddr, caddr);
            },
        }
    }
}

pub unsafe fn generate_extrns(_out: *mut String_Builder, extrns: *const [*const c_char],
                              funcs: *const [Func], globals: *const [Global],
                              asm_funcs: *const [AsmFunc], _asm: *mut Assembler) {
    'skip_function_or_global: for i in 0..extrns.len() {
        // assemble a few "stdlib" functions which can't be programmed in B
        let name = (*extrns)[i];
        for j in 0..funcs.len() {
            let func = (*funcs)[j].name;
            if strcmp(func, name) == 0 {
                continue 'skip_function_or_global
            }
        }
        for j in 0..globals.len() {
            let global = (*globals)[j].name;
            if strcmp(global, name) == 0 {
                continue 'skip_function_or_global
            }
        }
        for j in 0..asm_funcs.len() {
            let func = (*asm_funcs)[j].name;
            if strcmp(func, name) == 0 {
                continue 'skip_function_or_global
            }
        }

        log(Log_Level::ERROR, c!("6502: Unknown extrn: `%s`, can not link"), name);
        abort();
    }
}

pub unsafe fn generate_globals(out: *mut String_Builder, globals: *mut [Global], asm: *mut Assembler) {
    for i in 0..globals.len() {
        let global = (*globals)[i];
        add_external(global.name, (*out).count as u16, global.name_loc, asm);

        if global.is_vec {
            let address = create_address_label(asm);
            add_reloc(out, RelocationKind::Address{idx: address, relative: false}, asm);
            link_address_label_here(address, out, asm);
        }
        for j in 0..global.values.count {
            match *global.values.items.add(j) {
                ImmediateValue::Literal(lit) => write_word(out, lit as u16),
                ImmediateValue::Name(name) =>
                    add_reloc(out, RelocationKind::External{name, byte: Byte::Both, offset: 0, relative: false}, asm),
                ImmediateValue::DataOffset(offset) => {
                    add_reloc(out, RelocationKind::DataOffset{off: offset as u16, byte: Byte::Both}, asm);
                }
            }
        }

        for _ in global.values.count..global.minimum_size {
            write_word(out, 0);
        }
    }
}

pub unsafe fn generate_data_section(out: *mut String_Builder, data: *const [u8]) {
    for i in 0..data.len() {
        write_byte(out, (*data)[i]);
    }
}

pub unsafe fn generate_entry(out: *mut String_Builder, asm: *mut Assembler) {
    instr0(out, JSR, ABS);
    add_reloc(out, RelocationKind::External{name: c!("main"), offset: 0, byte: Byte::Both, relative: false}, asm);

    instr16(out, JMP, IND, 0xFFFC);
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
            log(Log_Level::ERROR, c!("6502: Unknown linker flag: %s"), flag);
            return None
        }
    }

    Some(config)
}

pub unsafe fn generate_asm_funcs(out: *mut String_Builder, asm_funcs: *const [AsmFunc],
                                 asm: *mut Assembler) {
    for i in 0..asm_funcs.len() {
        let asm_func = (*asm_funcs)[i];

        let fun_addr = (*out).count as u16;
        add_external(asm_func.name, fun_addr, asm_func.name_loc, asm);

        for j in 0..asm_func.body.count {
            let stmt = *asm_func.body.items.add(j);
            assemble_statement(out, stmt.line, stmt.loc, asm);
        }
    }
}

pub unsafe fn generate_program(out: *mut String_Builder, p: *const Program, config: Config) -> Option<()> {
    let mut asm: Assembler = zeroed();
    generate_entry(out, &mut asm);
    asm.code_start = config.load_offset;

    generate_funcs(out, da_slice((*p).funcs), &mut asm);
    generate_asm_funcs(out, da_slice((*p).asm_funcs), &mut asm);
    generate_extrns(out, da_slice((*p).extrns), da_slice((*p).funcs), da_slice((*p).globals), da_slice((*p).asm_funcs), &mut asm);

    let data_start = config.load_offset + (*out).count as u16;
    generate_data_section(out, da_slice((*p).data));
    generate_globals(out, da_slice((*p).globals), &mut asm);

    log(Log_Level::INFO, c!("Generated size: 0x%x"), (*out).count as c_uint);
    apply_relocations(out, data_start, &mut asm);
    arena::reset(&mut asm.string_arena);

    Some(())
}
