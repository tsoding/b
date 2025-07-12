use core::ffi::*;
use core::cmp;
use crate::nob::*;
use crate::crust::libc::*;
use core::mem::zeroed;
use core::ptr;
use crate::{Compiler, Binop, Op, OpWithLocation, Arg, Func, Global, AsmFunc, ImmediateValue, AsmStmt};
use crate::{Loc};

pub static addin_offset: u32 = 0x300000;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FUnresolved {
    // An offset _within_ the function
    pub offset: u32,
    pub symbol: String_Builder
}
#[repr(C)]
#[derive(Clone, Copy)]
pub struct FResolved {
    pub symbol: String_Builder,
    pub val: u32,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct GAssembler {
    pub output: *mut String_Builder,
    pub functions: Array<FAssembler>,

    pub symbols: Array<FResolved>
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FAssembler {
    pub base_address: u32,

    pub global_asm: *mut GAssembler,
    pub unresolved_symbols: Array<FUnresolved>,

    pub funcname: *const c_char,
    pub output: String_Builder,
    pub next_trampoline : usize,
    pub next_litteral : usize,

    pub next_jmppoint : usize
}

pub unsafe fn require_symbol(asm: *mut FAssembler, symbol: *const c_char) {
    let mut symb: FUnresolved = FUnresolved {
        offset: (*asm).output.count as u32,
        symbol: zeroed()
    };
    sb_appendf(&mut symb.symbol, c!("%s"), symbol);
    da_append(&mut (*asm).unresolved_symbols, symb);
    asm_write32(asm, 0xFFFF_FFFF);
}

pub unsafe fn align(s: *mut String_Builder, size: usize) {
    while (*s).count % size != 0 { da_append(s, 0 as c_char); }
}
pub unsafe fn asm_write32(asm: *mut FAssembler, val: u32) {
    align(&mut (*asm).output, 4);
    da_append(&mut (*asm).output, ((val >> (8 * 3)) & 0xFF) as u8 as c_char);
    da_append(&mut (*asm).output, ((val >> (8 * 2)) & 0xFF) as u8 as c_char);
    da_append(&mut (*asm).output, ((val >> (8 * 1)) & 0xFF) as u8 as c_char);
    da_append(&mut (*asm).output, ((val >> (8 * 0)) & 0xFF) as u8 as c_char);
}

macro_rules! instruction_set_matcher {
    ( $($instr:ident: $mask:literal,$prefix:expr,($($type:ty : $name:ident : $shl:literal),*);)* ) => {

        // TODO
        #[derive(Clone,Copy)]
        #[repr(C)]
        pub enum Instruction {
            $($instr { $($name: $type),* }),*,
        }

        pub unsafe fn write_instruction(instr: Instruction) -> u16 {
            return match instr {
                // TODO
                $(
                    Instruction::$instr { $($name),* } => {
                        ($mask as u16) | $((($name) as u16).wrapping_shl($shl))|*
                    }
                ),*
            }
        }

        pub unsafe fn try_matchpcrel(input: *const c_char) -> Option<(PCRel8,*const c_char)> {
            let mut cursor: *const c_char   = input;

            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }
            if *cursor != ('@' as c_char) { return None; }
            cursor = cursor.add(1);
            if *cursor != ('(' as c_char) { return None; }
            cursor = cursor.add(1);
            let _ = *cursor;                // Shut up, Rust!
            
            if let Some((off,ncursor)) = try_matchu8(cursor) {
                cursor = ncursor;

                while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }
                if *cursor != (',' as c_char) {
                    return None;
                }
                cursor = cursor.add(1);
                while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

                if tolower(*cursor as i32) != ('p' as i32) { return None; }
                cursor = cursor.add(1);
                if tolower(*cursor as i32) != ('c' as i32) { return None; }
                cursor = cursor.add(1);
                if *cursor != (')' as c_char) { return None; }
                cursor = cursor.add(1);
                return Some((off as PCRel8, cursor));
            } else { return None; }
        }
        pub unsafe fn try_matchmreg(input: *const c_char) -> Option<(MRegister,*const c_char)> {
            let mut cursor: *const c_char   = input;
            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            if *cursor != ('@' as c_char) { return None; }
            cursor = cursor.add(1);
            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            if let Some((reg,ncursor)) = try_matchreg(cursor) {
                return Some((reg as MRegister, ncursor));
            } else { return None; }
        }
        pub unsafe fn try_matchsreg(input: *const c_char) -> Option<(SRegister,*const c_char)> {
            let mut cursor: *const c_char   = input;
            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            if *cursor != ('@' as c_char) { return None; }
            cursor = cursor.add(1);
            if *cursor != ('-' as c_char) { return None; }
            cursor = cursor.add(1);
            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            if let Some((reg,ncursor)) = try_matchreg(cursor) {
                return Some((reg as SRegister, ncursor));
            } else { return None; }
        }
        pub unsafe fn try_matchregi(input: *const c_char) -> Option<(RegisterI,*const c_char)> {
            let mut cursor: *const c_char   = input;
            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            if *cursor != ('@' as c_char) { return None; }
            cursor = cursor.add(1);
            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            if let Some((reg,ncursor)) = try_matchreg(cursor) {
                cursor = ncursor;
                if *cursor != ('+' as c_char) { return None; }
                cursor = cursor.add(1);
                return Some((reg as RegisterI, cursor));
            } else { return None; }
        }
        pub unsafe fn try_matchkeyword(input: *const c_char, key: *const c_char) -> Option<*const c_char> {
            let mut cursor: *const c_char   = input;
            let kwlen = strlen(key);
            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            for i in 0..kwlen {
                let cursorchar = tolower(*cursor as i32) as c_char;
                let kwchar     = *key.add(i);

                if cursorchar == 0 { return None; }
                if kwchar != cursorchar { return None; }
                cursor = cursor.add(1);
            }
            return Some(cursor);
        }
        pub unsafe fn try_matchreg(input: *const c_char) -> Option<(Register,*const c_char)> {
            let mut cursor: *const c_char   = input;
            let mut end: *mut c_char        = ptr::null_mut();

            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            if *cursor != ('r' as c_char) { return None; }
            let v = strtoull(cursor.add(1), &mut end, 10);
            if v >= 16 { return None; }

            return Some((v as Register, end))
        }
        pub unsafe fn try_matchu8(input: *const c_char) -> Option<(u8,*const c_char)> {
            let mut cursor: *const c_char  = input;
            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            // We shall assume decimal by default, with 
            //  - b[NUM] being binary
            //  - x[NUM] being hexadecimal
            return match *cursor as u8 {
                b'x' => {
                    let mut end = ptr::null_mut();
                    let v = strtoull(cursor.add(1), &mut end, 16);

                    Some((v as u8,end))
                }
                b'b' => {
                    let mut end = ptr::null_mut();
                    let v = strtoull(cursor.add(1), &mut end, 2);

                    Some((v as u8,end))
                }
                b'0'..=b'9' => {
                    let mut end = ptr::null_mut();
                    let v = strtoull(cursor, &mut end, 10);

                    Some((v as u8,end))
                }
                _ => None
            };
        }
        pub unsafe fn try_matchs8(input: *const c_char) -> Option<(u8,*const c_char)> {
            let mut cursor: *const c_char  = input;
            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

            // We shall assume decimal by default, with 
            //  - b[NUM] being binary
            //  - x[NUM] being hexadecimal
            //  - -[NUM] being negative decimal
            return match *cursor as u8 {
                b'x' => {
                    let mut end = ptr::null_mut();
                    let v = strtoull(cursor.add(1), &mut end, 16);

                    Some((v as u8,end))
                }
                b'b' => {
                    let mut end = ptr::null_mut();
                    let v = strtoull(cursor.add(1), &mut end, 2);

                    Some((v as u8,end))
                }
                b'-' => {
                    let mut end = ptr::null_mut();
                    let v = -((strtoull(cursor.add(1), &mut end, 10)) as isize);

                    Some((v as u8,end))
                }
                b'0'..=b'9' => {
                    let mut end = ptr::null_mut();
                    let v = strtoull(cursor, &mut end, 10);

                    Some((v as u8,end))
                }
                _ => None
            };
        }
        pub unsafe fn try_matching(input: *const c_char, _loc: Loc) -> Option<Instruction> {
            let mut prefix: String_Builder = zeroed();
            let mut cursor: *const c_char  = input;
            while isspace(*cursor as i32) == 0 && *cursor != 0 {
                da_append(&mut prefix, tolower(*cursor as i32) as i8);
                cursor = cursor.add(1);
            }
            let prev_cursor = cursor;
            $(
                // the prefix may be the same for two or more instructions
                if strcmp(prefix.items, c!($prefix)) == 0 {
                    // Skip pointless whitespace
                    let mut i = 0;
                    $( let mut $name: $type = zeroed();)*
                    while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }

                    // Now, get parsin'
                    let mut parsed: bool = true;
                    cursor = prev_cursor;
                    $(
                        if i != 0 {
                            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }
                            if *cursor != (',' as c_char) {
                                parsed = false;
                            }
                            cursor = cursor.add(1);
                            while isspace(*cursor as i32) != 0 && *cursor != 0 { cursor = cursor.add(1); }
                        }
                        i += 1;
                        let _ = i;

                        // Fun hack here (we stringify and check instead of doing the reasonable
                        // thing.
                        if strcmp(c!(stringify!($type)), c!("u8")) == 0 {
                            // Unsigned 8-bit number
                            if let Some((v,ncursor)) = try_matchu8(cursor) {
                                $name = v as $type;
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else if strcmp(c!(stringify!($type)), c!("Sext8")) == 0 {
                            // Signed 8-bit number
                            if let Some((v,ncursor)) = try_matchs8(cursor) {
                                $name = v as $type;
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else if strcmp(c!(stringify!($type)), c!("Register")) == 0 {
                            // Register (r0-r15)
                            if let Some((v,ncursor)) = try_matchreg(cursor) {
                                $name = v as $type;
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else if strcmp(c!(stringify!($type)), c!("PCRel8")) == 0 {
                            // 8-bit PCRel
                            if let Some((v,ncursor)) = try_matchpcrel(cursor) {
                                $name = v as $type;
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else if strcmp(c!(stringify!($type)), c!("MRegister")) == 0 {
                            // @Rn
                            if let Some((v,ncursor)) = try_matchmreg(cursor) {
                                $name = v as $type;
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else if strcmp(c!(stringify!($type)), c!("SRegister")) == 0 {
                            // @Rn
                            if let Some((v,ncursor)) = try_matchsreg(cursor) {
                                $name = v as $type;
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else if strcmp(c!(stringify!($type)), c!("RegisterI")) == 0 {
                            // @Rn
                            if let Some((v,ncursor)) = try_matchsreg(cursor) {
                                $name = v as $type;
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else if strcmp(c!(stringify!($type)), c!("Dummy")) == 0 {
                            // Don't except anything (Dummy is always used when there is no
                            // arguments)
                        } else if strcmp(c!(stringify!($type)), c!("PR")) == 0 {
                            if let Some(ncursor) = try_matchkeyword(cursor, c!("pr")) {
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else if strcmp(c!(stringify!($type)), c!("MACH")) == 0 {
                            if let Some(ncursor) = try_matchkeyword(cursor, c!("mach")) {
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else if strcmp(c!(stringify!($type)), c!("MACL")) == 0 {
                            if let Some(ncursor) = try_matchkeyword(cursor, c!("macl")) {
                                cursor = ncursor;
                                let _ = *cursor;            // Shut up, Rust!
                            } else { parsed = false; }
                        } else {
                            // ????????????????????
                        }
                    )*
                    if parsed {
                        return Some(Instruction::$instr{$($name: $name),*});
                    } 
                }
            )*
            None
        }
        pub unsafe fn assemble_instruction(asm: *mut FAssembler, instr: Instruction) {
            let i: u16 = write_instruction(instr);
            da_append(&mut (*asm).output, ((i >> 8) & 0xFF) as c_char);
            da_append(&mut (*asm).output, ((i >> 0) & 0xFF) as c_char);
        }
    }
}

// Pseudo-types used by try_matching for specialisation purposes
pub type Register   = u8;           // rN
pub type Sext8      = u8;           // (sext) #imm8

// memory addressing
pub type PCRel8     = u8;           // @(disp,PC)
pub type MRegister  = u8;           // @rN
pub type SRegister  = u8;           // @-rN
pub type RegisterI  = u8;           // @rN+

// "fake" types, which don't correspond to anything
pub type Dummy      = u8;           // no-args
pub type PR         = Dummy;        // "PR" pseudo-register
pub type MACH       = Dummy;        // "MACH" pseudo-register
pub type MACL       = Dummy;        // "MACL" pseudo-register
instruction_set_matcher! {
    // name             :  OR mask              prefix    arguments...... 
    MoveRegToReg        : 0b0110_0000_0000_0011, "mov",     (Register:m:4, Register:n:8);
    MoveImmToReg        : 0b1110_0000_0000_0000, "mov",     (Sext8:imm8:0, Register:n:8);

    // mov[a|SZ] PCRel,rN
    MovaPCRelToR0       : 0b1100_0111_0000_0000, "mova",    (PCRel8:disp:0);
    MovwPCRelToReg      : 0b1001_0000_0000_0000, "mov.w",   (PCRel8:disp:0, Register:n:8);
    MovlPCRelToReg      : 0b1101_0000_0000_0000, "mov.l",   (PCRel8:disp:0, Register:n:8);

    // mov[SZ] @Rm, Rn
    MovbMRegToReg       : 0b0110_0000_0000_0000, "mov.b",   (MRegister:m:4, Register:n:8);
    MovwMRegToReg       : 0b0110_0000_0000_0001, "mov.w",   (MRegister:m:4, Register:n:8);
    MovlMRegToReg       : 0b0110_0000_0000_0010, "mov.l",   (MRegister:m:4, Register:n:8);
    // mov[SZ] Rm, @Rn
    MovbRegToMReg       : 0b0010_0000_0000_0000, "mov.b",   (Register:m:4, MRegister:n:8);
    MovwRegToMReg       : 0b0010_0000_0000_0001, "mov.w",   (Register:m:4, MRegister:n:8);
    MovlRegToMReg       : 0b0010_0000_0000_0010, "mov.l",   (Register:m:4, MRegister:n:8);

    // mov[SZ] @Rm+, Rn
    MovbRegIToReg       : 0b0110_0000_0000_0100, "mov.b",   (RegisterI:m:4, Register:n:8);
    MovwRegIToReg       : 0b0110_0000_0000_0101, "mov.w",   (RegisterI:m:4, Register:n:8);
    MovlRegIToReg       : 0b0110_0000_0000_0110, "mov.l",   (RegisterI:m:4, Register:n:8);
    // mov[SZ] Rm, @-Rn
    MovbRegToSReg       : 0b0010_0000_0000_0100, "mov.b",   (Register:m:4, SRegister:n:8);
    MovwRegToSReg       : 0b0010_0000_0000_0101, "mov.w",   (Register:m:4, SRegister:n:8);
    MovlRegToSReg       : 0b0010_0000_0000_0110, "mov.l",   (Register:m:4, SRegister:n:8);

    // mov.[b|w] @(disp,Rm), r0
    MovbDispToR0        : 0b1000_0100_0000_0000, "mov.b",   (Register:m:4, u8:disp:0);
    MovwDispToR0        : 0b1000_0101_0000_0000, "mov.w",   (Register:m:4, u8:disp:0);
    // mov.l @(disp,Rm), Rn
    MovlDispToReg       : 0b0101_0000_0000_0000, "mov.l",   (u8:disp:0, Register:m:4, Register:n:8);

    // mov.[b|w] R0, @(disp,Rn)
    MovbR0ToDisp        : 0b1000_0000_0000_0000, "mov.b",   (u8:disp:4, Register:n:0);
    MovwR0ToDisp        : 0b1000_0001_0000_0000, "mov.w",   (u8:disp:4, Register:n:0);
    // mov.l Rm, @(disp,Rn)
    MovbRegToDisp       : 0b0001_0000_0000_0000, "mov.b",   (Register:m:4, u8:disp:0, Register:n:8);

    // mov[SZ] @(R0,Rm), Rn
    MovbR0RegToReg      : 0b0000_0000_0000_1100, "mov.b",   (Register:m:4, Register:n:8);
    MovwR0RegToReg      : 0b0000_0000_0000_1101, "mov.w",   (Register:m:4, Register:n:8);
    MovlR0RegToReg      : 0b0000_0000_0000_1110, "mov.l",   (Register:m:4, Register:n:8);
    // mov[SZ] Rm, @(R0,Rn)
    MovbRegToR0Reg      : 0b0000_0000_0000_0100, "mov.b",   (Register:m:4, Register:n:8);
    MovwRegToR0Reg      : 0b0000_0000_0000_0101, "mov.w",   (Register:m:4, Register:n:8);
    MovlRegToR0Reg      : 0b0000_0000_0000_0110, "mov.l",   (Register:m:4, Register:n:8);

    // TODO: GBR-related and SH-4 specific routines
    Movt                : 0b0000_0000_0010_1001, "movt",    (Register:n:8);

    // TODO: swap.[b|w]/xtrct (unused by B itself but could be done with inline assembly


    // Arithmetic operations
    AddRegWithReg       : 0b0011_0000_0000_1100, "add",     (Register:m:4, Register:n:8);
    AddImmWithReg       : 0b0111_0000_0000_0000, "add",     (Register:n:8, Sext8:imm:0);
    AddcRegWithReg      : 0b0011_0000_0000_1110, "addc",    (Register:m:4, Register:n:8);
    AddvRegWithReg      : 0b0011_0000_0000_1111, "addv",    (Register:m:4, Register:n:8);
    CmpEqImmWithR0      : 0b1000_1000_0000_0000, "cmp/eq",  (Sext8:imm:0);                    // sign-extended
    CmpEq               : 0b0011_0000_0000_0000, "cmp/eq",  (Register:m:4, Register:n:8);
    CmpHs               : 0b0011_0000_0000_0010, "cmp/hs",  (Register:m:4, Register:n:8);
    CmpGe               : 0b0011_0000_0000_0011, "cmp/ge",  (Register:m:4, Register:n:8);
    CmpHi               : 0b0011_0000_0000_0110, "cmp/hi",  (Register:m:4, Register:n:8);
    CmpGt               : 0b0011_0000_0000_0111, "cmp/gt",  (Register:m:4, Register:n:8);
    // TODO: cmp/[pl/pz/str] / div* / up to mul.l

    MullRegWithReg      : 0b0000_0000_0000_0111, "mul.l",   (Register:m:4, Register:n:8);
    // TODO
    NegRegToReg         : 0b0110_0000_0000_1011, "neg",     (Register:m:4, Register:n:8);
    SubRegWithReg       : 0b0011_0000_0000_1000, "sub",     (Register:m:4, Register:n:8);
    

    // Logic operations
    AndRegWithReg       : 0b0010_0000_0000_1001, "and",     (Register:m:4, Register:n:8);
    AndImmWithR0        : 0b1100_1001_0000_0000, "and",     (u8:imm:0);
    // TODO: And with GBR-relative stuff
    NotRegToReg         : 0b0110_0000_0000_0111, "not",     (Register:m:4, Register:n:8);

    OrRegWithReg        : 0b0010_0000_0000_1011, "or",      (Register:m:4, Register:n:8);
    OrImmWithR0         : 0b1100_1011_0000_0000, "or",      (u8:imm:0);
    // TODO: Or with GBR-relative stuff

    // TODO: tas.b for inline assembly
    TstRegWithReg       : 0b0010_0000_0000_1000, "tst",     (Register:m:4, Register:n:8);
    TstImmWithR0        : 0b1100_1111_0000_0000, "tst",     (u8:imm:0);

    XorRegWithReg       : 0b0010_0000_0000_1010, "xor",     (Register:m:4, Register:n:8);
    XorImmWithR0        : 0b1100_1010_0000_0000, "xor",     (u8:imm:0);
    // TODO: Xor with GBR-relative stuff

    // Shift instructions
    Rotcl               : 0b0100_0000_0010_0100, "rotcl",   (Register:n:8);
    Rotcr               : 0b0100_0000_0010_0101, "rotcr",   (Register:n:8);
    Rotl                : 0b0100_0000_0000_0100, "rotl",    (Register:n:8);
    Rotr                : 0b0100_0000_0000_0101, "rotr",    (Register:n:8);

    Shad                : 0b0100_0000_0000_1100, "shad",    (Register:m:4, Register:n:8);

    Shal                : 0b0100_0000_0010_0000, "shal",    (Register:n:8);
    Shar                : 0b0100_0000_0010_0001, "shar",    (Register:n:8);

    Shld                : 0b0100_0000_0000_1101, "shld",    (Register:m:4, Register:n:8);

    Shll                : 0b0100_0000_0000_0000, "shll",    (Register:n:8);
    Shll2               : 0b0100_0000_0000_1000, "shll2",   (Register:n:8);
    Shll8               : 0b0100_0000_0001_1000, "shll8",   (Register:n:8);
    Shll16              : 0b0100_0000_0010_1000, "shll16",  (Register:n:8);

    Shlr                : 0b0100_0000_0000_0001, "shlr",    (Register:n:8);
    Shlr2               : 0b0100_0000_0000_1001, "shlr2",   (Register:n:8);
    Shlr8               : 0b0100_0000_0001_1001, "shlr8",   (Register:n:8);
    Shlr16              : 0b0100_0000_0010_1001, "shlr16",  (Register:n:8);

    // Branch instructions
    Bf                  : 0b1000_1011_0000_0000, "bf",      (i8:disp:0);
    Bfs                 : 0b1000_1111_0000_0000, "bf/s",    (i8:disp:0);
    Bt                  : 0b1000_1001_0000_0000, "bt",      (i8:disp:0);
    Bts                 : 0b1000_1101_0000_0000, "bt/s",    (i8:disp:0);

    Bra                 : 0b1010_0000_0000_0000, "bra",     (u16:disp12:0);            // Sext12 your displacements!
    Braf                : 0b0000_0000_0010_0011, "bra/f",   (Register:m:8);
    Bsr                 : 0b1011_0000_0000_0000, "bsr",     (u16:disp12:0);            // Sext12 your displacements!
    Bsrf                : 0b0000_0000_0000_0011, "bsr/f",   (Register:m:8);

    Jmp                 : 0b0100_0000_0010_1011, "jmp",     (MRegister:m:8);
    Jsr                 : 0b0100_0000_0000_1011, "jsr",     (MRegister:m:8);
    Rts                 : 0b0000_0000_0000_1011, "rts",     (Dummy:dummy:16);
    Nop                 : 0b0000_0000_0000_1001, "nop",     (Dummy:dummy:16);

    // System control instructions

    // sts.l pr, @-rN
    StsPRToSReg           : 0b0100_0000_0010_0010, "sts.l",   (PR:dummy:16,SRegister:n:8);

    // sts MACL, Rn
    StsMACLToReg          : 0b0000_0000_0001_1010, "sts",   (MACL:dummy:16,Register:n:8);

    // lds.l @rN+, pr
    LdsRegIToPR           : 0b0100_0000_0010_0110, "lds.l",   (RegisterI:n:8, PR:dummy:16);
}

pub unsafe fn assemble_statement(asm: *mut FAssembler, stmt: AsmStmt) {
    let mut line: *const c_char = stmt.line;
    while isspace(*line as c_int) != 0 {
        line = line.add(1);
    }

    // line now points at a mnemonic we need to parse
    // TODO: Write a proper assembler
    if let Some(instruction) = try_matching(line, stmt.loc) {
        assemble_instruction(asm, instruction);
    }
}


pub unsafe fn write_nextsym(asm: *mut FAssembler) -> u8 {
    // Try to predict the offset of the next symbol in bytes
    let mut off: u8 = 0;

    // First nop
    off += 2;
    while (((*asm).output).count+2+(off as usize)) % 4 != 0 { off += 2; }
    off += 8;
    
    // Now have an offset from our count to the next symbol
    off
}
pub unsafe fn next_litteral(asm: *mut FAssembler, lit: u32) -> usize {
    let index = (*asm).next_litteral;
    // Write a dirty "trampoline"

    assemble_instruction(asm, Instruction::Nop{dummy: 0});

    while ((*asm).output).count % 4 != 0 { assemble_instruction(asm, Instruction::Nop{dummy: 0}); }
    // We are now aligned to a 32-bit lword
    assemble_instruction(asm, Instruction::Bra{disp12: 4});
    assemble_instruction(asm, Instruction::Nop{dummy: 0});

    // One 32-bit word
    assemble_instruction(asm, Instruction::Nop{dummy: 0});
    assemble_instruction(asm, Instruction::Nop{dummy: 0});
    asm_write32(asm, lit);        // Another
    // We should now be there

    //next_trampoline(asm);
    (*asm).next_litteral += 1;
    index
}
// TODO: Rewrite all this stuff
pub unsafe fn write_jmppointsym(_assembler: *mut FAssembler) {
}
pub unsafe fn write_jmppointlbl(_assembler: *mut FAssembler) {
}
pub unsafe fn next_jmppoint(assembler: *mut FAssembler) {
    write_jmppointlbl(assembler);
    (*assembler).next_jmppoint += 1;
}
pub unsafe fn write_trampolinesym(_assembler: *mut FAssembler) {
}
pub unsafe fn write_trampolinelbl(_assembler: *mut FAssembler) {
}
pub unsafe fn next_trampoline(assembler: *mut FAssembler) {
    (*assembler).next_trampoline += 1;
}
pub unsafe fn next_symbol(assembler: *mut FAssembler, sym: *const c_char) -> usize {
    let index = (*assembler).next_litteral;
    // Write a dirty "trampoline"
    
    // TODO: Fix fun nop issues

    assemble_instruction(assembler, Instruction::Nop{dummy: 0});
    while ((*assembler).output).count % 4 != 0 { assemble_instruction(assembler, Instruction::Nop{dummy: 0}); }

    assemble_instruction(assembler, Instruction::Bra{disp12: 2});
    assemble_instruction(assembler, Instruction::Nop{dummy: 0});
    // still aligned
    require_symbol(assembler, sym);

    (*assembler).next_litteral += 1;
    (*assembler).next_trampoline += 1;
    index
}

pub unsafe fn call_arg(arg: Arg, loc: Loc, assembler: *mut FAssembler) {
    // The extra flag is because arg doesnt use references to a symbol but the symbol itself,
    // which then causes some fun issues, as load_arg_to_reg will try to dereference a value that
    // should NOT be dereferenced... this should be fixed in another PR >_>
    //                  - LDA: June 25th, 2025
    load_arg_to_reg(arg, 0, loc, assembler, true);
    assemble_instruction(assembler, Instruction::Jsr{m: 0});
    assemble_instruction(assembler, Instruction::Nop{dummy: 0});
}

pub unsafe fn load_literal_to_reg(reg: Register, literal: u32, assembler: *mut FAssembler) {
    let sliteral = literal as i64;

    // Literals that can fit in a byte are well-supported on SH-4
    if (sliteral >= -128) & (sliteral <= 127) {
        assemble_instruction(assembler, Instruction::MoveImmToReg{imm8: sliteral as u8, n: reg});
        return;
    }
    assemble_instruction(assembler, Instruction::MovlPCRelToReg{disp: (write_nextsym(assembler)>>2), n: reg});
    next_litteral(assembler, literal);
}
pub unsafe fn load_autovaraddr_to_reg(assembler: *mut FAssembler, index: usize, reg: Register) {
    let offset = -((index * 4) as isize);
    assemble_instruction(assembler, Instruction::MoveRegToReg{m: 14, n: reg});
    if offset >= -128 {
        assemble_instruction(assembler, Instruction::AddImmWithReg{imm: offset as u8, n: reg});
    } else {
        // Just in case anyone invested in more than like 32 autovars
        assemble_instruction(assembler, Instruction::MovlPCRelToReg{disp: (write_nextsym(assembler)>>2), n: 10});
        next_litteral(assembler, offset as u32);
        assemble_instruction(assembler, Instruction::AddRegWithReg{m: 10, n: reg});
    }
}
pub unsafe fn load_arg_to_reg(arg: Arg, reg: Register, _loc: Loc, assembler: *mut FAssembler, is_call: bool) {
    match arg {
        Arg::External(name) => {
            assemble_instruction(assembler, Instruction::MovlPCRelToReg{disp: (write_nextsym(assembler)>>2)-1, n: reg});
            next_symbol(assembler, name);
            if !is_call { assemble_instruction(assembler, Instruction::MovlMRegToReg{m: reg, n: reg}); }
        },
        Arg::Deref(index) => {
            load_autovaraddr_to_reg(assembler, index, 8);
            assemble_instruction(assembler, Instruction::MovlMRegToReg{m: 8, n: reg});
            assemble_instruction(assembler, Instruction::MovlMRegToReg{m: reg, n: reg});
        },
        Arg::RefAutoVar(index) => {
            load_autovaraddr_to_reg(assembler, index, reg);
        },
        Arg::RefExternal(name) => {
            assemble_instruction(assembler, Instruction::MovlPCRelToReg{disp: (write_nextsym(assembler)>>2)-1, n: reg});
            next_symbol(assembler, name);
        },
        Arg::AutoVar(index) => {
            load_autovaraddr_to_reg(assembler, index, 8);
            assemble_instruction(assembler, Instruction::MovlMRegToReg{m: 8, n: reg});
        },
        Arg::Literal(value) => {
            load_literal_to_reg(reg, value as u32, assembler);
        },
        Arg::DataOffset(offset) => {
            assemble_instruction(assembler, Instruction::MovlPCRelToReg{disp: (write_nextsym(assembler)>>2)-1, n: reg});
            next_symbol(assembler, c!("<data>"));

            if offset >= 127 {
                load_literal_to_reg(8, offset as u32, assembler);
                assemble_instruction(assembler, Instruction::AddRegWithReg{m: 8, n: reg});
            } else {
                // TODO: Manage this through loading a literal into a register and adding it if
                // possible
                if offset != 0 { 
                    assemble_instruction(assembler, Instruction::AddImmWithReg{imm: offset as u8, n: reg});
                }
            }
        },
        Arg::Bogus => unreachable!("bogus-amogus")
    };
}

pub unsafe fn write_r0(assembler: *mut FAssembler, argument: usize) {
    //sb_appendf(output, c!("    ! Storing into argument[%zu]\n"), argument);
    if argument > 0 {
        let offset = -((argument * 4) as isize);
        assemble_instruction(assembler, Instruction::MoveRegToReg{m: 0, n: 8});
        if offset >= -128 {
            assemble_instruction(assembler, Instruction::MoveImmToReg{imm8: offset as u8, n: 0});
        } else {
            load_literal_to_reg(0, -((argument * 4) as isize) as u32, assembler);
        }
        assemble_instruction(assembler, Instruction::MovlRegToR0Reg{m: 8, n: 14});
    } else {
        assemble_instruction(assembler, Instruction::MovlRegToMReg{m: 0, n: 14});
    }
}
pub unsafe fn generate_function(name: *const c_char, _name_loc: Loc, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], _c: *const Compiler, gas: *mut GAssembler, base: u32) -> FAssembler {
    let mut assembler: FAssembler = FAssembler {
        base_address: base,
        global_asm: gas,
        unresolved_symbols: zeroed(),
        funcname: name,
        output: zeroed(),
        next_trampoline: 0, next_litteral: 0,
        next_jmppoint: 0
    };

    // TODO: Mark a new symbol

    for i in 8..15 { assemble_instruction(&mut assembler, Instruction::MovlRegToSReg{m: i, n: 15}); }
    assemble_instruction(&mut assembler, Instruction::StsPRToSReg{n: 15, dummy: 0});
    assemble_instruction(&mut assembler, Instruction::MoveRegToReg{m: 15, n: 14});

    assert!(auto_vars_count >= params_count);

    const REGISTERS: *const[Register] = &[4, 5, 6, 7];

    // To address the arguments backwards, simply remove from r13
    assemble_instruction(&mut assembler, Instruction::MoveRegToReg{m: 15, n: 13});
    assemble_instruction(&mut assembler, Instruction::AddImmWithReg{imm: 8*4, n: 13});
    for i in 0..params_count {
        // ???
        let reg = if i < REGISTERS.len() { (*REGISTERS)[i] } else { 7 };

        if i >= REGISTERS.len() {
            assemble_instruction(&mut assembler, Instruction::MovlRegIToReg{m: 13, n: reg});
        }

        // Push that argument (from a register) onto the stack
        assemble_instruction(&mut assembler, Instruction::MovlRegToSReg{m: reg, n: 15});
    }
    let diff = auto_vars_count - params_count;
    let offset = -((4 * diff) as isize);
    if offset >= -128 {
        assemble_instruction(&mut assembler, Instruction::AddImmWithReg{imm: (offset as i8) as u8, n: 15});
    } else {
        // TODO
        load_literal_to_reg(8, offset as u32, &mut assembler);
        assemble_instruction(&mut assembler, Instruction::AddRegWithReg{m: 8, n: 15});
    }

    for i in 0..body.len() {
        let op = (*body)[i];
        match op.opcode {
            Op::Bogus => unreachable!("bogus-amogus"),
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg_to_reg(arg, 0, op.loc, &mut assembler, false);
                } else {
                    assemble_instruction(&mut assembler, Instruction::MoveImmToReg{imm8: 0, n: 0});
                }
                assemble_instruction(&mut assembler, Instruction::MoveRegToReg{m: 14, n: 15});
                assemble_instruction(&mut assembler, Instruction::LdsRegIToPR{n: 15, dummy: 0});
                for i in 8..15 { 
                    let j = (14 - i) + 8;
                    assemble_instruction(&mut assembler, Instruction::MovlRegIToReg{m: 15, n: j }); 
                }

                // Delay slot jumpscare
                assemble_instruction(&mut assembler, Instruction::Rts {dummy: 0});
                assemble_instruction(&mut assembler, Instruction::Nop {dummy: 0});
            }
            Op::Negate {result, arg} => {
                load_arg_to_reg(arg, 0, op.loc, &mut assembler, false);
                assemble_instruction(&mut assembler, Instruction::NegRegToReg {m: 0, n: 0});
                write_r0(&mut assembler, result);
            }
            Op::UnaryNot {result, arg} => {
                // This is probably not the most efficient way to do things, and it
                // also thrashes T, so yeah. but then again the B compiler doesn't seem
                // to be something that cares about processor flags all that much.
                load_arg_to_reg(arg, 0, op.loc, &mut assembler, false);
                assemble_instruction(&mut assembler, Instruction::CmpEqImmWithR0 {imm: 0});
                assemble_instruction(&mut assembler, Instruction::Movt {n: 0});
                write_r0(&mut assembler, result);
            },
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::OrRegWithReg {m: 1, n: 0});
                        write_r0(&mut assembler, index);
                    },
                    Binop::BitAnd => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::AndRegWithReg {m: 1, n: 0});
                        write_r0(&mut assembler, index);
                    },

                    Binop::BitShl => {
                        // TODO: There is an opportunity for optimisation for shifts by certain
                        // literals (16, 8, 4, 2, 1)
                        match rhs {
                            Arg::Literal(amount) => {
                                if amount >= 32 {
                                    // You may as well just clear the assembler.output.
                                    assemble_instruction(&mut assembler, Instruction::MoveImmToReg{imm8: 0, n: 0});
                                } else {
                                    const SHIFTS: [(u64,Instruction);4] = [
                                        (16,Instruction::Shll16{n:0}),
                                        (8, Instruction::Shll8{n: 0}),
                                        (2, Instruction::Shll2{n: 0}),
                                        (1, Instruction::Shll {n: 0})
                                    ];
                                    let mut remaining = amount;
                                    let mut index = 0;
                                    load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                                    while remaining > 0 {
                                        let off = SHIFTS[index].0;
                                        let shft = SHIFTS[index].1;
                                        if remaining >= off {
                                            if off != 1 {
                                                assemble_instruction(&mut assembler, shft);
                                            } else {
                                                assemble_instruction(&mut assembler, Instruction::Shll{n: 0});
                                            }
                                            remaining -= off;
                                            continue;
                                        }
                                        index += 1;
                                    }
                                }
                            }
                            _ => {
                                load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                                load_arg_to_reg(rhs, 5, op.loc, &mut assembler, false);
                                // TODO: How should shifting by negative amounts be handled?
                                // Should the absolute value be taken, or should it be interpreted as a
                                // right shift?
                                assemble_instruction(&mut assembler, Instruction::Shld{m: 5, n: 0});
                            }
                        }
                        write_r0(&mut assembler, index);
                    },
                    Binop::BitShr => {
                        // TODO: There is an opportunity for optimisation for shifts by certain
                        // literals (16, 8, 4, 2, 1)
                        match rhs {
                            Arg::Literal(amount) => {
                                if amount >= 32 {
                                    // You may as welr just clear the assembler.output.
                                    assemble_instruction(&mut assembler, Instruction::MoveImmToReg{imm8: 0, n: 0});
                                } else {
                                    const SHIFTS: [(u64,Instruction);4] = [
                                        (16,Instruction::Shlr16{n:0}),
                                        (8, Instruction::Shlr8{n: 0}),
                                        (2, Instruction::Shlr2{n: 0}),
                                        (1, Instruction::Shlr {n: 0})
                                    ];
                                    let mut remaining = amount;
                                    let mut index = 0;
                                    load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                                    while remaining > 0 {
                                        let off = SHIFTS[index].0;
                                        let shft = SHIFTS[index].1;
                                        if remaining >= off {
                                            if off != 1 {
                                                assemble_instruction(&mut assembler, shft);
                                            } else {
                                                assemble_instruction(&mut assembler, Instruction::Shlr{n: 0});
                                            }
                                            remaining -= off;
                                            continue;
                                        }
                                        index += 1;
                                    }
                                }
                            }
                            _ => {
                                load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                                load_arg_to_reg(rhs, 5, op.loc, &mut assembler, false);

                                assemble_instruction(&mut assembler, Instruction::NegRegToReg{m: 5, n: 5});
                                assemble_instruction(&mut assembler, Instruction::Shld{m: 5, n: 0});
                            }
                        }
                        write_r0(&mut assembler, index);
                    },
                    Binop::Plus => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::AddRegWithReg{m: 1, n: 0});
                        write_r0(&mut assembler, index);
                    }
                    Binop::Minus => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::SubRegWithReg{m: 1, n: 0});
                        write_r0(&mut assembler, index);
                    },

                    // Oh god. God no. Don't let me put the 32 div1 jumpscare.
                    Binop::Mod => {
                        // TODO
                        load_arg_to_reg(lhs, 4, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 5, op.loc, &mut assembler, false);
                        call_arg(Arg::External(c!("intrisic_mod")), op.loc, &mut assembler);
                        write_r0(&mut assembler, index);
                    }
                    Binop::Div => {
                        // TODO
                        load_arg_to_reg(lhs, 4, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 5, op.loc, &mut assembler, false);
                        call_arg(Arg::External(c!("intrisic_div")), op.loc, &mut assembler);
                        write_r0(&mut assembler, index);
                    }

                    // This sounds more reasonable to implement
                    Binop::Mult => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::MullRegWithReg{m: 0, n: 1});
                        assemble_instruction(&mut assembler, Instruction::StsMACLToReg{n: 0, dummy: 0});
                        write_r0(&mut assembler, index);
                    },

                    // TODO: Test these out
                    Binop::Less => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::CmpGt{m: 0, n: 1});
                        assemble_instruction(&mut assembler, Instruction::Movt{n: 0});
                        write_r0(&mut assembler, index);
                    }
                    Binop::Greater => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::CmpGt{m: 1, n: 0});
                        assemble_instruction(&mut assembler, Instruction::Movt{n: 0});
                        write_r0(&mut assembler, index);
                    }
                    Binop::Equal => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::CmpEq{m: 1, n: 0});
                        assemble_instruction(&mut assembler, Instruction::Movt{n: 0});
                        write_r0(&mut assembler, index);
                    }
                    Binop::NotEqual => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::CmpEq{m: 1, n: 0});
                        assemble_instruction(&mut assembler, Instruction::Movt{n: 0});
                        assemble_instruction(&mut assembler, Instruction::XorImmWithR0{imm: 1});
                        write_r0(&mut assembler, index);
                    }
                    Binop::GreaterEqual => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::CmpGe{m: 1, n: 0});
                        assemble_instruction(&mut assembler, Instruction::Movt{n: 0});
                        write_r0(&mut assembler, index);
                    },
                    Binop::LessEqual => {
                        load_arg_to_reg(lhs, 0, op.loc, &mut assembler, false);
                        load_arg_to_reg(rhs, 1, op.loc, &mut assembler, false);
                        assemble_instruction(&mut assembler, Instruction::CmpGe{m: 0, n: 1});
                        assemble_instruction(&mut assembler, Instruction::Movt{n: 0});
                        write_r0(&mut assembler, index);
                    },
                }
            }
            Op::ExternalAssign{name, arg} => {
                load_arg_to_reg(arg, 0, op.loc, &mut assembler, false);
                assemble_instruction(&mut assembler, 
                    Instruction::MovlPCRelToReg{disp: (write_nextsym(&mut assembler)>>2)-1, n: 1}
                );
                next_symbol(&mut assembler, name);
                assemble_instruction(&mut assembler, Instruction::MovlRegToMReg{m: 0, n: 1});
            }
            Op::AutoAssign {index, arg} => {
                load_arg_to_reg(arg, 0, op.loc, &mut assembler, false);
                write_r0(&mut assembler, index);
            },
            Op::Store {index, arg} => {
                load_arg_to_reg(arg, 0, op.loc, &mut assembler, false);
                assemble_instruction(&mut assembler, Instruction::MoveRegToReg{m: 14, n: 1});
                assemble_instruction(&mut assembler, Instruction::AddImmWithReg{imm: (-((index*4) as isize)) as u8, n: 1});
                assemble_instruction(&mut assembler, Instruction::MovlMRegToReg{m: 1, n: 1});
                assemble_instruction(&mut assembler, Instruction::MovlRegToMReg{m: 0, n: 1});
            },

            Op::Funcall {result, fun, args} => {
                let registers: *const[Register] = &[ 4, 5, 6, 7 ];
                let reg_args_count = cmp::min(args.count, registers.len());
                let stack_args_count = args.count - reg_args_count;

                for i in 0..reg_args_count {
                    let reg = (*registers)[i];

                    // why is it called add???
                    load_arg_to_reg(*args.items.add(i), reg, op.loc, &mut assembler, false);
                }
                for i in 0..stack_args_count {
                    let j = stack_args_count - i - 1;
                    load_arg_to_reg(*args.items.add(reg_args_count+j), 0, op.loc, &mut assembler, false);
                    assemble_instruction(&mut assembler, Instruction::MovlRegToSReg{m: 0, n: 15});
                }
                call_arg(fun, op.loc, &mut assembler);
                write_r0(&mut assembler, result);
                if stack_args_count > 0 {
                    assemble_instruction(&mut assembler, Instruction::AddImmWithReg{imm: 4*stack_args_count as u8, n: 15});
                }
            },
            Op::Asm { stmts } => {
                for i in 0..stmts.count {
                    let stmt = *stmts.items.add(i);
                    assemble_statement(&mut assembler, stmt);
                }
            }
            Op::Label {label} => {
                let label = temp_sprintf(c!("%s.label_%zu"), name, label);
                provide_symbol(gas, label, base + assembler.output.count as u32);
            }
            Op::JmpLabel {label} => {
                // TODO: Deal with __more__ PCrel nonsense
                // (well, to be frank, GNU as, by default, does some "trampoline" magic
                // to ensure those jumps are always addressible (but I still think they should
                // be managed properly :3)
                let label = temp_sprintf(c!("%s.label_%zu"), name, label);
                assemble_instruction(&mut assembler, 
                    Instruction::MovlPCRelToReg{disp: (write_nextsym(&mut assembler)>>2)-1, n: 0}
                );
                next_symbol(&mut assembler, label);

                assemble_instruction(&mut assembler, Instruction::Nop{dummy: 0});
                assemble_instruction(&mut assembler, Instruction::Jmp{m: 0});
                assemble_instruction(&mut assembler, Instruction::Nop{dummy: 0});
            }
            Op::JmpIfNotLabel {label, arg} => {
                // r0 = cond
                // r1 = label address
                let label = temp_sprintf(c!("%s.label_%zu"), name, label);
                assemble_instruction(&mut assembler, 
                    Instruction::MovlPCRelToReg{disp: (write_nextsym(&mut assembler)>>2)-1, n: 1}
                );
                next_symbol(&mut assembler, label);

                load_arg_to_reg(arg, 0, op.loc, &mut assembler, false);
                assemble_instruction(&mut assembler, Instruction::TstRegWithReg{n: 0, m: 0});

                assemble_instruction(&mut assembler, Instruction::Bf{disp: 2});
                assemble_instruction(&mut assembler, Instruction::Nop{dummy: 0});

                assemble_instruction(&mut assembler, Instruction::Jmp{m: 1});
                assemble_instruction(&mut assembler, Instruction::Nop{dummy: 0});
            }
            Op::Index {result, arg, offset} => {
                load_arg_to_reg(arg, 0, op.loc, &mut assembler, false);
                load_arg_to_reg(offset, 1, op.loc, &mut assembler, false);
                assemble_instruction(&mut assembler, Instruction::Shll2{n: 1});
                assemble_instruction(&mut assembler, Instruction::AddRegWithReg{m: 1, n: 0});
                write_r0(&mut assembler, result);
            }
        }
    }

    assemble_instruction(&mut assembler, Instruction::MoveImmToReg{imm8: 0, n: 0});
    assemble_instruction(&mut assembler, Instruction::MoveRegToReg{m: 14, n: 15});
    assemble_instruction(&mut assembler, Instruction::LdsRegIToPR{n: 15, dummy: 0});
    for i in 8..15 { 
        let j = (14 - i) + 8;
        assemble_instruction(&mut assembler, Instruction::MovlRegIToReg{m: 15, n: j }); 
    }

    // Delay slot jumpscare
    assemble_instruction(&mut assembler, Instruction::Rts {dummy: 0});
    assemble_instruction(&mut assembler, Instruction::Nop {dummy: 0});
    assembler
}

pub unsafe fn generate_funcs(funcs: *const [Func], c: *const Compiler, gas: *mut GAssembler) {
    // Start by generating start
    let mut addr: u32 = addin_offset;
    for i in 0..funcs.len() {
        if strcmp((*funcs)[i].name, c!("start")) != 0 {
            continue;
        }

        if addr % 4 != 0 { addr += 4-(addr%4); }
        let func = generate_function((*funcs)[i].name, (*funcs)[i].name_loc, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), c, gas, addr);
        addr += func.output.count as u32;
        da_append(&mut (*gas).functions, func);
    }

    // Do every other function
    for i in 0..funcs.len() {
        if strcmp((*funcs)[i].name, c!("start")) == 0 {
            continue;
        }
        if addr % 4 != 0 { addr += 4-(addr%4); }
        let func = generate_function((*funcs)[i].name, (*funcs)[i].name_loc, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), c, gas, addr);
        addr += func.output.count as u32;
        da_append(&mut (*gas).functions, func);
    }
}

pub unsafe fn generate_data_section(data: *const [u8], gas: *mut GAssembler) {
    let output: *mut String_Builder = (*gas).output;
    provide_symbol(gas, c!("<data>"), (*(*gas).output).count as u32 + addin_offset);
    if data.len() > 0 {
        printf(c!("DATA %d %08X\n"), data.len() as c_int, ((*output).count as u32 + addin_offset) as c_uint);
        for i in 0..data.len() { da_append(output, (*data)[i] as c_char); }
    }
}

pub unsafe fn generate_globals(globals: *const [Global], gas: *mut GAssembler) {
    if globals.len() > 0 {
        // TODO: consider splitting globals into bss and data sections,
        // depending on whether it's zero
        for i in 0..globals.len() {
            let global = (*globals)[i];
            provide_symbol(gas, global.name, (*(*gas).output).count as u32 + addin_offset);
            if global.is_vec {
                write_u32((*gas).output, ((*(*gas).output).count + 4) as u32 + addin_offset, false);
            }
            for j in 0..global.values.count {
                match *global.values.items.add(j) {
                    ImmediateValue::Literal(lit) => {
                        write_u32((*gas).output, lit as u32, false);
                    }
                    ImmediateValue::Name(_name) => {
                        todo!("name literals");
                    }
                    ImmediateValue::DataOffset(_offset) => {
                        todo!("data offsets");
                    }
                }
            }
            if global.values.count < global.minimum_size {
                let len = 4*(global.minimum_size - global.values.count);
                for _i in 0..len {
                    da_append((*gas).output, 0 as c_char);
                }
            }
        }
    }
}

pub unsafe fn generate_asm_funcs(_output: *mut String_Builder, asm_funcs: *const [AsmFunc]) {
    for _i in 0..asm_funcs.len() {
        todo!("asm funcs");
        /*let asm_func = (*asm_funcs)[i];
        sb_appendf(output, c!(".global %s\n"), asm_func.name);
        sb_appendf(output, c!("%s:\n"), asm_func.name);
        for j in 0..asm_func.body.count {
            let stmt = *asm_func.body.items.add(j);
            sb_appendf(output, c!("    %s\n"), stmt.line);
        }*/
    }
}

pub unsafe fn provide_symbol(gassembler: *mut GAssembler, sym: *const c_char, val: u32) {
    let mut symb = FResolved { symbol: zeroed(), val: val };
    sb_appendf(&mut symb.symbol, c!("%s"), sym);
    da_append(&mut (*gassembler).symbols, symb);
}
pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    let mut gassembler: GAssembler = GAssembler {
        functions: zeroed(),
        symbols: zeroed(),
        output: output
    };
    // TODO: Different ABIs (I could think Renesas v. GCC instead of the custom one here)
    generate_funcs(da_slice((*c).funcs), c, &mut gassembler);
    generate_asm_funcs(output, da_slice((*c).asm_funcs));

    // Write functions out
    for i in 0..gassembler.functions.count {
        let func: *mut FAssembler = gassembler.functions.items.add(i);
        let fout = (*func).output;

        align(output, 4);
        provide_symbol(&mut gassembler, (*func).funcname, (*func).base_address);
        for j in 0..fout.count {
            da_append(output, *(fout.items.add(j)));
        }

    }

    // Resolve globals
    // TODO: Manage unresolved ones
    generate_data_section(da_slice((*c).data), &mut gassembler);
    generate_globals(da_slice((*c). globals), &mut gassembler);
    for i in 0..gassembler.symbols.count {
        let res: FResolved = *(gassembler.symbols.items.add(i));

        for j in 0..gassembler.functions.count {
            let func: FAssembler = *gassembler.functions.items.add(j);
            for k in 0..func.unresolved_symbols.count {
                let unresolved: FUnresolved = *func.unresolved_symbols.items.add(k);
                if strcmp(unresolved.symbol.items, res.symbol.items) == 0 {
                    let off: u32 = (func.base_address-addin_offset) + unresolved.offset;
                    rewrite_u32(output, off as usize, res.val, false);
                }
            }
        }
    }
}


#[repr(C)]
#[derive(Clone, Copy)]
pub struct G3A {
    // All *useful* information about a G3A
    // Note that the header is inverted when writing, and values are big-endian

    // The addins' codesize (ignoring headers+checksums)
    pub inner_size: u32,
    pub name: *const c_char,
    pub filename: *const c_char,

    pub selected_bitmap: Array<u16>,
    pub unselected_bitmap: Array<u16>,

    pub addin: *mut String_Builder
}


pub unsafe fn write_u8(output: *mut String_Builder, mut byte: u8, is_header: bool) {
    if is_header { byte = 0xFF ^ byte; }
    da_append(output, byte as c_char);
}
pub unsafe fn rewrite_u8(output: *mut String_Builder, addr: usize, mut byte: u8, is_header: bool) {
    if is_header { byte = 0xFF ^ byte; }
    *((*output).items.add(addr)) = byte as c_char;
}
pub unsafe fn rewrite_u32(output: *mut String_Builder, addr: usize, val: u32, is_header: bool) {
    for i in 0..4 {
        let byte: u8 = ((val >> (8 * (3 - i))) & 0xFF) as u8;
        rewrite_u8(output, addr + i, byte, is_header);
    }
}
pub unsafe fn write_u16(output: *mut String_Builder, val: u16, is_header: bool) {
    for i in 0..2 {
        let byte: u8 = ((val >> (8 * (1 - i))) & 0xFF) as u8;
        write_u8(output, byte, is_header);
    }
}
pub unsafe fn write_u32(output: *mut String_Builder, val: u32, is_header: bool) {
    for i in 0..4 {
        let byte: u8 = ((val >> (8 * (3 - i))) & 0xFF) as u8;
        write_u8(output, byte, is_header);
    }
}
pub unsafe fn write_string(output: *mut String_Builder, val: *const c_char, is_header: bool) {
    let len = strlen(val);
    for i in 0..len {
        let byte: u8 = *(val.add(i)) as u8;
        write_u8(output, byte, is_header);
    }
}

pub unsafe fn read_16(addin: *const G3A, address: usize) -> u16 {
    let mut ret: u16 = 0x0000;
    let up: u8 = *((*((*addin).addin)).items.add(address + 0)) as u8;
    let down: u8 = *((*((*addin).addin)).items.add(address + 1)) as u8;
    ret |= (up as u16) << 8;
    ret |= (down as u16) << 0;

    ret
}

pub unsafe fn write_g3a(output: *mut String_Builder, addin: *const G3A) {
    // Writing out the header

    // The full header is 0x7000 bytes, with a 4 byte ending checksum.
    let complete_size = ((*((*addin).addin)).count + 0x7000 + 0x4) as u32;
    write_string(output, c!("USBPower"), true);

    // 8-byte block
    write_u8(output, 0x2C, true);
    // Control bytes
    write_u8(output, 0x00, true);
    write_u8(output, 0x01, true);
    write_u8(output, 0x00, true);
    write_u8(output, 0x01, true);
    write_u8(output, 0x00, true);
    // LSB of size + 0x41
    write_u8(output, ((complete_size & 0xFF) as u8).wrapping_add(0x41), true);
    write_u8(output, 0x01, true);

    // Actual filesize
    write_u32(output, complete_size, true);

    // Another LSB + 0xB8
    write_u8(output, ((complete_size & 0xFF) as u8).wrapping_add(0xB8), true);
    write_u8(output, 0x00, true);
    // Checksum of 8 16-bit words from 0x7100
    // 0x7100 -> 0x100 (add-in space)
    let mut sumw: u16 = 0x0000;
    for i in 0..8 { 
        let word = read_16(addin, 0x100 + i * 2);
        sumw = sumw.wrapping_add(word); 
    }
    write_u16(output, sumw, true);
    for _i in 0..8 { write_u8(output, 0x69, false); }

    // ----------- Here, we leave the inverted section. -------------------

    let checksum_addr = (*output).count;
    write_u32(output, 0xBE00DEAD, false);

    // Control bytes
    write_u16(output, 0x0101, false);

    // Padding?
    write_u32(output, 0xBE00DEAD, false);
    write_u32(output, 0xBE00DEAD, false);

    // Actual add-in size
    write_u32(output, (*((*addin).addin)).count as u32, false);
    // More padding
    for _i in 0..14 { write_u8(output, 0x69, false); }

    let name_len = strlen((*addin).name);
    for i in 0..16 { 
        if i < name_len && i != 15 {
            write_u8(output, *((*addin).name.add(i)) as u8, false);
        } else {
            write_u8(output, 0x00, false);
        }
    }

    // More padding
    for _i in 0..12 { write_u8(output, 0x69, false); }

    // Actual filesize
    write_u32(output, complete_size, false);

    // Internal ID (I'll just use a placeholder)
    write_string(output, c!("@BG3AKOGASA"), false);
    for _lang in 0..8 {
        for i in 0..24 { 
            if i < name_len && i != 23 {
                write_u8(output, *((*addin).name.add(i)) as u8, false);
            } else {
                write_u8(output, 0x00, false);
            }
        }
    }

    // who up makin their b addins e-act strips
    write_u8(output, 0x00, false);

    // More padding!
    write_u32(output, 0x00000000, false);

    // Version
    write_string(output, c!("01.00.0000"), false);
    write_u16(output, 0x00, false);

    // Date (TODO: use system date instead of UFO) (YYYY-MMDD-HHMM)
    write_string(output, c!("2009.0815.1200"), false);

    for _i in 0..38 { write_u8(output, 0x00, false); }

    // More eAct nonsense
    for _lang in 0..8 {
        for i in 0..36 { 
            if i < name_len && i != 35 {
                write_u8(output, *((*addin).name.add(i)) as u8, false);
            } else {
                write_u8(output, 0x00, false);
            }
        }
    }

    // This is intended to be the strip logo
    for _i in 0..768 { write_u8(output, 0x44, false); }
    for _i in 0..0x92C { write_u8(output, 0x55, false); }

    let filename_len = strlen((*addin).filename);
    for i in 0..0x144 { 
        if i < filename_len && i != 0x144 {
            write_u8(output, *((*addin).filename.add(i)) as u8, false);
        } else {
            write_u8(output, 0x00, false);
        }
    }

    // 16-bit flat icons
    for i in 0..5888 {
        if i < (*addin).selected_bitmap.count {
            write_u16(output, *((*addin).selected_bitmap.items.add(i)), false);
        } else {
            write_u16(output, 0xFFFF, false);
        }
    }
    for _i in 0..0x200 { write_u8(output, 0x00, false); }

    for i in 0..5888 {
         if i < (*addin).unselected_bitmap.count {
            write_u16(output, *((*addin).unselected_bitmap.items.add(i)), false);
         } else {
            write_u16(output, 0x0000, false);
         }
    }
    for _i in 0..0x200 { write_u8(output, 0x00, false); }

    // Here, we assume the add-in is aligned to 4.
    for i in 0..(*(*addin).addin).count {
        write_u8(output, *((*(*addin).addin).items.add(i)) as u8, false);
    }

    // Now, compute the checksum
    let mut checksum: u32 = 0x00000000;
    for i in 0x00..0x20             { checksum += (*((*output).items.add(i)) as u8) as u32; }
    for i in 0x24..(*output).count  { checksum += (*((*output).items.add(i)) as u8) as u32; }
    write_u32(output, checksum, false);

    // Overwrite the old checksum
    rewrite_u32(output, checksum_addr, checksum, false);
}
pub unsafe fn generate_g3a(binary: *const c_char, name: *const c_char, output_file: *const c_char) -> Option<()> {
    let mut g3a: G3A = zeroed();    
    let mut g3a_out: String_Builder = zeroed();
    let mut bin_in: String_Builder = zeroed();

    read_entire_file(binary, &mut bin_in)?;
    g3a.name = name;
    g3a.filename = output_file;

    // TODO: read from a raw rgb16 image
    g3a.selected_bitmap = zeroed();
    g3a.unselected_bitmap = zeroed();

    g3a.addin = &mut bin_in;

    write_g3a(&mut g3a_out, &g3a);
    write_entire_file(output_file, g3a_out.items as *const c_void, g3a_out.count)?;

    Some(())
}
