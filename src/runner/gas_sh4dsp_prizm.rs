use crate::nob::*;
use crate::crust::libc::*;
use core::ffi::*;

// This is meant to be a SH4 emulator for testing that somewhat looks like a basic CASIO Prizm, thus ignoring
// some details (like on-chip memory, etc...) that aren't important for tests (including any syscall, using them
// WILL not work).
//
// Also, I've only really implemented the subset that B actually requires, so most DSP operations
// will naturally not work. Then again, this doesn't even have any on-chip memory (we use fake
// "ILRAM" addresses for I/O, here.)
pub mod sh4 {
    use core::mem::zeroed;
    use crate::nob::*;
    use crate::crust::libc::*;
    use core::ffi::*;

    #[derive(Clone, Copy)]
    pub struct CPUState {
        // Let's not worry about banked registers (they are only useful when dealing with
        // privileged code which we do not generate)
        pub r: [u32;16],

        // We only care about some flags in SR
        pub sr: u32,
        pub gbr: u32,

        pub mach: u32,
        pub macl: u32,

        pub pr: u32,
        pub pc: u32,
    }

    pub static mut ADDIN: [u8; 2<<20] = unsafe { zeroed() };
    pub static mut RAM: [u8; 512<<10] = unsafe { zeroed() };
    pub static mut CPU: CPUState = unsafe { zeroed() };
    pub static mut OUT: *mut FILE = unsafe { zeroed() };

    pub unsafe fn get_t() -> bool {
        if (CPU.sr & 1) == 1 {
            true
        } else {
            false
        }
    }
    pub unsafe fn set_t(t: bool) {
        if t {
            CPU.sr = CPU.sr | 1;
        } else {
            CPU.sr = CPU.sr & 0xFFFFFFFE;
        }
    }
    pub unsafe fn get_m() -> bool {
        if (CPU.sr & (1 << 9)) != 0 {
            true
        } else {
            false
        }
    }
    pub unsafe fn set_m(t: bool) {
        if t {
            CPU.sr = CPU.sr | (1 << 9);
        } else {
            CPU.sr = CPU.sr & (!(1 << 9) as u32);
        }
    }
    pub unsafe fn get_q() -> bool {
        if (CPU.sr & (1 << 8)) != 0 {
            true
        } else {
            false
        }
    }
    pub unsafe fn set_q(t: bool) {
        if t {
            CPU.sr = CPU.sr | (1 << 8);
        } else {
            CPU.sr = CPU.sr & (!(1 << 8) as u32);
        }
    }

    pub unsafe fn sext8(n: u8) -> u32 {
        if (n & 0x80) != 0 {
            return 0xFFFFFF00 | (n as u32);
        }
        n as u32
    }
    pub unsafe fn sext12(n: u16) -> u32 {
        let nm = n & 0xFFF;
        if (nm & 0x800) != 0 {
            return 0xFFFFF000 | (nm as u32);
        }
        nm as u32
    }
    pub unsafe fn sext16(n: u16) -> u32 {
        if (n & 0x8000) != 0 {
            return 0xFFFF0000 | (n as u32);
        }
        n as u32
    }

    // Note that we don't do any sort of verification.
    pub unsafe fn load_addin(rom: String_Builder, stream: *mut FILE) {
        OUT = stream;
        if rom.count < 0x7000 {
            // TODO: Tell the user off. This shit ain't an addin.
            panic!("YOUR TAKING TOO LONG");
        }
        for i in 0..(rom.count-0x7000) {
            ADDIN[i] = *rom.items.add(i + 0x7000) as u8;
        }
    }

    // TODO: Manage unaligned accesses!
    pub unsafe fn read(address: u32) -> u8 {
        let mut ret: u8 = 0x55;
        if address >= 0x300000 && address < 0x500000 {
            ret = ADDIN[(address as usize) - 0x300000];
        } else if address >= 0x08100000 && address < (0x08100000 + (512 << 10)) {
            ret = RAM[address as usize - 0x08100000];
        }

        // Uninitialised memory on the fx-CG50 tends to be 0x55
        ret
    }
    pub unsafe fn read16(address: u32) -> u16 {
        // TODO: Alignment
        let ret = ((read(address) as u16) << 8) | (read(address + 1) as u16);

        ret
    }
    pub unsafe fn read32(address: u32) -> u32 {
        // TODO: Alignment
        let ret = ((read16(address) as u32) << 16) | (read16(address + 2) as u32);

        if address == 0xE5200004 {
            getchar() as u32
        } else {
            ret
        }
    }

    pub unsafe fn write(address: u32, value: u8) {
        if address >= 0x300000 && address < 0x500000 {
            ADDIN[address as usize - 0x300000] = value;
        } else if address >= 0x08100000 && address < (0x08100000 + (512 << 10)) {
            RAM[address as usize - 0x08100000] = value;
        }

    }
    pub unsafe fn write16(address: u32, value: u16) {
        // TODO: Alignment
        write(address + 0, ((value & 0xFF00) >> 8) as u8);
        write(address + 1, ((value & 0x00FF) >> 0) as u8);
    }
    pub unsafe fn write32(address: u32, value: u32) {
        // TODO: Alignment
        write16(address + 0, ((value & 0xFFFF0000) >> 16) as u16);
        write16(address + 2, ((value & 0x0000FFFF) >> 0) as u16);

        if address == 0xE5200004 {
            fprintf(OUT, c!("%c"), value as c_int);
        }
    }

    pub unsafe fn reset() {
        for i in 0..16 {
            CPU.r[i] = 0x00000000;
        }
        CPU.r[15] = 0x08100000 + (512 << 10) - 4;

        CPU.pc = 0x00300000;
        CPU.pr = 0xFFFFFFFF;

        CPU.mach = 0;
        CPU.macl = 0;

        CPU.sr = 0b01110000000000000000000011110000;
        CPU.gbr = 0xAABEDEAD;
    }

    pub unsafe fn mov_rr(instr: u16) {
        let rn = ((instr & 0x0F00) >> (2 * 4)) & 0xF;
        let rm = ((instr & 0x00F0) >> (1 * 4)) & 0xF;

        CPU.r[rn as usize] = CPU.r[rm as usize];
        CPU.pc += 2;
    }
    pub unsafe fn mov_ir(instr: u16) {
        let rn = ((instr & 0x0F00) >> (2 * 4)) & 0xF;
        let imm8 = sext8((instr & 0x00FF) as u8);

        // TODO: Does this manage sign-extension?
        CPU.r[rn as usize] = imm8;
        CPU.pc += 2;
    }
    pub unsafe fn mova_pcr0(instr: u16) {
        // TODO: Manage delay slots!
        let disp = (instr & 0x00FF) as u8;

        // TODO: Does this manage sign-extension?
        CPU.r[0] = (disp*4) as u32 + (CPU.pc & 0xFFFFFFFC) + 4;
        CPU.pc += 2;
    }
    pub unsafe fn movw_pcr(instr: u16) {
        // TODO: Manage delay slots!
        let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
        let disp = (instr & 0x00FF) as u8;

        // TODO: Does this manage sign-extension?
        CPU.r[rn as usize] = sext16(read16(CPU.pc + 4 + (disp as u32) * 2)) as u32;
        CPU.pc += 2;
    }
    pub unsafe fn movl_pcr(instr: u16) {
        // TODO: Manage delay slots!
        let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
        let disp = (instr & 0x00FF) as u8;

        // TODO: Does this manage sign-extension?
        CPU.r[rn as usize] = read32((disp*4) as u32 + (CPU.pc & 0xFFFFFFFC) + 4);
        CPU.pc += 2;
    }
    pub unsafe fn movb_ar(instr: u16, postinc: bool) {
        let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
        let rm = ((instr & 0x00F0) >> (4 * 1)) & 0xF;
        CPU.r[rn as usize] = sext8(read(CPU.r[rm as usize])) as u32;

        if postinc { CPU.r[rm as usize] += 1; }
        CPU.pc += 2;
    }
    pub unsafe fn movw_ar(instr: u16, postinc: bool) {
        let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
        let rm = ((instr & 0x00F0) >> (4 * 1)) & 0xF;
        CPU.r[rn as usize] = sext16(read16(CPU.r[rm as usize])) as u32;

        if postinc { CPU.r[rm as usize] += 2; }
        CPU.pc += 2;
    }
    pub unsafe fn movl_ar(instr: u16, postinc: bool) {
        let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
        let rm = ((instr & 0x00F0) >> (4 * 1)) & 0xF;
        CPU.r[rn as usize] = read32(CPU.r[rm as usize]);

        if postinc { CPU.r[rm as usize] += 4; }
        CPU.pc += 2;
    }

    // TODO
    pub unsafe fn movb_ra(instr: u16, predec: bool) {
        let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
        let rm = ((instr & 0x00F0) >> (4 * 1)) & 0xF;

        if predec { CPU.r[rn as usize] -= 1; }
        write(CPU.r[rn as usize], (CPU.r[rm as usize] & 0xFF) as u8);
        CPU.pc += 2;
    }
    pub unsafe fn movw_ra(instr: u16, predec: bool) {
        let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
        let rm = ((instr & 0x00F0) >> (4 * 1)) & 0xF;

        if predec { CPU.r[rn as usize] -= 2; }
        write16(CPU.r[rn as usize], (CPU.r[rm as usize] & 0xFFFF) as u16);
        CPU.pc += 2;
    }
    pub unsafe fn movl_ra(instr: u16, predec: bool) {
        let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
        let rm = ((instr & 0x00F0) >> (4 * 1)) & 0xF;

        if predec { CPU.r[rn as usize] -= 4; }
        write32(CPU.r[rn as usize], CPU.r[rm as usize]);
        CPU.pc += 2;
    }
    pub unsafe fn mov_rdc(instr: u16, size: u32) {
        let rm = ((instr & 0x00F0) >> (4 * 1)) & 0xF;
        let dp = (((instr & 0x000F) >> (4 * 0)) & (0xF)) as u32;
        if size == 1 {
            CPU.r[0] = sext8(read(CPU.r[rm as usize] + dp * size)) as u32;
        } else if size == 2 {
            CPU.r[0] = sext16(read16(CPU.r[rm as usize] + dp * size)) as u32;
        } else if size == 4 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
            CPU.r[rn as usize] = read32(CPU.r[rm as usize] + dp * size);
        }
        CPU.pc += 2;
    }
    pub unsafe fn mov_dcr(instr: u16, size: u32) {
        let rm = ((instr & 0x00F0) >> (4 * 1)) & 0xF;
        let dp = (((instr & 0x000F) >> (4 * 0)) & 0xF) as u32;
        if size == 1 {
            CPU.r[0] = sext8(read(CPU.r[rm as usize] + dp * size)) as u32;
        } else if size == 2 {
            CPU.r[0] = sext16(read16(CPU.r[rm as usize] + dp * size)) as u32;
        } else if size == 4 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) & 0xF;
            CPU.r[rn as usize] = read32(CPU.r[rm as usize] + dp * size);
        }
        CPU.pc += 2;
    }

    pub unsafe fn step(delay_slot: bool) -> bool {
        let instr = read16(CPU.pc);
        if (instr & 0xF00F) == 0x6003 {
            mov_rr(instr);
            true
        } else if (instr & 0xF000) == 0xE000 {
            mov_ir(instr);
            true
        } else if (instr & 0xFF00) == 0xC700 {
            if delay_slot {
                // TODO
                return false;
            }
            mova_pcr0(instr);
            true
        } else if (instr & 0xF000) == 0x9000 {
            if delay_slot {
                // TODO
                return false;
            }
            movw_pcr(instr);
            true
        } else if (instr & 0xF000) == 0xD000 {
            if delay_slot {
                // TODO
                return false;
            }
            movl_pcr(instr);
            true
        } else if (instr & 0xF00B) == 0x6000 {
            movb_ar(instr, (instr & 0b0100) == 0b0100);
            true
        } else if (instr & 0xF00B) == 0x6001 {
            movw_ar(instr, (instr & 0b0100) == 0b0100);
            true
        } else if (instr & 0xF00B) == 0x6002 {
            movl_ar(instr, (instr & 0b0100) == 0b0100);
            true
        } else if (instr & 0xF00B) == 0x2000 {
            movb_ra(instr, (instr & 0b0100) == 0b0100);
            true
        } else if (instr & 0xF00B) == 0x2001 {
            movw_ra(instr, (instr & 0b0100) == 0b0100);
            true
        } else if (instr & 0xF00B) == 0x2002 {
            movl_ra(instr, (instr & 0b0100) == 0b0100);
            true
        } else if (instr & 0xFF00) == 0x8400 {
            mov_rdc(instr, 1);
            true
        } else if (instr & 0xFF00) == 0x8500 {
            mov_rdc(instr, 2);
            true
        } else if (instr & 0xF000) == 0x5000 {
            mov_rdc(instr, 4);
            true
        } else if (instr & 0xFF00) == 0x8000 {
            mov_dcr(instr, 1);
            true
        } else if (instr & 0xFF00) == 0x8100 {
            mov_dcr(instr, 2);
            true
        } else if (instr & 0xF0FF) == 0x4022 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.r[rn] -= 4;
            write32(CPU.r[rn], CPU.pr);
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4024 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let t = get_t() as u32;
            set_t((CPU.r[rn] & 0x80000000) != 0);
            CPU.r[rn] <<= 1;
            CPU.r[rn] |= t;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4000 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            set_t((CPU.r[rn] & 0x80000000) != 0);
            CPU.r[rn] <<= 1;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4008 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.r[rn] <<= 2;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4018 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.r[rn] <<= 8;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4028 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.r[rn] <<= 16;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4001 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            // TODO
            set_t((CPU.r[rn] & 0x00000001) != 0);
            CPU.r[rn] >>= 1;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4009 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.r[rn] >>= 2;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4019 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.r[rn] >>= 8;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4029 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.r[rn] >>= 16;
            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x400D {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let rm = ((instr & 0x00F0) >> (4 * 1)) as usize;
            let m = CPU.r[rm] as i32;
            if m >= 0 {
                CPU.r[rn] <<= m;
            } else {
                CPU.r[rn] >>= -m;
            }
            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x2007 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let rm = ((instr & 0x00F0) >> (4 * 1)) as usize;
            set_m((CPU.r[rn] & 0x80000000) != 0);
            set_q((CPU.r[rm] & 0x80000000) != 0);
            set_t(get_m() != get_q());
            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x3004 {
            let n = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let m = ((instr & 0x00F0) >> (4 * 1)) as usize;

            let tmp2 = CPU.r[m];
            let tmp0: u32;
            let tmp1: bool;
            let oldq: bool = get_q();

            set_q((CPU.r[n] & 0x80000000) != 0);
            CPU.r[n] <<= 1;
            CPU.r[n] |= get_t() as u32;

            if !oldq {
                if !get_m() {
                    tmp0 = CPU.r[n];
                    CPU.r[n] = CPU.r[n].overflowing_sub(tmp2).0;
                    tmp1 = CPU.r[n] > tmp0;
                    if get_q() {
                        set_q(tmp1 == false);
                    } else {
                        set_q(tmp1);
                    }
                } else {
                    tmp0 = CPU.r[n];
                    CPU.r[n] = CPU.r[n].overflowing_add(tmp2).0;
                    tmp1 = CPU.r[n] < tmp0;

                    if get_q() {
                        set_q(tmp1);
                    } else {
                        set_q(tmp1 == false);
                    }
                }
            } else {
                if !get_m() {
                    tmp0 = CPU.r[n];
                    CPU.r[n] = CPU.r[n].overflowing_add(tmp2).0;
                    tmp1 = CPU.r[n] < tmp0;

                    if get_q() {
                        set_q(tmp1 == false);
                    } else {
                        set_q(tmp1);
                    }
                } else {
                    tmp0 = CPU.r[n];
                    CPU.r[n] = CPU.r[n].overflowing_sub(tmp2).0;
                    tmp1 = CPU.r[n] > tmp0;

                    if get_q() {
                        set_q(tmp1);
                    } else {
                        set_q(tmp1 == false);
                    }
                }
            }

            set_t(get_q() == get_m());
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x001A {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.r[rn] = CPU.macl;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x002A {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.r[rn] = CPU.pr;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x4026 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            CPU.pr = read32(CPU.r[rn]);
            CPU.r[rn] += 4;
            CPU.pc += 2;
            true
        } else if (instr & 0xF000) == 0x7000 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let imm8 = sext8((instr & 0xFF).try_into().unwrap_or(0));

            CPU.r[rn] = CPU.r[rn].overflowing_add(imm8).0;
            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x300C {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let rm = ((instr & 0x00F0) >> (4 * 1)) as usize;

            CPU.r[rn] = CPU.r[rn].overflowing_add(CPU.r[rm]).0;
            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x600B {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let rm = ((instr & 0x00F0) >> (4 * 1)) as usize;

            CPU.r[rn] = (0u32).overflowing_sub(CPU.r[rm]).0;
            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x3008 {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let rm = ((instr & 0x00F0) >> (4 * 1)) as usize;

            CPU.r[rn] = CPU.r[rn].overflowing_sub(CPU.r[rm]).0;
            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x300A {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let rm = ((instr & 0x00F0) >> (4 * 1)) as usize;

            let tmp1 = CPU.r[rn].overflowing_sub(CPU.r[rm]).0;
            let tmp0 = CPU.r[rn];

            CPU.r[rn] = (tmp1).overflowing_sub(get_t() as u32).0;

            set_t(false);
            if tmp0 < tmp1 { set_t(true); }
            if tmp1 < CPU.r[rn] { set_t(true); }
            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x300E {
            let rn = ((instr & 0x0F00) >> (4 * 2)) as usize;
            let rm = ((instr & 0x00F0) >> (4 * 1)) as usize;
            let tmp0 = CPU.r[rn];
            let tmp1 = CPU.r[rn].overflowing_add(CPU.r[rm]).0;

            CPU.r[rn] = (tmp1).overflowing_add(get_t() as u32).0;

            set_t(false);
            if tmp0 > tmp1 { set_t(true); }

            if tmp1 > CPU.r[rn] { set_t(true); }
            CPU.pc += 2;
            true
        } else if instr == 0x0009 {
            CPU.pc += 2;
            true
        } else if (instr & 0xF000) == 0xA000 {
            if delay_slot {
                // TODO
                return false;
            }
            let disp12 = sext12(instr & 0x0FFF) as i32;
            let npc = (CPU.pc as i32 + 4 + disp12 * 2) as u32;
            // Run the next instruction
            CPU.pc += 2;
            if !step(true) {
                return false
            }
            CPU.pc = npc;
            true
        } else if (instr & 0xF0FF) == 0x400B {
            let rm = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let npc = CPU.r[rm];
            if delay_slot {
                // TODO
                return false;
            }
            CPU.pr = CPU.pc + 4;
            // Run the next instruction
            CPU.pc += 2;
            if !step(true) {
                return false
            }
            CPU.pc = npc;
            true
        } else if (instr & 0xF0FF) == 0x402B {
            let rm = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let npc = CPU.r[rm];
            if delay_slot {
                // TODO
                return false;
            }
            // Run the next instruction
            CPU.pc += 2;
            if !step(true) {
                return false;
            }
            CPU.pc = npc;
            true
        } else if instr == 0x000B {
            if delay_slot {
                // TODO
                return false;
            }
            CPU.pc += 2;
            if !step(true) {
                return false
            }
            CPU.pc = CPU.pr;
            true
        } else if (instr & 0xFF00) == 0x8900 {
            if get_t() {
                let imm8 = sext8((instr & 0xFF) as u8);
                CPU.pc += 4 + imm8 * 2;
            } else {
                CPU.pc += 2;
            }
            true
        } else if (instr & 0xFF00) == 0x8B00 {
            if !get_t() {
                let imm8 = sext8((instr & 0xFF) as u8);
                CPU.pc += 4 + imm8 * 2;
            } else {
                CPU.pc += 2;
            }
            true
        } else if (instr & 0xFF00) == 0xCA00 {
            let imm8 = (instr & 0xFF) as u32;
            CPU.r[0] ^= imm8;
            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x3003 {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let m = (((instr & 0x00F0) >> (4 * 1)) & 0xF) as usize;
            let rn = CPU.r[n] as i32;
            let rm = CPU.r[m] as i32;

            set_t(rn >= rm);

            CPU.pc += 2;
            true
        } else if (instr & 0xFF00) == 0x8800 {
            let rn = CPU.r[0] as i32;
            let imm8 = sext8((instr & 0xFF) as u8) as i32;
            set_t(rn == imm8);

            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x3007 {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let m = (((instr & 0x00F0) >> (4 * 1)) & 0xF) as usize;
            let rn = CPU.r[n] as i32;
            let rm = CPU.r[m] as i32;

            set_t(rn > rm);

            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x3000 {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let m = (((instr & 0x00F0) >> (4 * 1)) & 0xF) as usize;
            let rn = CPU.r[n] as i32;
            let rm = CPU.r[m] as i32;

            set_t(rn == rm);

            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x2008 {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let m = (((instr & 0x00F0) >> (4 * 1)) & 0xF) as usize;
            let rn = CPU.r[n];
            let rm = CPU.r[m];

            set_t((rn & rm) == 0);

            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x0006 {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let m = (((instr & 0x00F0) >> (4 * 1)) & 0xF) as usize;
            let rn = CPU.r[n];
            let rm = CPU.r[m];

            write32(rn.overflowing_add(CPU.r[0]).0, rm);

            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x200B {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let m = (((instr & 0x00F0) >> (4 * 1)) & 0xF) as usize;
            let rn = CPU.r[n];
            let rm = CPU.r[m];

            CPU.r[n] = rn | rm;

            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x200A {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let m = (((instr & 0x00F0) >> (4 * 1)) & 0xF) as usize;
            let rn = CPU.r[n];
            let rm = CPU.r[m];

            CPU.r[n] = rn ^ rm;

            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x2009 {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let m = (((instr & 0x00F0) >> (4 * 1)) & 0xF) as usize;
            let rn = CPU.r[n];
            let rm = CPU.r[m];

            CPU.r[n] = rn & rm;

            CPU.pc += 2;
            true
        } else if (instr & 0xF00F) == 0x0007 {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            let m = (((instr & 0x00F0) >> (4 * 1)) & 0xF) as usize;
            let rn = CPU.r[n] as u64;
            let rm = CPU.r[m] as u64;

            let macl = ((rn * rm) & 0xFFFFFFFF) as u32;
            CPU.macl = macl;
            CPU.pc += 2;
            true
        } else if (instr & 0xF0FF) == 0x0029 {
            let n = (((instr & 0x0F00) >> (4 * 2)) & 0xF) as usize;
            CPU.r[n] = 0;
            if get_t() {
                CPU.r[n] = 1;
            }
            CPU.pc += 2;
            true
        } else {
            false
        }
    }

}

pub unsafe fn run(output: *mut String_Builder, output_path: *const c_char, stdout_path: Option<*const c_char>) -> Option<()> {
    // TODO: implement accepting command line arguments
    let stream = if let Some(stdout_path) = stdout_path {
        let stream = fopen(stdout_path, c!("wb"));
        if stream.is_null() { return None }
        stream
    } else {
        stdout()
    };
    (*output).count = 0;
    read_entire_file(output_path, output)?;


    sh4::load_addin(*output, stream);
    sh4::reset();

    while sh4::CPU.pc != 0xFFFFFFFF { // The convetion is stop executing when pc == 0xFFFFFFFF
        if sh4::CPU.pc == 0x80020070 {
            // TODO: Consider having a syscall handler that tries to use raylib for Bdisp syscalls
            // and other text related shenanigans. If anyone wants to have a little fun homework
            // exercice, I recommend that they look at the following pages I left here:
            //      - https://prizm.cemetech.net/Syscalls/ for a list of known syscalls on the
            //      Prizm
            //      - libb/gas-sh4dsp-prizm.b has a list of all syscalls *used by B* and their
            //      parameters
            //      - https://prizm.cemetech.net/Technical_Documentation/Display/ for some basic
            //      display specs
            //      - https://git.planet-casio.com/Lephenixnoir/mq for a more complete Prizm
            //      emulator. Due to its nature (it runs gint add-ins, which are mostly independent
            //      from the Prizm OS), it mostly aims at emulating hardware, and not OS
            //      interfaces.A
            //      - https://shared-ptr.com/sh_insns.html for a general SHx instruction set. If
            //      you want to implement more instructions, ONLY follow those marked as SH4A or
            //      DSP. Do NOT try to implement ANY instructions linked to floating-point
            //      arithmetic.
            //
            let syscall_id: u32 = sh4::CPU.r[0];
            fprintf(stream, c!("UMIMPLEMENTED SYSCALL ID: %04X\n"), syscall_id as c_uint);
            panic!();
        }
        if !sh4::step(false) {
            fprintf(stream, c!("Umimplemented instruction 0x%04X @ PC=%08X\n"), sh4::read16(sh4::CPU.pc) as c_uint, sh4::CPU.pc);
            return None
        }
    }
    let code = sh4::CPU.r[0] as c_uint;
    printf(c!("\nExited with code %d\n"), code);
    if stdout_path.is_some() {
        fclose(stream);
    }

    if code != 0 {
        return None;
    }
    Some(())
}
