use core::mem::zeroed;
use crate::nob::*;
use core::ffi::*;
use crate::printf;

static mut MEMORY: [u8; 1<<16] = unsafe { zeroed() };

pub unsafe fn load_rom_at(rom: String_Builder, offset: u16) {
    for i in 0..rom.count {
        MEMORY[i + offset as usize] = *rom.items.add(i) as u8;
    }
}

#[no_mangle]
pub unsafe extern "C" fn read6502(address: u16) -> u8 {
    MEMORY[address as usize]
}

#[no_mangle]
pub unsafe extern "C" fn write6502(address: u16, value: u8) {
    MEMORY[address as usize] = value;
}

pub unsafe fn extrn_functions() {
    match pc {
        0xFFEF => { // Wozmon ECHO
                    // Char is in the `a` register.
            printf(c!("%c"), a as c_uint);
            rts();

        }
        _ => {}
    }
}

extern "C" {
    #[link_name = "reset6502"]
    pub fn reset();
    #[link_name = "step6502"]
    pub fn step();
    pub fn rts();
    pub static mut pc: u16;
    pub static mut a: u8;
}
