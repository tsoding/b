use crate::nob::*;
use crate::crust::libc::*;
use core::ffi::*;
use core::mem::zeroed;

pub const DEFAULT_LOAD_OFFSET: u16 = 0xE000;

#[derive(Clone, Copy)]
pub struct Config {
    pub load_offset: u16,
}

pub mod fake6502 {
    use core::mem::zeroed;
    use crate::nob::*;

    pub static mut MEMORY: [u8; 1<<16] = unsafe { zeroed() };

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

    extern "C" {
        #[link_name = "reset6502"]
        pub fn reset();
        #[link_name = "step6502"]
        pub fn step();
        pub fn rts();
        pub static mut pc: u16;
        pub static mut a: u8;
        pub static mut y: u8;
    }

}

pub unsafe fn run(output: *mut String_Builder, config: Config, output_path: *const c_char) -> Option<()> {
    (*output).count = 0;
    if !read_entire_file(output_path, output) { return None; }

    fake6502::load_rom_at(*output, config.load_offset);
    fake6502::reset();
    fake6502::pc = config.load_offset;

    // set reset to $0000 to exit on reset
    fake6502::MEMORY[0xFFFC] = 0;
    fake6502::MEMORY[0xFFFD] = 0;

    while fake6502::pc != 0 { // The convetion is stop executing when pc == $0000
        fake6502::step();
        if fake6502::pc == 0xFFEF { // Emulating wozmon ECHO routine
            printf(c!("%c"), fake6502::a as c_uint);
            fake6502::rts();
        }
    }
    // print exit code (in Y:A)
    printf(c!("Exited with code %u\n"),
           ((fake6502::y as c_uint) << 8) | fake6502::a as c_uint);

    Some(())
}

pub unsafe fn run_async(config: Config, output_path: *const c_char) -> Proc {
    let pid = fork();

    if pid < 0 { return -1; }
    if pid != 0 { return pid; }

    // This is a child process
    let mut output: String_Builder = zeroed();
    if let Some(_) = run(&mut output, config, output_path) {
        exit(0);
        0
    } else {
        exit(1);
        1
    }
}
