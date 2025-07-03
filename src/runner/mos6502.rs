use crate::nob::*;
use crate::crust::libc::*;
use core::ffi::*;

pub const DEFAULT_LOAD_OFFSET: u16 = 0x8000;

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
        pub static mut sp: u16;
        pub static mut a: u8;
        pub static mut x: u8;
        pub static mut y: u8;
    }

}

pub unsafe fn run_impl(output: *mut String_Builder, config: Config, stdout: *mut FILE) -> Option<()> {
    fake6502::load_rom_at(*output, config.load_offset);
    fake6502::reset();
    fake6502::pc = config.load_offset;

    // set reset to $0000 to exit on reset
    fake6502::MEMORY[0xFFFC] = 0;
    fake6502::MEMORY[0xFFFD] = 0;

    while fake6502::pc != 0 { // The convetion is stop executing when pc == $0000
        let prev_sp = fake6502::sp & 0xFF;
        let opcode  = fake6502::MEMORY[fake6502::pc as usize];
        fake6502::step();

        let curr_sp = fake6502::sp & 0xFF;
        if opcode == 0x48 && curr_sp > prev_sp { // PHA instruction
            printf(c!("[FATAL] Stack overflow detected\n[FATAL] SP changed from $%02X to $%02X after PHA instruction\n"),
                   prev_sp as c_uint, curr_sp as c_uint);
            return None;
        }

        if fake6502::pc == 0xFFEF { // Emulating wozmon ECHO routine
            fprintf(stdout, c!("%c"), fake6502::a as c_uint);
            fake6502::rts();
        }
    }
    // print exit code (in Y:A)
    let code = ((fake6502::y as c_uint) << 8) | fake6502::a as c_uint;
    fprintf(stderr(), c!("Exited with code %hd\n"), code);

    if code != 0 {
        return None;
    }
    Some(())
}

pub unsafe fn run(output: *mut String_Builder, config: Config, program_path: *const c_char, stdout_path: Option<*const c_char>) -> Option<()> {
    (*output).count = 0;
    read_entire_file(program_path, output)?;

    let stdout = if let Some(stdout_path) = stdout_path {
        let stdout = fopen(stdout_path, c!("wb"));
        if stdout.is_null() {
            return None
        }
        stdout
    } else {
        stdout()
    };
    let result = run_impl(output, config, stdout);
    if stdout_path.is_some() {
        fclose(stdout);
    }
    result
}
