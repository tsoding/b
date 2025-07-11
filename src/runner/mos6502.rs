use crate::nob::*;
use crate::crust::libc::*;
use core::ffi::*;

// load directly after stack page
pub const DEFAULT_LOAD_OFFSET: u16 = 0x0200;

#[derive(Clone, Copy)]
pub struct Config {
    pub load_offset: u16,
}

pub mod fake6502 {
    use core::mem::zeroed;
    use super::*;

    pub static mut MEMORY: [u8; 1<<16] = unsafe { zeroed() };
    pub static mut stdout: *mut FILE = unsafe { zeroed() };

    pub unsafe fn load_rom_at(rom: String_Builder, offset: u16) {
        for i in 0..rom.count {
            MEMORY[i + offset as usize] = *rom.items.add(i) as u8;
        }
    }

    #[no_mangle]
    pub unsafe extern "C" fn read6502(address: u16) -> u8 {
        // https://www.sbprojects.net/projects/apple1/wozmon.php
        if address == 0xD011 {
            // KBDCR: always set, key always available
            // TODO: this is not very accurate, real Apple-1
            // progams would use this bit to check if key was
            // pressed, we would have to check here if a byte
            // is in stdin queue.
            MEMORY[address as usize] |= 0b1000_0000;
        } else if address == 0xD010 {
            // TODO: if we want to be completely accurate,
            // we would have to set bit 7 here and filter it
            // out in B.
            MEMORY[address as usize] = getchar() as i8 as u8;
        }

        MEMORY[address as usize]
    }

    #[no_mangle]
    pub unsafe extern "C" fn write6502(address: u16, mut value: u8) {
        // https://www.sbprojects.net/projects/apple1/wozmon.php
        if address == 0xD012 { // DSP
            // accept immediatetely => clear bit 7
            value &= 0b0111_1111;
            fprintf(stdout, c!("%c"), value as c_int);
        }

        if address == 0x0206 {
            printf(c!("HERE! [0x0206 := $%x], pc := %p\n"), value as c_int, pc as c_int);
        }

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

pub unsafe fn run_impl(output: *mut String_Builder, config: Config, argv_addr: u16, stdout: *mut FILE) -> Option<()> {
    fake6502::load_rom_at(*output, config.load_offset);
    fake6502::reset();
    fake6502::pc = config.load_offset;
    fake6502::stdout = stdout;

    // set reset to $0000 to exit on reset
    fake6502::MEMORY[0xFFFC] = 0;
    fake6502::MEMORY[0xFFFD] = 0;

    while fake6502::pc != 0 { // The convetion is stop executing when pc == $0000
        let prev_sp = fake6502::sp & 0xFF;
        let opcode  = fake6502::MEMORY[fake6502::pc as usize];
        fake6502::step();

        // printf(c!("pc=%p\n"), fake6502::pc as c_int);

        let curr_sp = fake6502::sp & 0xFF;
        if opcode == 0x48 && curr_sp > prev_sp { // PHA instruction
            log(Log_Level::ERROR, c!("Stack overflow detected"));
            log(Log_Level::ERROR, c!("SP changed from $%02X to $%02X after PHA instruction"), prev_sp as c_uint, curr_sp as c_uint);
            return None;
        }
    }
    // print exit code (in Y:A)
    let code = ((fake6502::y as c_uint) << 8) | fake6502::a as c_uint;
    log(Log_Level::INFO, c!("Exited with code %hd"), code);

    if code != 0 {
        return None;
    }
    Some(())
}

pub unsafe fn run(output: *mut String_Builder, config: Config, program_path: *const c_char, argv_addr: Option<u16>, stdout_path: Option<*const c_char>) -> Option<()> {
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
    let result = run_impl(output, config, argv_addr, stdout);
    if stdout_path.is_some() {
        fclose(stdout);
    }
    result
}
