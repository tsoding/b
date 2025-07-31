use core::ffi::*;
use crate::strcmp;

// TODO: add wasm target
//   Don't touch this TODO! @rexim wants to stream it!
enum_with_order! {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Target in TARGET_ORDER {
        // TODO: maybe instead of Gas_ the prefix should be Gnu_, 'cause that makes more sense.
        Gas_x86_64_Windows,
        Gas_x86_64_Linux,
        Gas_x86_64_Darwin,
        Gas_AArch64_Linux,
        Gas_AArch64_Darwin,
        Gas_SH4_Prizm,
        Uxn,
        Mos6502_Posix,
        ILasm_Mono,
    }
}

impl Target {
    pub unsafe fn file_ext(self) -> *const c_char {
        match self {
            Self::Gas_x86_64_Windows => c!(".exe"),
            Self::Gas_x86_64_Linux   => c!(""),
            Self::Gas_x86_64_Darwin  => c!(""),
            Self::Gas_AArch64_Linux  => c!(""),
            Self::Gas_AArch64_Darwin => c!(""),
            Self::Uxn                => c!(".rom"),
            Self::Mos6502_Posix      => c!(".6502"),
            Self::ILasm_Mono         => c!(".exe"),
            Self::Gas_SH4_Prizm      => c!(".g3a"),
        }
    }

    pub unsafe fn name(self) -> *const c_char {
        match self {
            Self::Gas_x86_64_Windows  => c!("gas-x86_64-windows"),
            Self::Gas_x86_64_Linux    => c!("gas-x86_64-linux"),
            Self::Gas_x86_64_Darwin   => c!("gas-x86_64-darwin"),
            Self::Gas_AArch64_Linux   => c!("gas-aarch64-linux"),
            Self::Gas_SH4_Prizm       => c!("gas-sh4dsp-prizm"),
            Self::Gas_AArch64_Darwin  => c!("gas-aarch64-darwin"),
            Self::Uxn                 => c!("uxn"),
            Self::Mos6502_Posix       => c!("6502-posix"),
            Self::ILasm_Mono          => c!("ilasm-mono"),
        }
    }

    pub unsafe fn by_name(name: *const c_char) -> Option<Self> {
        for i in 0..TARGET_ORDER.len() {
            let target = (*TARGET_ORDER)[i];
            if strcmp(target.name(), name) == 0 {
                return Some(target);
            }
        }
        None
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Os {
    Linux,
    Windows,
    Darwin,
}
