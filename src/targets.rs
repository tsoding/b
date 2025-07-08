use core::ffi::*;
use crate::strcmp;

// TODO: add wasm target
//   Don't touch this TODO! @rexim wants to stream it!
enum_with_order! {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Target in TARGET_ORDER {
        Gas_x86_64_Windows,
        Gas_x86_64_Linux,
        Gas_x86_64_Darwin,
        Gas_AArch64_Linux,
        Gas_AArch64_Darwin,
        Uxn,
        Mos6502,
        Fasm_x86_64_Windows,
        Fasm_x86_64_Linux,
    }
}

impl Target {
    pub unsafe fn name(self) -> *const c_char {
        match self {
            Self::Fasm_x86_64_Windows => c!("fasm-x86_64-windows"),
            Self::Fasm_x86_64_Linux   => c!("fasm-x86_64-linux"),
            Self::Gas_x86_64_Windows  => c!("gas-x86_64-windows"),
            Self::Gas_x86_64_Linux    => c!("gas-x86_64-linux"),
            Self::Gas_x86_64_Darwin   => c!("gas-x86_64-darwin"),
            Self::Gas_AArch64_Linux   => c!("gas-aarch64-linux"),
            Self::Gas_AArch64_Darwin  => c!("gas-aarch64-darwin"),
            Self::Uxn                 => c!("uxn"),
            Self::Mos6502             => c!("6502"),
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
