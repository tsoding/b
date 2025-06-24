use core::ffi::*;
use crate::strcmp;

// TODO: add wasm target
//   Don't touch this TODO! @rexim wants to stream it!
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Target {
    Fasm_x86_64_Windows,
    Fasm_x86_64_Linux,
    Gas_x86_64_Windows,
    Gas_x86_64_Linux,
    Gas_AArch64_Linux,
    Gas_SH4_Prizm,
    Uxn,
    Mos6502,
}

#[derive(Clone, Copy)]
pub enum Os {
    Linux,
    Windows,
}

#[derive(Clone, Copy)]
pub struct Target_Name {
    pub name: *const c_char,
    pub target: Target,
}

pub const TARGET_NAMES: *const [Target_Name] = &[
    Target_Name { name: c!("fasm-x86_64-windows"), target: Target::Fasm_x86_64_Windows },
    Target_Name { name: c!("fasm-x86_64-linux"),   target: Target::Fasm_x86_64_Linux   },
    Target_Name { name: c!("gas-x86_64-windows"),  target: Target::Gas_x86_64_Windows  },
    Target_Name { name: c!("gas-x86_64-linux"),    target: Target::Gas_x86_64_Linux    },
    Target_Name { name: c!("gas-aarch64-linux"),   target: Target::Gas_AArch64_Linux   },
    Target_Name { name: c!("gas-sh4dsp-prizm"),    target: Target::Gas_SH4_Prizm       },
    Target_Name { name: c!("uxn"),                 target: Target::Uxn                 },
    Target_Name { name: c!("6502"),                target: Target::Mos6502             },
];

pub unsafe fn name_of_target(target: Target) -> Option<*const c_char> {
    for i in 0..TARGET_NAMES.len() {
        if target == (*TARGET_NAMES)[i].target {
            return Some((*TARGET_NAMES)[i].name);
        }
    }
    None
}

pub unsafe fn target_by_name(name: *const c_char) -> Option<Target> {
    for i in 0..TARGET_NAMES.len() {
        if strcmp(name, (*TARGET_NAMES)[i].name) == 0 {
            return Some((*TARGET_NAMES)[i].target);
        }
    }
    None
}

pub unsafe fn target_word_size(target: Target) -> u64 {
    match target {
        Target::Fasm_x86_64_Windows => 8,
        Target::Fasm_x86_64_Linux   => 8,
        Target::Gas_x86_64_Windows  => 8,
        Target::Gas_x86_64_Linux    => 8,
        Target::Gas_AArch64_Linux   => 8,
        Target::Uxn                 => 2,
        Target::Mos6502             => 2,
        Target::Gas_SH4_Prizm       => 4,
    }
}
