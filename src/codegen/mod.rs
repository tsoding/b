use core::ffi::*;
use crate::strcmp;

pub mod gas_aarch64_linux;
pub mod fasm_x86_64;
pub mod ir;
pub mod uxn;

// TODO: add wasm target
//   Don't touch this TODO! @rexim wants to stream it!
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Target {
    Fasm_x86_64_Windows,
    Fasm_x86_64_Linux,
    Gas_AArch64_Linux,
    Uxn,
    IR,
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

// TODO: How do we make this place fail compiling when you add a new target above?
//   Maybe we can introduce some sort of macro that generates all of this from a single list of targets
pub const TARGET_NAMES: *const [Target_Name] = &[
    Target_Name { name: c!("fasm-x86_64-windows"), target: Target::Fasm_x86_64_Windows },
    Target_Name { name: c!("fasm-x86_64-linux"),   target: Target::Fasm_x86_64_Linux   },
    Target_Name { name: c!("gas-aarch64-linux"),   target: Target::Gas_AArch64_Linux   },
    Target_Name { name: c!("uxn"),                 target: Target::Uxn                 },
    Target_Name { name: c!("ir"),                  target: Target::IR                  },
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
