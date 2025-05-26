use core::ffi::*;
use crate::strcmp;

pub mod gas_aarch64_linux;
pub mod fasm_x86_64_linux;
pub mod ir;
pub mod html_js;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Target {
    Fasm_x86_64_Linux,
    Gas_AArch64_Linux,
    Html_Js,
    IR,
}

#[derive(Clone, Copy)]
pub struct Target_Name {
    pub name: *const c_char,
    pub target: Target,
}

pub const TARGET_NAMES: *const [Target_Name] = &[
    Target_Name { name: c!("fasm-x86_64-linux"), target: Target::Fasm_x86_64_Linux },
    Target_Name { name: c!("gas-aarch64-linux"), target: Target::Gas_AArch64_Linux },
    Target_Name { name: c!("html-js"),           target: Target::Html_Js           },
    Target_Name { name: c!("ir"),                target: Target::IR                },
];

pub unsafe fn name_of_target(target: Target) -> Option<*const c_char> {
    for i in 0..(*TARGET_NAMES).len() {
        if target == (*TARGET_NAMES)[i].target {
            return Some((*TARGET_NAMES)[i].name);
        }
    }
    None
}

pub unsafe fn target_by_name(name: *const c_char) -> Option<Target> {
    for i in 0..(*TARGET_NAMES).len() {
        if strcmp(name, (*TARGET_NAMES)[i].name) == 0 {
            return Some((*TARGET_NAMES)[i].target);
        }
    }
    None
}
