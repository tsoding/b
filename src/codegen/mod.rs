use core::ffi::*;
use crate::strcmp;

pub mod gas_aarch64_linux;
pub mod fasm_x86_64_linux;
pub mod ir;
pub mod uxn;

macro_rules! define_targets {
    ($($variant:ident => $name:expr),* $(,)?) => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        /// Represents the various targets supported by the code generator.
        /// Each variant corresponds to a specific target platform or architecture.
        pub enum Target {
            $($variant),*
        }
        
        #[derive(Clone, Copy)]
        pub struct Target_Name {
            pub name: *const c_char,
            pub target: Target,
        }
        
        pub const TARGET_NAMES: &[Target_Name] = &[
            $(
                Target_Name {
                    name: c!($name),
                    target: Target::$variant,
                }
            ),*
            ];
        };
    }
    
// TODO: add wasm target
//   Don't touch this TODO! @rexim wants to stream it!
define_targets! {
    Fasm_x86_64_Linux => "fasm-x86_64-linux",
    Gas_AArch64_Linux => "gas-aarch64-linux",
    Uxn               => "uxn",
    IR                => "ir",
}
    
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
