use core::ffi::*;
use crate::strcmp;

pub mod gas_aarch64_linux;
pub mod fasm_x86_64_linux;
pub mod ir;
pub mod html_js;

macro_rules! generate_targets {
    [$({ $variant:ident: $name:literal },)+] => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub enum Target {
            $($variant,)+
        }

        pub const TARGET_NAMES: *const [*const c_char] = &[
            $(c!($name),)+
        ];

        pub unsafe fn name_of_target(target: Target) -> *const c_char {
            match target {
                $(Target::$variant => c!($name),)+
            }
        }

        pub unsafe fn target_by_name(name: *const c_char) -> Option<Target> {
            $(if strcmp(name, c!($name)) == 0 {
                return Some(Target::$variant);
            })+
            None
        }
    }
}

generate_targets! [
    { Fasm_x86_64_Linux: "fasm-x86_64-linux" },
    { Gas_AArch64_Linux: "gas-aarch64-linux" },
    {           Html_Js: "html-js"           },
    {                IR: "ir"                },
];
