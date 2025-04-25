use core::ffi::*;
use crate::crust::libc::*;

extern "C" {
    pub fn flag_bool(name: *const c_char, def: bool, desc: *const c_char) -> *mut bool;
    pub fn flag_str(name: *const c_char, def: *const c_char, desc: *const c_char) -> *mut*mut c_char;
    pub fn flag_parse(argc: c_int, argv: *mut*mut c_char) -> bool;
    pub fn flag_rest_argc() -> c_int;
    pub fn flag_rest_argv() -> *mut*mut c_char;
    pub fn flag_print_error(stream: *mut FILE);
    pub fn flag_print_options(stream: *mut FILE);
    pub fn flag_program_name() -> *const c_char;
}

pub unsafe fn flag_name<T>(val: *mut T) -> *mut c_char {
    extern "C" {
        #[link_name="flag_name"]
        pub fn flag_name_raw(val: *mut c_void) -> *mut c_char;
    }
    flag_name_raw(val as *mut c_void)
}
