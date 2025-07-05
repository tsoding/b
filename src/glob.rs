use core::ffi::*;

#[repr(C)]
pub enum Glob_Result {
    OOM_ERROR      = -4,
    ENCODING_ERROR = -3,
    SYNTAX_ERROR   = -2,
    UNMATCHED      = -1,
    MATCHED        =  0,
}

extern "C" {
    pub fn glob_utf8(pattern: *const c_char, text: *const c_char) -> Glob_Result;
}
