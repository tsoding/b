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

pub unsafe fn glob_error_string(result: Glob_Result) -> *const c_char {
    match result {
        Glob_Result::OOM_ERROR      => c!("out of memory"),
        Glob_Result::ENCODING_ERROR => c!("encoding error"),
        Glob_Result::SYNTAX_ERROR   => c!("syntax error"),
        Glob_Result::UNMATCHED | Glob_Result::MATCHED => unreachable!(),
    }
}
