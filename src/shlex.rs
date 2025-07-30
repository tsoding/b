use core::ffi::*;

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Shlex {
    pub source: *const c_char,
    pub source_end: *const c_char,
    pub point: *const c_char,

    pub string: *mut c_char,
    pub string_count: usize,
    pub string_capacity: usize,
}

extern "C" {
    pub fn shlex_init(s: *mut Shlex, source: *const c_char, source_end: *const c_char);
    pub fn shlex_next(s: *mut Shlex) -> *mut c_char;
    pub fn shlex_append_quoted(s: *mut Shlex, cstr: *const c_char);
    pub fn shlex_join(s: *mut Shlex) -> *mut c_char;
    pub fn shlex_free(s: *mut Shlex);
}
