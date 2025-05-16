use core::ffi::*;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Arena {
    pub begin: *const c_void,
    pub end: *const c_void,
}

extern "C" {
    #[link_name = "arena_strdup"]
    pub fn strdup(a: *mut Arena, cstr: *const c_char) -> *mut c_char;
    #[link_name = "arena_alloc"]
    pub fn alloc(a: *mut Arena, size_bytes: usize) -> *mut c_void;
}
