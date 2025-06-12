use core::ffi::*;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Arena {
    pub begin: *const c_void,
    pub end: *const c_void,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Arena_Mark {
    pub region: *const c_void,
    pub count: usize,
}

extern "C" {
    #[link_name = "arena_reset"]
    pub fn reset(a: *mut Arena) -> c_void;
    #[link_name = "arena_strdup"]
    pub fn strdup(a: *mut Arena, cstr: *const c_char) -> *mut c_char;
    #[link_name = "arena_snapshot"]
    pub fn snapshot(a: *mut Arena) -> Arena_Mark;
    #[link_name = "arena_rewind"]
    pub fn rewind(a: *mut Arena, m: Arena_Mark) -> c_void;
    #[link_name = "arena_sprintf"]
    pub fn sprintf(a: *mut Arena, format: *const c_char, ...) -> *mut c_char;
}
