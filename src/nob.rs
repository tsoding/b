use core::ffi::*;
use core::slice;
use crate::crust::libc;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Array<T> {
    pub items: *mut T,
    pub count: usize,
    pub capacity: usize,
}

pub unsafe fn da_last<T>(xs: *const Array<T>) -> *const T {
    assert!((*xs).count > 0);
    (*xs).items.add((*xs).count-1)
}

pub unsafe fn da_last_mut<T>(xs: *mut Array<T>) -> *mut T {
    assert!((*xs).count > 0);
    (*xs).items.add((*xs).count-1)
}

pub unsafe fn da_slice<T>(xs: Array<T>) -> *mut [T] {
    slice::from_raw_parts_mut(xs.items, xs.count)
}

pub unsafe fn da_append<T>(xs: *mut Array<T>, item: T) {
    if (*xs).count >= (*xs).capacity {
        if (*xs).capacity == 0 {
            (*xs).capacity = 256;
        } else {
            (*xs).capacity *= 2;
        }
        (*xs).items = libc::realloc_items((*xs).items, (*xs).capacity);

        // ZERO INITILIZE NEWLY ALLOCATED MEMORY
        let size = size_of::<T>() * ((*xs).capacity - (*xs).count);
        libc::memset((*xs).items.add((*xs).count) as _ , 0, size);
    }
    *((*xs).items.add((*xs).count)) = item;
    (*xs).count += 1;
}

pub unsafe fn da_append_many<T: Clone + Copy>(xs: *mut Array<T>, items: *const [T]) {
    for i in 0..items.len() {
        da_append(xs, (*items)[i]);
    }
}

#[macro_export]
macro_rules! shift {
    ($ptr:ident, $len:ident) => {{
        let result = *$ptr;
        $ptr = $ptr.add(1);
        $len -= 1;
        result
    }};
}

pub type Cmd = Array<*const c_char>;

#[macro_export]
macro_rules! cmd_append {
    ($cmd:expr,) => {};
    ($cmd:expr, $arg:expr, $($tail:tt)*) => {
        da_append($cmd, $arg);
        cmd_append!($cmd, $($tail)*);
    }
}

extern "C" {
    #[link_name = "nob_cmd_run_sync_and_reset"]
    pub fn cmd_run_sync_and_reset(cmd: *mut Cmd) -> bool;
}

pub type String_Builder = Array<c_char>;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct String_View {
    pub count: usize,
    pub data: *const c_char,
}

extern "C" {
    #[link_name = "nob_read_entire_file"]
    pub fn read_entire_file(path: *const c_char, sb: *mut String_Builder) -> bool;
    #[link_name = "nob_write_entire_file"]
    pub fn write_entire_file(path: *const c_char, data: *const c_void, size: usize) -> bool;
    #[link_name = "nob_temp_sprintf"]
    pub fn temp_sprintf(format: *const c_char, ...) -> *mut c_char;
    #[link_name = "nob_sb_appendf"]
    pub fn sb_appendf(sb: *mut String_Builder, fmt: *const c_char, ...) -> c_int;
    #[link_name = "nob_sv_from_cstr"]
    pub fn sv_from_cstr(cstr: *const c_char) -> String_View;
    #[link_name = "nob_sv_end_with"]
    pub fn sv_end_with(sv: String_View, cstr: *const c_char) -> bool;
    #[link_name = "nob_temp_sv_to_cstr"]
    pub fn temp_sv_to_cstr(sv: String_View) -> *const c_char;
    #[link_name = "nob_sv_from_parts"]
    pub fn sv_from_parts(data: *const c_char, count: usize) -> String_View;
    #[link_name = "nob_sv_starts_with"]
    pub fn sv_starts_with(sv: String_View, expected_prefix: String_View) -> bool;
}
