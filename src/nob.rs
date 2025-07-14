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

pub unsafe fn da_last<T>(xs: *const Array<T>) -> Option<*const T> {
    if (*xs).count > 0 {
        Some((*xs).items.add((*xs).count-1))
    } else {
        None
    }
}

pub unsafe fn da_last_mut<T>(xs: *mut Array<T>) -> Option<*mut T> {
    if (*xs).count > 0 {
        Some((*xs).items.add((*xs).count-1))
    } else {
        None
    }
}

pub unsafe fn da_slice<T>(xs: Array<T>) -> *mut [T] {
    if xs.items.is_null() {
        // Caught by the debug mode unsafe precondition runtime check (I think it automatically enables when you do -C opt-level=0):
        // > panicked: unsafe precondition(s) violated: slice::from_raw_parts_mut requires the pointer to be aligned and non-null,
        // > and the total size of the slice not to exceed `isize::MAX`
        // The docs for slice::from_raw_parts_mut confirm:
        // > `data` must be non-null, valid for both reads and writes for `len * size_of::<T>()` many bytes, and it must be properly aligned.
        &mut []
    } else {
        slice::from_raw_parts_mut(xs.items, xs.count)
    }
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
    ($cmd:expr, $($arg:expr),+ $(,)?) => {
        $(da_append($cmd, $arg);)+
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

pub type File_Paths = Array<*const c_char>;

#[cfg(target_os = "windows")]
type Fd = *mut c_void;
#[cfg(not(target_os = "windows"))]
type Fd = c_int;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Cmd_Redirect {
    pub fdin: *mut Fd,
    pub fdout: *mut Fd,
    pub fderr: *mut Fd,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum Log_Level {
    INFO = 0,
    WARNING,
    ERROR,
    NO_LOGS,
}

extern "C" {
    #[link_name = "nob_temp_sprintf"]
    pub fn temp_sprintf(format: *const c_char, ...) -> *mut c_char;
    #[link_name = "nob_temp_save"]
    pub fn temp_save() -> usize;
    #[link_name = "nob_temp_rewind"]
    pub fn temp_rewind(checkpoint: usize);
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
    #[link_name = "nob_mkdir_if_not_exists"]
    pub fn mkdir_if_not_exists(path: *const c_char) -> bool;
    #[link_name = "nob_read_entire_dir"]
    pub fn read_entire_dir(parent: *const c_char, children: *mut File_Paths) -> bool;
    #[link_name = "nob_cmd_run_sync_redirect_and_reset"]
    pub fn cmd_run_sync_redirect_and_reset(cmd: *mut Cmd, redirect: Cmd_Redirect) -> bool;
    #[link_name = "nob_fd_open_for_write"]
    pub fn fd_open_for_write(path: *const c_char) -> Fd;
    #[link_name = "nob_log"]
    pub fn log(level: Log_Level, fmt: *const c_char, ...);
    #[link_name = "nob_minimal_log_level"]
    pub static mut minimal_log_level: Log_Level;
}

pub unsafe fn write_entire_file(path: *const c_char, data: *const c_void, size: usize) -> Option<()> {
    extern "C" {
        #[link_name = "nob_write_entire_file"]
        pub fn nob_write_entire_file(path: *const c_char, data: *const c_void, size: usize) -> bool;
    }
    if nob_write_entire_file(path, data, size) {
        Some(())
    } else {
        None
    }
}

pub unsafe fn read_entire_file(path: *const c_char, sb: *mut String_Builder) -> Option<()> {
    extern "C" {
        #[link_name = "nob_read_entire_file"]
        pub fn nob_read_entire_file(path: *const c_char, sb: *mut String_Builder) -> bool;
    }
    if nob_read_entire_file(path, sb) {
        Some(())
    } else {
        None
    }
}

pub unsafe fn file_exists(file_path: *const c_char) -> Option<bool> {
    extern "C" {
        #[link_name = "nob_file_exists"]
        pub fn nob_file_exists(file_path: *const c_char) -> c_int;
    }
    let exists = nob_file_exists(file_path);
    if exists < 0 {
        None
    } else {
        Some(exists > 0)
    }
}


// TODO: This is a generally useful function. Consider making it a part of nob.h
pub unsafe fn temp_strip_suffix(s: *const c_char, suffix: *const c_char) -> Option<*const c_char> {
    let mut sv = sv_from_cstr(s);
    let suffix_len = libc::strlen(suffix);
    if sv_end_with(sv, suffix) {
        sv.count -= suffix_len;
        Some(temp_sv_to_cstr(sv))
    } else {
        None
    }
}
