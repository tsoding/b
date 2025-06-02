// This is a module that facilitates Crust-style programming - https://github.com/tsoding/crust
use crate::crust::libc::*;
use core::panic::PanicInfo;
use core::ffi::*;

#[macro_export]
macro_rules! c {
    ($l:expr) => {
        concat!($l, "\0").as_ptr() as *const c_char
    }
}

#[macro_use]
pub mod libc {
    use core::ffi::*;

    pub type FILE = c_void;

    extern "C" {
        #[link_name = "get_stdin"]
        pub fn stdin() -> *mut FILE;
        #[link_name = "get_stdout"]
        pub fn stdout() -> *mut FILE;
        #[link_name = "get_stderr"]
        pub fn stderr() -> *mut FILE;
        pub fn strcmp(s1: *const c_char, s2: *const c_char) -> c_int;
        pub fn strchr(s: *const c_char, c: c_int) -> *const c_char;
        pub fn strlen(s: *const c_char) -> usize;
        pub fn abort() -> !;
        pub fn strdup(s: *const c_char) -> *mut c_char;
        pub fn printf(fmt: *const c_char, ...) -> c_int;
        pub fn fprintf(stream: *mut FILE, fmt: *const c_char, ...) -> c_int;
        pub fn memset(dest: *mut c_void, byte: c_int, size: usize) -> c_int;
    }

    // count is the amount of items, not bytes
    pub unsafe fn realloc_items<T>(ptr: *mut T, count: usize) -> *mut T {
        extern "C" {
            #[link_name = "realloc"]
            fn realloc_raw(ptr: *mut c_void, size: usize) -> *mut c_void;
        }
        realloc_raw(ptr as *mut c_void, size_of::<T>()*count) as *mut T
    }

    pub unsafe fn free<T>(ptr: *mut T) {
        extern "C" {
            #[link_name = "free"]
            fn free_raw(ptr: *mut c_void);
        }
        free_raw(ptr as *mut c_void);
    }
}

#[panic_handler]
pub unsafe fn panic_handler(info: &PanicInfo) -> ! {
    // TODO: What's the best way to implement the panic handler within the Crust spirit
    //   PanicInfo must be passed by reference.
    if let Some(location) = info.location() {
        fprintf(stderr(), c!("%.*s:%d: "), location.file().len(), location.file().as_ptr(), location.line());
    }
    fprintf(stderr(), c!("panicked"));
    if let Some(message) = info.message().as_str() {
        fprintf(stderr(), c!(": %.*s"), message.len(), message.as_ptr());
    }
    fprintf(stderr(), c!("\n"));
    abort()
}

#[export_name="main"]
pub unsafe extern "C" fn crust_entry_point(argc: i32, argv: *mut*mut c_char) -> i32 {
    match crate::main(argc, argv) {
        Some(()) => 0,
        None => 1,
    }
}

#[no_mangle]
pub unsafe fn rust_eh_personality() {
    // TODO: Research more what this is used for. Maybe we could put something useful in here.
}
