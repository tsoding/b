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

#[macro_export]
macro_rules! enum_with_order {
    (
        #[derive($($traits:tt)*)]
        enum $name:ident in $order_name:ident {
            $($items:tt)*
        }
    ) => {
        #[derive($($traits)*)]
        pub enum $name {
            $($items)*
        }
        pub const $order_name: *const [$name] = {
            use $name::*;
            &[$($items)*]
        };
    }
}

pub unsafe fn slice_contains<Value: PartialEq>(slice: *const [Value], needle: *const Value) -> bool {
    for i in 0..slice.len() {
        if (*slice)[i] == *needle {
            return true
        }
    }
    false
}

pub unsafe fn assoc_lookup_cstr_mut<Value>(assoc: *mut [(*const c_char, Value)], needle: *const c_char) -> Option<*mut Value> {
    for i in 0..assoc.len() {
        if strcmp((*assoc)[i].0, needle) == 0 {
            return Some(&mut (*assoc)[i].1);
        }
    }
    None
}

pub unsafe fn assoc_lookup_cstr<Value>(assoc: *const [(*const c_char, Value)], needle: *const c_char) -> Option<*const Value> {
    for i in 0..assoc.len() {
        if strcmp((*assoc)[i].0, needle) == 0 {
            return Some(&(*assoc)[i].1);
        }
    }
    None
}

pub unsafe fn assoc_lookup_mut<Key, Value>(assoc: *mut [(Key, Value)], needle: *const Key) -> Option<*mut Value>
where Key: PartialEq
{
    for i in 0..assoc.len() {
        if (*assoc)[i].0 == *needle {
            return Some(&mut (*assoc)[i].1);
        }
    }
    None
}

pub unsafe fn assoc_lookup<Key, Value>(assoc: *const [(Key, Value)], needle: *const Key) -> Option<*const Value>
where Key: PartialEq
{
    for i in 0..assoc.len() {
        if (*assoc)[i].0 == *needle {
            return Some(&(*assoc)[i].1);
        }
    }
    None
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
        pub fn fopen(pathname: *const c_char, mode: *const c_char) -> *mut FILE;
        pub fn fclose(stream: *mut FILE) -> c_int;
        pub fn strcmp(s1: *const c_char, s2: *const c_char) -> c_int;
        pub fn strchr(s: *const c_char, c: c_int) -> *const c_char;
        pub fn strrchr(s: *const c_char, c: c_int) -> *const c_char;
        pub fn strlen(s: *const c_char) -> usize;
        pub fn strtoull(nptr: *const c_char, endptr: *mut*mut c_char, base: c_int) -> c_ulonglong;
        pub fn fwrite(ptr: *const c_void, size: usize, nmemb: usize, stream: *mut FILE) -> usize;
        pub fn getchar() -> c_int;

        pub fn abort() -> !;
        pub fn strdup(s: *const c_char) -> *mut c_char;
        pub fn strncpy(dst: *mut c_char, src: *const c_char, dsize: usize) -> *mut c_char;
        pub fn printf(fmt: *const c_char, ...) -> c_int;
        pub fn fprintf(stream: *mut FILE, fmt: *const c_char, ...) -> c_int;
        pub fn memset(dest: *mut c_void, byte: c_int, size: usize) -> c_int;
        pub fn isspace(c: c_int) -> c_int;
        pub fn isalpha(c: c_int) -> c_int;
        pub fn isalnum(c: c_int) -> c_int;
        pub fn isdigit(c: c_int) -> c_int;
        pub fn isprint(c: c_int) -> c_int;
        pub fn tolower(c: c_int) -> c_int;
        pub fn toupper(c: c_int) -> c_int;
        pub fn qsort(base: *mut c_void, nmemb: usize, size: usize, compar: unsafe extern "C" fn(*const c_void, *const c_void) -> c_int);
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
