use core::ffi::*;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Jimp {
    pub file_path: *const c_char,
    pub start: *const c_char,
    pub end: *const c_char,
    pub point: *const c_char,

    pub token: c_int,
    pub token_start: *const c_char,

    pub string: *mut c_char,
    pub string_count: usize,
    pub string_capacity: usize,
    pub number: c_double,
    pub boolean: bool,
}

pub unsafe fn jimp_boolean(jimp: *mut Jimp) -> Option<()> {
    extern "C" {
        #[link_name="jimp_boolean"]
        pub fn jimp_boolean_raw(jimp: *mut Jimp) -> bool;
    }
    if jimp_boolean_raw(jimp) {
        Some(())
    } else {
        None
    }
}

pub unsafe fn jimp_number(jimp: *mut Jimp) -> Option<()> {
    extern "C" {
        #[link_name="jimp_number"]
        pub fn jimp_number_raw(jimp: *mut Jimp) -> bool;
    }
    if jimp_number_raw(jimp) {
        Some(())
    } else {
        None
    }
}

pub unsafe fn jimp_string(jimp: *mut Jimp) -> Option<()> {
    extern "C" {
        #[link_name="jimp_string"]
        pub fn jimp_string_raw(jimp: *mut Jimp) -> bool;
    }
    if jimp_string_raw(jimp) {
        Some(())
    } else {
        None
    }
}

pub unsafe fn jimp_object_begin(jimp: *mut Jimp) -> Option<()> {
    extern "C" {
        #[link_name="jimp_object_begin"]
        pub fn jimp_object_begin_raw(jimp: *mut Jimp) -> bool;
    }
    if jimp_object_begin_raw(jimp) {
        Some(())
    } else {
        None
    }
}

pub unsafe fn jimp_object_member(jimp: *mut Jimp) -> Option<()> {
    extern "C" {
        #[link_name="jimp_object_member"]
        pub fn jimp_object_member_raw(jimp: *mut Jimp) -> bool;
    }
    if jimp_object_member_raw(jimp) {
        Some(())
    } else {
        None
    }
}

pub unsafe fn jimp_object_end(jimp: *mut Jimp) -> Option<()> {
    extern "C" {
        #[link_name="jimp_object_end"]
        pub fn jimp_object_end_raw(jimp: *mut Jimp) -> bool;
    }
    if jimp_object_end_raw(jimp) {
        Some(())
    } else {
        None
    }
}

extern "C" {
    pub fn jimp_begin(jimp: *mut Jimp, file_path: *const c_char, input: *const c_char, input_size: usize);
    pub fn jimp_unknown_member(jimp: *mut Jimp);
    pub fn jimp_diagf(jimp: *mut Jimp, fmt: *const c_char, ...);
}
