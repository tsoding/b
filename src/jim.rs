use core::ffi::*;

pub type Jim_Sink = *mut c_void;
pub type Jim_Write = unsafe extern "C" fn(ptr: *const c_void, size: usize, nmemb: usize, sink: Jim_Sink) -> usize;

#[derive(Copy, Clone)]
#[repr(C)]
pub enum Jim_Error {
    JIM_OK = 0,
    JIM_WRITE_ERROR,
    JIM_SCOPES_OVERFLOW,
    JIM_SCOPES_UNDERFLOW,
    JIM_OUT_OF_SCOPE_KEY,
    JIM_DOUBLE_KEY,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub enum Jim_Scope_Kind {
    JIM_ARRAY_SCOPE,
    JIM_OBJECT_SCOPE,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Jim_Scope {
    pub kind: Jim_Scope_Kind,
    pub tail: c_int,
    pub key: c_int,
}

pub const JIM_SCOPES_CAPACITY: usize = 128;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Jim {
    pub sink: Jim_Sink,
    pub write: Jim_Write,
    pub error: Jim_Error,
    pub scopes: [Jim_Scope; JIM_SCOPES_CAPACITY],
    pub scopes_size: usize,
}

extern "C" {
    pub fn jim_object_begin(jim: *mut Jim);
    pub fn jim_member_key(jim: *mut Jim, s: *const c_char);
    pub fn jim_object_end(jim: *mut Jim);
    pub fn jim_string(jim: *mut Jim, s: *const c_char);
}
