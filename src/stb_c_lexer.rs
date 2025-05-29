#![allow(non_upper_case_globals)]
use core::ffi::{c_char, c_long, c_longlong, c_double, c_int};

#[macro_export]
macro_rules! non_soy_enum {
    (iota = $value:expr,) => {};
    (iota = $value:expr, $name:ident, $($tail:tt)*) => {
        pub const $name: c_long = $value;
        non_soy_enum!(iota = $value + 1, $($tail)*);
    };
}

non_soy_enum! {
    iota = 256              ,
    CLEX_eof                ,
    CLEX_parse_error        ,
    CLEX_intlit             ,
    CLEX_floatlit           ,
    CLEX_id                 ,
    CLEX_dqstring           ,
    CLEX_sqstring           ,
    CLEX_charlit            ,
    CLEX_eq                 ,
    CLEX_noteq              ,
    CLEX_lesseq             ,
    CLEX_greatereq          ,
    CLEX_andand             ,
    CLEX_oror               ,
    CLEX_shl                ,
    CLEX_shr                ,
    CLEX_plusplus           ,
    CLEX_minusminus         ,
    CLEX_pluseq             ,
    CLEX_minuseq            ,
    CLEX_muleq              ,
    CLEX_diveq              ,
    CLEX_modeq              ,
    CLEX_andeq              ,
    CLEX_oreq               ,
    CLEX_xoreq              ,
    CLEX_arrow              ,
    CLEX_eqarrow            ,
    CLEX_shleq              ,
    CLEX_shreq              ,
    CLEX_first_unused_token ,
}

#[repr(C)]
pub struct stb_lexer {
    // lexer variables
    pub input_stream   : *mut c_char,
    pub eof            : *mut c_char,
    pub parse_point    : *mut c_char,
    pub string_storage : *mut c_char,
    pub string_storage_len: c_int,

    // lexer parse location for error messages
    pub where_firstchar: *mut c_char,
    pub where_lastchar: *mut c_char,

    // lexer token variables
    pub token: c_long,
    pub real_number: c_double,
    pub int_number: c_longlong,
    pub string: *mut c_char,
    pub string_len: c_int,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct stb_lex_location {
    pub line_number: c_int,
    pub line_offset: c_int,
}

extern "C" {
    #[link_name="stb_c_lexer_init"]
    pub fn stb_c_lexer_init(lexer: *mut stb_lexer, input_stream: *const c_char, input_stream_end: *const c_char, string_store: *mut c_char, store_length: c_int);
    #[link_name="stb_c_lexer_get_token"]
    pub fn stb_c_lexer_get_token(lexer: *mut stb_lexer) -> c_int;
    #[link_name="stb_c_lexer_get_location"]
    pub fn stb_c_lexer_get_location(input_stream: *const c_char, hwere: *const c_char, loc: *mut stb_lex_location);
}
