use core::ffi::*;
use crate::arena::*;
use crate::crust::libc::*;

#[derive(Clone, Copy)]
pub struct Loc {
    pub input_path: *const c_char,
    pub line_number: c_int,
    pub line_offset: c_int,
}

#[derive(Clone, Copy)]
pub enum Token {
    OCurly,
    CCurly,
}

#[derive(Clone, Copy)]
pub struct Parse_Point {
    pub current: *mut c_char,
    pub line_start: *mut c_char,
    pub line_number: usize,
}

#[derive(Clone, Copy)]
pub struct Lexer {
    pub input_path: *mut c_char,
    pub input_stream: *mut c_char,
    pub eof: *mut c_char,
    pub parse_point: Parse_Point,

    pub string_storage: Arena,
    pub token: Token,
    pub string: *const c_char,
    pub number: c_long,
    pub loc: Loc,
}

pub unsafe fn skip_char(l: *mut Lexer) -> Option<c_char> {
    skip_char_if(l, |_| true)
}

pub unsafe fn skip_chars(l: *mut Lexer, mut n: usize) {
    while n > 0 {
        if let None = skip_char(l) {
            break;
        }
        n -= 1;
    }
}

pub unsafe fn skip_char_if(l: *mut Lexer, p: unsafe fn(c_char) -> bool) -> Option<c_char> {
    if is_eof(l) {
        return None
    }

    let x = *(*l).parse_point.current;
    if !p(x) {
        return None
    }

    (*l).parse_point.current = (*l).parse_point.current.add(1);
    if x == '\n' as c_char {
        (*l).parse_point.line_start = (*l).parse_point.current;
        (*l).parse_point.line_number += 1;
    }
    Some(x)
}

pub unsafe fn is_eof(l: *mut Lexer) -> bool {
    (*l).parse_point.current >= (*l).eof
}

pub unsafe fn skip_whitespaces(l: *mut Lexer) {
    while let Some(_) = skip_char_if(l, |x| isspace(x as i32) != 0) {}
}

pub const PUNCTS: *const [(*const c_char, Token)] = &[
    (c!("{"), Token::OCurly),
    (c!("}"), Token::CCurly),
];

pub unsafe fn skip_prefix(l: *mut Lexer, mut prefix: *const c_char) -> bool {
    let saved_point = (*l).parse_point;
    while *prefix != 0 {
        let Some(x) = skip_char(l) else {
            (*l).parse_point = saved_point;
            return false;
        };
        if x != *prefix {
            (*l).parse_point = saved_point;
            return false;
        }
        prefix = prefix.add(1)
    }
    true
}

pub unsafe fn loc(_l: *mut Lexer) -> Loc {
    todo!()
}

pub unsafe fn get_token(l: *mut Lexer) -> bool {
    skip_whitespaces(l);

    if is_eof(l) {
        return false;
    }

    for i in 0..PUNCTS.len() {
        let (prefix, token) = (*PUNCTS)[i];
        let loc = loc(l);
        if skip_prefix(l, prefix) {
            (*l).token = token;
            (*l).loc = loc;
            return true
        }
    }

    todo!()
}
