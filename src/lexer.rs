use core::ffi::*;
use core::mem::zeroed;
use crate::arena::*;
use crate::crust::libc::*;

#[derive(Clone, Copy)]
pub struct Loc {
    pub input_path: *const c_char,
    pub line_number: c_int,
    pub line_offset: c_int,
}

#[derive(Clone, Copy, PartialEq)]
pub enum Token {
    EOF,
    ParseError,
    ID,
    String,
    CharLit,
    IntLit,
    OCurly,
    CCurly,
    OParen,
    CParen,
    OBracket,
    CBracket,
    Not,
    Mul,
    Div,
    Mod,
    And,
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Or,
    Eq,
    EqEq,
    NotEq,
    Shl,
    ShlEq,
    Shr,
    ShrEq,
    ModEq,
    OrEq,
    AndEq,
    PlusEq,
    MinusEq,
    MulEq,
    DivEq,
    Question,
    Colon,
    SemiColon,
    Comma,
}

pub unsafe fn display_token(token: Token) -> *const c_char {
    match token {
        Token::EOF        => c!("end of file"),
        Token::ParseError => c!("parse error"),
        Token::ID         => c!("identifier"),
        Token::String     => c!("string"),
        Token::CharLit    => c!("character"),
        Token::IntLit     => c!("integer literal"),
        Token::OCurly     => c!("{"),
        Token::CCurly     => c!("}"),
        Token::OParen     => c!("("),
        Token::CParen     => c!(")"),
        Token::OBracket   => c!("["),
        Token::CBracket   => c!("]"),
        Token::Not        => c!("!"),
        Token::Mul        => c!("*"),
        Token::Div        => c!("/"),
        Token::Mod        => c!("%"),
        Token::And        => c!("&"),
        Token::Plus       => c!("+"),
        Token::PlusPlus   => c!("++"),
        Token::Minus      => c!("-"),
        Token::MinusMinus => c!("--"),
        Token::Less       => c!("<"),
        Token::LessEq     => c!("<="),
        Token::Greater    => c!(">"),
        Token::GreaterEq  => c!(">="),
        Token::Or         => c!("|"),
        Token::NotEq      => c!("!="),
        Token::Eq         => c!("="),
        Token::EqEq       => c!("=="),
        Token::Shl        => c!("<<"),
        Token::ShlEq      => c!("<<="),
        Token::Shr        => c!(">>"),
        Token::ShrEq      => c!(">>="),
        Token::ModEq      => c!("%="),
        Token::OrEq       => c!("|="),
        Token::AndEq      => c!("&="),
        Token::PlusEq     => c!("+="),
        Token::MinusEq    => c!("-="),
        Token::MulEq      => c!("*="),
        Token::DivEq      => c!("/="),
        Token::Question   => c!("?"),
        Token::Colon      => c!(":"),
        Token::SemiColon  => c!(";"),
        Token::Comma      => c!(","),
    }
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
    pub int_number: c_long,
    pub loc: Loc,
}

pub unsafe fn new(input_path: *mut c_char, input_stream: *mut c_char, eof: *mut c_char) -> Lexer {
    let mut l: Lexer = zeroed();
    l.input_path              = input_path;
    l.input_stream            = input_stream;
    l.eof                     = eof;
    l.parse_point.current     = input_stream;
    l.parse_point.line_start  = input_stream;
    l.parse_point.line_number = 1;
    l
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
    (c!("++"), Token::PlusPlus),
    (c!("--"), Token::MinusMinus),
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

pub unsafe fn get_token(l: *mut Lexer) -> bool {
    skip_whitespaces(l);

    (*l).loc = Loc {
        input_path:  (*l).input_path,
        line_number: (*l).parse_point.line_number as i32,
        line_offset: (*l).parse_point.current.offset_from((*l).parse_point.line_start) as i32,
    };

    if is_eof(l) {
        (*l).token = Token::EOF;
        return false;
    }

    for i in 0..PUNCTS.len() {
        let (prefix, token) = (*PUNCTS)[i];
        if skip_prefix(l, prefix) {
            (*l).token = token;
            return true
        }
    }

    (*l).token = Token::ParseError;
    false
}
