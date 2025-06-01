use core::ffi::*;
use core::mem::zeroed;
use crate::nob::*;
use crate::crust::libc::*;

#[derive(Clone, Copy)]
pub struct Loc {
    pub input_path: *const c_char,
    pub line_number: c_int,
    pub line_offset: c_int,
}

#[macro_export]
macro_rules! diagf {
    ($loc:expr, $($args:tt)*) => {{
        fprintf(stderr, c!("%s:%d:%d: "), $loc.input_path, $loc.line_number, $loc.line_offset);
        fprintf(stderr, $($args)*);
    }};
}

#[macro_export]
macro_rules! missingf {
    ($loc:expr, $($args:tt)*) => {{
        let file = file!();
        fprintf(stderr, c!("%s:%d:%d: TODO: "), $loc.input_path, $loc.line_number, $loc.line_offset);
        fprintf(stderr, $($args)*);
        fprintf(stderr, c!("%.*s:%d: INFO: implementation should go here\n"), file.len(), file.as_ptr(), line!());
        abort();
    }}
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
        Token::OCurly     => c!("`{`"),
        Token::CCurly     => c!("`}`"),
        Token::OParen     => c!("`(`"),
        Token::CParen     => c!("`)`"),
        Token::OBracket   => c!("`[`"),
        Token::CBracket   => c!("`]`"),
        Token::Not        => c!("`!`"),
        Token::Mul        => c!("`*`"),
        Token::Div        => c!("`/`"),
        Token::Mod        => c!("`%`"),
        Token::And        => c!("`&`"),
        Token::Plus       => c!("`+`"),
        Token::PlusPlus   => c!("`++`"),
        Token::Minus      => c!("`-`"),
        Token::MinusMinus => c!("`--`"),
        Token::Less       => c!("`<`"),
        Token::LessEq     => c!("`<=`"),
        Token::Greater    => c!("`>`"),
        Token::GreaterEq  => c!("`>=`"),
        Token::Or         => c!("`|`"),
        Token::NotEq      => c!("`!=`"),
        Token::Eq         => c!("`=`"),
        Token::EqEq       => c!("`==`"),
        Token::Shl        => c!("`<<`"),
        Token::ShlEq      => c!("`<<=`"),
        Token::Shr        => c!("`>>`"),
        Token::ShrEq      => c!("`>>=`"),
        Token::ModEq      => c!("`%=`"),
        Token::OrEq       => c!("`|=`"),
        Token::AndEq      => c!("`&=`"),
        Token::PlusEq     => c!("`+=`"),
        Token::MinusEq    => c!("`-=`"),
        Token::MulEq      => c!("`*=`"),
        Token::DivEq      => c!("`/=`"),
        Token::Question   => c!("`?`"),
        Token::Colon      => c!("`:`"),
        Token::SemiColon  => c!("`;`"),
        Token::Comma      => c!("`,`"),
    }
}

// TODO: Some punctuations are not historically accurate. =+ instead of +=, etc.
pub const PUNCTS: *const [(*const c_char, Token)] = &[
    (c!("?"), Token::Question),
    (c!("{"), Token::OCurly),
    (c!("}"), Token::CCurly),
    (c!("("), Token::OParen),
    (c!(")"), Token::CParen),
    (c!("["), Token::OBracket),
    (c!("]"), Token::CBracket),
    (c!(";"), Token::SemiColon),
    (c!(":"), Token::Colon),
    (c!(","), Token::Comma),
    (c!("--"), Token::MinusMinus),
    (c!("-="), Token::MinusEq),
    (c!("-"), Token::Minus),
    (c!("++"), Token::PlusPlus),
    (c!("+="), Token::PlusEq),
    (c!("+"), Token::Plus),
    (c!("*="), Token::MulEq),
    (c!("*"), Token::Mul),
    (c!("%="), Token::ModEq),
    (c!("%"), Token::Mod),
    (c!("/="), Token::DivEq),
    (c!("/"), Token::Div),
    (c!("|="), Token::OrEq),
    (c!("|"), Token::Or),
    (c!("&="), Token::AndEq),
    (c!("&"), Token::And),
    (c!("=="), Token::EqEq),
    (c!("="), Token::Eq),
    (c!("!="), Token::NotEq),
    (c!("!"), Token::Not),
    (c!("<<="), Token::ShlEq),
    (c!("<<"), Token::Shl),
    (c!("<="), Token::LessEq),
    (c!("<"), Token::Less),
    (c!(">>="), Token::ShrEq),
    (c!(">>"), Token::Shr),
    (c!(">="), Token::GreaterEq),
    (c!(">"), Token::Greater),
];

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

    pub string_storage: String_Builder,
    pub token: Token,
    pub string: *const c_char,
    pub int_number: u64,
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

pub unsafe fn is_eof(l: *mut Lexer) -> bool {
    (*l).parse_point.current >= (*l).eof
}

pub unsafe fn peek_char(l: *mut Lexer) -> Option<c_char> {
    if is_eof(l) {
        None
    } else {
        Some(*(*l).parse_point.current)
    }
}

pub unsafe fn skip_char(l: *mut Lexer) {
    assert!(!is_eof(l));

    let x = *(*l).parse_point.current;
    (*l).parse_point.current = (*l).parse_point.current.add(1);
    if x == '\n' as c_char {
        (*l).parse_point.line_start = (*l).parse_point.current;
        (*l).parse_point.line_number += 1;
    }
}

pub unsafe fn skip_chars(l: *mut Lexer, mut n: usize) {
    while n > 0 && !is_eof(l){
        skip_char(l);
        n -= 1;
    }
}

pub unsafe fn skip_whitespaces(l: *mut Lexer) {
    while let Some(x) = peek_char(l) {
        if isspace(x as i32) != 0 {
            skip_char(l)
        } else {
            break
        }
    }
}

pub unsafe fn skip_prefix(l: *mut Lexer, mut prefix: *const c_char) -> bool {
    let saved_point = (*l).parse_point;
    while *prefix != 0 {
        let Some(x) = peek_char(l) else {
            (*l).parse_point = saved_point;
            return false;
        };
        if x != *prefix {
            (*l).parse_point = saved_point;
            return false;
        }
        skip_char(l);
        prefix = prefix.add(1);
    }
    true
}

pub unsafe fn is_identifier(x: c_char) -> bool {
    isalnum(x as c_int) != 0 || x == '_' as c_char
}

pub unsafe fn is_identifier_start(x: c_char) -> bool {
    isalpha(x as c_int) != 0 || x == '_' as c_char
}

pub unsafe fn skip_line(l: *mut Lexer) {
    while let Some(x) = peek_char(l) {
        skip_char(l);
        if x == '\n' as c_char {
            break
        }
    }
}

pub unsafe fn loc(l: *mut Lexer) -> Loc {
    Loc {
        input_path:  (*l).input_path,
        line_number: (*l).parse_point.line_number as i32,
        line_offset: (*l).parse_point.current.offset_from((*l).parse_point.line_start) as i32 + 1,
    }
}

pub unsafe fn parse_string_into_storage(l: *mut Lexer, delim: c_char) -> Option<()> {
    while let Some(x) = peek_char(l) {
        match x {
            // TODO: string escaping is not historically accurate. The escape symbol should be * instead of \
            x if x == '\\' as c_char => {
                skip_char(l);
                let Some(x) = peek_char(l) else {
                    (*l).token = Token::ParseError;
                    diagf!(loc(l), c!("LEXER ERROR: Unfinished escape sequence\n"));
                    return None;
                };
                let x = match x {
                    x if x == 'n'   as c_char => '\n' as c_char,
                    x if x == 't'   as c_char => '\t' as c_char,
                    x if x == delim           => delim,
                    x if x == '\\'  as c_char => '\\' as c_char,
                    x => {
                        (*l).token = Token::ParseError;
                        diagf!(loc(l), c!("LEXER ERROR: Unknown escape sequence starting with `%c`\n"), x as c_int);
                        return None;
                    }
                };
                da_append(&mut (*l).string_storage, x);
                skip_char(l);
            },
            x if x == delim => break,
            _ => {
                da_append(&mut (*l).string_storage, x);
                skip_char(l);
            },
        }
    }
    Some(())
}

pub unsafe fn get_token(l: *mut Lexer) -> Option<()> {
    skip_whitespaces(l);

    // TODO: C++ style comments are not particularly historically accurate
    while skip_prefix(l, c!("//")) {
        skip_line(l);
        skip_whitespaces(l);
    }

    while skip_prefix(l, c!("/*")) {
        while !is_eof(l) && !skip_prefix(l, c!("*/")) {
            skip_char(l);
        }
        skip_whitespaces(l);
    }

    (*l).loc = loc(l);

    let Some(x) = peek_char(l) else {
        (*l).token = Token::EOF;
        return Some(())
    };

    for i in 0..PUNCTS.len() {
        let (prefix, token) = (*PUNCTS)[i];
        if skip_prefix(l, prefix) {
            (*l).token = token;
            return Some(())
        }
    }

    if is_identifier_start(x) {
        (*l).token = Token::ID;
        (*l).string_storage.count = 0;
        while let Some(x) = peek_char(l) {
            if is_identifier(x) {
                da_append(&mut (*l).string_storage, x);
                skip_char(l);
            } else {
                break
            }
        }
        da_append(&mut (*l).string_storage, 0);
        (*l).string = (*l).string_storage.items;
        return Some(())
    }

    // TODO: B originally does not support hex literals actually.
    if skip_prefix(l, c!("0x")) {
        (*l).token = Token::IntLit;
        (*l).int_number = 0;
        while let Some(x) = peek_char(l) {
            // TODO: check for overflows?
            if isdigit(x as c_int) != 0 {
                (*l).int_number *= 16;
                (*l).int_number += x as u64 - '0' as u64;
                skip_char(l);
            } else if 'a' as c_char <= x && x <= 'f' as c_char {
                (*l).int_number *= 16;
                (*l).int_number += x as u64 - 'a' as u64 + 10;
                skip_char(l);
            } else if 'A' as c_char <= x && x <= 'F' as c_char {
                (*l).int_number *= 16;
                (*l).int_number += x as u64 - 'A' as u64 + 10;
                skip_char(l);
            } else {
                break
            }
        }
        return Some(());
    }

    if isdigit(x as c_int) != 0 {
        (*l).token = Token::IntLit;
        (*l).int_number = 0;
        while let Some(x) = peek_char(l) {
            // TODO: check for overflows?
            if isdigit(x as c_int) != 0 {
                (*l).int_number *= 10;
                (*l).int_number += x as u64 - '0' as u64;
                skip_char(l);
            } else {
                break
            }
        }
        return Some(())
    }

    if x == '"' as c_char {
        skip_char(l);
        (*l).token = Token::String;
        (*l).string_storage.count = 0;
        parse_string_into_storage(l, '"' as c_char)?;
        if is_eof(l) {
            diagf!(loc(l), c!("LEXER ERROR: Unfinished string literal\n"));
            (*l).token = Token::ParseError;
            return None;
        }
        skip_char(l);
        da_append(&mut (*l).string_storage, 0);
        return Some(());
    }

    if x == '\'' as c_char {
        skip_char(l);
        (*l).token = Token::CharLit;
        (*l).string_storage.count = 0;
        parse_string_into_storage(l, '\'' as c_char)?;
        if is_eof(l) {
            diagf!(loc(l), c!("LEXER ERROR: Unfinished character literal\n"));
            (*l).token = Token::ParseError;
            return None;
        }
        skip_char(l);
        if (*l).string_storage.count == 0 {
            diagf!((*l).loc, c!("LEXER ERROR: Empty character literal\n"));
            (*l).token = Token::ParseError;
            return None;
        }
        if (*l).string_storage.count > 2 {
            // TODO: maybe we should allow more on targets with 64 bits?
            diagf!((*l).loc, c!("LEXER ERROR: Character literal contains more than two characters\n"));
            (*l).token = Token::ParseError;
            return None;
        }
        // TODO: I don't know in which order the characters should be packed in the char literal
        (*l).int_number = 0;
        for i in 0..(*l).string_storage.count {
            (*l).int_number *= 0xFF;
            (*l).int_number += *(*l).string_storage.items.add(i) as u64;
        }

        return Some(());
    }

    diagf!((*l).loc, c!("LEXER ERROR: Unknown token %c\n"), *(*l).parse_point.current as c_int);
    (*l).token = Token::ParseError;
    None
}
