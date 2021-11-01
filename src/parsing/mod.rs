mod file_server;
mod lexer;
mod parser;

pub use file_server::*;
pub use lexer::*;
pub use parser::*;

#[derive(Debug, Clone, Copy)]
pub struct TextPosition {
    pub abs: usize,
    pub line: usize,
    pub col: usize,
}
impl TextPosition {
    #[inline]
    pub const fn new() -> Self {
        Self {
            abs: 0,
            line: 0,
            col: 0,
        }
    }

    pub fn increment(&mut self, new_line: bool) {
        self.abs += 1;
        if new_line {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    AddAssign,        // +=
    SubAssign,        // -=
    MulAssign,        // *=
    DivAssign,        // /=
    RemAssign,        // %=
    Add,              // +
    Sub,              // -
    Mul,              // *
    Div,              // /
    Rem,              // %
    LeftShiftAssign,  // <<=
    RightShiftAssign, // >>=
    LeftShift,        // <<
    RightShift,       // >>
    Equals,           // ==
    NotEquals,        // !=
    LessEqual,        // <=
    Less,             // <
    GreaterEqual,     // >=
    Greater,          // >
    AndAssign,        // &=
    OrAssign,         // |=
    XorAssign,        // ^=
    LogicAnd,         // &&
    LogicOr,          // ||
    BitAnd,           // &
    BitOr,            // |
    Xor,              // ^
    Not,              // !
    Assign,           // =
    OpenParen,        // (
    CloseParen,       // )
    OpenBrace,        // [
    CloseBrace,       // ]
    OpenCurl,         // {
    CloseCurl,        // }
    Semicolon,        // ;
    Comma,            // ,
    Dot,              // .
    Colon,            // :
    Var,              // var
    Const,            // const
    If,               // if
    Else,             // else
    Switch,           // switch
    Case,             // case
    While,            // while
    For,              // for
    Break,            // break
    Continue,         // continue
    Fn,               // fn
    Return,           // return
    Word,             // word
    UWord,            // uword
    Int,              // int
    UInt,             // uint
    Long,             // long
    ULong,            // ulong
    Char,             // char
    SizeOf,           // sizeof
    Struct,           // struct
    Union,            // union
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Whitespace(bool),
    IntegerLiteral(i128),
    CharLiteral(char),
    StringLiteral(String),
    Keyword(Keyword),
    Identifier(String),
}

#[derive(Debug, Clone, Copy)]
pub struct TokenPosition {
    pub file: FileHandle,
    pub start_text_pos: TextPosition,
    pub end_text_pos: TextPosition,
}
impl TokenPosition {
    pub const NONE: Self = Self {
        file: FileHandle::NULL,
        start_text_pos: TextPosition {
            abs: 0,
            line: 0,
            col: 0,
        },
        end_text_pos: TextPosition {
            abs: 0,
            line: 0,
            col: 0,
        },
    };
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: TokenPosition,
}
impl Token {
    pub const fn new(kind: TokenKind, pos: TokenPosition) -> Self {
        Self { kind, pos }
    }
}
