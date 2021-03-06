#![feature(prelude_import)]
#![feature(try_trait_v2)]
#![feature(termination_trait_lib)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
mod parsing {
    mod file_server {
        use std::borrow::Cow;
        use std::collections::HashMap;
        use std::io;
        use std::path::{Path, PathBuf};
        pub struct FileHandle(u64);
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for FileHandle {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match *self {
                    FileHandle(ref __self_0_0) => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_tuple(f, "FileHandle");
                        let _ =
                            ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0_0));
                        ::core::fmt::DebugTuple::finish(debug_trait_builder)
                    }
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::clone::Clone for FileHandle {
            #[inline]
            fn clone(&self) -> FileHandle {
                {
                    let _: ::core::clone::AssertParamIsClone<u64>;
                    *self
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::marker::Copy for FileHandle {}
        impl ::core::marker::StructuralPartialEq for FileHandle {}
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::cmp::PartialEq for FileHandle {
            #[inline]
            fn eq(&self, other: &FileHandle) -> bool {
                match *other {
                    FileHandle(ref __self_1_0) => match *self {
                        FileHandle(ref __self_0_0) => (*__self_0_0) == (*__self_1_0),
                    },
                }
            }
            #[inline]
            fn ne(&self, other: &FileHandle) -> bool {
                match *other {
                    FileHandle(ref __self_1_0) => match *self {
                        FileHandle(ref __self_0_0) => (*__self_0_0) != (*__self_1_0),
                    },
                }
            }
        }
        impl ::core::marker::StructuralEq for FileHandle {}
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::cmp::Eq for FileHandle {
            #[inline]
            #[doc(hidden)]
            #[no_coverage]
            fn assert_receiver_is_total_eq(&self) -> () {
                {
                    let _: ::core::cmp::AssertParamIsEq<u64>;
                }
            }
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::hash::Hash for FileHandle {
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
                match *self {
                    FileHandle(ref __self_0_0) => ::core::hash::Hash::hash(&(*__self_0_0), state),
                }
            }
        }
        trait FileSource {
            fn full_name(&self) -> Cow<str>;
            fn read_text(&self) -> io::Result<Box<[char]>>;
        }
        struct RealFile {
            path: PathBuf,
        }
        impl RealFile {
            pub fn create<P: AsRef<Path>>(path: P) -> io::Result<Self> {
                let path = Path::canonicalize(path.as_ref())?;
                Ok(Self { path })
            }
        }
        impl FileSource for RealFile {
            fn full_name(&self) -> Cow<str> {
                self.path.to_string_lossy()
            }
            fn read_text(&self) -> io::Result<Box<[char]>> {
                let s = std::fs::read_to_string(&self.path)?;
                let text: Box<[char]> = s.chars().collect();
                Ok(text)
            }
        }
        struct DummyFile {
            name: String,
            contents: String,
        }
        impl DummyFile {
            pub fn new<S1, S2>(name: S1, contents: S2) -> Self
            where
                S1: ToString,
                S2: ToString,
            {
                Self {
                    name: name.to_string(),
                    contents: contents.to_string(),
                }
            }
        }
        impl FileSource for DummyFile {
            fn full_name(&self) -> Cow<str> {
                Cow::Borrowed(&self.name)
            }
            fn read_text(&self) -> io::Result<Box<[char]>> {
                let text: Box<[char]> = self.contents.chars().collect();
                Ok(text)
            }
        }
        pub struct FileServer {
            next_handle: FileHandle,
            files: HashMap<FileHandle, Box<dyn FileSource>>,
            cache: HashMap<FileHandle, Box<[char]>>,
        }
        #[allow(dead_code)]
        impl FileServer {
            pub fn new() -> Self {
                Self {
                    next_handle: FileHandle(0),
                    files: HashMap::new(),
                    cache: HashMap::new(),
                }
            }
            pub fn add_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<FileHandle> {
                let handle = self.next_handle;
                let file = Box::new(RealFile::create(path)?);
                let prev = self.files.insert(handle, file);
                if !prev.is_none() {
                    ::core::panicking::panic("assertion failed: prev.is_none()")
                };
                self.next_handle.0 += 1;
                Ok(handle)
            }
            pub fn add_dummy_file<S1, S2>(&mut self, name: S1, contents: S2) -> FileHandle
            where
                S1: ToString,
                S2: ToString,
            {
                let handle = self.next_handle;
                let file = Box::new(DummyFile::new(name, contents));
                let prev = self.files.insert(handle, file);
                if !prev.is_none() {
                    ::core::panicking::panic("assertion failed: prev.is_none()")
                };
                self.next_handle.0 += 1;
                handle
            }
            pub fn files(&self) -> Vec<FileHandle> {
                self.files.keys().map(|h| *h).collect()
            }
            pub fn get_file_name(&self, handle: FileHandle) -> Cow<str> {
                let file = self.files.get(&handle).expect("Invalid file handle");
                file.full_name()
            }
            pub fn get_text<'a>(&'a mut self, handle: FileHandle) -> io::Result<&'a [char]> {
                if self.cache.contains_key(&handle) {
                    Ok(&self.cache[&handle])
                } else {
                    let file = self.files.get(&handle).expect("Invalid file handle");
                    let text = file.read_text()?;
                    let prev = self.cache.insert(handle, text);
                    if !prev.is_none() {
                        ::core::panicking::panic("assertion failed: prev.is_none()")
                    };
                    Ok(&self.cache[&handle])
                }
            }
        }
    }
    mod lexer {
        use super::*;
        use std::fmt::Display;
        use std::io;
        struct TextInput<'a> {
            text: &'a [char],
            pos: TextPosition,
            saved_pos: TextPosition,
        }
        impl<'a> TextInput<'a> {
            fn create(file_server: &'a mut FileServer, file: FileHandle) -> io::Result<Self> {
                let text = file_server.get_text(file)?;
                Ok(Self {
                    text,
                    pos: TextPosition::new(),
                    saved_pos: TextPosition::new(),
                })
            }
            #[inline]
            const fn pos(&self) -> TextPosition {
                self.pos
            }
            #[inline]
            fn save_state(&mut self) {
                self.saved_pos = self.pos;
            }
            #[inline]
            fn restore_state(&mut self) {
                self.pos = self.saved_pos;
            }
            const fn current(&self) -> Option<char> {
                if self.pos.abs >= self.text.len() {
                    None
                } else {
                    Some(self.text[self.pos.abs])
                }
            }
            fn advance(&mut self) {
                if self.pos.abs < self.text.len() {
                    let c = self.text[self.pos.abs];
                    self.pos.increment(c == '\n');
                }
            }
        }
        pub struct LexerError {
            pub msg: &'static str,
            pub file: String,
            pub line: usize,
            pub col: usize,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for LexerError {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match *self {
                    LexerError {
                        msg: ref __self_0_0,
                        file: ref __self_0_1,
                        line: ref __self_0_2,
                        col: ref __self_0_3,
                    } => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_struct(f, "LexerError");
                        let _ = ::core::fmt::DebugStruct::field(
                            debug_trait_builder,
                            "msg",
                            &&(*__self_0_0),
                        );
                        let _ = ::core::fmt::DebugStruct::field(
                            debug_trait_builder,
                            "file",
                            &&(*__self_0_1),
                        );
                        let _ = ::core::fmt::DebugStruct::field(
                            debug_trait_builder,
                            "line",
                            &&(*__self_0_2),
                        );
                        let _ = ::core::fmt::DebugStruct::field(
                            debug_trait_builder,
                            "col",
                            &&(*__self_0_3),
                        );
                        ::core::fmt::DebugStruct::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl LexerError {
            const fn new(msg: &'static str, file: String, line: usize, col: usize) -> Self {
                Self {
                    msg,
                    file,
                    line,
                    col,
                }
            }
        }
        impl Display for LexerError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["", " at ", ":", ":"],
                    &match (&self.msg, &self.file, &self.line, &self.col) {
                        (arg0, arg1, arg2, arg3) => [
                            ::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Display::fmt),
                            ::core::fmt::ArgumentV1::new(arg1, ::core::fmt::Display::fmt),
                            ::core::fmt::ArgumentV1::new(arg2, ::core::fmt::Display::fmt),
                            ::core::fmt::ArgumentV1::new(arg3, ::core::fmt::Display::fmt),
                        ],
                    },
                ))
            }
        }
        pub type LexerResult<T> = crate::OptResult<T, LexerError>;
        const fn is_ascii_decdigit(c: &char) -> bool {
            c.is_ascii_digit() || (*c == '_')
        }
        const fn is_ascii_hexdigit(c: &char) -> bool {
            c.is_ascii_hexdigit() || (*c == '_')
        }
        const fn is_ascii_bindigit(c: &char) -> bool {
            (*c == '0') || (*c == '1') || (*c == '_')
        }
        const fn is_ascii_octdigit(c: &char) -> bool {
            (*c == '0')
                || (*c == '1')
                || (*c == '2')
                || (*c == '3')
                || (*c == '4')
                || (*c == '5')
                || (*c == '6')
                || (*c == '7')
                || (*c == '_')
        }
        pub struct Lexer<'a> {
            file: FileHandle,
            file_name: String,
            input: TextInput<'a>,
        }
        impl<'a> Lexer<'a> {
            pub fn create(
                file_server: &'a mut FileServer,
                file: FileHandle,
            ) -> std::io::Result<Self> {
                let file_name = file_server.get_file_name(file).to_string();
                let input = TextInput::create(file_server, file)?;
                Ok(Self {
                    file,
                    file_name,
                    input,
                })
            }
            fn advance_while(&mut self, predicate: fn(&char) -> bool) -> String {
                let mut result = String::new();
                while let Some(c) = self.input.current() {
                    if predicate(&c) {
                        self.input.advance();
                        result.push(c);
                    } else {
                        break;
                    }
                }
                result
            }
            fn advance_while_char(&mut self, predicate: char) {
                while let Some(c) = self.input.current() {
                    if c == predicate {
                        self.input.advance();
                    } else {
                        break;
                    }
                }
            }
            fn advance_by(&mut self, mut count: usize) -> String {
                let mut result = String::new();
                while let Some(c) = self.input.current() {
                    if count > 0 {
                        self.input.advance();
                        result.push(c);
                    } else {
                        break;
                    }
                    count -= 1;
                }
                result
            }
            fn next_whitespace(&mut self) -> LexerResult<Token> {
                if let Some(c) = self.input.current() {
                    {
                        let start = self.input.pos();
                        if c.is_ascii_whitespace() {
                            self.input.advance();
                            let s = self.advance_while(char::is_ascii_whitespace);
                            let contains_new_line = (c == '\n') || s.contains('\n');
                            let tpos = TokenPosition {
                                file: self.file,
                                start_text_pos: start,
                                end_text_pos: self.input.pos(),
                            };
                            let token = Token::new(TokenKind::Whitespace(contains_new_line), tpos);
                            LexerResult::Some(token)
                        } else {
                            LexerResult::None
                        }
                    }
                } else {
                    LexerResult::None
                }
            }
            fn next_prefixed_integer_literal(
                &mut self,
                prefix_char: char,
                predicate: fn(&char) -> bool,
                radix: u32,
                err_msg: &'static str,
            ) -> LexerResult<Token> {
                if let Some(c) = self.input.current() {
                    {
                        self.input.save_state();
                        let start = self.input.pos();
                        if c == '0' {
                            self.input.advance();
                            if let Some(c) = self.input.current() {
                                if c.to_ascii_lowercase() == prefix_char {
                                    self.input.advance();
                                } else {
                                    self.input.restore_state();
                                    return LexerResult::None;
                                }
                            }
                            self.advance_while_char('_');
                            if let Some(c) = self.input.current() {
                                if predicate(&c) {
                                    self.input.advance();
                                    let s = self.advance_while(predicate);
                                    let tpos = TokenPosition {
                                        file: self.file,
                                        start_text_pos: start,
                                        end_text_pos: self.input.pos(),
                                    };
                                    let val = i128::from_str_radix(&s, radix).unwrap();
                                    let token = Token::new(TokenKind::IntegerLiteral(val), tpos);
                                    return LexerResult::Some(token);
                                }
                            }
                            let pos = self.input.pos();
                            let err = LexerError::new(
                                err_msg,
                                self.file_name.to_string(),
                                pos.line,
                                pos.col,
                            );
                            LexerResult::Err(err)
                        } else {
                            LexerResult::None
                        }
                    }
                } else {
                    LexerResult::None
                }
            }
            fn next_hex_integer_literal(&mut self) -> LexerResult<Token> {
                self.next_prefixed_integer_literal('x', is_ascii_hexdigit, 16, "Expected hex digit")
            }
            fn next_bin_integer_literal(&mut self) -> LexerResult<Token> {
                self.next_prefixed_integer_literal(
                    'b',
                    is_ascii_bindigit,
                    2,
                    "Expected binary digit",
                )
            }
            fn next_oct_integer_literal(&mut self) -> LexerResult<Token> {
                self.next_prefixed_integer_literal(
                    'o',
                    is_ascii_octdigit,
                    8,
                    "Expected octal digit",
                )
            }
            fn next_dec_integer_literal(&mut self) -> LexerResult<Token> {
                if let Some(c) = self.input.current() {
                    {
                        let start = self.input.pos();
                        if c.is_ascii_digit() {
                            let s = self.advance_while(is_ascii_decdigit);
                            let tpos = TokenPosition {
                                file: self.file,
                                start_text_pos: start,
                                end_text_pos: self.input.pos(),
                            };
                            let val = i128::from_str_radix(&s, 10).unwrap();
                            let token = Token::new(TokenKind::IntegerLiteral(val), tpos);
                            LexerResult::Some(token)
                        } else {
                            LexerResult::None
                        }
                    }
                } else {
                    LexerResult::None
                }
            }
            fn next_char(&mut self) -> LexerResult<(char, bool)> {
                if let Some(c) = self.input.current() {
                    {
                        if c == '\\' {
                            self.input.advance();
                            if let Some(c) = self.input.current() {
                                let c = match c {
                                    'r' => '\r',
                                    'n' => '\n',
                                    't' => '\t',
                                    c => c,
                                };
                                self.input.advance();
                                LexerResult::Some((c, true))
                            } else {
                                let pos = self.input.pos();
                                let err = LexerError::new(
                                    "Expected escape sequence",
                                    self.file_name.to_string(),
                                    pos.line,
                                    pos.col,
                                );
                                LexerResult::Err(err)
                            }
                        } else {
                            LexerResult::Some((c, false))
                        }
                    }
                } else {
                    LexerResult::None
                }
            }
            fn next_char_literal(&mut self) -> LexerResult<Token> {
                if let Some(c) = self.input.current() {
                    {
                        let start = self.input.pos();
                        if c == '\'' {
                            self.input.advance();
                            if let Some((c, _)) = self.next_char()? {
                                if let Some(close) = self.input.current() {
                                    if close == '\'' {
                                        self.input.advance();
                                        let tpos = TokenPosition {
                                            file: self.file,
                                            start_text_pos: start,
                                            end_text_pos: self.input.pos(),
                                        };
                                        let token = Token::new(TokenKind::CharLiteral(c), tpos);
                                        return LexerResult::Some(token);
                                    }
                                }
                                let pos = self.input.pos();
                                let err = LexerError::new(
                                    "Expected closing quote",
                                    self.file_name.to_string(),
                                    pos.line,
                                    pos.col,
                                );
                                LexerResult::Err(err)
                            } else {
                                let pos = self.input.pos();
                                let err = LexerError::new(
                                    "Expected char",
                                    self.file_name.to_string(),
                                    pos.line,
                                    pos.col,
                                );
                                LexerResult::Err(err)
                            }
                        } else {
                            LexerResult::None
                        }
                    }
                } else {
                    LexerResult::None
                }
            }
            fn next_string_literal(&mut self) -> LexerResult<Token> {
                if let Some(c) = self.input.current() {
                    {
                        let start = self.input.pos();
                        if c == '"' {
                            self.input.advance();
                            let mut s = String::new();
                            while let Some((c, escaped)) = self.next_char()? {
                                if (c == '"') && !escaped {
                                    break;
                                } else {
                                    s.push(c);
                                }
                            }
                            let tpos = TokenPosition {
                                file: self.file,
                                start_text_pos: start,
                                end_text_pos: self.input.pos(),
                            };
                            let token = Token::new(TokenKind::StringLiteral(s), tpos);
                            LexerResult::Some(token)
                        } else {
                            LexerResult::None
                        }
                    }
                } else {
                    LexerResult::None
                }
            }
            fn next_match(&mut self, pattern: &str) -> LexerResult<()> {
                self.input.save_state();
                let s = self.advance_by(pattern.chars().count());
                if s.eq(pattern) {
                    LexerResult::Some(())
                } else {
                    self.input.restore_state();
                    LexerResult::None
                }
            }
            fn next_keyword(&mut self) -> LexerResult<Token> {
                const PATTERN_TABLE: &'static [(&'static str, Keyword)] = &[
                    ("+=", Keyword::AddAssign),
                    ("-=", Keyword::SubAssign),
                    ("*=", Keyword::MulAssign),
                    ("/=", Keyword::DivAssign),
                    ("%=", Keyword::RemAssign),
                    ("+", Keyword::Add),
                    ("-", Keyword::Sub),
                    ("*", Keyword::Mul),
                    ("/", Keyword::Div),
                    ("%", Keyword::Rem),
                    ("<<=", Keyword::LeftShiftAssign),
                    (">>=", Keyword::RightShiftAssign),
                    ("<<", Keyword::LeftShift),
                    (">>", Keyword::RightShift),
                    ("==", Keyword::Equals),
                    ("!=", Keyword::NotEquals),
                    ("<=", Keyword::LessEqual),
                    ("<", Keyword::Less),
                    (">=", Keyword::GreaterEqual),
                    (">", Keyword::Greater),
                    ("&&=", Keyword::LogicAndAssign),
                    ("||=", Keyword::LogicOrAssign),
                    ("&=", Keyword::BitAndAssign),
                    ("|=", Keyword::BitOrAssign),
                    ("^=", Keyword::XorAssign),
                    ("&&", Keyword::LogicAnd),
                    ("||", Keyword::LogicOr),
                    ("&", Keyword::BitAnd),
                    ("|", Keyword::BitOr),
                    ("^", Keyword::Xor),
                    ("!", Keyword::Not),
                    ("=", Keyword::Assign),
                    ("(", Keyword::OpenParen),
                    (")", Keyword::CloseParen),
                    ("[", Keyword::OpenBrace),
                    ("]", Keyword::CloseBrace),
                    ("{", Keyword::OpenCurl),
                    ("}", Keyword::CloseCurl),
                    (";", Keyword::Semicolon),
                    (",", Keyword::Comma),
                    (".", Keyword::Dot),
                    (":", Keyword::Colon),
                    ("var", Keyword::Var),
                    ("const", Keyword::Const),
                    ("if", Keyword::If),
                    ("else", Keyword::Else),
                    ("while", Keyword::While),
                    ("for", Keyword::For),
                    ("break", Keyword::Break),
                    ("fn", Keyword::Fn),
                    ("return", Keyword::Return),
                    ("word", Keyword::Word),
                    ("uword", Keyword::UWord),
                    ("int", Keyword::Int),
                    ("uint", Keyword::UInt),
                    ("long", Keyword::Long),
                    ("ulong", Keyword::ULong),
                    ("char", Keyword::Char),
                    ("sizeof", Keyword::SizeOf),
                    ("struct", Keyword::Struct),
                    ("union", Keyword::Union),
                ];
                let start = self.input.pos();
                for (pattern, keyword) in PATTERN_TABLE.iter() {
                    if let Some(_) = self.next_match(pattern)? {
                        let tpos = TokenPosition {
                            file: self.file,
                            start_text_pos: start,
                            end_text_pos: self.input.pos(),
                        };
                        return LexerResult::Some(Token::new(TokenKind::Keyword(*keyword), tpos));
                    }
                }
                LexerResult::None
            }
            fn next_identifier(&mut self) -> LexerResult<Token> {
                if let Some(c) = self.input.current() {
                    {
                        let start = self.input.pos();
                        if c.is_ascii_alphabetic() || (c == '_') {
                            let s =
                                self.advance_while(|c| c.is_ascii_alphanumeric() || (*c == '_'));
                            let tpos = TokenPosition {
                                file: self.file,
                                start_text_pos: start,
                                end_text_pos: self.input.pos(),
                            };
                            let token = Token::new(TokenKind::Identifier(s), tpos);
                            LexerResult::Some(token)
                        } else {
                            LexerResult::None
                        }
                    }
                } else {
                    LexerResult::None
                }
            }
            pub fn next_token(&mut self) -> LexerResult<Token> {
                if self.input.current().is_none() {
                    LexerResult::None
                } else if let Some(token) = self.next_whitespace()? {
                    LexerResult::Some(token)
                } else if let Some(token) = self.next_hex_integer_literal()? {
                    LexerResult::Some(token)
                } else if let Some(token) = self.next_bin_integer_literal()? {
                    LexerResult::Some(token)
                } else if let Some(token) = self.next_oct_integer_literal()? {
                    LexerResult::Some(token)
                } else if let Some(token) = self.next_dec_integer_literal()? {
                    LexerResult::Some(token)
                } else if let Some(token) = self.next_char_literal()? {
                    LexerResult::Some(token)
                } else if let Some(token) = self.next_string_literal()? {
                    LexerResult::Some(token)
                } else if let Some(token) = self.next_keyword()? {
                    LexerResult::Some(token)
                } else if let Some(token) = self.next_identifier()? {
                    LexerResult::Some(token)
                } else {
                    let pos = self.input.pos();
                    let err = LexerError::new(
                        "Unexpected character",
                        self.file_name.to_string(),
                        pos.line,
                        pos.col,
                    );
                    LexerResult::Err(err)
                }
            }
        }
    }
    mod parser {
        use super::*;
        use std::fmt::Display;
        struct TokenInput<'a> {
            tokens: &'a [Token],
            pos: usize,
            pos_stack: Vec<usize>,
        }
        impl<'a> TokenInput<'a> {
            pub const fn new(tokens: &'a [Token]) -> Self {
                Self {
                    tokens,
                    pos: 0,
                    pos_stack: Vec::new(),
                }
            }
            #[inline]
            pub fn push_state(&mut self) {
                self.pos_stack.push(self.pos);
            }
            #[inline]
            pub fn pop_state(&mut self) {
                self.pos = self.pos_stack.pop().expect("Position stack is empty");
            }
            #[inline]
            pub fn drop_state(&mut self) {
                self.pos_stack.pop().expect("Position stack is empty");
            }
            pub fn next_token(&mut self) -> Option<Token> {
                if self.pos >= self.tokens.len() {
                    None
                } else {
                    let result = self.tokens[self.pos].clone();
                    self.pos += 1;
                    Some(result)
                }
            }
        }
        pub struct ParseError {
            pub msg: &'static str,
            pub file: String,
            pub line: usize,
            pub col: usize,
        }
        #[automatically_derived]
        #[allow(unused_qualifications)]
        impl ::core::fmt::Debug for ParseError {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match *self {
                    ParseError {
                        msg: ref __self_0_0,
                        file: ref __self_0_1,
                        line: ref __self_0_2,
                        col: ref __self_0_3,
                    } => {
                        let debug_trait_builder =
                            &mut ::core::fmt::Formatter::debug_struct(f, "ParseError");
                        let _ = ::core::fmt::DebugStruct::field(
                            debug_trait_builder,
                            "msg",
                            &&(*__self_0_0),
                        );
                        let _ = ::core::fmt::DebugStruct::field(
                            debug_trait_builder,
                            "file",
                            &&(*__self_0_1),
                        );
                        let _ = ::core::fmt::DebugStruct::field(
                            debug_trait_builder,
                            "line",
                            &&(*__self_0_2),
                        );
                        let _ = ::core::fmt::DebugStruct::field(
                            debug_trait_builder,
                            "col",
                            &&(*__self_0_3),
                        );
                        ::core::fmt::DebugStruct::finish(debug_trait_builder)
                    }
                }
            }
        }
        impl ParseError {
            const fn new(msg: &'static str, file: String, line: usize, col: usize) -> Self {
                Self {
                    msg,
                    file,
                    line,
                    col,
                }
            }
        }
        impl Display for ParseError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["", " at ", ":", ":"],
                    &match (&self.msg, &self.file, &self.line, &self.col) {
                        (arg0, arg1, arg2, arg3) => [
                            ::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Display::fmt),
                            ::core::fmt::ArgumentV1::new(arg1, ::core::fmt::Display::fmt),
                            ::core::fmt::ArgumentV1::new(arg2, ::core::fmt::Display::fmt),
                            ::core::fmt::ArgumentV1::new(arg3, ::core::fmt::Display::fmt),
                        ],
                    },
                ))
            }
        }
        pub type ParseResult<T> = crate::OptResult<(T, TokenPosition), ParseError>;
        pub struct Parser<'a> {
            file_server: &'a FileServer,
            input: TokenInput<'a>,
        }
        impl<'a> Parser<'a> {
            pub fn new(file_server: &'a FileServer, input: &'a [Token]) -> Self {
                Self {
                    file_server,
                    input: TokenInput::new(input),
                }
            }
            fn parse_whitespace(&mut self) -> ParseResult<bool> {
                self.input.push_state();
                if let Some(token) = self.input.next_token() {
                    if let TokenKind::Whitespace(has_newline) = token.kind {
                        self.input.drop_state();
                        ParseResult::Some((has_newline, token.pos))
                    } else {
                        self.input.pop_state();
                        ParseResult::None
                    }
                } else {
                    self.input.drop_state();
                    ParseResult::None
                }
            }
            fn parse_integer_constant(&mut self) -> ParseResult<i128> {
                {
                    self.input.push_state();
                    self.parse_whitespace()?;
                    if let Some(token) = self.input.next_token() {
                        let result = {
                            {
                                if let TokenKind::IntegerLiteral(value) = token.kind {
                                    ParseResult::Some((value, token.pos))
                                } else {
                                    ParseResult::None
                                }
                            }
                        };
                        match result {
                            ParseResult::None => {
                                self.input.pop_state();
                            }
                            _ => {
                                self.input.drop_state();
                            }
                        }
                        result
                    } else {
                        self.input.pop_state();
                        return ParseResult::None;
                    }
                }
            }
            fn parse_char_constant(&mut self) -> ParseResult<char> {
                {
                    self.input.push_state();
                    self.parse_whitespace()?;
                    if let Some(token) = self.input.next_token() {
                        let result = {
                            {
                                if let TokenKind::CharLiteral(c) = token.kind {
                                    ParseResult::Some((c, token.pos))
                                } else {
                                    ParseResult::None
                                }
                            }
                        };
                        match result {
                            ParseResult::None => {
                                self.input.pop_state();
                            }
                            _ => {
                                self.input.drop_state();
                            }
                        }
                        result
                    } else {
                        self.input.pop_state();
                        return ParseResult::None;
                    }
                }
            }
            fn parse_string_constant(&mut self) -> ParseResult<String> {
                {
                    self.input.push_state();
                    self.parse_whitespace()?;
                    if let Some(token) = self.input.next_token() {
                        let result = {
                            {
                                if let TokenKind::StringLiteral(s) = token.kind {
                                    ParseResult::Some((s, token.pos))
                                } else {
                                    ParseResult::None
                                }
                            }
                        };
                        match result {
                            ParseResult::None => {
                                self.input.pop_state();
                            }
                            _ => {
                                self.input.drop_state();
                            }
                        }
                        result
                    } else {
                        self.input.pop_state();
                        return ParseResult::None;
                    }
                }
            }
            fn parse_keyword(&mut self, keyword: Keyword) -> ParseResult<()> {
                {
                    self.input.push_state();
                    self.parse_whitespace()?;
                    if let Some(token) = self.input.next_token() {
                        let result = {
                            {
                                if token.kind == TokenKind::Keyword(keyword) {
                                    ParseResult::Some(((), token.pos))
                                } else {
                                    ParseResult::None
                                }
                            }
                        };
                        match result {
                            ParseResult::None => {
                                self.input.pop_state();
                            }
                            _ => {
                                self.input.drop_state();
                            }
                        }
                        result
                    } else {
                        self.input.pop_state();
                        return ParseResult::None;
                    }
                }
            }
            fn parse_identifier(&mut self) -> ParseResult<String> {
                {
                    self.input.push_state();
                    self.parse_whitespace()?;
                    if let Some(token) = self.input.next_token() {
                        let result = {
                            {
                                if let TokenKind::Identifier(identifier) = token.kind {
                                    ParseResult::Some((identifier, token.pos))
                                } else {
                                    ParseResult::None
                                }
                            }
                        };
                        match result {
                            ParseResult::None => {
                                self.input.pop_state();
                            }
                            _ => {
                                self.input.drop_state();
                            }
                        }
                        result
                    } else {
                        self.input.pop_state();
                        return ParseResult::None;
                    }
                }
            }
            fn parse_list<T>(
                &mut self,
                mut parse_item: impl FnMut(&mut Self) -> ParseResult<T>,
                delimiter: Keyword,
                err_msg: &'static str,
            ) -> ParseResult<Vec<T>> {
                self.input.push_state();
                self.parse_whitespace()?;
                if let Some((first, first_pos)) = parse_item(self)? {
                    self.input.drop_state();
                    let mut result: Vec<T> = Vec::new();
                    result.push(first);
                    loop {
                        if let Some((_, pos)) = self.parse_keyword(delimiter)? {
                            self.parse_whitespace()?;
                            if let Some((item, _)) = parse_item(self)? {
                                result.push(item);
                            } else {
                                let file = self.file_server.get_file_name(pos.file).into_owned();
                                let err = ParseError::new(
                                    err_msg,
                                    file,
                                    pos.end_text_pos.line,
                                    pos.end_text_pos.col,
                                );
                                return ParseResult::Err(err);
                            }
                        } else {
                            break;
                        }
                    }
                    ParseResult::Some((result, first_pos))
                } else {
                    self.input.pop_state();
                    ParseResult::None
                }
            }
            fn parse_between<T>(
                &mut self,
                mut parse_item: impl FnMut(&mut Self) -> ParseResult<T>,
                before: Keyword,
                after: Keyword,
                err_msg: &'static str,
            ) -> ParseResult<T> {
                if let Some((_, first_pos)) = self.parse_keyword(before)? {
                    self.parse_whitespace()?;
                    if let Some((result, pos)) = parse_item(self)? {
                        if let Some(_) = self.parse_keyword(after)? {
                            ParseResult::Some((result, first_pos))
                        } else {
                            let file = self.file_server.get_file_name(pos.file).into_owned();
                            let err = ParseError::new(
                                err_msg,
                                file,
                                pos.end_text_pos.line,
                                pos.end_text_pos.col,
                            );
                            ParseResult::Err(err)
                        }
                    } else {
                        let file = self.file_server.get_file_name(first_pos.file).into_owned();
                        let err = ParseError::new(
                            err_msg,
                            file,
                            first_pos.end_text_pos.line,
                            first_pos.end_text_pos.col,
                        );
                        ParseResult::Err(err)
                    }
                } else {
                    ParseResult::None
                }
            }
            fn parse_qualified_path(&mut self) -> ParseResult<QualifiedPath> {
                if let Some((parts, pos)) =
                    self.parse_list(Self::parse_identifier, Keyword::Dot, "Expected identifier")?
                {
                    ParseResult::Some((QualifiedPath(parts), pos))
                } else {
                    ParseResult::None
                }
            }
            fn parse_sizeof_arg(&mut self) -> ParseResult<Expression> {
                if let Some((path, pos)) = self.parse_between(
                    Self::parse_qualified_path,
                    Keyword::OpenParen,
                    Keyword::CloseParen,
                    "Expected type or variable",
                )? {
                    ParseResult::Some((Expression::SizeOf(path), pos))
                } else {
                    ParseResult::None
                }
            }
            fn parse_function_call_args(&mut self) -> ParseResult<Vec<Expression>> {
                if let Some((args, pos)) = self.parse_between(
                    |s| {
                        s.parse_list(
                            Self::parse_expression,
                            Keyword::Comma,
                            "Expected argument list",
                        )
                    },
                    Keyword::OpenParen,
                    Keyword::CloseParen,
                    "Expected argument list",
                )? {
                    ParseResult::Some((args, pos))
                } else {
                    ParseResult::None
                }
            }
            fn parse_array_access_arg(&mut self) -> ParseResult<Expression> {
                if let Some((expr, pos)) = self.parse_between(
                    Self::parse_expression,
                    Keyword::OpenBrace,
                    Keyword::CloseBrace,
                    "Expected expression",
                )? {
                    ParseResult::Some((expr, pos))
                } else {
                    ParseResult::None
                }
            }
            fn parse_leaf_expression(&mut self) -> ParseResult<Expression> {
                if let Some((expr, pos)) = self.parse_parenthesised_expression()? {
                    ParseResult::Some((expr, pos))
                } else if let Some((value, pos)) = self.parse_integer_constant()? {
                    ParseResult::Some((Expression::IntegerConstant(value), pos))
                } else if let Some((c, pos)) = self.parse_char_constant()? {
                    ParseResult::Some((Expression::CharConstant(c), pos))
                } else if let Some((s, pos)) = self.parse_string_constant()? {
                    ParseResult::Some((Expression::StringConstant(s), pos))
                } else if let Some((_, pos)) = self.parse_keyword(Keyword::SizeOf)? {
                    if let Some((expr, _)) = self.parse_sizeof_arg()? {
                        ParseResult::Some((expr, pos))
                    } else {
                        ParseResult::None
                    }
                } else if let Some((path, pos)) = self.parse_qualified_path()? {
                    if let Some((args, _)) = self.parse_function_call_args()? {
                        ParseResult::Some((Expression::FunctionCall(path, args), pos))
                    } else {
                        ParseResult::Some((Expression::Variable(path), pos))
                    }
                } else {
                    ParseResult::None
                }
            }
            fn parse_array_access_expression(&mut self) -> ParseResult<Expression> {
                if let Some((expr, pos)) = self.parse_leaf_expression()? {
                    if let Some((arg, _)) = self.parse_array_access_arg()? {
                        ParseResult::Some((
                            Expression::ArrayAccess(Box::new(expr), Box::new(arg)),
                            pos,
                        ))
                    } else {
                        ParseResult::Some((expr, pos))
                    }
                } else {
                    ParseResult::None
                }
            }
            fn parse_unary_expression(&mut self) -> ParseResult<Expression> {
                if let Some(expr) = self.parse_array_access_expression()? {
                    ParseResult::Some(expr)
                } else {
                    let (op, (_, pos)) = {
                        if let Some(t) = self.parse_keyword(Keyword::Add)? {
                            (UnaryOperator::Positive, t)
                        } else if let Some(t) = self.parse_keyword(Keyword::Sub)? {
                            (UnaryOperator::Negative, t)
                        } else if let Some(t) = self.parse_keyword(Keyword::Not)? {
                            (UnaryOperator::Not, t)
                        } else if let Some(t) = self.parse_keyword(Keyword::BitAnd)? {
                            (UnaryOperator::AddressOf, t)
                        } else if let Some(t) = self.parse_keyword(Keyword::Mul)? {
                            (UnaryOperator::Deref, t)
                        } else {
                            return ParseResult::None;
                        }
                    };
                    if let Some((expr, _)) = self.parse_unary_expression()? {
                        ParseResult::Some((Expression::UnaryOperator(op, Box::new(expr)), pos))
                    } else {
                        let file = self.file_server.get_file_name(pos.file).into_owned();
                        let err = ParseError::new(
                            "Expected expression",
                            file,
                            pos.end_text_pos.line,
                            pos.end_text_pos.col,
                        );
                        ParseResult::Err(err)
                    }
                }
            }
            fn parse_aggregate_op(
                &mut self,
                ops: &[(Keyword, BinaryOperator)],
            ) -> ParseResult<BinaryOperator> {
                for (keyword, op) in ops.iter() {
                    if let Some((_, pos)) = self.parse_keyword(*keyword)? {
                        return ParseResult::Some((*op, pos));
                    }
                }
                ParseResult::None
            }
            fn parse_aggregate_expression(
                &mut self,
                mut parse_inner: impl FnMut(&mut Self) -> ParseResult<Expression>,
                ops: &[(Keyword, BinaryOperator)],
            ) -> ParseResult<Expression> {
                self.input.push_state();
                self.parse_whitespace()?;
                if let Some((first, first_pos)) = parse_inner(self)? {
                    self.input.drop_state();
                    let mut result = first;
                    loop {
                        if let Some((op, pos)) = self.parse_aggregate_op(ops)? {
                            if let Some((expr, _)) = parse_inner(self)? {
                                result = Expression::BinaryOperator(
                                    op,
                                    Box::new(result),
                                    Box::new(expr),
                                );
                            } else {
                                let file = self.file_server.get_file_name(pos.file).into_owned();
                                let err = ParseError::new(
                                    "Expected expression",
                                    file,
                                    pos.end_text_pos.line,
                                    pos.end_text_pos.col,
                                );
                                return ParseResult::Err(err);
                            }
                        } else {
                            break;
                        }
                    }
                    ParseResult::Some((result, first_pos))
                } else {
                    self.input.pop_state();
                    ParseResult::None
                }
            }
            fn parse_mul_div_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 3] = [
                    (Keyword::Mul, BinaryOperator::Multiply),
                    (Keyword::Div, BinaryOperator::Divide),
                    (Keyword::Rem, BinaryOperator::Remainder),
                ];
                self.parse_aggregate_expression(Self::parse_unary_expression, &OPS)
            }
            fn parse_add_sub_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 2] = [
                    (Keyword::Add, BinaryOperator::Add),
                    (Keyword::Sub, BinaryOperator::Subtract),
                ];
                self.parse_aggregate_expression(Self::parse_mul_div_expression, &OPS)
            }
            fn parse_shift_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 2] = [
                    (Keyword::LeftShift, BinaryOperator::LeftShift),
                    (Keyword::RightShift, BinaryOperator::RightShift),
                ];
                self.parse_aggregate_expression(Self::parse_add_sub_expression, &OPS)
            }
            fn parse_relational_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 4] = [
                    (Keyword::LessEqual, BinaryOperator::LessEqual),
                    (Keyword::Less, BinaryOperator::Less),
                    (Keyword::GreaterEqual, BinaryOperator::GreaterEqual),
                    (Keyword::Greater, BinaryOperator::Greater),
                ];
                self.parse_aggregate_expression(Self::parse_shift_expression, &OPS)
            }
            fn parse_equality_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 2] = [
                    (Keyword::Equals, BinaryOperator::Equals),
                    (Keyword::NotEquals, BinaryOperator::NotEquals),
                ];
                self.parse_aggregate_expression(Self::parse_relational_expression, &OPS)
            }
            fn parse_and_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 1] =
                    [(Keyword::BitAnd, BinaryOperator::And)];
                self.parse_aggregate_expression(Self::parse_equality_expression, &OPS)
            }
            fn parse_xor_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 1] = [(Keyword::Xor, BinaryOperator::Xor)];
                self.parse_aggregate_expression(Self::parse_and_expression, &OPS)
            }
            fn parse_or_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 1] = [(Keyword::BitOr, BinaryOperator::Or)];
                self.parse_aggregate_expression(Self::parse_xor_expression, &OPS)
            }
            fn parse_short_circuit_and_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 1] =
                    [(Keyword::LogicAnd, BinaryOperator::ShortCircuitAnd)];
                self.parse_aggregate_expression(Self::parse_or_expression, &OPS)
            }
            fn parse_short_circuit_or_expression(&mut self) -> ParseResult<Expression> {
                const OPS: [(Keyword, BinaryOperator); 1] =
                    [(Keyword::LogicOr, BinaryOperator::ShortCircuitOr)];
                self.parse_aggregate_expression(Self::parse_short_circuit_and_expression, &OPS)
            }
            pub fn parse_expression(&mut self) -> ParseResult<Expression> {
                self.parse_short_circuit_or_expression()
            }
            fn parse_parenthesised_expression(&mut self) -> ParseResult<Expression> {
                self.parse_between(
                    Self::parse_expression,
                    Keyword::OpenParen,
                    Keyword::CloseParen,
                    "Expected expression",
                )
            }
            fn parse_constant_declaration(&mut self) -> ParseResult<ConstantDeclaration> {
                let parse_const_keyword = |s: &mut Self| s.parse_keyword(Keyword::Const);
                let parse_colon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Colon);
                let parse_assign_keyword = |s: &mut Self| s.parse_keyword(Keyword::Assign);
                let parse_semicolon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Semicolon);
                if let Some((_t, first_pos)) = parse_const_keyword(self)? {
                    let pos = first_pos;
                    if let Some((_t, pos)) = Self::parse_whitespace(self)? {
                        if let Some((ident, pos)) = Self::parse_identifier(self)? {
                            if let Some((_t, pos)) = parse_colon_keyword(self)? {
                                if let Some((const_type, pos)) = Self::parse_identifier(self)? {
                                    if let Some((_t, pos)) = parse_assign_keyword(self)? {
                                        if let Some((expr, pos)) = Self::parse_expression(self)? {
                                            if let Some((_t, pos)) = parse_semicolon_keyword(self)?
                                            {
                                                {
                                                    ParseResult::None
                                                }
                                            } else {
                                                let file = self
                                                    .file_server
                                                    .get_file_name(pos.file)
                                                    .into_owned();
                                                let err = ParseError::new(
                                                    "",
                                                    file,
                                                    pos.end_text_pos.line,
                                                    pos.end_text_pos.col,
                                                );
                                                ParseResult::Err(err)
                                            }
                                        } else {
                                            let file = self
                                                .file_server
                                                .get_file_name(pos.file)
                                                .into_owned();
                                            let err = ParseError::new(
                                                "",
                                                file,
                                                pos.end_text_pos.line,
                                                pos.end_text_pos.col,
                                            );
                                            ParseResult::Err(err)
                                        }
                                    } else {
                                        let file =
                                            self.file_server.get_file_name(pos.file).into_owned();
                                        let err = ParseError::new(
                                            "",
                                            file,
                                            pos.end_text_pos.line,
                                            pos.end_text_pos.col,
                                        );
                                        ParseResult::Err(err)
                                    }
                                } else {
                                    let file =
                                        self.file_server.get_file_name(pos.file).into_owned();
                                    let err = ParseError::new(
                                        "",
                                        file,
                                        pos.end_text_pos.line,
                                        pos.end_text_pos.col,
                                    );
                                    ParseResult::Err(err)
                                }
                            } else {
                                let file = self.file_server.get_file_name(pos.file).into_owned();
                                let err = ParseError::new(
                                    "",
                                    file,
                                    pos.end_text_pos.line,
                                    pos.end_text_pos.col,
                                );
                                ParseResult::Err(err)
                            }
                        } else {
                            let file = self.file_server.get_file_name(pos.file).into_owned();
                            let err = ParseError::new(
                                "",
                                file,
                                pos.end_text_pos.line,
                                pos.end_text_pos.col,
                            );
                            ParseResult::Err(err)
                        }
                    } else {
                        let file = self.file_server.get_file_name(pos.file).into_owned();
                        let err =
                            ParseError::new("", file, pos.end_text_pos.line, pos.end_text_pos.col);
                        ParseResult::Err(err)
                    }
                } else {
                    ParseResult::None
                }
            }
        }
    }
    pub use file_server::*;
    pub use lexer::*;
    pub use parser::*;
    pub struct TextPosition {
        pub abs: usize,
        pub line: usize,
        pub col: usize,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for TextPosition {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                TextPosition {
                    abs: ref __self_0_0,
                    line: ref __self_0_1,
                    col: ref __self_0_2,
                } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f, "TextPosition");
                    let _ = ::core::fmt::DebugStruct::field(
                        debug_trait_builder,
                        "abs",
                        &&(*__self_0_0),
                    );
                    let _ = ::core::fmt::DebugStruct::field(
                        debug_trait_builder,
                        "line",
                        &&(*__self_0_1),
                    );
                    let _ = ::core::fmt::DebugStruct::field(
                        debug_trait_builder,
                        "col",
                        &&(*__self_0_2),
                    );
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for TextPosition {
        #[inline]
        fn clone(&self) -> TextPosition {
            {
                let _: ::core::clone::AssertParamIsClone<usize>;
                let _: ::core::clone::AssertParamIsClone<usize>;
                let _: ::core::clone::AssertParamIsClone<usize>;
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for TextPosition {}
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
    pub enum Keyword {
        AddAssign,
        SubAssign,
        MulAssign,
        DivAssign,
        RemAssign,
        Add,
        Sub,
        Mul,
        Div,
        Rem,
        LeftShiftAssign,
        RightShiftAssign,
        LeftShift,
        RightShift,
        Equals,
        NotEquals,
        LessEqual,
        Less,
        GreaterEqual,
        Greater,
        LogicAndAssign,
        LogicOrAssign,
        BitAndAssign,
        BitOrAssign,
        XorAssign,
        LogicAnd,
        LogicOr,
        BitAnd,
        BitOr,
        Xor,
        Not,
        Assign,
        OpenParen,
        CloseParen,
        OpenBrace,
        CloseBrace,
        OpenCurl,
        CloseCurl,
        Semicolon,
        Comma,
        Dot,
        Colon,
        Var,
        Const,
        If,
        Else,
        While,
        For,
        Break,
        Fn,
        Return,
        Word,
        UWord,
        Int,
        UInt,
        Long,
        ULong,
        Char,
        SizeOf,
        Struct,
        Union,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for Keyword {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&Keyword::AddAssign,) => ::core::fmt::Formatter::write_str(f, "AddAssign"),
                (&Keyword::SubAssign,) => ::core::fmt::Formatter::write_str(f, "SubAssign"),
                (&Keyword::MulAssign,) => ::core::fmt::Formatter::write_str(f, "MulAssign"),
                (&Keyword::DivAssign,) => ::core::fmt::Formatter::write_str(f, "DivAssign"),
                (&Keyword::RemAssign,) => ::core::fmt::Formatter::write_str(f, "RemAssign"),
                (&Keyword::Add,) => ::core::fmt::Formatter::write_str(f, "Add"),
                (&Keyword::Sub,) => ::core::fmt::Formatter::write_str(f, "Sub"),
                (&Keyword::Mul,) => ::core::fmt::Formatter::write_str(f, "Mul"),
                (&Keyword::Div,) => ::core::fmt::Formatter::write_str(f, "Div"),
                (&Keyword::Rem,) => ::core::fmt::Formatter::write_str(f, "Rem"),
                (&Keyword::LeftShiftAssign,) => {
                    ::core::fmt::Formatter::write_str(f, "LeftShiftAssign")
                }
                (&Keyword::RightShiftAssign,) => {
                    ::core::fmt::Formatter::write_str(f, "RightShiftAssign")
                }
                (&Keyword::LeftShift,) => ::core::fmt::Formatter::write_str(f, "LeftShift"),
                (&Keyword::RightShift,) => ::core::fmt::Formatter::write_str(f, "RightShift"),
                (&Keyword::Equals,) => ::core::fmt::Formatter::write_str(f, "Equals"),
                (&Keyword::NotEquals,) => ::core::fmt::Formatter::write_str(f, "NotEquals"),
                (&Keyword::LessEqual,) => ::core::fmt::Formatter::write_str(f, "LessEqual"),
                (&Keyword::Less,) => ::core::fmt::Formatter::write_str(f, "Less"),
                (&Keyword::GreaterEqual,) => ::core::fmt::Formatter::write_str(f, "GreaterEqual"),
                (&Keyword::Greater,) => ::core::fmt::Formatter::write_str(f, "Greater"),
                (&Keyword::LogicAndAssign,) => {
                    ::core::fmt::Formatter::write_str(f, "LogicAndAssign")
                }
                (&Keyword::LogicOrAssign,) => ::core::fmt::Formatter::write_str(f, "LogicOrAssign"),
                (&Keyword::BitAndAssign,) => ::core::fmt::Formatter::write_str(f, "BitAndAssign"),
                (&Keyword::BitOrAssign,) => ::core::fmt::Formatter::write_str(f, "BitOrAssign"),
                (&Keyword::XorAssign,) => ::core::fmt::Formatter::write_str(f, "XorAssign"),
                (&Keyword::LogicAnd,) => ::core::fmt::Formatter::write_str(f, "LogicAnd"),
                (&Keyword::LogicOr,) => ::core::fmt::Formatter::write_str(f, "LogicOr"),
                (&Keyword::BitAnd,) => ::core::fmt::Formatter::write_str(f, "BitAnd"),
                (&Keyword::BitOr,) => ::core::fmt::Formatter::write_str(f, "BitOr"),
                (&Keyword::Xor,) => ::core::fmt::Formatter::write_str(f, "Xor"),
                (&Keyword::Not,) => ::core::fmt::Formatter::write_str(f, "Not"),
                (&Keyword::Assign,) => ::core::fmt::Formatter::write_str(f, "Assign"),
                (&Keyword::OpenParen,) => ::core::fmt::Formatter::write_str(f, "OpenParen"),
                (&Keyword::CloseParen,) => ::core::fmt::Formatter::write_str(f, "CloseParen"),
                (&Keyword::OpenBrace,) => ::core::fmt::Formatter::write_str(f, "OpenBrace"),
                (&Keyword::CloseBrace,) => ::core::fmt::Formatter::write_str(f, "CloseBrace"),
                (&Keyword::OpenCurl,) => ::core::fmt::Formatter::write_str(f, "OpenCurl"),
                (&Keyword::CloseCurl,) => ::core::fmt::Formatter::write_str(f, "CloseCurl"),
                (&Keyword::Semicolon,) => ::core::fmt::Formatter::write_str(f, "Semicolon"),
                (&Keyword::Comma,) => ::core::fmt::Formatter::write_str(f, "Comma"),
                (&Keyword::Dot,) => ::core::fmt::Formatter::write_str(f, "Dot"),
                (&Keyword::Colon,) => ::core::fmt::Formatter::write_str(f, "Colon"),
                (&Keyword::Var,) => ::core::fmt::Formatter::write_str(f, "Var"),
                (&Keyword::Const,) => ::core::fmt::Formatter::write_str(f, "Const"),
                (&Keyword::If,) => ::core::fmt::Formatter::write_str(f, "If"),
                (&Keyword::Else,) => ::core::fmt::Formatter::write_str(f, "Else"),
                (&Keyword::While,) => ::core::fmt::Formatter::write_str(f, "While"),
                (&Keyword::For,) => ::core::fmt::Formatter::write_str(f, "For"),
                (&Keyword::Break,) => ::core::fmt::Formatter::write_str(f, "Break"),
                (&Keyword::Fn,) => ::core::fmt::Formatter::write_str(f, "Fn"),
                (&Keyword::Return,) => ::core::fmt::Formatter::write_str(f, "Return"),
                (&Keyword::Word,) => ::core::fmt::Formatter::write_str(f, "Word"),
                (&Keyword::UWord,) => ::core::fmt::Formatter::write_str(f, "UWord"),
                (&Keyword::Int,) => ::core::fmt::Formatter::write_str(f, "Int"),
                (&Keyword::UInt,) => ::core::fmt::Formatter::write_str(f, "UInt"),
                (&Keyword::Long,) => ::core::fmt::Formatter::write_str(f, "Long"),
                (&Keyword::ULong,) => ::core::fmt::Formatter::write_str(f, "ULong"),
                (&Keyword::Char,) => ::core::fmt::Formatter::write_str(f, "Char"),
                (&Keyword::SizeOf,) => ::core::fmt::Formatter::write_str(f, "SizeOf"),
                (&Keyword::Struct,) => ::core::fmt::Formatter::write_str(f, "Struct"),
                (&Keyword::Union,) => ::core::fmt::Formatter::write_str(f, "Union"),
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for Keyword {
        #[inline]
        fn clone(&self) -> Keyword {
            {
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for Keyword {}
    impl ::core::marker::StructuralPartialEq for Keyword {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for Keyword {
        #[inline]
        fn eq(&self, other: &Keyword) -> bool {
            {
                let __self_vi = ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        _ => true,
                    }
                } else {
                    false
                }
            }
        }
    }
    impl ::core::marker::StructuralEq for Keyword {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for Keyword {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {}
        }
    }
    pub enum TokenKind {
        Whitespace(bool),
        IntegerLiteral(i128),
        CharLiteral(char),
        StringLiteral(String),
        Keyword(Keyword),
        Identifier(String),
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for TokenKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&TokenKind::Whitespace(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Whitespace");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&TokenKind::IntegerLiteral(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "IntegerLiteral");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&TokenKind::CharLiteral(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "CharLiteral");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&TokenKind::StringLiteral(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "StringLiteral");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&TokenKind::Keyword(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Keyword");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&TokenKind::Identifier(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Identifier");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for TokenKind {
        #[inline]
        fn clone(&self) -> TokenKind {
            match (&*self,) {
                (&TokenKind::Whitespace(ref __self_0),) => {
                    TokenKind::Whitespace(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&TokenKind::IntegerLiteral(ref __self_0),) => {
                    TokenKind::IntegerLiteral(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&TokenKind::CharLiteral(ref __self_0),) => {
                    TokenKind::CharLiteral(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&TokenKind::StringLiteral(ref __self_0),) => {
                    TokenKind::StringLiteral(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&TokenKind::Keyword(ref __self_0),) => {
                    TokenKind::Keyword(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&TokenKind::Identifier(ref __self_0),) => {
                    TokenKind::Identifier(::core::clone::Clone::clone(&(*__self_0)))
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for TokenKind {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for TokenKind {
        #[inline]
        fn eq(&self, other: &TokenKind) -> bool {
            {
                let __self_vi = ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (
                            &TokenKind::Whitespace(ref __self_0),
                            &TokenKind::Whitespace(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &TokenKind::IntegerLiteral(ref __self_0),
                            &TokenKind::IntegerLiteral(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &TokenKind::CharLiteral(ref __self_0),
                            &TokenKind::CharLiteral(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &TokenKind::StringLiteral(ref __self_0),
                            &TokenKind::StringLiteral(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&TokenKind::Keyword(ref __self_0), &TokenKind::Keyword(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &TokenKind::Identifier(ref __self_0),
                            &TokenKind::Identifier(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() },
                    }
                } else {
                    false
                }
            }
        }
        #[inline]
        fn ne(&self, other: &TokenKind) -> bool {
            {
                let __self_vi = ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (
                            &TokenKind::Whitespace(ref __self_0),
                            &TokenKind::Whitespace(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &TokenKind::IntegerLiteral(ref __self_0),
                            &TokenKind::IntegerLiteral(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &TokenKind::CharLiteral(ref __self_0),
                            &TokenKind::CharLiteral(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &TokenKind::StringLiteral(ref __self_0),
                            &TokenKind::StringLiteral(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&TokenKind::Keyword(ref __self_0), &TokenKind::Keyword(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &TokenKind::Identifier(ref __self_0),
                            &TokenKind::Identifier(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() },
                    }
                } else {
                    true
                }
            }
        }
    }
    impl ::core::marker::StructuralEq for TokenKind {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for TokenKind {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<bool>;
                let _: ::core::cmp::AssertParamIsEq<i128>;
                let _: ::core::cmp::AssertParamIsEq<char>;
                let _: ::core::cmp::AssertParamIsEq<String>;
                let _: ::core::cmp::AssertParamIsEq<Keyword>;
                let _: ::core::cmp::AssertParamIsEq<String>;
            }
        }
    }
    pub struct TokenPosition {
        pub file: FileHandle,
        pub start_text_pos: TextPosition,
        pub end_text_pos: TextPosition,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for TokenPosition {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                TokenPosition {
                    file: ref __self_0_0,
                    start_text_pos: ref __self_0_1,
                    end_text_pos: ref __self_0_2,
                } => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_struct(f, "TokenPosition");
                    let _ = ::core::fmt::DebugStruct::field(
                        debug_trait_builder,
                        "file",
                        &&(*__self_0_0),
                    );
                    let _ = ::core::fmt::DebugStruct::field(
                        debug_trait_builder,
                        "start_text_pos",
                        &&(*__self_0_1),
                    );
                    let _ = ::core::fmt::DebugStruct::field(
                        debug_trait_builder,
                        "end_text_pos",
                        &&(*__self_0_2),
                    );
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for TokenPosition {
        #[inline]
        fn clone(&self) -> TokenPosition {
            {
                let _: ::core::clone::AssertParamIsClone<FileHandle>;
                let _: ::core::clone::AssertParamIsClone<TextPosition>;
                let _: ::core::clone::AssertParamIsClone<TextPosition>;
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for TokenPosition {}
    pub struct Token {
        pub kind: TokenKind,
        pub pos: TokenPosition,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for Token {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                Token {
                    kind: ref __self_0_0,
                    pos: ref __self_0_1,
                } => {
                    let debug_trait_builder = &mut ::core::fmt::Formatter::debug_struct(f, "Token");
                    let _ = ::core::fmt::DebugStruct::field(
                        debug_trait_builder,
                        "kind",
                        &&(*__self_0_0),
                    );
                    let _ = ::core::fmt::DebugStruct::field(
                        debug_trait_builder,
                        "pos",
                        &&(*__self_0_1),
                    );
                    ::core::fmt::DebugStruct::finish(debug_trait_builder)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for Token {
        #[inline]
        fn clone(&self) -> Token {
            match *self {
                Token {
                    kind: ref __self_0_0,
                    pos: ref __self_0_1,
                } => Token {
                    kind: ::core::clone::Clone::clone(&(*__self_0_0)),
                    pos: ::core::clone::Clone::clone(&(*__self_0_1)),
                },
            }
        }
    }
    impl Token {
        pub const fn new(kind: TokenKind, pos: TokenPosition) -> Self {
            Self { kind, pos }
        }
    }
    pub enum UnaryOperator {
        Positive,
        Negative,
        Not,
        AddressOf,
        Deref,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for UnaryOperator {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&UnaryOperator::Positive,) => ::core::fmt::Formatter::write_str(f, "Positive"),
                (&UnaryOperator::Negative,) => ::core::fmt::Formatter::write_str(f, "Negative"),
                (&UnaryOperator::Not,) => ::core::fmt::Formatter::write_str(f, "Not"),
                (&UnaryOperator::AddressOf,) => ::core::fmt::Formatter::write_str(f, "AddressOf"),
                (&UnaryOperator::Deref,) => ::core::fmt::Formatter::write_str(f, "Deref"),
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for UnaryOperator {
        #[inline]
        fn clone(&self) -> UnaryOperator {
            {
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for UnaryOperator {}
    impl ::core::marker::StructuralPartialEq for UnaryOperator {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for UnaryOperator {
        #[inline]
        fn eq(&self, other: &UnaryOperator) -> bool {
            {
                let __self_vi = ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        _ => true,
                    }
                } else {
                    false
                }
            }
        }
    }
    impl ::core::marker::StructuralEq for UnaryOperator {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for UnaryOperator {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {}
        }
    }
    pub enum BinaryOperator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Remainder,
        LeftShift,
        RightShift,
        ShortCircuitAnd,
        ShortCircuitOr,
        And,
        Or,
        Xor,
        Equals,
        NotEquals,
        LessEqual,
        Less,
        GreaterEqual,
        Greater,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for BinaryOperator {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&BinaryOperator::Add,) => ::core::fmt::Formatter::write_str(f, "Add"),
                (&BinaryOperator::Subtract,) => ::core::fmt::Formatter::write_str(f, "Subtract"),
                (&BinaryOperator::Multiply,) => ::core::fmt::Formatter::write_str(f, "Multiply"),
                (&BinaryOperator::Divide,) => ::core::fmt::Formatter::write_str(f, "Divide"),
                (&BinaryOperator::Remainder,) => ::core::fmt::Formatter::write_str(f, "Remainder"),
                (&BinaryOperator::LeftShift,) => ::core::fmt::Formatter::write_str(f, "LeftShift"),
                (&BinaryOperator::RightShift,) => {
                    ::core::fmt::Formatter::write_str(f, "RightShift")
                }
                (&BinaryOperator::ShortCircuitAnd,) => {
                    ::core::fmt::Formatter::write_str(f, "ShortCircuitAnd")
                }
                (&BinaryOperator::ShortCircuitOr,) => {
                    ::core::fmt::Formatter::write_str(f, "ShortCircuitOr")
                }
                (&BinaryOperator::And,) => ::core::fmt::Formatter::write_str(f, "And"),
                (&BinaryOperator::Or,) => ::core::fmt::Formatter::write_str(f, "Or"),
                (&BinaryOperator::Xor,) => ::core::fmt::Formatter::write_str(f, "Xor"),
                (&BinaryOperator::Equals,) => ::core::fmt::Formatter::write_str(f, "Equals"),
                (&BinaryOperator::NotEquals,) => ::core::fmt::Formatter::write_str(f, "NotEquals"),
                (&BinaryOperator::LessEqual,) => ::core::fmt::Formatter::write_str(f, "LessEqual"),
                (&BinaryOperator::Less,) => ::core::fmt::Formatter::write_str(f, "Less"),
                (&BinaryOperator::GreaterEqual,) => {
                    ::core::fmt::Formatter::write_str(f, "GreaterEqual")
                }
                (&BinaryOperator::Greater,) => ::core::fmt::Formatter::write_str(f, "Greater"),
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for BinaryOperator {
        #[inline]
        fn clone(&self) -> BinaryOperator {
            {
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for BinaryOperator {}
    impl ::core::marker::StructuralPartialEq for BinaryOperator {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for BinaryOperator {
        #[inline]
        fn eq(&self, other: &BinaryOperator) -> bool {
            {
                let __self_vi = ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        _ => true,
                    }
                } else {
                    false
                }
            }
        }
    }
    impl ::core::marker::StructuralEq for BinaryOperator {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for BinaryOperator {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {}
        }
    }
    pub struct QualifiedPath(pub Vec<String>);
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for QualifiedPath {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {
                QualifiedPath(ref __self_0_0) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "QualifiedPath");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for QualifiedPath {
        #[inline]
        fn clone(&self) -> QualifiedPath {
            match *self {
                QualifiedPath(ref __self_0_0) => {
                    QualifiedPath(::core::clone::Clone::clone(&(*__self_0_0)))
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for QualifiedPath {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for QualifiedPath {
        #[inline]
        fn eq(&self, other: &QualifiedPath) -> bool {
            match *other {
                QualifiedPath(ref __self_1_0) => match *self {
                    QualifiedPath(ref __self_0_0) => (*__self_0_0) == (*__self_1_0),
                },
            }
        }
        #[inline]
        fn ne(&self, other: &QualifiedPath) -> bool {
            match *other {
                QualifiedPath(ref __self_1_0) => match *self {
                    QualifiedPath(ref __self_0_0) => (*__self_0_0) != (*__self_1_0),
                },
            }
        }
    }
    impl ::core::marker::StructuralEq for QualifiedPath {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for QualifiedPath {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<Vec<String>>;
            }
        }
    }
    pub enum Expression {
        IntegerConstant(i128),
        CharConstant(char),
        StringConstant(String),
        Variable(QualifiedPath),
        SizeOf(QualifiedPath),
        FunctionCall(QualifiedPath, Vec<Expression>),
        ArrayAccess(Box<Expression>, Box<Expression>),
        UnaryOperator(UnaryOperator, Box<Expression>),
        BinaryOperator(BinaryOperator, Box<Expression>, Box<Expression>),
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for Expression {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&Expression::IntegerConstant(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "IntegerConstant");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&Expression::CharConstant(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "CharConstant");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&Expression::StringConstant(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "StringConstant");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&Expression::Variable(ref __self_0),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "Variable");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&Expression::SizeOf(ref __self_0),) => {
                    let debug_trait_builder = &mut ::core::fmt::Formatter::debug_tuple(f, "SizeOf");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&Expression::FunctionCall(ref __self_0, ref __self_1),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "FunctionCall");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_1));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&Expression::ArrayAccess(ref __self_0, ref __self_1),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "ArrayAccess");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_1));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&Expression::UnaryOperator(ref __self_0, ref __self_1),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "UnaryOperator");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_1));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
                (&Expression::BinaryOperator(ref __self_0, ref __self_1, ref __self_2),) => {
                    let debug_trait_builder =
                        &mut ::core::fmt::Formatter::debug_tuple(f, "BinaryOperator");
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_1));
                    let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_2));
                    ::core::fmt::DebugTuple::finish(debug_trait_builder)
                }
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for Expression {
        #[inline]
        fn clone(&self) -> Expression {
            match (&*self,) {
                (&Expression::IntegerConstant(ref __self_0),) => {
                    Expression::IntegerConstant(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&Expression::CharConstant(ref __self_0),) => {
                    Expression::CharConstant(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&Expression::StringConstant(ref __self_0),) => {
                    Expression::StringConstant(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&Expression::Variable(ref __self_0),) => {
                    Expression::Variable(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&Expression::SizeOf(ref __self_0),) => {
                    Expression::SizeOf(::core::clone::Clone::clone(&(*__self_0)))
                }
                (&Expression::FunctionCall(ref __self_0, ref __self_1),) => {
                    Expression::FunctionCall(
                        ::core::clone::Clone::clone(&(*__self_0)),
                        ::core::clone::Clone::clone(&(*__self_1)),
                    )
                }
                (&Expression::ArrayAccess(ref __self_0, ref __self_1),) => Expression::ArrayAccess(
                    ::core::clone::Clone::clone(&(*__self_0)),
                    ::core::clone::Clone::clone(&(*__self_1)),
                ),
                (&Expression::UnaryOperator(ref __self_0, ref __self_1),) => {
                    Expression::UnaryOperator(
                        ::core::clone::Clone::clone(&(*__self_0)),
                        ::core::clone::Clone::clone(&(*__self_1)),
                    )
                }
                (&Expression::BinaryOperator(ref __self_0, ref __self_1, ref __self_2),) => {
                    Expression::BinaryOperator(
                        ::core::clone::Clone::clone(&(*__self_0)),
                        ::core::clone::Clone::clone(&(*__self_1)),
                        ::core::clone::Clone::clone(&(*__self_2)),
                    )
                }
            }
        }
    }
    pub type Type = String;
    pub struct ConstantDeclaration {
        pub identifier: String,
        pub const_type: Type,
        pub value: Expression,
    }
    pub struct VariableDeclaration {
        pub identifier: String,
        pub var_type: Type,
        pub initial_value: Option<Expression>,
    }
    pub enum AssignmentKind {
        Normal,
        Add,
        Subtract,
        Multiply,
        Divide,
        Remainder,
        LeftShift,
        RightShift,
        LogicAnd,
        LogicOr,
        BitAnd,
        BitOr,
        Xor,
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::fmt::Debug for AssignmentKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match (&*self,) {
                (&AssignmentKind::Normal,) => ::core::fmt::Formatter::write_str(f, "Normal"),
                (&AssignmentKind::Add,) => ::core::fmt::Formatter::write_str(f, "Add"),
                (&AssignmentKind::Subtract,) => ::core::fmt::Formatter::write_str(f, "Subtract"),
                (&AssignmentKind::Multiply,) => ::core::fmt::Formatter::write_str(f, "Multiply"),
                (&AssignmentKind::Divide,) => ::core::fmt::Formatter::write_str(f, "Divide"),
                (&AssignmentKind::Remainder,) => ::core::fmt::Formatter::write_str(f, "Remainder"),
                (&AssignmentKind::LeftShift,) => ::core::fmt::Formatter::write_str(f, "LeftShift"),
                (&AssignmentKind::RightShift,) => {
                    ::core::fmt::Formatter::write_str(f, "RightShift")
                }
                (&AssignmentKind::LogicAnd,) => ::core::fmt::Formatter::write_str(f, "LogicAnd"),
                (&AssignmentKind::LogicOr,) => ::core::fmt::Formatter::write_str(f, "LogicOr"),
                (&AssignmentKind::BitAnd,) => ::core::fmt::Formatter::write_str(f, "BitAnd"),
                (&AssignmentKind::BitOr,) => ::core::fmt::Formatter::write_str(f, "BitOr"),
                (&AssignmentKind::Xor,) => ::core::fmt::Formatter::write_str(f, "Xor"),
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::clone::Clone for AssignmentKind {
        #[inline]
        fn clone(&self) -> AssignmentKind {
            {
                *self
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::marker::Copy for AssignmentKind {}
    impl ::core::marker::StructuralPartialEq for AssignmentKind {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::PartialEq for AssignmentKind {
        #[inline]
        fn eq(&self, other: &AssignmentKind) -> bool {
            {
                let __self_vi = ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        _ => true,
                    }
                } else {
                    false
                }
            }
        }
    }
    impl ::core::marker::StructuralEq for AssignmentKind {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl ::core::cmp::Eq for AssignmentKind {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            {}
        }
    }
    pub struct Assignment {
        pub target: QualifiedPath,
        pub kind: AssignmentKind,
        pub value: Expression,
    }
    pub struct IfStatement {
        pub condition: Expression,
        pub true_body: Vec<Statement>,
        pub false_body: Vec<Statement>,
    }
    pub struct WhileLoop {
        pub condition: Expression,
        pub body: Vec<Statement>,
    }
    pub struct ForLoop {
        pub condition: Expression,
        pub body: Vec<Statement>,
    }
    pub enum Statement {
        ConstantDeclaration(ConstantDeclaration),
        VariableDeclaration(VariableDeclaration),
        Assignment(Assignment),
        If(IfStatement),
        While(WhileLoop),
        For(ForLoop),
        Break(usize),
        Return,
    }
    pub struct Argument {
        pub identifier: String,
        pub arg_type: Type,
    }
    pub struct FunctionDefinition {
        pub identifier: String,
        pub return_type: Option<Type>,
        pub args: Vec<Argument>,
        pub body: Vec<Statement>,
    }
    pub struct FieldDefinition {
        pub identifier: String,
        pub field_type: Type,
    }
    pub struct StructDefinition {
        pub identifier: String,
        pub fields: Vec<FieldDefinition>,
    }
    pub enum TopLevelStatement {
        ConstantDeclaration(ConstantDeclaration),
        FunctionDefinition(FunctionDefinition),
        StructDefinition(StructDefinition),
        UnionDefinition(StructDefinition),
    }
}
use parsing::*;
use std::io;
use std::ops::{FromResidual, Try};
const TEST_FILE: &str = "*1[0] * (&1 + -1) * 1";
pub enum OptResult<T, E> {
    None,
    Some(T),
    Err(E),
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl<T: ::core::fmt::Debug, E: ::core::fmt::Debug> ::core::fmt::Debug for OptResult<T, E> {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match (&*self,) {
            (&OptResult::None,) => ::core::fmt::Formatter::write_str(f, "None"),
            (&OptResult::Some(ref __self_0),) => {
                let debug_trait_builder = &mut ::core::fmt::Formatter::debug_tuple(f, "Some");
                let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&OptResult::Err(ref __self_0),) => {
                let debug_trait_builder = &mut ::core::fmt::Formatter::debug_tuple(f, "Err");
                let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
        }
    }
}
impl<T, E> FromResidual for OptResult<T, E> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        Self::Err(residual)
    }
}
impl<T, E> Try for OptResult<T, E> {
    type Output = Option<T>;
    type Residual = E;
    fn from_output(output: Self::Output) -> Self {
        match output {
            Some(t) => Self::Some(t),
            None => Self::None,
        }
    }
    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            OptResult::None => std::ops::ControlFlow::Continue(None),
            OptResult::Some(t) => std::ops::ControlFlow::Continue(Some(t)),
            OptResult::Err(err) => std::ops::ControlFlow::Break(err),
        }
    }
}
impl<T, E> Into<Result<Option<T>, E>> for OptResult<T, E> {
    fn into(self) -> Result<Option<T>, E> {
        match self {
            OptResult::None => Ok(None),
            OptResult::Some(t) => Ok(Some(t)),
            OptResult::Err(err) => Err(err),
        }
    }
}
enum CompilerError {
    Custom(String),
    IoError(io::Error),
    LexerError(LexerError),
    ParseError(ParseError),
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for CompilerError {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match (&*self,) {
            (&CompilerError::Custom(ref __self_0),) => {
                let debug_trait_builder = &mut ::core::fmt::Formatter::debug_tuple(f, "Custom");
                let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&CompilerError::IoError(ref __self_0),) => {
                let debug_trait_builder = &mut ::core::fmt::Formatter::debug_tuple(f, "IoError");
                let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&CompilerError::LexerError(ref __self_0),) => {
                let debug_trait_builder = &mut ::core::fmt::Formatter::debug_tuple(f, "LexerError");
                let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
            (&CompilerError::ParseError(ref __self_0),) => {
                let debug_trait_builder = &mut ::core::fmt::Formatter::debug_tuple(f, "ParseError");
                let _ = ::core::fmt::DebugTuple::field(debug_trait_builder, &&(*__self_0));
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
        }
    }
}
impl Into<()> for CompilerError {
    fn into(self) -> () {}
}
impl From<&str> for CompilerError {
    fn from(err: &str) -> Self {
        Self::Custom(err.to_string())
    }
}
impl From<io::Error> for CompilerError {
    fn from(err: io::Error) -> Self {
        Self::IoError(err)
    }
}
impl From<LexerError> for CompilerError {
    fn from(err: LexerError) -> Self {
        Self::LexerError(err)
    }
}
impl From<ParseError> for CompilerError {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}
type CompilerResult<T> = Result<T, CompilerError>;
fn tokenize(file_server: &mut FileServer, file: FileHandle) -> CompilerResult<Vec<Token>> {
    let mut lexer = Lexer::create(file_server, file)?;
    let mut result: Vec<Token> = Vec::new();
    loop {
        match lexer.next_token() {
            OptResult::None => break,
            OptResult::Some(token) => result.push(token),
            OptResult::Err(err) => return Err(err.into()),
        }
    }
    Ok(result)
}
fn main() -> CompilerResult<()> {
    let mut file_server = FileServer::new();
    let file = file_server.add_dummy_file("test.c", TEST_FILE);
    let tokens = tokenize(&mut file_server, file)?;
    let mut parser = Parser::new(&file_server, &tokens);
    match parser.parse_expression() {
        OptResult::None => return Err("Expected expression".into()),
        OptResult::Some(expr) => {
            ::std::io::_print(::core::fmt::Arguments::new_v1_formatted(
                &["", "\n"],
                &match (&expr,) {
                    (arg0,) => [::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Debug::fmt)],
                },
                &[::core::fmt::rt::v1::Argument {
                    position: 0usize,
                    format: ::core::fmt::rt::v1::FormatSpec {
                        fill: ' ',
                        align: ::core::fmt::rt::v1::Alignment::Unknown,
                        flags: 4u32,
                        precision: ::core::fmt::rt::v1::Count::Implied,
                        width: ::core::fmt::rt::v1::Count::Implied,
                    },
                }],
                unsafe { ::core::fmt::UnsafeArg::new() },
            ));
        }
        OptResult::Err(err) => return Err(err.into()),
    }
    Ok(())
}
