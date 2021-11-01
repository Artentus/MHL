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

#[derive(Debug)]
pub struct LexerError {
    pub msg: &'static str,
    pub file: String,
    pub line: usize,
    pub col: usize,
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
        write!(
            f,
            "{} at {}:{}:{}",
            self.msg, self.file, self.line, self.col
        )
    }
}

pub type LexerResult<T> = crate::OptResult<T, LexerError>;

macro_rules! next {
    ($self: expr, $c: ident, $inner: block) => {
        if let Some($c) = $self.input.current() {
            $inner
        } else {
            LexerResult::None
        }
    };
}

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
    pub fn create(file_server: &'a mut FileServer, file: FileHandle) -> std::io::Result<Self> {
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
        next!(self, c, {
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
        })
    }

    fn next_prefixed_integer_literal(
        &mut self,
        prefix_char: char,
        predicate: fn(&char) -> bool,
        radix: u32,
        err_msg: &'static str,
    ) -> LexerResult<Token> {
        next!(self, c, {
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
                let err = LexerError::new(err_msg, self.file_name.to_string(), pos.line, pos.col);
                LexerResult::Err(err)
            } else {
                LexerResult::None
            }
        })
    }

    fn next_hex_integer_literal(&mut self) -> LexerResult<Token> {
        self.next_prefixed_integer_literal('x', is_ascii_hexdigit, 16, "Expected hex digit")
    }

    fn next_bin_integer_literal(&mut self) -> LexerResult<Token> {
        self.next_prefixed_integer_literal('b', is_ascii_bindigit, 2, "Expected binary digit")
    }

    fn next_oct_integer_literal(&mut self) -> LexerResult<Token> {
        self.next_prefixed_integer_literal('o', is_ascii_octdigit, 8, "Expected octal digit")
    }

    fn next_dec_integer_literal(&mut self) -> LexerResult<Token> {
        next!(self, c, {
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
        })
    }

    fn next_char(&mut self) -> LexerResult<(char, bool)> {
        next!(self, c, {
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
        })
    }

    fn next_char_literal(&mut self) -> LexerResult<Token> {
        next!(self, c, {
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
        })
    }

    fn next_string_literal(&mut self) -> LexerResult<Token> {
        next!(self, c, {
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
        })
    }

    fn next_match(&mut self, pattern: &str, check_for_identifier: bool) -> LexerResult<()> {
        self.input.save_state();

        let s = self.advance_by(pattern.chars().count());
        if s.eq(pattern) {
            if let Some(c) = self.input.current() {
                if check_for_identifier && (c.is_ascii_alphanumeric() || (c == '_')) {
                    self.input.restore_state();
                    LexerResult::None
                } else {
                    LexerResult::Some(())
                }
            } else {
                LexerResult::Some(())
            }
        } else {
            self.input.restore_state();
            LexerResult::None
        }
    }

    fn next_keyword(&mut self) -> LexerResult<Token> {
        const PATTERN_TABLE: &'static [(&'static str, bool, Keyword)] = &[
            ("+=", false, Keyword::AddAssign),
            ("-=", false, Keyword::SubAssign),
            ("*=", false, Keyword::MulAssign),
            ("/=", false, Keyword::DivAssign),
            ("%=", false, Keyword::RemAssign),
            ("+", false, Keyword::Add),
            ("-", false, Keyword::Sub),
            ("*", false, Keyword::Mul),
            ("/", false, Keyword::Div),
            ("%", false, Keyword::Rem),
            ("<<=", false, Keyword::LeftShiftAssign),
            (">>=", false, Keyword::RightShiftAssign),
            ("<<", false, Keyword::LeftShift),
            (">>", false, Keyword::RightShift),
            ("==", false, Keyword::Equals),
            ("!=", false, Keyword::NotEquals),
            ("<=", false, Keyword::LessEqual),
            ("<", false, Keyword::Less),
            (">=", false, Keyword::GreaterEqual),
            (">", false, Keyword::Greater),
            ("&=", false, Keyword::AndAssign),
            ("|=", false, Keyword::OrAssign),
            ("^=", false, Keyword::XorAssign),
            ("&&", false, Keyword::LogicAnd),
            ("||", false, Keyword::LogicOr),
            ("&", false, Keyword::BitAnd),
            ("|", false, Keyword::BitOr),
            ("^", false, Keyword::Xor),
            ("!", false, Keyword::Not),
            ("=", false, Keyword::Assign),
            ("(", false, Keyword::OpenParen),
            (")", false, Keyword::CloseParen),
            ("[", false, Keyword::OpenBrace),
            ("]", false, Keyword::CloseBrace),
            ("{", false, Keyword::OpenCurl),
            ("}", false, Keyword::CloseCurl),
            (";", false, Keyword::Semicolon),
            (",", false, Keyword::Comma),
            (".", false, Keyword::Dot),
            (":", false, Keyword::Colon),
            ("var", true, Keyword::Var),
            ("const", true, Keyword::Const),
            ("if", true, Keyword::If),
            ("else", true, Keyword::Else),
            ("switch", true, Keyword::Switch),
            ("case", true, Keyword::Case),
            ("while", true, Keyword::While),
            ("for", true, Keyword::For),
            ("break", true, Keyword::Break),
            ("continue", true, Keyword::Continue),
            ("fn", true, Keyword::Fn),
            ("return", true, Keyword::Return),
            ("word", true, Keyword::Word),
            ("uword", true, Keyword::UWord),
            ("int", true, Keyword::Int),
            ("uint", true, Keyword::UInt),
            ("long", true, Keyword::Long),
            ("ulong", true, Keyword::ULong),
            ("char", true, Keyword::Char),
            ("sizeof", true, Keyword::SizeOf),
            ("struct", true, Keyword::Struct),
            ("union", true, Keyword::Union),
        ];

        let start = self.input.pos();
        for (pattern, check_for_identifier, keyword) in PATTERN_TABLE.iter() {
            if let Some(_) = self.next_match(*pattern, *check_for_identifier)? {
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
        next!(self, c, {
            let start = self.input.pos();

            if c.is_ascii_alphabetic() || (c == '_') {
                let s = self.advance_while(|c| c.is_ascii_alphanumeric() || (*c == '_'));

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
        })
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
