use super::*;
use crate::ast::*;
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

#[derive(Debug)]
pub struct ParseError {
    pub msg: &'static str,
    pub file: String,
    pub line: usize,
    pub col: usize,
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
        write!(
            f,
            "{} at {}:{}:{}",
            self.msg, self.file, self.line, self.col
        )
    }
}

pub type ParseResult<T> = crate::OptResult<(T, TokenPosition), ParseError>;

enum Either<T1, T2> {
    A(T1),
    B(T2),
}

macro_rules! next {
    ($self: expr, $token: ident, $body: block) => {{
        $self.input.push_state();
        $self.parse_whitespace()?;

        if let Some($token) = $self.input.next_token() {
            let result = { $body };
            match result {
                ParseResult::None => {
                    $self.input.pop_state();
                }
                _ => {
                    $self.input.drop_state();
                }
            }
            result
        } else {
            $self.input.pop_state();
            return ParseResult::None;
        }
    }};
}

macro_rules! parse_sequence_chain {
    (
        $self: expr,
        $pos: ident,
        $body: block,
        $first: expr,
        $first_ident: ident,
        $first_err_msg: literal
    ) => {
        #[allow(unused_variables)]
        if let Some(($first_ident, $pos)) = $first($self)? {
            #[warn(unused_variables)]
            $body
        } else {
            let file = $self.file_server.get_file_name($pos.file).into_owned();
            let err = ParseError::new(
                $first_err_msg,
                file,
                $pos.end_text_pos.line,
                $pos.end_text_pos.col,
            );
            ParseResult::Err(err)
        }
    };
    (
        $self: expr,
        $pos: ident,
        $body: block,
        $first: expr,
        $first_ident: ident,
        $first_err_msg: literal,
        $($seq: expr, $idents: ident, $err_msg: literal),+
    ) => {
        if let Some(($first_ident, $pos)) = $first($self)? {
            parse_sequence_chain!($self, $pos, $body, $($seq, $idents, $err_msg),+)
        } else {
            let file = $self.file_server.get_file_name($pos.file).into_owned();
            let err = ParseError::new(
                $first_err_msg,
                file,
                $pos.end_text_pos.line,
                $pos.end_text_pos.col,
            );
            ParseResult::Err(err)
        }
    };
}

macro_rules! parse_sequence {
    (
        $self: expr,
        $first_pos: ident,
        $pos: ident,
        $body: block,
        $first: expr,
        $first_ident: ident,
        $($seq: expr, $idents: ident, $err_msg: literal),+
    ) => {
        if let Some(($first_ident, $first_pos)) = $first($self)? {
            let $pos = $first_pos;
            parse_sequence_chain!($self, $pos, $body, $($seq, $idents, $err_msg),+)
        } else {
            ParseResult::None
        }
    };
}

pub struct Parser<'a> {
    file_server: &'a FileServer,
    input: TokenInput<'a>,
}
impl<'a> Parser<'a> {
    pub const fn new(file_server: &'a FileServer, input: &'a [Token]) -> Self {
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
        next!(self, token, {
            if let TokenKind::IntegerLiteral(value) = token.kind {
                ParseResult::Some((value, token.pos))
            } else {
                ParseResult::None
            }
        })
    }

    fn parse_char_constant(&mut self) -> ParseResult<char> {
        next!(self, token, {
            if let TokenKind::CharLiteral(c) = token.kind {
                ParseResult::Some((c, token.pos))
            } else {
                ParseResult::None
            }
        })
    }

    fn parse_string_constant(&mut self) -> ParseResult<String> {
        next!(self, token, {
            if let TokenKind::StringLiteral(s) = token.kind {
                ParseResult::Some((s, token.pos))
            } else {
                ParseResult::None
            }
        })
    }

    fn parse_keyword(&mut self, keyword: Keyword) -> ParseResult<()> {
        next!(self, token, {
            if token.kind == TokenKind::Keyword(keyword) {
                ParseResult::Some(((), token.pos))
            } else {
                ParseResult::None
            }
        })
    }

    fn parse_identifier(&mut self) -> ParseResult<String> {
        next!(self, token, {
            if let TokenKind::Identifier(identifier) = token.kind {
                ParseResult::Some((identifier, token.pos))
            } else {
                ParseResult::None
            }
        })
    }

    fn parse_list<T>(
        &mut self,
        mut parse_item: impl FnMut(&mut Self) -> ParseResult<T>,
        delimiter: Keyword,
        allow_empty: bool,
        allow_trailing: bool,
        err_msg: &'static str,
    ) -> ParseResult<Vec<T>> {
        self.input.push_state();
        let mut result: Vec<T> = Vec::new();

        if let Some((first, first_pos)) = parse_item(self)? {
            self.input.drop_state();
            result.push(first);
            let mut last_pos = first_pos;

            loop {
                if let Some((_, pos)) = self.parse_keyword(delimiter)? {
                    if let Some((item, pos)) = parse_item(self)? {
                        last_pos = pos;
                        result.push(item);
                    } else {
                        if allow_trailing {
                            last_pos = pos;
                            break;
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
                    }
                } else {
                    break;
                }
            }

            ParseResult::Some((
                result,
                TokenPosition {
                    file: first_pos.file,
                    start_text_pos: first_pos.start_text_pos,
                    end_text_pos: last_pos.end_text_pos,
                },
            ))
        } else {
            self.input.pop_state();
            if allow_empty {
                ParseResult::Some((result, TokenPosition::NONE))
            } else {
                ParseResult::None
            }
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
                if let Some((_, pos)) = self.parse_keyword(after)? {
                    ParseResult::Some((
                        result,
                        TokenPosition {
                            file: first_pos.file,
                            start_text_pos: first_pos.start_text_pos,
                            end_text_pos: pos.end_text_pos,
                        },
                    ))
                } else {
                    let file = self.file_server.get_file_name(pos.file).into_owned();
                    let err =
                        ParseError::new(err_msg, file, pos.end_text_pos.line, pos.end_text_pos.col);
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

    fn parse_many<T>(
        &mut self,
        mut parse_item: impl FnMut(&mut Self) -> ParseResult<T>,
        allow_empty: bool,
    ) -> ParseResult<Vec<T>> {
        self.input.push_state();
        let mut result: Vec<T> = Vec::new();

        if let Some((first, first_pos)) = parse_item(self)? {
            self.input.drop_state();
            result.push(first);
            let mut last_pos = first_pos;

            loop {
                if let Some((item, pos)) = parse_item(self)? {
                    last_pos = pos;
                    result.push(item);
                } else {
                    break;
                }
            }

            ParseResult::Some((
                result,
                TokenPosition {
                    file: first_pos.file,
                    start_text_pos: first_pos.start_text_pos,
                    end_text_pos: last_pos.end_text_pos,
                },
            ))
        } else {
            self.input.pop_state();
            if allow_empty {
                ParseResult::Some((result, TokenPosition::NONE))
            } else {
                ParseResult::None
            }
        }
    }

    fn parse_either_or<T1, T2>(
        &mut self,
        mut parse_a: impl FnMut(&mut Self) -> ParseResult<T1>,
        mut parse_b: impl FnMut(&mut Self) -> ParseResult<T2>,
    ) -> ParseResult<Either<T1, T2>> {
        if let Some((a, pos)) = parse_a(self)? {
            ParseResult::Some((Either::A(a), pos))
        } else if let Some((b, pos)) = parse_b(self)? {
            ParseResult::Some((Either::B(b), pos))
        } else {
            ParseResult::None
        }
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        let (mut t, first_pos) = {
            if let Some((_, pos)) = self.parse_keyword(Keyword::Word)? {
                (Type::Word, pos)
            } else if let Some((_, pos)) = self.parse_keyword(Keyword::UWord)? {
                (Type::UWord, pos)
            } else if let Some((_, pos)) = self.parse_keyword(Keyword::Int)? {
                (Type::Int, pos)
            } else if let Some((_, pos)) = self.parse_keyword(Keyword::UInt)? {
                (Type::UInt, pos)
            } else if let Some((_, pos)) = self.parse_keyword(Keyword::Long)? {
                (Type::Long, pos)
            } else if let Some((_, pos)) = self.parse_keyword(Keyword::ULong)? {
                (Type::ULong, pos)
            } else if let Some((_, pos)) = self.parse_keyword(Keyword::Char)? {
                (Type::Char, pos)
            } else if let Some((ident, pos)) = self.parse_identifier()? {
                (Type::Custom(ident), pos)
            } else {
                return ParseResult::None;
            }
        };

        let mut last_pos = first_pos;

        if let Some((ptrs, pos)) = self.parse_many(|s| s.parse_keyword(Keyword::Mul), false)? {
            last_pos = pos;

            for _ in ptrs {
                t = Type::Pointer(Box::new(t));
            }
        }

        let parse_array_bounds = |s: &mut Self| {
            s.parse_between(
                Self::parse_integer_constant,
                Keyword::OpenBrace,
                Keyword::CloseBrace,
                "Expected array bounds",
            )
        };
        if let Some((bounds, pos)) = self.parse_many(parse_array_bounds, false)? {
            last_pos = pos;

            for bound in bounds {
                if let Ok(b) = bound.try_into() {
                    t = Type::Array(Box::new(t), b);
                } else {
                    let file = self.file_server.get_file_name(pos.file).into_owned();
                    let err = ParseError::new(
                        "Invalid array size",
                        file,
                        pos.start_text_pos.line,
                        pos.start_text_pos.col,
                    );
                    return ParseResult::Err(err);
                }
            }
        }

        ParseResult::Some((
            t,
            TokenPosition {
                file: first_pos.file,
                start_text_pos: first_pos.start_text_pos,
                end_text_pos: last_pos.end_text_pos,
            },
        ))
    }

    fn parse_sizeof_arg(&mut self) -> ParseResult<Expression> {
        if let Some((type_arg, pos)) = self.parse_between(
            Self::parse_type,
            Keyword::OpenParen,
            Keyword::CloseParen,
            "Expected type",
        )? {
            ParseResult::Some((Expression::SizeOf(type_arg), pos))
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
                    true,
                    true,
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

    fn parse_dot_access_field(&mut self) -> ParseResult<String> {
        if let Some((_, first_pos)) = self.parse_keyword(Keyword::Dot)? {
            if let Some((field, pos)) = self.parse_identifier()? {
                ParseResult::Some((
                    field,
                    TokenPosition {
                        file: first_pos.file,
                        start_text_pos: first_pos.start_text_pos,
                        end_text_pos: pos.end_text_pos,
                    },
                ))
            } else {
                let file = self.file_server.get_file_name(first_pos.file).into_owned();
                let err = ParseError::new(
                    "Expected identifier",
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

    fn parse_cast_type(&mut self) -> ParseResult<Type> {
        self.parse_between(
            Self::parse_type,
            Keyword::Less,
            Keyword::Greater,
            "Expected cast",
        )
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
        } else if let Some((ident, pos)) = self.parse_identifier()? {
            if let Some((args, _)) = self.parse_function_call_args()? {
                ParseResult::Some((Expression::FunctionCall(ident, args), pos))
            } else {
                ParseResult::Some((Expression::Variable(ident), pos))
            }
        } else {
            ParseResult::None
        }
    }

    fn parse_unary_postfix_expression(&mut self) -> ParseResult<Expression> {
        let parse_inner = |s: &mut Self| {
            s.parse_either_or(Self::parse_array_access_arg, Self::parse_dot_access_field)
        };

        if let Some((expr, first_pos)) = self.parse_leaf_expression()? {
            let mut result = expr;
            let mut last_pos = first_pos;

            if let Some((ops, pos)) = self.parse_many(parse_inner, false)? {
                last_pos = pos;

                for op in ops {
                    match op {
                        Either::A(array_index) => {
                            result =
                                Expression::ArrayAccess(Box::new(result), Box::new(array_index))
                        }
                        Either::B(field_ident) => {
                            result = Expression::DotAccess(Box::new(result), field_ident)
                        }
                    }
                }
            }

            ParseResult::Some((
                result,
                TokenPosition {
                    file: first_pos.file,
                    start_text_pos: first_pos.start_text_pos,
                    end_text_pos: last_pos.end_text_pos,
                },
            ))
        } else {
            ParseResult::None
        }
    }

    fn parse_unary_prefix_expression(&mut self) -> ParseResult<Expression> {
        if let Some(expr) = self.parse_unary_postfix_expression()? {
            ParseResult::Some(expr)
        } else {
            let (op, (_, pos)) = {
                if let Some(t) = self.parse_keyword(Keyword::Add)? {
                    (Either::A(UnaryOperator::Positive), t)
                } else if let Some(t) = self.parse_keyword(Keyword::Sub)? {
                    (Either::A(UnaryOperator::Negative), t)
                } else if let Some(t) = self.parse_keyword(Keyword::Not)? {
                    (Either::A(UnaryOperator::Not), t)
                } else if let Some(t) = self.parse_keyword(Keyword::BitAnd)? {
                    (Either::A(UnaryOperator::AddressOf), t)
                } else if let Some(t) = self.parse_keyword(Keyword::Mul)? {
                    (Either::A(UnaryOperator::Deref), t)
                } else if let Some((cast_type, pos)) = self.parse_cast_type()? {
                    (Either::B(cast_type), ((), pos))
                } else {
                    return ParseResult::None;
                }
            };

            if let Some((expr, _)) = self.parse_unary_prefix_expression()? {
                match op {
                    Either::A(op) => {
                        ParseResult::Some((Expression::UnaryOperator(op, Box::new(expr)), pos))
                    }
                    Either::B(cast_type) => {
                        ParseResult::Some((Expression::Cast(cast_type, Box::new(expr)), pos))
                    }
                }
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
            let mut last_pos = first_pos;

            loop {
                if let Some((op, pos)) = self.parse_aggregate_op(ops)? {
                    last_pos = pos;

                    if let Some((expr, _)) = parse_inner(self)? {
                        result = Expression::BinaryOperator(op, Box::new(result), Box::new(expr));
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

            ParseResult::Some((
                result,
                TokenPosition {
                    file: first_pos.file,
                    start_text_pos: first_pos.start_text_pos,
                    end_text_pos: last_pos.end_text_pos,
                },
            ))
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

        self.parse_aggregate_expression(Self::parse_unary_prefix_expression, &OPS)
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
        const OPS: [(Keyword, BinaryOperator); 1] = [(Keyword::BitAnd, BinaryOperator::And)];

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

    fn parse_expression(&mut self) -> ParseResult<Expression> {
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

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn parse_constant_declaration(&mut self) -> ParseResult<ConstantDeclaration> {
        let parse_const_keyword = |s: &mut Self| s.parse_keyword(Keyword::Const);
        let parse_colon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Colon);
        let parse_assign_keyword = |s: &mut Self| s.parse_keyword(Keyword::Assign);
        let parse_semicolon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Semicolon);

        parse_sequence!(
            self, first_pos, pos,
            {
                ParseResult::Some((ConstantDeclaration {
                    identifier: ident,
                    const_type,
                    value: expr,
                }, first_pos))
            },
            parse_const_keyword, _t,
            Self::parse_whitespace, _t, "Expected whitespace",
            Self::parse_identifier, ident, "Expected identifier",
            parse_colon_keyword, _t, "Expected ':'",
            Self::parse_type, const_type, "Expected type",
            parse_assign_keyword, _t, "Expected '='",
            Self::parse_expression, expr, "Expected expression",
            parse_semicolon_keyword, _t, "Expected ';'"
        )
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn parse_variable_declaration(&mut self) -> ParseResult<VariableDeclaration> {
        let parse_var_keyword = |s: &mut Self| s.parse_keyword(Keyword::Var);
        let parse_colon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Colon);
        let parse_assign_keyword = |s: &mut Self| s.parse_keyword(Keyword::Assign);
        let parse_semicolon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Semicolon);

        let parse_initialization = |s: &mut Self| {
            parse_sequence!(
                s, first_pos, pos,
                {
                    ParseResult::Some((expr, first_pos))
                },
                parse_assign_keyword, _t,
                Self::parse_expression, expr, "Expected expression",
                parse_semicolon_keyword, _t, "Expected ';'"
            )
        };

        parse_sequence!(
            self, first_pos, pos,
            {
                match self.parse_either_or(parse_semicolon_keyword, parse_initialization)? {
                    Some((Either::A(_), _)) => {
                        ParseResult::Some((VariableDeclaration {
                            identifier: ident,
                            var_type,
                            initial_value: None,
                        }, first_pos))
                    },
                    Some((Either::B(expr), _)) => {
                        ParseResult::Some((VariableDeclaration {
                            identifier: ident,
                            var_type,
                            initial_value: Some(expr),
                        }, first_pos))
                    },
                    None => {
                        let file = self.file_server.get_file_name(pos.file).into_owned();
                        let err = ParseError::new(
                            "Expected initialization or ';'",
                            file,
                            pos.end_text_pos.line,
                            pos.end_text_pos.col,
                        );
                        ParseResult::Err(err)
                    },
                }
            },
            parse_var_keyword, _t,
            Self::parse_whitespace, _t, "Expected whitespace",
            Self::parse_identifier, ident, "Expected identifier",
            parse_colon_keyword, _t, "Expected ':'",
            Self::parse_type, var_type, "Expected type"
        )
    }

    fn parse_assignment_operator(&mut self) -> ParseResult<AssignmentKind> {
        if let Some((_, pos)) = self.parse_keyword(Keyword::Assign)? {
            ParseResult::Some((AssignmentKind::Normal, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::AddAssign)? {
            ParseResult::Some((AssignmentKind::Add, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::SubAssign)? {
            ParseResult::Some((AssignmentKind::Subtract, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::MulAssign)? {
            ParseResult::Some((AssignmentKind::Multiply, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::DivAssign)? {
            ParseResult::Some((AssignmentKind::Divide, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::RemAssign)? {
            ParseResult::Some((AssignmentKind::Remainder, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::LeftShiftAssign)? {
            ParseResult::Some((AssignmentKind::LeftShift, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::RightShiftAssign)? {
            ParseResult::Some((AssignmentKind::RightShift, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::AndAssign)? {
            ParseResult::Some((AssignmentKind::And, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::OrAssign)? {
            ParseResult::Some((AssignmentKind::Or, pos))
        } else if let Some((_, pos)) = self.parse_keyword(Keyword::XorAssign)? {
            ParseResult::Some((AssignmentKind::Xor, pos))
        } else {
            ParseResult::None
        }
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn parse_assignment(&mut self) -> ParseResult<Assignment> {
        let parse_semicolon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Semicolon);

        parse_sequence!(
            self, first_pos, pos,
            {
                ParseResult::Some((Assignment {
                    target,
                    kind,
                    source,
                }, TokenPosition {
                    file: first_pos.file,
                    start_text_pos: first_pos.start_text_pos,
                    end_text_pos: pos.end_text_pos,
                }))
            },
            Self::parse_expression, target,
            Self::parse_assignment_operator, kind, "Expected assignment operator",
            Self::parse_expression, source, "Expected expression",
            parse_semicolon_keyword, _t, "Expected ';'"
        )
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn parse_if_statement(&mut self) -> ParseResult<IfStatement> {
        let parse_if_keyword = |s: &mut Self| s.parse_keyword(Keyword::If);

        parse_sequence!(
            self, first_pos, pos,
            {
                if let Some((_, pos)) = self.parse_keyword(Keyword::Else)? {
                    if let Some((false_body, pos)) = self.parse_either_or(Self::parse_block, Self::parse_if_statement)? {
                        match false_body {
                            Either::A(block) => {
                                ParseResult::Some((
                                    IfStatement {
                                        condition,
                                        true_body,
                                        false_body: Some(block),
                                    },
                                    TokenPosition {
                                        file: first_pos.file,
                                        start_text_pos: first_pos.start_text_pos,
                                        end_text_pos: pos.end_text_pos,
                                    }
                                ))
                            }
                            Either::B(next_if) => {
                                ParseResult::Some((
                                    IfStatement {
                                        condition,
                                        true_body,
                                        false_body: Some(vec![Statement::If(next_if)]),
                                    },
                                    TokenPosition {
                                        file: first_pos.file,
                                        start_text_pos: first_pos.start_text_pos,
                                        end_text_pos: pos.end_text_pos,
                                    }
                                ))
                            }
                        }
                    } else {
                        let file = self.file_server.get_file_name(pos.file).into_owned();
                        let err = ParseError::new(
                            "Expected block",
                            file,
                            pos.end_text_pos.line,
                            pos.end_text_pos.col,
                        );
                        ParseResult::Err(err)
                    }
                } else {
                    ParseResult::Some((
                        IfStatement {
                            condition,
                            true_body,
                            false_body: None,
                        },
                        TokenPosition {
                            file: first_pos.file,
                            start_text_pos: first_pos.start_text_pos,
                            end_text_pos: pos.end_text_pos,
                        }
                    ))
                }
            },
            parse_if_keyword, _t,
            Self::parse_expression, condition, "Expected expression",
            Self::parse_block, true_body, "Expected block"
        )
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn parse_while_loop(&mut self) -> ParseResult<WhileLoop> {
        let parse_while_keyword = |s: &mut Self| s.parse_keyword(Keyword::While);

        parse_sequence!(
            self, first_pos, pos,
            {
                ParseResult::Some((
                    WhileLoop {
                        condition,
                        body,
                    },
                    TokenPosition {
                        file: first_pos.file,
                        start_text_pos: first_pos.start_text_pos,
                        end_text_pos: pos.end_text_pos,
                    }
                ))
            },
            parse_while_keyword, _t,
            Self::parse_expression, condition, "Expected expression",
            Self::parse_block, body, "Expected block"
        )
    }

    fn parse_break_or_continue_statement(&mut self, is_break: bool) -> ParseResult<usize> {
        let parse_semicolon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Semicolon);

        #[cfg_attr(rustfmt, rustfmt_skip)]
        let parse_count = |s: &mut Self| {
            parse_sequence!(
                s, first_pos, pos,
                {
                    ParseResult::Some((count, TokenPosition {
                        file: first_pos.file,
                        start_text_pos: first_pos.start_text_pos,
                        end_text_pos: pos.end_text_pos,
                    }))
                },
                Self::parse_integer_constant, count,
                parse_semicolon_keyword, _t, "Expected ';'"
            )
        };

        let keyword = if is_break {
            Keyword::Break
        } else {
            Keyword::Continue
        };

        if let Some((_, first_pos)) = self.parse_keyword(keyword)? {
            if let Some((count, pos)) =
                self.parse_either_or(parse_count, parse_semicolon_keyword)?
            {
                match count {
                    Either::A(count) => {
                        if let Ok(c) = count.try_into() {
                            ParseResult::Some((
                                c,
                                TokenPosition {
                                    file: first_pos.file,
                                    start_text_pos: first_pos.start_text_pos,
                                    end_text_pos: pos.end_text_pos,
                                },
                            ))
                        } else {
                            let file = self.file_server.get_file_name(pos.file).into_owned();
                            let err = ParseError::new(
                                "Invalid count",
                                file,
                                pos.start_text_pos.line,
                                pos.start_text_pos.col,
                            );
                            ParseResult::Err(err)
                        }
                    }
                    Either::B(_) => ParseResult::Some((
                        0,
                        TokenPosition {
                            file: first_pos.file,
                            start_text_pos: first_pos.start_text_pos,
                            end_text_pos: pos.end_text_pos,
                        },
                    )),
                }
            } else {
                let file = self.file_server.get_file_name(first_pos.file).into_owned();
                let err = ParseError::new(
                    "Expected integer literal or ';'",
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

    fn parse_return_statement(&mut self) -> ParseResult<Option<Expression>> {
        let parse_semicolon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Semicolon);

        #[cfg_attr(rustfmt, rustfmt_skip)]
        let parse_expr = |s: &mut Self| {
            parse_sequence!(
                s, first_pos, pos,
                {
                    ParseResult::Some((expr, TokenPosition {
                        file: first_pos.file,
                        start_text_pos: first_pos.start_text_pos,
                        end_text_pos: pos.end_text_pos,
                    }))
                },
                Self::parse_expression, expr,
                parse_semicolon_keyword, _t, "Expected ';'"
            )
        };

        if let Some((_, first_pos)) = self.parse_keyword(Keyword::Return)? {
            if let Some((expr, pos)) = self.parse_either_or(parse_expr, parse_semicolon_keyword)? {
                match expr {
                    Either::A(expr) => ParseResult::Some((
                        Some(expr),
                        TokenPosition {
                            file: first_pos.file,
                            start_text_pos: first_pos.start_text_pos,
                            end_text_pos: pos.end_text_pos,
                        },
                    )),
                    Either::B(_) => ParseResult::Some((
                        None,
                        TokenPosition {
                            file: first_pos.file,
                            start_text_pos: first_pos.start_text_pos,
                            end_text_pos: pos.end_text_pos,
                        },
                    )),
                }
            } else {
                let file = self.file_server.get_file_name(first_pos.file).into_owned();
                let err = ParseError::new(
                    "Expected expression or ';'",
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

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        if let Some((const_decl, pos)) = self.parse_constant_declaration()? {
            ParseResult::Some((Statement::ConstantDeclaration(const_decl), pos))
        } else if let Some((var_decl, pos)) = self.parse_variable_declaration()? {
            ParseResult::Some((Statement::VariableDeclaration(var_decl), pos))
        } else if let Some((assignment, pos)) = self.parse_assignment()? {
            ParseResult::Some((Statement::Assignment(assignment), pos))
        } else if let Some((if_statement, pos)) = self.parse_if_statement()? {
            ParseResult::Some((Statement::If(if_statement), pos))
        } else if let Some((while_loop, pos)) = self.parse_while_loop()? {
            ParseResult::Some((Statement::While(while_loop), pos))
        } else if let Some((count, pos)) = self.parse_break_or_continue_statement(true)? {
            ParseResult::Some((Statement::Break(count), pos))
        } else if let Some((count, pos)) = self.parse_break_or_continue_statement(false)? {
            ParseResult::Some((Statement::Continue(count), pos))
        } else if let Some((expr, pos)) = self.parse_return_statement()? {
            ParseResult::Some((Statement::Return(expr), pos))
        } else {
            ParseResult::None
        }
    }

    fn parse_block(&mut self) -> ParseResult<Vec<Statement>> {
        let parse_statement_list = |s: &mut Self| s.parse_many(Self::parse_statement, true);

        self.parse_between(
            parse_statement_list,
            Keyword::OpenCurl,
            Keyword::CloseCurl,
            "Expected statement list",
        )
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn parse_argument_definition(&mut self) -> ParseResult<ArgumentDefinition> {
        let parse_colon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Colon);

        parse_sequence!(
            self, first_pos, pos,
            {
                ParseResult::Some((
                    ArgumentDefinition {
                        identifier: ident,
                        arg_type,
                    },
                    TokenPosition {
                        file: first_pos.file,
                        start_text_pos: first_pos.start_text_pos,
                        end_text_pos: pos.end_text_pos,
                    },
                ))
            },
            Self::parse_identifier, ident,
            parse_colon_keyword, _t, "Expected ':'",
            Self::parse_type, arg_type, "Expected type"
        )
    }

    fn parse_function_definition(&mut self) -> ParseResult<FunctionDefinition> {
        let parse_fn_keyword = |s: &mut Self| s.parse_keyword(Keyword::Fn);
        let parse_open_paren_keyword = |s: &mut Self| s.parse_keyword(Keyword::OpenParen);
        let parse_close_paren_keyword = |s: &mut Self| s.parse_keyword(Keyword::CloseParen);
        let parse_colon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Colon);

        let parse_args = |s: &mut Self| {
            s.parse_list(
                Self::parse_argument_definition,
                Keyword::Comma,
                true,
                true,
                "Expected argument definition",
            )
        };

        #[cfg_attr(rustfmt, rustfmt_skip)]
        let parse_return_type = |s: &mut Self| {
            parse_sequence!(
                s, first_pos, pos,
                {
                    ParseResult::Some((
                        ret_type,
                        TokenPosition {
                            file: first_pos.file,
                            start_text_pos: first_pos.start_text_pos,
                            end_text_pos: pos.end_text_pos,
                        },
                    ))
                },
                parse_colon_keyword, _t,
                Self::parse_type, ret_type, "Expected type"
            )
        };

        #[cfg_attr(rustfmt, rustfmt_skip)]
        parse_sequence!(
            self, first_pos, pos,
            {
                let ret_type = {
                    if let Some((ret_type, _)) = parse_return_type(self)? {
                        Some(ret_type)
                    } else {
                        None
                    }
                };

                if let Some((body, pos)) = self.parse_block()? {
                    ParseResult::Some((
                        FunctionDefinition {
                            identifier: ident,
                            return_type: ret_type,
                            args,
                            body,
                        },
                        TokenPosition {
                            file: first_pos.file,
                            start_text_pos: first_pos.start_text_pos,
                            end_text_pos: pos.end_text_pos,
                        },
                    ))
                } else {
                    let file = self.file_server.get_file_name(pos.file).into_owned();
                    let err = ParseError::new(
                        "Expected block",
                        file,
                        pos.end_text_pos.line,
                        pos.end_text_pos.col,
                    );
                    ParseResult::Err(err)
                }
            },
            parse_fn_keyword, _t,
            Self::parse_identifier, ident, "Expected identifier",
            parse_open_paren_keyword, _t, "Expected '('",
            parse_args, args, "Expected arguments",
            parse_close_paren_keyword, _t, "Expected ')'"
        )
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn parse_field_definition(&mut self) -> ParseResult<FieldDefinition> {
        let parse_colon_keyword = |s: &mut Self| s.parse_keyword(Keyword::Colon);

        parse_sequence!(
            self, first_pos, pos,
            {
                ParseResult::Some((
                    FieldDefinition {
                        identifier: ident,
                        field_type,
                    },
                    TokenPosition {
                        file: first_pos.file,
                        start_text_pos: first_pos.start_text_pos,
                        end_text_pos: pos.end_text_pos,
                    },
                ))
            },
            Self::parse_identifier, ident,
            parse_colon_keyword, _t, "Expected ':'",
            Self::parse_type, field_type, "Expected type"
        )
    }

    fn parse_struct_definition(&mut self, is_union: bool) -> ParseResult<StructDefinition> {
        let parse_open_curl_keyword = |s: &mut Self| s.parse_keyword(Keyword::OpenCurl);
        let parse_close_curl_keyword = |s: &mut Self| s.parse_keyword(Keyword::CloseCurl);

        let parse_first_keyword = if is_union {
            |s: &mut Self| s.parse_keyword(Keyword::Union)
        } else {
            |s: &mut Self| s.parse_keyword(Keyword::Struct)
        };

        let parse_fields = |s: &mut Self| {
            s.parse_list(
                Self::parse_field_definition,
                Keyword::Comma,
                true,
                true,
                "Expected field definition",
            )
        };

        #[cfg_attr(rustfmt, rustfmt_skip)]
        parse_sequence!(
            self, first_pos, pos,
            {
                ParseResult::Some((
                    StructDefinition {
                        identifier: ident,
                        fields
                    },
                    TokenPosition {
                        file: first_pos.file,
                        start_text_pos: first_pos.start_text_pos,
                        end_text_pos: pos.end_text_pos,
                    },
                ))
            },
            parse_first_keyword, _t,
            Self::parse_identifier, ident, "Expected identifier",
            parse_open_curl_keyword, _t, "Expected '{'",
            parse_fields, fields, "Expected field definitions",
            parse_close_curl_keyword, _t, "Expected '}'"
        )
    }

    pub fn parse_top_level_statement(&mut self) -> ParseResult<TopLevelStatement> {
        if let Some((const_decl, pos)) = self.parse_constant_declaration()? {
            ParseResult::Some((TopLevelStatement::ConstantDeclaration(const_decl), pos))
        } else if let Some((fn_def, pos)) = self.parse_function_definition()? {
            ParseResult::Some((TopLevelStatement::FunctionDefinition(fn_def), pos))
        } else if let Some((struct_def, pos)) = self.parse_struct_definition(false)? {
            ParseResult::Some((TopLevelStatement::StructDefinition(struct_def), pos))
        } else if let Some((union_def, pos)) = self.parse_struct_definition(true)? {
            ParseResult::Some((TopLevelStatement::UnionDefinition(union_def), pos))
        } else {
            ParseResult::None
        }
    }
}
