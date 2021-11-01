#![feature(try_trait_v2)]
#![feature(termination_trait_lib)]

mod ast;
mod parsing;

use ast::*;
use parsing::*;

use std::io;
use std::ops::{FromResidual, Try};

const TEST_FILE: &str = "
struct String {
    length: uword,
    chars: char*,
}

fn main(arg_count: uword, args: char**): word {
    if arg_count == 1 {
        return 10;
    } else if arg_count == 2 {
        return 20;
    } else {
        return 0;
    }
}
";

#[derive(Debug)]
pub enum OptResult<T, E> {
    None,
    Some(T),
    Err(E),
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

#[derive(Debug)]
enum CompilerError {
    Custom(String),
    IoError(io::Error),
    LexerError(LexerError),
    ParseError(ParseError),
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

fn parse(file_server: &FileServer, tokens: &[Token]) -> CompilerResult<Vec<TopLevelStatement>> {
    let mut parser = Parser::new(file_server, tokens);
    let mut result: Vec<TopLevelStatement> = Vec::new();

    loop {
        match parser.parse_top_level_statement() {
            OptResult::None => break,
            OptResult::Some((statement, _)) => result.push(statement),
            OptResult::Err(err) => return Err(err.into()),
        }
    }

    Ok(result)
}

fn main() -> CompilerResult<()> {
    let mut file_server = FileServer::new();
    let file = file_server.add_dummy_file("test.c", TEST_FILE);

    let tokens = tokenize(&mut file_server, file)?;
    //println!("{:#?}", tokens);
    let ast = parse(&file_server, &tokens)?;
    println!("{:#?}", ast);

    Ok(())
}
