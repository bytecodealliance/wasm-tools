use std::fmt;

pub mod ast;
pub mod binary;
pub mod lexer;
pub mod parser;
pub mod resolve;

/// Converts an offset within `self.input` to a line/column number.
///
/// Note that both the line and the column are 0-based.
fn to_linecol(input: &str, offset: usize) -> (usize, usize) {
    let mut cur = 0;
    // Use split_terminator instead of lines so that if there is a `\r`,
    // it is included in the offset calculation. The `+1` values below
    // account for the `\n`.
    for (i, line) in input.split_terminator('\n').enumerate() {
        if cur + line.len() + 1 > offset {
            return (i, offset - cur);
        }
        cur += line.len() + 1;
    }
    (input.lines().count(), 0)
}

#[derive(Debug)]
pub enum Error {
    Lex(lexer::LexError),
    Parse(parser::Error),
    Resolve(resolve::ResolveError),
}

impl Error {
    pub fn line(&self) -> usize {
        match self {
            Error::Lex(e) => e.line(),
            Error::Parse(e) => e.line(),
            Error::Resolve(e) => e.line(),
        }
    }

    pub fn col(&self) -> usize {
        match self {
            Error::Lex(e) => e.col(),
            Error::Parse(e) => e.col(),
            Error::Resolve(e) => e.col(),
        }
    }
}

impl From<lexer::LexError> for Error {
    fn from(err: lexer::LexError) -> Error {
        Error::Lex(err)
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Error {
        Error::Parse(err)
    }
}

impl From<resolve::ResolveError> for Error {
    fn from(err: resolve::ResolveError) -> Error {
        Error::Resolve(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Lex(e) => e.fmt(f),
            Error::Parse(e) => e.fmt(f),
            Error::Resolve(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for Error {}
