use crate::lexer::{Float, Integer, LexError, Lexer, Source, Token};
use std::cell::Cell;
use std::fmt;

pub trait Parse<'a>: Sized {
    fn parse(parser: Parser<'a>) -> Result<Self>;
}

pub trait Peek {
    fn peek(cursor: Cursor<'_>) -> bool;
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct ParseBuffer<'a> {
    tokens: Box<[Token<'a>]>,
    input: &'a str,
    cur: Cell<usize>,
}

#[derive(Copy, Clone)]
pub struct Parser<'a> {
    buf: &'a ParseBuffer<'a>,
}

#[derive(Copy, Clone)]
pub struct Cursor<'a> {
    parser: Parser<'a>,
    cur: usize,
}

#[derive(Debug, Clone)]
pub struct Error {
    inner: Box<ErrorInner>,
}

#[derive(Debug, Clone)]
enum ErrorInner {
    LexError(LexError),
    Custom {
        line: usize,
        col: usize,
        message: String,
    },
}

impl ParseBuffer<'_> {
    pub fn new(input: &str) -> Result<ParseBuffer<'_>> {
        let mut tokens = Vec::new();
        for token in Lexer::new(input) {
            if let Source::Token(t) = token? {
                tokens.push(t);
            }
        }
        Ok(ParseBuffer {
            tokens: tokens.into_boxed_slice(),
            cur: Cell::new(0),
            input,
        })
    }

    pub fn parser(&self) -> Parser<'_> {
        Parser { buf: self }
    }
}

impl<'a> Parser<'a> {
    pub fn parse<T: Parse<'a>>(self) -> Result<T> {
        T::parse(self)
    }

    pub fn peek<T: Peek>(self) -> bool {
        T::peek(self.cursor())
    }

    pub fn cursor(self) -> Cursor<'a> {
        Cursor {
            parser: self,
            cur: self.buf.cur.get(),
        }
    }

    pub fn step<F, T>(self, f: F) -> Result<T>
    where
        F: FnOnce(Cursor<'a>) -> Result<(T, Cursor<'a>)>,
    {
        let (result, cursor) = f(self.cursor())?;
        self.buf.cur.set(cursor.cur);
        Ok(result)
    }

    pub fn parens<T>(self, f: impl FnOnce(Parser<'a>) -> Result<T>) -> Result<T> {
        let before = self.buf.cur.get();
        let res = self.step(|cursor| {
            let mut cursor = match cursor.lparen() {
                Some(rest) => rest,
                None => return Err(cursor.error("expected `(`")),
            };
            cursor.parser.buf.cur.set(cursor.cur);
            let result = f(cursor.parser)?;
            cursor.cur = cursor.parser.buf.cur.get();
            match cursor.rparen() {
                Some(rest) => Ok((result, rest)),
                None => Err(cursor.error("expected `)`")),
            }
        });
        if res.is_err() {
            self.buf.cur.set(before);
        }
        return res;
    }

    pub fn is_empty(self) -> bool {
        match self.buf.tokens.get(self.buf.cur.get()) {
            Some(Token::RParen(_)) => true,
            Some(_) => false,
            None => true,
        }
    }

    pub fn error(self, msg: impl fmt::Display) -> Error {
        self.error_at_token(self.buf.cur.get(), &msg)
    }

    fn error_at_token(self, token: usize, msg: &dyn fmt::Display) -> Error {
        let last_pos = match self.buf.tokens.get(token) {
            None => self.buf.input.len(),
            Some(t) => self.input_pos(t.src()),
        };
        self._error_at(last_pos, msg)
    }

    fn _error_at(self, pos: usize, msg: &dyn fmt::Display) -> Error {
        let (line, col) = crate::to_linecol(self.buf.input, pos);
        Error {
            inner: Box::new(ErrorInner::Custom {
                line,
                col,
                message: msg.to_string(),
            }),
        }
    }

    fn input_pos(self, src: &'a str) -> usize {
        src.as_ptr() as usize - self.buf.input.as_ptr() as usize
    }
}

impl<'a> Cursor<'a> {
    pub fn error(&self, msg: impl fmt::Display) -> Error {
        self.parser.error_at_token(self.cur, &msg)
    }

    pub fn lparen(mut self) -> Option<Self> {
        match self.advance()? {
            Token::LParen(_) => Some(self),
            _ => None,
        }
    }

    pub fn rparen(mut self) -> Option<Self> {
        match self.advance()? {
            Token::RParen(_) => Some(self),
            _ => None,
        }
    }

    pub fn id(mut self) -> Option<(&'a str, Self)> {
        match self.advance()? {
            Token::Id(id) => Some((&id[1..], self)),
            _ => None,
        }
    }

    pub fn keyword(mut self) -> Option<(&'a str, Self)> {
        match self.advance()? {
            Token::Keyword(id) => Some((id, self)),
            _ => None,
        }
    }

    pub fn integer(mut self) -> Option<(&'a Integer<'a>, Self)> {
        match self.advance()? {
            Token::Integer(i) => Some((i, self)),
            _ => None,
        }
    }

    pub fn float(mut self) -> Option<(&'a Float<'a>, Self)> {
        match self.advance()? {
            Token::Float(f) => Some((f, self)),
            _ => None,
        }
    }

    fn advance(&mut self) -> Option<&'a Token<'a>> {
        self.cur += 1;
        self.parser.buf.tokens.get(self.cur - 1)
    }
}

impl From<LexError> for Error {
    fn from(err: LexError) -> Error {
        Error {
            inner: Box::new(ErrorInner::LexError(err)),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self.inner {
            ErrorInner::LexError(e) => e.fmt(f),
            ErrorInner::Custom { line, col, message } => {
                write!(f, "{} at line {} column {}", message, line + 1, col + 1)
            }
        }
    }
}

impl std::error::Error for Error {}
