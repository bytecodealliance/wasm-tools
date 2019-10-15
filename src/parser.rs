use crate::lexer::{Float, Integer, LexError, Lexer, Source, Token};
use std::cell::Cell;
use std::fmt;

pub fn parse<'a, T: Parse<'a>>(buf: &'a ParseBuffer<'a>) -> Result<T> {
    let parser = buf.parser();
    let result = parser.parse()?;
    if parser.is_empty() {
        Ok(result)
    } else {
        Err(parser.error("extra tokens remaining after parse"))
    }
}

pub trait Parse<'a>: Sized {
    fn parse(parser: Parser<'a>) -> Result<Self>;
}

pub trait Peek {
    fn peek(cursor: Cursor<'_>) -> bool;
    fn display() -> &'static str;
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

pub struct Lookahead1<'a> {
    parser: Parser<'a>,
    attempts: Vec<&'static str>,
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

    pub fn peek2<T: Peek>(self) -> bool {
        let mut cursor = self.cursor();
        if cursor.advance().is_some() {
            T::peek(cursor)
        } else {
            false
        }
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

    pub fn lookahead1(self) -> Lookahead1<'a> {
        Lookahead1 {
            attempts: Vec::new(),
            parser: self,
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
    pub fn input(&self) -> &'a str {
        self.parser.buf.input
    }

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

    pub fn string(mut self) -> Option<(&'a [u8], Self)> {
        match self.advance()? {
            Token::String { val, .. } => Some((&**val, self)),
            _ => None,
        }
    }

    fn advance(&mut self) -> Option<&'a Token<'a>> {
        self.cur += 1;
        self.parser.buf.tokens.get(self.cur - 1)
    }
}

impl Lookahead1<'_> {
    pub fn peek<T: Peek>(&mut self) -> bool {
        if self.parser.peek::<T>() {
            true
        } else {
            self.attempts.push(T::display());
            false
        }
    }

    pub fn error(self) -> Error {
        match self.attempts.len() {
            0 => {
                if self.parser.is_empty() {
                    self.parser.error("unexpected end of input")
                } else {
                    self.parser.error("unexpected token")
                }
            }
            1 => {
                let message = format!("unexpected token, expected {}", self.attempts[0]);
                self.parser.error(&message)
            }
            2 => {
                let message = format!(
                    "unexpected token, expected {} or {}",
                    self.attempts[0], self.attempts[1]
                );
                self.parser.error(&message)
            }
            _ => {
                let join = self.attempts.join(", ");
                let message = format!("unexpected token, expected one of: {}", join);
                self.parser.error(&message)
            }
        }
    }
}

impl Error {
    pub fn line(&self) -> usize {
        match &*self.inner {
            ErrorInner::LexError(e) => e.line(),
            ErrorInner::Custom { line, .. } => *line,
        }
    }

    pub fn col(&self) -> usize {
        match &*self.inner {
            ErrorInner::LexError(e) => e.col(),
            ErrorInner::Custom { col, .. } => *col,
        }
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
            ErrorInner::Custom { message, .. } => message.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl<'a, T: Peek + Parse<'a>> Parse<'a> for Option<T> {
    fn parse(parser: Parser<'a>) -> Result<Option<T>> {
        if parser.peek::<T>() {
            Ok(Some(parser.parse()?))
        } else {
            Ok(None)
        }
    }
}
