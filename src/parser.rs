use crate::lexer::{Lexer, Source, Token, LexError};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

pub struct Error {
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser<'_> {
        Parser {
            lexer: Lexer::new(input),
        }
    }

    pub fn token(&mut self) -> Result<Option<Token<'a>>, LexError> {
        loop {
            return Ok(match self.lexer.parse()? {
                Some(Source::Token(t)) => Some(t),
                Some(_) => continue,
                None => None,
            })
        }
    }


}
