use crate::Lexer;

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


}
