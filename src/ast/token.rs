use crate::parser::{Cursor, Parse, Parser, Peek, Result};

#[derive(PartialEq, Debug)]
pub struct Id<'a> {
    name: &'a str,
}

impl<'a> Id<'a> {
    pub fn new(name: &str) -> Id<'_> {
        Id { name }
    }

    pub fn name(&self) -> &'a str {
        self.name
    }
}

impl<'a> Parse<'a> for Id<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((name, rest)) = c.id() {
                return Ok((Id { name }, rest));
            }
            Err(c.error("expected an identifier"))
        })
    }
}

#[derive(PartialEq, Debug)]
pub enum Index<'a> {
    Num(u32),
    Id(Id<'a>),
}

impl<'a> Parse<'a> for Index<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(match parser.parse::<Id>() {
            Ok(id) => Index::Id(id),
            Err(_) => Index::Num(parser.parse()?),
        })
    }
}

impl<'a> Parse<'a> for u32 {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((i, rest)) = c.integer() {
                return match i.get_u32() {
                    Some(u) => Ok((u, rest)),
                    None => Err(c.error("invalid u32 number")),
                };
            }
            Err(c.error("expected an integer"))
        })
    }
}

impl Peek for u32 {
    fn peek(cursor: Cursor<'_>) -> bool {
        cursor.integer().is_some()
    }
}

pub struct LParen {
    _priv: (),
}

impl Peek for LParen {
    fn peek(cursor: Cursor<'_>) -> bool {
        cursor.lparen().is_some()
    }
}
