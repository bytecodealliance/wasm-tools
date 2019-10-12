use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use std::str;

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

impl Peek for Id<'_> {
    fn peek(cursor: Cursor<'_>) -> bool {
        cursor.id().is_some()
    }

    fn display() -> &'static str {
        "an identifier"
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

impl Peek for Index<'_> {
    fn peek(cursor: Cursor<'_>) -> bool {
        u32::peek(cursor) || Id::peek(cursor)
    }

    fn display() -> &'static str {
        "an index"
    }
}

impl<'a> Parse<'a> for u32 {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((i, rest)) = c.integer() {
                return match i.get_u32() {
                    Some(u) => Ok((u, rest)),
                    None => Err(c.error(concat!("invalid u32 number"))),
                };
            }
            Err(c.error("expected a u32"))
        })
    }
}

impl Peek for u32 {
    fn peek(cursor: Cursor<'_>) -> bool {
        cursor.integer().is_some()
    }

    fn display() -> &'static str {
        "u32"
    }
}

impl<'a> Parse<'a> for &'a [u8] {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((i, rest)) = c.string() {
                return Ok((i, rest));
            }
            Err(c.error("expected a string"))
        })
    }
}

impl Peek for &'_ [u8] {
    fn peek(cursor: Cursor<'_>) -> bool {
        cursor.string().is_some()
    }

    fn display() -> &'static str {
        "string"
    }
}

impl<'a> Parse<'a> for &'a str {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        str::from_utf8(parser.parse()?).map_err(|_| parser.error("invalid utf-8"))
    }
}

impl Peek for &'_ str {
    fn peek(cursor: Cursor<'_>) -> bool {
        <&[u8]>::peek(cursor)
    }

    fn display() -> &'static str {
        <&[u8]>::display()
    }
}

#[derive(Debug, PartialEq)]
pub struct Int32<'a> {
    src: &'a str,
}

impl<'a> Parse<'a> for Int32<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((i, rest)) = c.integer() {
                return Ok((Int32 { src: i.src() }, rest));
            }
            Err(c.error("expected an integer"))
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Int64<'a> {
    src: &'a str,
}

impl<'a> Parse<'a> for Int64<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((i, rest)) = c.integer() {
                return Ok((Int64 { src: i.src() }, rest));
            }
            Err(c.error("expected an integer"))
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Float32<'a> {
    src: &'a str,
}

impl<'a> Parse<'a> for Float32<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((f, rest)) = c.float() {
                return Ok((Float32 { src: f.src() }, rest));
            }
            if let Some((i, rest)) = c.integer() {
                return Ok((Float32 { src: i.src() }, rest));
            }
            Err(c.error("expected a float"))
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Float64<'a> {
    src: &'a str,
}

impl<'a> Parse<'a> for Float64<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((f, rest)) = c.float() {
                return Ok((Float64 { src: f.src() }, rest));
            }
            if let Some((i, rest)) = c.integer() {
                return Ok((Float64 { src: i.src() }, rest));
            }
            Err(c.error("expected a float"))
        })
    }
}

pub struct LParen {
    _priv: (),
}

impl Peek for LParen {
    fn peek(cursor: Cursor<'_>) -> bool {
        cursor.lparen().is_some()
    }

    fn display() -> &'static str {
        "left paren"
    }
}
