use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(Debug, PartialEq)]
pub struct Import<'a> {
    pub module: &'a str,
    pub name: &'a str,
    pub desc: ImportDesc<'a>,
}

impl<'a> Parse<'a> for Import<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::import>()?;
        let module = parser.parse()?;
        let name = parser.parse()?;
        let desc = parser.parens(ImportDesc::parse)?;
        Ok(Import { module, name, desc })
    }
}

#[derive(Debug, PartialEq)]
pub struct ImportDesc<'a> {
    pub name: Option<ast::Id<'a>>,
    pub kind: ImportKind<'a>,
}

impl<'a> Parse<'a> for ImportDesc<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let (name, kind) = if parser.peek::<kw::func>() {
            parser.parse::<kw::func>()?;
            (parser.parse()?, ImportKind::Function(parser.parse()?))
        } else if parser.peek::<kw::table>() {
            parser.parse::<kw::table>()?;
            (parser.parse()?, ImportKind::Table(parser.parse()?))
        } else if parser.peek::<kw::memory>() {
            parser.parse::<kw::memory>()?;
            (parser.parse()?, ImportKind::Memory(parser.parse()?))
        } else if parser.peek::<kw::global>() {
            parser.parse::<kw::global>()?;
            (parser.parse()?, ImportKind::Global(parser.parse()?))
        } else {
            return Err(parser.error("invalid import specifier"));
        };
        Ok(ImportDesc { name, kind })
    }
}

#[derive(Debug, PartialEq)]
pub enum ImportKind<'a> {
    Function(&'a str),
    Table(ast::TableType),
    Memory(ast::MemoryType),
    Global(ast::GlobalType),
}
