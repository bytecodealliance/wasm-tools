use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(Debug, PartialEq)]
pub struct Import<'a> {
    pub module: &'a str,
    pub name: &'a str,
    pub id: Option<ast::Id<'a>>,
    pub kind: ImportKind<'a>,
}

#[derive(Debug, PartialEq)]
pub enum ImportKind<'a> {
    Func(ast::TypeUse<'a>),
    Table(ast::TableType),
    Memory(ast::MemoryType),
    Global(ast::GlobalType),
}

impl<'a> Parse<'a> for Import<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::import>()?;
        let module = parser.parse()?;
        let name = parser.parse()?;
        let (id, kind) = parser.parens(|parser| {
            let mut l = parser.lookahead1();
            if l.peek::<kw::func>() {
                parser.parse::<kw::func>()?;
                Ok((parser.parse()?, ImportKind::Func(parser.parse()?)))
            } else if l.peek::<kw::table>() {
                parser.parse::<kw::table>()?;
                Ok((parser.parse()?, ImportKind::Table(parser.parse()?)))
            } else if l.peek::<kw::memory>() {
                parser.parse::<kw::memory>()?;
                Ok((parser.parse()?, ImportKind::Memory(parser.parse()?)))
            } else if l.peek::<kw::global>() {
                parser.parse::<kw::global>()?;
                Ok((parser.parse()?, ImportKind::Global(parser.parse()?)))
            } else {
                Err(l.error())
            }
        })?;
        Ok(Import {
            module,
            name,
            id,
            kind,
        })
    }
}
