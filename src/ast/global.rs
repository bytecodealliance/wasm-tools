use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(Debug, PartialEq)]
pub struct Global<'a> {
    pub name: Option<ast::Id<'a>>,
    pub exports: ast::InlineExport<'a>,
    pub ty: ast::GlobalType,
    pub kind: GlobalKind<'a>,
}

#[derive(Debug, PartialEq)]
pub enum GlobalKind<'a> {
    Import { module: &'a str, name: &'a str },
    Inline(ast::Expression<'a>),
}

impl<'a> Parse<'a> for Global<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::global>()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        let (ty, kind) = if parser.peek2::<kw::import>() {
            let (module, name) = parser.parens(|p| {
                p.parse::<kw::import>()?;
                Ok((p.parse()?, p.parse()?))
            })?;
            (parser.parse()?, GlobalKind::Import { module, name })
        } else {
            (parser.parse()?, GlobalKind::Inline(parser.parse()?))
        };
        Ok(Global {
            name,
            exports,
            ty,
            kind,
        })
    }
}
