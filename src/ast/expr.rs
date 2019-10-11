use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(Debug, PartialEq)]
pub struct Expression<'a> {
    _a: &'a (),
}

impl<'a> Parse<'a> for Expression<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        panic!()
        // parser.parse::<kw::global>()?;
        // let name = parser.parse()?;
        // let exports = parser.parse()?;
        //
        // let (ty, kind) = if parser.peek2::<kw::import>() {
        //     let (module, name) = parser.parens(|p| {
        //         p.parse::<kw::import>()?;
        //         Ok((p.parse()?, p.parse()?))
        //     })?;
        //     (parser.parse()?, GlobalKind::Import { module, name })
        // } else {
        //     (parser.parse()?, GlobalKind::Inline(parser.parse()?))
        // };
        // Ok(Global {
        //     name,
        //     exports,
        //     ty,
        //     kind,
        // })
    }
}
