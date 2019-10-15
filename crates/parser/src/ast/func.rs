use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(Debug, PartialEq)]
pub struct Func<'a> {
    pub name: Option<ast::Id<'a>>,
    pub exports: ast::InlineExport<'a>,
    pub kind: FuncKind<'a>,
    pub ty: ast::TypeUse<'a>,
}

#[derive(Debug, PartialEq)]
pub enum FuncKind<'a> {
    Import {
        module: &'a str,
        name: &'a str,
    },
    Inline {
        locals: Vec<(Option<ast::Id<'a>>, ast::ValType)>,
        expression: ast::Expression<'a>,
    },
}

impl<'a> Parse<'a> for Func<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::func>()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        let (ty, kind) = if parser.peek2::<kw::import>() {
            let (module, name) = parser.parens(|p| {
                p.parse::<kw::import>()?;
                Ok((p.parse()?, p.parse()?))
            })?;
            (parser.parse()?, FuncKind::Import { module, name })
        } else {
            let ty = parser.parse()?;
            let mut locals = Vec::new();
            while parser.peek2::<kw::local>() {
                parser.parens(|p| {
                    p.parse::<kw::local>()?;
                    if p.is_empty() {
                        return Ok(());
                    }
                    let id: Option<_> = p.parse()?;
                    let ty = p.parse()?;
                    let parse_more = id.is_none();
                    locals.push((id, ty));
                    while parse_more && !p.is_empty() {
                        locals.push((None, p.parse()?));
                    }
                    Ok(())
                })?;
            }
            (
                ty,
                FuncKind::Inline {
                    locals,
                    expression: parser.parse()?,
                },
            )
        };

        Ok(Func {
            name,
            exports,
            ty,
            kind,
        })
    }
}
