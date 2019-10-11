use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(Debug, PartialEq)]
pub struct Memory<'a> {
    pub name: Option<ast::Id<'a>>,
    pub exports: ast::InlineExport<'a>,
    pub style: MemoryStyle<'a>,
}

#[derive(Debug, PartialEq)]
pub enum MemoryStyle<'a> {
    Import {
        module: &'a str,
        name: &'a str,
        ty: ast::MemoryType,
    },
    Normal(ast::MemoryType),
    Inline(Vec<&'a [u8]>),
}

impl<'a> Parse<'a> for Memory<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::memory>()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        // Afterwards figure out which style this is, either:
        //
        //  *   `(data ...)`
        //  *   `(import "a" "b") limits`
        //  *   `limits`
        let style = if parser.peek2::<kw::data>() {
            let mut data = Vec::new();
            parser.parens(|p| {
                p.parse::<kw::data>()?;
                while !p.is_empty() {
                    data.push(p.parse()?);
                }
                Ok(())
            })?;
            MemoryStyle::Inline(data)
        } else if parser.peek2::<kw::import>() {
            let (module, name) = parser.parens(|p| {
                p.parse::<kw::import>()?;
                Ok((p.parse()?, p.parse()?))
            })?;
            MemoryStyle::Import {
                module,
                name,
                ty: parser.parse()?,
            }
        } else {
            MemoryStyle::Normal(parser.parse()?)
        };
        Ok(Memory {
            name,
            exports,
            style,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Data<'a> {
    pub memory: Option<ast::Index<'a>>,
    pub offset: ast::Expression<'a>,
    pub data: Vec<&'a [u8]>,
}

impl<'a> Parse<'a> for Data<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::data>()?;
        let memory = parser.parse()?;
        let offset = parser.parens(|parser| {
            if parser.peek::<kw::offset>() {
                parser.parse::<kw::offset>()?;
            }
            parser.parse()
        })?;
        let mut data = Vec::new();
        while !parser.is_empty() {
            data.push(parser.parse()?);
        }
        Ok(Data {
            memory,
            offset,
            data,
        })
    }
}
