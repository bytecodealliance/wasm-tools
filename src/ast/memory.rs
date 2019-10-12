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
        let mut l = parser.lookahead1();
        let style = if l.peek::<ast::LParen>() {
            enum Which<'a, T> {
                Inline(Vec<T>),
                Import(&'a str, &'a str),
            }
            let result = parser.parens(|parser| {
                let mut l = parser.lookahead1();
                if l.peek::<kw::data>() {
                    parser.parse::<kw::data>()?;
                    let mut data = Vec::new();
                    while !parser.is_empty() {
                        data.push(parser.parse()?);
                    }
                    Ok(Which::Inline(data))
                } else if l.peek::<kw::import>() {
                    parser.parse::<kw::import>()?;
                    Ok(Which::Import(parser.parse()?, parser.parse()?))
                } else {
                    Err(l.error())
                }
            })?;
            match result {
                Which::Inline(data) => MemoryStyle::Inline(data),
                Which::Import(module, name) => MemoryStyle::Import {
                    module,
                    name,
                    ty: parser.parse()?,
                },
            }
        } else if l.peek::<u32>() {
            MemoryStyle::Normal(parser.parse()?)
        } else {
            return Err(l.error());
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
    /// The memory that this `Data` will be associated with.
    ///
    /// Not present for passive segments and otherwise defaults to memory 0
    pub memory: Option<ast::Index<'a>>,

    /// Initial offset to load this data segment at, or `None` if this is a
    /// passive memory segment.
    pub offset: Option<ast::Expression<'a>>,

    /// Bytes for this `Data` segment, viewed as the concatenation of all the
    /// contained slices.
    pub data: Vec<&'a [u8]>,
}

impl<'a> Parse<'a> for Data<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::data>()?;
        let memory = parser.parse()?;
        let offset = if parser.peek::<ast::LParen>() {
            Some(parser.parens(|parser| {
                if parser.peek::<kw::offset>() {
                    parser.parse::<kw::offset>()?;
                }
                parser.parse()
            })?)
        } else {
            None
        };
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
