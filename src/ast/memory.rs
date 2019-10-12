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
    /// The optional name of this data segment
    pub name: Option<ast::Id<'a>>,

    /// Whether this data segment is passive or active
    pub kind: DataKind<'a>,

    /// Bytes for this `Data` segment, viewed as the concatenation of all the
    /// contained slices.
    pub data: Vec<&'a [u8]>,
}

#[derive(Debug, PartialEq)]
pub enum DataKind<'a> {
    Passive,
    Active {
        /// The memory that this `Data` will be associated with.
        memory: ast::Index<'a>,

        /// Initial offset to load this data segment at
        offset: ast::Expression<'a>,
    },
}

impl<'a> Parse<'a> for Data<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::data>()?;
        let name = parser.parse()?;

        // The `passive` keyword is mentioned in the current spec but isn't
        // mentioned in `wabt` tests, so consider it optional for now
        let kind = if parser.peek::<kw::passive>() {
            parser.parse::<kw::passive>()?;
            DataKind::Passive

        // If data directly follows then assume this is a passive segment
        } else if parser.peek::<&[u8]>() {
            DataKind::Passive

        // ... and otherwise we must be attached to a particular memory as well
        // as having an initialization offset.
        } else {
            let memory = parser.parse::<Option<ast::Index>>()?;
            let offset = parser.parens(|parser| {
                if parser.peek::<kw::offset>() {
                    parser.parse::<kw::offset>()?;
                }
                parser.parse()
            })?;
            DataKind::Active {
                memory: memory.unwrap_or(ast::Index::Num(0)),
                offset,
            }
        };

        let mut data = Vec::new();
        while !parser.is_empty() {
            data.push(parser.parse()?);
        }
        Ok(Data { name, kind, data })
    }
}
