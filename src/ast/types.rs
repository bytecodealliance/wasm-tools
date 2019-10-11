use crate::ast::{self, kw};
use crate::parser::{Cursor, Parse, Parser, Peek, Result};

/// The value types for a wasm module.
#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

impl<'a> Parse<'a> for ValType {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<kw::i32>() {
            parser.parse::<kw::i32>()?;
            return Ok(ValType::I32);
        }
        if parser.peek::<kw::i64>() {
            parser.parse::<kw::i64>()?;
            return Ok(ValType::I64);
        }
        if parser.peek::<kw::f32>() {
            parser.parse::<kw::f32>()?;
            return Ok(ValType::F32);
        }
        if parser.peek::<kw::f64>() {
            parser.parse::<kw::f64>()?;
            return Ok(ValType::F64);
        }
        Err(parser.error("expected a value type"))
    }
}

/// Type for a `global` in a wasm module
#[derive(Debug, PartialEq)]
pub struct GlobalType {
    /// The element type of this `global`
    pub ty: ValType,
    /// Whether or not the global is mutable or not.
    pub mutable: bool,
}

impl<'a> Parse<'a> for GlobalType {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek2::<kw::r#mut>() {
            parser.parens(|p| {
                p.parse::<kw::r#mut>()?;
                Ok(GlobalType {
                    ty: parser.parse()?,
                    mutable: true,
                })
            })
        } else {
            Ok(GlobalType {
                ty: parser.parse()?,
                mutable: false,
            })
        }
    }
}

/// List of different kinds of table types we can have.
///
/// Currently there's only one, a `funcref`.
#[derive(Debug, PartialEq)]
pub enum TableElemType {
    /// An element for a table that is a list of functions.
    Funcref,
}

impl<'a> Parse<'a> for TableElemType {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::funcref>()?;
        Ok(TableElemType::Funcref)
    }
}

impl Peek for TableElemType {
    fn peek(cursor: Cursor<'_>) -> bool {
        kw::funcref::peek(cursor)
    }
}

/// Min/max limits used for tables/memories.
#[derive(Debug, PartialEq)]
pub struct Limits {
    /// The minimum number of units for this type.
    pub min: u32,
    /// An optional maximum number of units for this type.
    pub max: Option<u32>,
}

impl<'a> Parse<'a> for Limits {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let min = parser.parse()?;
        let max = if parser.peek::<u32>() {
            Some(parser.parse()?)
        } else {
            None
        };
        Ok(Limits { min, max })
    }
}

/// Configuration for a table of a wasm mdoule
#[derive(Debug, PartialEq)]
pub struct TableType {
    /// Limits on the element sizes of this table
    pub limits: Limits,
    /// The type of element stored in this table
    pub elem: TableElemType,
}

impl<'a> Parse<'a> for TableType {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(TableType {
            limits: parser.parse()?,
            elem: parser.parse()?,
        })
    }
}

/// Configuration for a memory of a wasm module
#[derive(Debug, PartialEq)]
pub struct MemoryType {
    /// Limits on the page sizes of this memory
    pub limits: Limits,
}

impl<'a> Parse<'a> for MemoryType {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(MemoryType {
            limits: parser.parse()?,
        })
    }
}

/// A function type with parameters and results.
#[derive(Debug, PartialEq)]
pub struct FunctionType<'a> {
    pub params: Vec<(Option<ast::Id<'a>>, ValType)>,
    pub results: Vec<ValType>,
}

impl<'a> FunctionType<'a> {
    fn finish_parse(&mut self, parser: Parser<'a>) -> Result<()> {
        while !parser.is_empty() {
            parser.parens(|p| {
                if p.peek::<kw::param>() {
                    p.parse::<kw::param>()?;
                    if self.results.len() > 0 {
                        return Err(p.error("cannot list params after results"));
                    }
                    let id = p.parse::<Option<_>>()?;
                    let parse_more = id.is_none();
                    let ty = p.parse()?;
                    self.params.push((id, ty));
                    while parse_more && !p.is_empty() {
                        self.params.push((None, p.parse()?));
                    }
                } else {
                    p.parse::<kw::result>()?;
                    self.results.push(p.parse()?);
                    while !p.is_empty() {
                        self.results.push(p.parse()?);
                    }
                }
                Ok(())
            })?;
        }
        Ok(())
    }
}

impl<'a> Parse<'a> for FunctionType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::func>()?;
        let mut ret = FunctionType {
            params: Vec::new(),
            results: Vec::new(),
        };
        ret.finish_parse(parser)?;
        Ok(ret)
    }
}

/// A type declaration in a module
#[derive(Debug, PartialEq)]
pub struct Type<'a> {
    pub name: Option<ast::Id<'a>>,
    pub func: FunctionType<'a>,
}

impl<'a> Parse<'a> for Type<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::r#type>()?;
        let name = parser.parse()?;
        let func = parser.parens(FunctionType::parse)?;
        Ok(Type { name, func })
    }
}

/// A type declaration in a module
#[derive(Debug, PartialEq)]
pub enum TypeUse<'a> {
    Index(ast::Index<'a>),
    Inline(FunctionType<'a>),
}

impl<'a> Parse<'a> for TypeUse<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek2::<kw::r#type>() {
            parser.parens(|parser| {
                parser.parse::<kw::r#type>()?;
                Ok(TypeUse::Index(parser.parse()?))
            })
        } else {
            let mut ft = FunctionType {
                params: Vec::new(),
                results: Vec::new(),
            };
            ft.finish_parse(parser)?;
            Ok(TypeUse::Inline(ft))
        }
    }
}
