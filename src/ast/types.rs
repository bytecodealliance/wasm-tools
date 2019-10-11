use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

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
        if parser.peek::<ast::LParen>() {
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
pub struct FunctionType {
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

impl<'a> Parse<'a> for FunctionType {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::func>()?;
        let mut params = Vec::new();
        let mut results = Vec::new();

        while !parser.is_empty() {
            parser.parens(|p| {
                let dst = if p.peek::<kw::param>() {
                    p.parse::<kw::param>()?;
                    if results.len() > 0 {
                        return Err(p.error("cannot list params after results"));
                    }
                    &mut params
                } else {
                    p.parse::<kw::result>()?;
                    &mut results
                };

                dst.push(p.parse()?);

                while !p.is_empty() {
                    dst.push(p.parse()?);
                }
                Ok(())
            })?;
        }

        Ok(FunctionType { params, results })
    }
}
