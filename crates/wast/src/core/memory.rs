use crate::core::*;
use crate::kw;
use crate::parser::{Lookahead1, Parse, Parser, Peek, Result};
use crate::token::*;

/// A defined WebAssembly memory instance inside of a module.
#[derive(Debug)]
pub struct Memory<'a> {
    /// Where this `memory` was defined
    pub span: Span,
    /// An optional name to refer to this memory by.
    pub id: Option<Id<'a>>,
    /// An optional name for this function stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// If present, inline export annotations which indicate names this
    /// definition should be exported under.
    pub exports: InlineExport<'a>,
    /// How this memory is defined in the module.
    pub kind: MemoryKind<'a>,
}

/// Different syntactical ways a memory can be defined in a module.
#[derive(Debug)]
pub enum MemoryKind<'a> {
    /// This memory is actually an inlined import definition.
    #[allow(missing_docs)]
    Import {
        import: InlineImport<'a>,
        ty: MemoryType,
    },

    /// A typical memory definition which simply says the limits of the memory
    Normal(MemoryType),

    /// The data of this memory, starting from 0, explicitly listed
    Inline {
        /// Whether or not this will be creating a 32-bit memory
        is_32: bool,
        /// The inline data specified for this memory
        data: Vec<DataVal<'a>>,
    },
}

impl<'a> Parse<'a> for Memory<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::memory>()?.0;
        let id = parser.parse()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        // Afterwards figure out which style this is, either:
        //
        //  *   `(import "a" "b") limits`
        //  *   `(data ...)`
        //  *   `limits`
        let mut l = parser.lookahead1();
        let kind = if let Some(import) = parser.parse()? {
            MemoryKind::Import {
                import,
                ty: parser.parse()?,
            }
        } else if l.peek::<LParen>() || parser.peek2::<LParen>() {
            let is_32 = if parser.parse::<Option<kw::i32>>()?.is_some() {
                true
            } else {
                parser.parse::<Option<kw::i64>>()?.is_none()
            };
            let data = parser.parens(|parser| {
                parser.parse::<kw::data>()?;
                let mut data = Vec::new();
                while !parser.is_empty() {
                    data.push(parser.parse()?);
                }
                Ok(data)
            })?;
            MemoryKind::Inline { data, is_32 }
        } else if l.peek::<u32>() || l.peek::<kw::i32>() || l.peek::<kw::i64>() {
            MemoryKind::Normal(parser.parse()?)
        } else {
            return Err(l.error());
        };
        Ok(Memory {
            span,
            id,
            name,
            exports,
            kind,
        })
    }
}

/// A `data` directive in a WebAssembly module.
#[derive(Debug)]
pub struct Data<'a> {
    /// Where this `data` was defined
    pub span: Span,

    /// The optional name of this data segment
    pub id: Option<Id<'a>>,

    /// An optional name for this data stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,

    /// Whether this data segment is passive or active
    pub kind: DataKind<'a>,

    /// Bytes for this `Data` segment, viewed as the concatenation of all the
    /// contained slices.
    pub data: Vec<DataVal<'a>>,
}

/// Different kinds of data segments, either passive or active.
#[derive(Debug)]
pub enum DataKind<'a> {
    /// A passive data segment which isn't associated with a memory and is
    /// referenced from various instructions.
    Passive,

    /// An active data segment which is associated and loaded into a particular
    /// memory on module instantiation.
    Active {
        /// The memory that this `Data` will be associated with.
        memory: Index<'a>,

        /// Initial offset to load this data segment at
        offset: Expression<'a>,
    },
}

impl<'a> Parse<'a> for Data<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::data>()?.0;
        let id = parser.parse()?;
        let name = parser.parse()?;

        let kind = if parser.peek::<&[u8]>() {
            DataKind::Passive

        // ... and otherwise we must be attached to a particular memory as well
        // as having an initialization offset.
        } else {
            let memory = if parser.peek::<u32>() {
                // FIXME: this is only here to accomodate
                // proposals/threads/imports.wast at this current moment in
                // time, this probably should get removed when the threads
                // proposal is rebased on the current spec.
                Index::Num(parser.parse()?, span)
            } else if parser.peek2::<kw::memory>() {
                parser.parens(|p| {
                    p.parse::<kw::memory>()?;
                    p.parse()
                })?
            } else {
                Index::Num(0, span)
            };
            let offset = parser.parens(|parser| {
                if parser.peek::<kw::offset>() {
                    parser.parse::<kw::offset>()?;
                    parser.parse()
                } else {
                    // This is all that the spec allows, which is that if
                    // `offset` isn't present then this is "sugar" for a
                    // single-instruction expression.
                    let insn = parser.parse()?;
                    if parser.is_empty() {
                        return Ok(Expression {
                            instrs: [insn].into(),
                        });
                    }

                    // This is support for what is currently invalid syntax
                    // according to the strict specification but is otherwise
                    // present in the spec test suite:
                    //
                    //    (data (i32.add (i32.const 0) (i32.const 0)))
                    //
                    // Technically the spec says this should be:
                    //
                    //    (data (offset ...))
                    //
                    // but alas
                    let expr: Expression = parser.parse()?;
                    let mut instrs = Vec::from(expr.instrs);
                    instrs.push(insn);
                    Ok(Expression {
                        instrs: instrs.into(),
                    })
                }
            })?;
            DataKind::Active { memory, offset }
        };

        let mut data = Vec::new();
        while !parser.is_empty() {
            data.push(parser.parse()?);
        }
        Ok(Data {
            span,
            id,
            name,
            kind,
            data,
        })
    }
}

/// Differnet ways the value of a data segment can be defined.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum DataVal<'a> {
    String(&'a [u8]),
    Integral(Vec<u8>),
}

impl DataVal<'_> {
    /// Returns the length, in bytes, of the memory used to represent this data
    /// value.
    pub fn len(&self) -> usize {
        match self {
            DataVal::String(s) => s.len(),
            DataVal::Integral(s) => s.len(),
        }
    }

    /// Pushes the value of this data value onto the provided list of bytes.
    pub fn push_onto(&self, dst: &mut Vec<u8>) {
        match self {
            DataVal::String(s) => dst.extend_from_slice(s),
            DataVal::Integral(s) => dst.extend_from_slice(s),
        }
    }
}

impl<'a> Parse<'a> for DataVal<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if !parser.peek::<LParen>() {
            return Ok(DataVal::String(parser.parse()?));
        }

        return parser.parens(|p| {
            let mut result = Vec::new();
            let mut lookahead = p.lookahead1();
            let l = &mut lookahead;
            let r = &mut result;
            if consume::<kw::i8, i8, _>(p, l, r, |u, v| v.push(u as u8))?
                || consume::<kw::i16, i16, _>(p, l, r, |u, v| v.extend(&u.to_le_bytes()))?
                || consume::<kw::i32, i32, _>(p, l, r, |u, v| v.extend(&u.to_le_bytes()))?
                || consume::<kw::i64, i64, _>(p, l, r, |u, v| v.extend(&u.to_le_bytes()))?
                || consume::<kw::f32, Float32, _>(p, l, r, |u, v| v.extend(&u.bits.to_le_bytes()))?
                || consume::<kw::f64, Float64, _>(p, l, r, |u, v| v.extend(&u.bits.to_le_bytes()))?
                || consume::<kw::v128, V128Const, _>(p, l, r, |u, v| v.extend(&u.to_le_bytes()))?
            {
                Ok(DataVal::Integral(result))
            } else {
                Err(lookahead.error())
            }
        });

        fn consume<'a, T: Peek + Parse<'a>, U: Parse<'a>, F>(
            parser: Parser<'a>,
            lookahead: &mut Lookahead1<'a>,
            dst: &mut Vec<u8>,
            push: F,
        ) -> Result<bool>
        where
            F: Fn(U, &mut Vec<u8>),
        {
            if !lookahead.peek::<T>() {
                return Ok(false);
            }
            parser.parse::<T>()?;
            while !parser.is_empty() {
                let val = parser.parse::<U>()?;
                push(val, dst);
            }
            Ok(true)
        }
    }
}
