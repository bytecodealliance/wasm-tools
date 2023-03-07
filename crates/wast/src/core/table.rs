use crate::core::*;
use crate::kw;
use crate::parser::{Parse, Parser, Result};
use crate::token::{Id, Index, LParen, NameAnnotation, Span};

/// A WebAssembly `table` directive in a module.
#[derive(Debug)]
pub struct Table<'a> {
    /// Where this table was defined.
    pub span: Span,
    /// An optional name to refer to this table by.
    pub id: Option<Id<'a>>,
    /// An optional name for this function stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// If present, inline export annotations which indicate names this
    /// definition should be exported under.
    pub exports: InlineExport<'a>,
    /// How this table is textually defined in the module.
    pub kind: TableKind<'a>,
}

/// Different ways to textually define a table.
#[derive(Debug)]
pub enum TableKind<'a> {
    /// This table is actually an inlined import definition.
    #[allow(missing_docs)]
    Import {
        import: InlineImport<'a>,
        ty: TableType<'a>,
    },

    /// A typical memory definition which simply says the limits of the table
    Normal {
        /// Table type.
        ty: TableType<'a>,
        /// Optional items initializer expression.
        init_expr: Option<Expression<'a>>,
    },

    /// The elem segments of this table, starting from 0, explicitly listed
    Inline {
        /// The element type of this table.
        elem: RefType<'a>,
        /// The element table entries to have, and the length of this list is
        /// the limits of the table as well.
        payload: ElemPayload<'a>,
    },
}

impl<'a> Parse<'a> for Table<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::table>()?.0;
        let id = parser.parse()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        // Afterwards figure out which style this is, either:
        //
        //  *   `elemtype (elem ...)`
        //  *   `(import "a" "b") limits`
        //  *   `limits`
        let mut l = parser.lookahead1();
        let kind = if l.peek::<RefType>() {
            let elem = parser.parse()?;
            let payload = parser.parens(|p| {
                p.parse::<kw::elem>()?;
                let ty = match elem {
                    rt @ RefType { heap: HeapType::Index(_), .. } => Some(rt),
                    elem if parser.peek::<LParen>() => Some(elem),
                    _ => None,
                };
                Ok(ElemPayload::parse_tail(parser, ty)?)
            })?;
            TableKind::Inline { elem, payload }
        } else if l.peek::<u32>() {
            TableKind::Normal {
                ty: parser.parse()?,
                init_expr: if !parser.is_empty() {
                    Some(parser.parse::<Expression>()?)
                } else {
                    None
                },
            }
        } else if let Some(import) = parser.parse()? {
            TableKind::Import {
                import,
                ty: parser.parse()?,
            }
        } else {
            return Err(l.error());
        };
        Ok(Table {
            span,
            id,
            name,
            exports,
            kind,
        })
    }
}

/// An `elem` segment in a WebAssembly module.
#[derive(Debug)]
pub struct Elem<'a> {
    /// Where this `elem` was defined.
    pub span: Span,
    /// An optional name by which to refer to this segment.
    pub id: Option<Id<'a>>,
    /// An optional name for this element stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// The way this segment was defined in the module.
    pub kind: ElemKind<'a>,
    /// The payload of this element segment, typically a list of functions.
    pub payload: ElemPayload<'a>,
}

/// Different ways to define an element segment in an mdoule.
#[derive(Debug)]
pub enum ElemKind<'a> {
    /// A passive segment that isn't associated with a table and can be used in
    /// various bulk-memory instructions.
    Passive,

    /// A declared element segment that is purely used to declare function
    /// references.
    Declared,

    /// An active segment associated with a table.
    Active {
        /// The table this `elem` is initializing.
        table: Index<'a>,
        /// The offset within `table` that we'll initialize at.
        offset: Expression<'a>,
    },
}

/// Different ways to define the element segment payload in a module.
#[derive(Debug)]
pub enum ElemPayload<'a> {
    /// This element segment has a contiguous list of function indices
    Indices(Vec<Index<'a>>),

    /// This element segment has a list of optional function indices,
    /// represented as expressions using `ref.func` and `ref.null`.
    Exprs {
        /// The desired type of each expression below.
        ty: RefType<'a>,
        /// The expressions in this segment.
        exprs: Vec<Expression<'a>>,
    },
}

impl<'a> Parse<'a> for Elem<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::elem>()?.0;
        let id = parser.parse()?;
        let name = parser.parse()?;

        let kind = if parser.peek::<kw::declare>() {
            parser.parse::<kw::declare>()?;
            ElemKind::Declared
        } else if parser.peek::<u32>() || (parser.peek::<LParen>() && !parser.peek::<RefType>()) {
            let table = if parser.peek::<u32>() {
                // FIXME: this is only here to accomodate
                // proposals/threads/imports.wast at this current moment in
                // time, this probably should get removed when the threads
                // proposal is rebased on the current spec.
                Index::Num(parser.parse()?, span)
            } else if parser.peek2::<kw::table>() {
                parser.parens(|p| {
                    p.parse::<kw::table>()?;
                    p.parse()
                })?
            } else {
                Index::Num(0, span)
            };
            let offset = parser.parens(|parser| {
                if parser.peek::<kw::offset>() {
                    parser.parse::<kw::offset>()?;
                }
                parser.parse()
            })?;
            ElemKind::Active { table, offset }
        } else {
            ElemKind::Passive
        };
        let payload = parser.parse()?;
        Ok(Elem {
            span,
            id,
            name,
            kind,
            payload,
        })
    }
}

impl<'a> Parse<'a> for ElemPayload<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        ElemPayload::parse_tail(parser, parser.parse()?)
    }
}

impl<'a> ElemPayload<'a> {
    fn parse_tail(parser: Parser<'a>, ty: Option<RefType<'a>>) -> Result<Self> {

        // We are in the middle of parsing an element
        // (elem XXXX indices/expressions)
        //       ^^^^
        //       | we are here. We use the provided context `ty` + parser lookahead to learn which
        //         abbreviation notation we are currently parsing.
        match ty {
            None => {
                // case: "XXXX" is "func", we expect to parse a list
                // of untyped function references indices.
                parser.parse::<Option<kw::func>>()?;
                let mut elems = Vec::new();
                while !parser.is_empty() {
                    elems.push(parser.parse()?);
                }
                Ok(ElemPayload::Indices(elems))
            }
            Some(RefType { heap: HeapType::Func, .. }) if parser.peek::<Index<'_>>() => {
                // case: "XXXX" is the empty string. The context tells
                // us to expect a list of untyped function references.
                let mut elems = Vec::new();
                while !parser.is_empty() {
                    elems.push(parser.parse()?);
                }
                Ok(ElemPayload::Indices(elems))
            }
            Some(ty @ RefType { heap: HeapType::Index(_), .. }) if parser.peek::<Index<'_>>() => {
                // case: "XXXX" is the empty string. The context tells
                // us to expect a list of typeful function references.
                let mut instrs = Vec::new();
                while !parser.is_empty() {
                    instrs.push(Instruction::RefFunc(parser.parse()?));
                }
                Ok(ElemPayload::Exprs {
                    ty,
                    exprs: vec![Expression { instrs:instrs.into() }] })
            }
            Some(ty) => {
                // default case: "XXXX" is the empty string. We expect
                // to parse a list of constant expressions.
                let mut exprs = Vec::new();
                while !parser.is_empty() {
                    let expr = parser.parens(|parser| {
                        if parser.peek::<kw::item>() {
                            parser.parse::<kw::item>()?;
                            parser.parse()
                        } else {
                            // Without `item` this is "sugar" for a single-instruction
                            // expression.
                            let insn = parser.parse()?;
                            Ok(Expression {
                                instrs: [insn].into(),
                            })
                        }
                    })?;
                    exprs.push(expr);
                }
                Ok(ElemPayload::Exprs { exprs, ty })
            }
        }
    }
}
