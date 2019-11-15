use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

/// A WebAssembly `table` directive in a module.
#[derive(Debug)]
pub struct Table<'a> {
    /// Where this table was defined.
    pub span: ast::Span,
    /// An optional name to refer to this table by.
    pub name: Option<ast::Id<'a>>,
    /// If present, inline export annotations which indicate names this
    /// definition should be exported under.
    pub exports: ast::InlineExport<'a>,
    /// How this table is textually defined in the module.
    pub kind: TableKind<'a>,
}

/// Different ways to textually define a table.
#[derive(Debug)]
pub enum TableKind<'a> {
    /// This table is actually an inlined import definition.
    #[allow(missing_docs)]
    Import {
        module: &'a str,
        name: &'a str,
        ty: ast::TableType,
    },

    /// A typical memory definition which simply says the limits of the table
    Normal(ast::TableType),

    /// The elem segments of this table, starting from 0, explicitly listed
    Inline {
        /// The element type of this table.
        elem: ast::TableElemType,
        /// The element table entries to have, and the length of this list is
        /// the limits of the table as well.
        elems: Vec<ast::Index<'a>>,
    },
}

impl<'a> Parse<'a> for Table<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::table>()?.0;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        // Afterwards figure out which style this is, either:
        //
        //  *   `elemtype (elem ...)`
        //  *   `(import "a" "b") limits`
        //  *   `limits`
        let mut l = parser.lookahead1();
        let kind = if l.peek::<ast::TableElemType>() {
            let elem = parser.parse()?;
            let mut elems = Vec::new();
            parser.parens(|p| {
                p.parse::<kw::elem>()?;
                while !p.is_empty() {
                    elems.push(p.parse()?);
                }
                Ok(())
            })?;
            TableKind::Inline { elem, elems }
        } else if l.peek::<u32>() {
            TableKind::Normal(parser.parse()?)
        } else if l.peek::<ast::LParen>() {
            let (module, name) = parser.parens(|p| {
                p.parse::<kw::import>()?;
                Ok((p.parse()?, p.parse()?))
            })?;
            TableKind::Import {
                module,
                name,
                ty: parser.parse()?,
            }
        } else {
            return Err(l.error());
        };
        Ok(Table {
            span,
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
    pub span: ast::Span,
    /// An optional name by which to refer to this segment.
    pub name: Option<ast::Id<'a>>,
    /// The way this segment was defined in the module.
    pub kind: ElemKind<'a>,
}

/// Different ways to define an element segment in an mdoule.
#[derive(Debug)]
pub enum ElemKind<'a> {
    /// A passive segment that isn't associated with a table and can be used in
    /// various bulk-memory instructions.
    Passive {
        /// The type of elements within this segment.
        ty: ast::TableElemType,
        /// The function indices (for now) of elements in this segment. `None`
        /// entries represent `ref.null` instructions.
        elems: Vec<Option<ast::Index<'a>>>,
    },

    /// An active segment associated with a table.
    Active {
        /// The table this `elem` is initializing.
        table: ast::Index<'a>,
        /// The offset within `table` that we'll initialize at.
        offset: ast::Expression<'a>,
        /// The function indices that will be inserted into the table.
        elems: Vec<ast::Index<'a>>,
    },
}

impl<'a> Parse<'a> for Elem<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::elem>()?.0;
        let mut name = parser.parse::<Option<_>>()?;

        let kind = if parser.peek::<ast::TableElemType>() {
            let ty = parser.parse::<ast::TableElemType>()?;
            let mut elems = Vec::new();
            if parser.peek::<ast::LParen>() {
                while !parser.is_empty() {
                    elems.push(parser.parens(|p| {
                        let mut l = p.lookahead1();
                        if l.peek::<kw::ref_null>() {
                            p.parse::<kw::ref_null>()?;
                            Ok(None)
                        } else if l.peek::<kw::ref_func>() {
                            p.parse::<kw::ref_func>()?;
                            Ok(Some(p.parse()?))
                        } else {
                            Err(l.error())
                        }
                    })?);
                }
            } else {
                while !parser.is_empty() {
                    elems.push(Some(parser.parse()?));
                }
            }
            ElemKind::Passive { ty, elems }
        } else {
            // TODO: this should be brought up with the bulk memory proposal,
            // it's sort of ambiguous but apparently if one name is present it's
            // a table name, but if two then it's an element name and a segment
            // name. I'm a bit confused but this seems to pass tests.
            let mut table = parser.parse::<Option<ast::Index>>()?;
            if table.is_none() {
                if let Some(name) = name.take() {
                    table = Some(ast::Index::Id(name));
                }
            }
            let offset = parser.parens(|parser| {
                if parser.peek::<kw::offset>() {
                    parser.parse::<kw::offset>()?;
                }
                parser.parse()
            })?;
            let mut elems = Vec::new();
            while !parser.is_empty() {
                elems.push(parser.parse()?);
            }
            ElemKind::Active {
                table: table.unwrap_or(ast::Index::Num(0)),
                offset,
                elems,
            }
        };
        Ok(Elem { span, name, kind })
    }
}
