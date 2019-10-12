use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub name: Option<ast::Id<'a>>,
    pub exports: ast::InlineExport<'a>,
    pub style: TableStyle<'a>,
}

#[derive(Debug, PartialEq)]
pub enum TableStyle<'a> {
    Import {
        module: &'a str,
        name: &'a str,
        ty: ast::TableType,
    },
    Normal(ast::TableType),
    Inline {
        elem: ast::TableElemType,
        elems: Vec<ast::Index<'a>>,
    },
}

impl<'a> Parse<'a> for Table<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::table>()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        // Afterwards figure out which style this is, either:
        //
        //  *   `elemtype (elem ...)`
        //  *   `(import "a" "b") limits`
        //  *   `limits`
        let mut l = parser.lookahead1();
        let style = if l.peek::<ast::TableElemType>() {
            let elem = parser.parse()?;
            let mut elems = Vec::new();
            parser.parens(|p| {
                p.parse::<kw::elem>()?;
                while !p.is_empty() {
                    elems.push(p.parse()?);
                }
                Ok(())
            })?;
            TableStyle::Inline { elem, elems }
        } else if l.peek::<u32>() {
            TableStyle::Normal(parser.parse()?)
        } else if l.peek::<ast::LParen>() {
            let (module, name) = parser.parens(|p| {
                p.parse::<kw::import>()?;
                Ok((p.parse()?, p.parse()?))
            })?;
            TableStyle::Import {
                module,
                name,
                ty: parser.parse()?,
            }
        } else {
            return Err(l.error());
        };
        Ok(Table {
            name,
            exports,
            style,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Elem<'a> {
    /// The optional name of this elem segment
    pub name: Option<ast::Id<'a>>,

    /// Whether this elem segment is passive or active
    pub kind: ElemKind<'a>,

    /// Functions referenced in this `elem` segment
    pub elems: Vec<ast::Index<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum ElemKind<'a> {
    Passive {
        ty: ast::TableElemType,
    },
    Active {
        table: ast::Index<'a>,
        offset: ast::Expression<'a>,
    },
}

impl<'a> Parse<'a> for Elem<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::elem>()?;
        let name = parser.parse()?;

        let kind = if parser.peek::<ast::TableElemType>() {
            let ty = parser.parse::<ast::TableElemType>()?;
            ElemKind::Passive { ty }
        } else {
            let table = parser.parse::<Option<ast::Index>>()?;
            let offset = parser.parens(|parser| {
                if parser.peek::<kw::offset>() {
                    parser.parse::<kw::offset>()?;
                }
                parser.parse()
            })?;
            ElemKind::Active {
                table: table.unwrap_or(ast::Index::Num(0)),
                offset,
            }
        };

        let mut elems = Vec::new();
        while !parser.is_empty() {
            elems.push(parser.parse()?);
        }
        Ok(Elem { name, kind, elems })
    }
}
