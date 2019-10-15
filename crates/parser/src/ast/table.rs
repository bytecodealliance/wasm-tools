use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub name: Option<ast::Id<'a>>,
    pub exports: ast::InlineExport<'a>,
    pub kind: TableKind<'a>,
}

#[derive(Debug, PartialEq)]
pub enum TableKind<'a> {
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
            name,
            exports,
            kind,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Elem<'a> {
    pub name: Option<ast::Id<'a>>,
    pub kind: ElemKind<'a>,
}

#[derive(Debug, PartialEq)]
pub enum ElemKind<'a> {
    Passive {
        ty: ast::TableElemType,
        elems: Vec<Option<ast::Index<'a>>>,
    },
    Active {
        table: ast::Index<'a>,
        offset: ast::Expression<'a>,
        elems: Vec<ast::Index<'a>>,
    },
}

impl<'a> Parse<'a> for Elem<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::elem>()?;
        let name = parser.parse()?;

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
            let table = parser.parse::<Option<ast::Index>>()?;
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
        Ok(Elem { name, kind })
    }
}
