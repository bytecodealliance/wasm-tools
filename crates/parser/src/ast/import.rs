use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

/// An `import` statement and entry in a WebAssembly module.
#[derive(Debug)]
pub struct Import<'a> {
    /// Where this `import` was defined
    pub span: ast::Span,
    /// The module that this statement is importing from
    pub module: &'a str,
    /// The name of the field in the module this statement imports from.
    pub name: &'a str,
    /// An optional identifier to refer to this import as in the rest of the
    /// module.
    pub id: Option<ast::Id<'a>>,
    /// What kind of item is being imported.
    pub kind: ImportKind<'a>,
}

/// All possible types of items that can be imported into a wasm module.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum ImportKind<'a> {
    Func(ast::TypeUse<'a>),
    Table(ast::TableType),
    Memory(ast::MemoryType),
    Global(ast::GlobalType),
}

impl<'a> Parse<'a> for Import<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::import>()?.0;
        let module = parser.parse()?;
        let name = parser.parse()?;
        let (id, kind) = parser.parens(|parser| {
            let mut l = parser.lookahead1();
            if l.peek::<kw::func>() {
                parser.parse::<kw::func>()?;
                Ok((parser.parse()?, ImportKind::Func(parser.parse()?)))
            } else if l.peek::<kw::table>() {
                parser.parse::<kw::table>()?;
                Ok((parser.parse()?, ImportKind::Table(parser.parse()?)))
            } else if l.peek::<kw::memory>() {
                parser.parse::<kw::memory>()?;
                Ok((parser.parse()?, ImportKind::Memory(parser.parse()?)))
            } else if l.peek::<kw::global>() {
                parser.parse::<kw::global>()?;
                Ok((parser.parse()?, ImportKind::Global(parser.parse()?)))
            } else {
                Err(l.error())
            }
        })?;
        Ok(Import {
            span,
            module,
            name,
            id,
            kind,
        })
    }
}
