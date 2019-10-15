use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

/// A WebAssembly global in a module
#[derive(Debug, PartialEq)]
pub struct Global<'a> {
    /// An optional name to reference this global by
    pub name: Option<ast::Id<'a>>,
    /// If present, inline export annotations which indicate names this
    /// definition should be exported under.
    pub exports: ast::InlineExport<'a>,
    /// The type of this global, both its value type and whether it's mutable.
    pub ty: ast::GlobalType,
    /// What kind of global this defined as.
    pub kind: GlobalKind<'a>,
}

/// Different kinds of globals that can be defined in a module.
#[derive(Debug, PartialEq)]
pub enum GlobalKind<'a> {
    /// A global which is actually defined as an import, such as:
    ///
    /// ```text
    /// (global i32 (import "foo" "bar"))
    /// ```
    Import {
        /// The module that this function is imported from
        module: &'a str,
        /// The module field name this function is imported from
        name: &'a str,
    },

    /// A global defined inline in the module itself
    Inline(ast::Expression<'a>),
}

impl<'a> Parse<'a> for Global<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::global>()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        let (ty, kind) = if parser.peek2::<kw::import>() {
            let (module, name) = parser.parens(|p| {
                p.parse::<kw::import>()?;
                Ok((p.parse()?, p.parse()?))
            })?;
            (parser.parse()?, GlobalKind::Import { module, name })
        } else {
            (parser.parse()?, GlobalKind::Inline(parser.parse()?))
        };
        Ok(Global {
            name,
            exports,
            ty,
            kind,
        })
    }
}
