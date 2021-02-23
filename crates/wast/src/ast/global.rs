use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

/// A WebAssembly global in a module
#[derive(Debug)]
pub struct Global<'a> {
    /// Where this `global` was defined.
    pub span: ast::Span,
    /// An optional name to reference this global by
    pub id: Option<ast::Id<'a>>,
    /// If present, inline export annotations which indicate names this
    /// definition should be exported under.
    pub exports: ast::InlineExport<'a>,
    /// What kind of global this defined as.
    pub kind: GlobalKind<'a>,
}

/// Different kinds of globals that can be defined in a module.
#[derive(Debug)]
pub enum GlobalKind<'a> {
    /// A global which is actually defined as an import, such as:
    ///
    /// ```text
    /// (global i32 (import "foo" "bar"))
    /// ```
    #[allow(missing_docs)]
    Import {
        import: ast::InlineImport<'a>,
        ty: ast::GlobalType<'a>,
    },

    /// A global which is actually defined as an inline alias.
    Alias(ast::InlineAlias<'a>),

    /// A global defined inline in the module itself
    #[allow(missing_docs)]
    Inline {
        init: ast::Expression<'a>,
        ty: ast::GlobalType<'a>,
    },
}

impl<'a> Parse<'a> for Global<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::global>()?.0;
        let id = parser.parse()?;
        let exports = parser.parse()?;

        let kind = if let Some(import) = parser.parse()? {
            GlobalKind::Import {
                import,
                ty: parser.parse()?,
            }
        } else if let Some(alias) = parser.parse()? {
            GlobalKind::Alias(alias)
        } else {
            GlobalKind::Inline {
                ty: parser.parse()?,
                init: parser.parse()?,
            }
        };
        Ok(Global {
            span,
            id,
            exports,
            kind,
        })
    }
}
