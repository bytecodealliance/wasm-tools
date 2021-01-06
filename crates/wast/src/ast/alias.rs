use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

/// An `alias` statement used to juggle indices with nested modules.
#[derive(Debug)]
pub struct Alias<'a> {
    /// Where this `alias` was defined.
    pub span: ast::Span,
    /// An identifier that this alias is resolved with (optionally) for name
    /// resolution.
    pub id: Option<ast::Id<'a>>,
    /// An optional name for this alias stored in the custom `name` section.
    pub name: Option<ast::NameAnnotation<'a>>,
    /// The item in the parent instance that we're aliasing.
    pub kind: AliasKind<'a>,
}

#[derive(Debug)]
#[allow(missing_docs)]
pub enum AliasKind<'a> {
    InstanceExport {
        instance: ast::ItemRef<'a, kw::instance>,
        export: &'a str,
        kind: ast::ExportKind,
    },
    Parent {
        parent_index: ast::Index<'a>,
        kind: ast::ExportKind,
    },
}

impl Alias<'_> {
    /// Returns the kind of item defined by this alias.
    pub fn item_kind(&self) -> ast::ExportKind {
        match self.kind {
            AliasKind::InstanceExport { kind, .. } => kind,
            AliasKind::Parent { kind, .. } => kind,
        }
    }
}

impl<'a> Parse<'a> for Alias<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::alias>()?.0;
        let (id, name, kind) = if parser.parse::<Option<kw::parent>>()?.is_some() {
            // (alias parent $parent_idx (type $my_name))
            let parent_index = parser.parse()?;
            let (kind, id) = parser.parens(|p| Ok((p.parse()?, p.parse()?)))?;
            (id, None, AliasKind::Parent { parent_index, kind })
        } else {
            // (alias $instance "export" (type $my_name))
            let instance = parser.parse::<ast::IndexOrRef<_>>()?.0;
            let export = parser.parse()?;
            let (kind, id, name) = parser.parens(|p| Ok((p.parse()?, p.parse()?, p.parse()?)))?;
            (
                id,
                name,
                AliasKind::InstanceExport {
                    instance,
                    export,
                    kind,
                },
            )
        };

        Ok(Alias {
            span,
            id,
            name,
            kind,
        })
    }
}
