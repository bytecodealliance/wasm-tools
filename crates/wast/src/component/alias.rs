use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

/// An `alias` statement used to juggle indices with nested components.
#[derive(Debug, Clone)]
pub struct Alias<'a> {
    /// Where this `alias` was defined.
    pub span: ast::Span,
    /// An identifier that this alias is resolved with (optionally) for name
    /// resolution.
    pub id: Option<ast::Id<'a>>,
    /// An optional name for this alias stored in the custom `name` section.
    pub name: Option<ast::NameAnnotation<'a>>,
    /// The target of this alias.
    pub target: AliasTarget<'a>,
    /// The kind of item that's being aliased.
    pub kind: AliasKind,
}

/// aliaskind   ::= (module <id>?)
///               | (component <id>?)
///               | (instance <id>?)
///               | (func <id>?)
///               | (value <id>?)
///               | (type <id>?)
///               | (table <id>?)
///               | (memory <id>?)
///               | (global <id>?)
///               | ... other Post-MVP Core definition kinds
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum AliasKind {
    Module,
    Component,
    Instance,
    Value,
    ExportKind(ast::ExportKind),
}

impl<'a> Parse<'a> for AliasKind {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<ast::ExportKind>() {
            let kind = parser.parse::<ast::ExportKind>()?;
            Ok(AliasKind::ExportKind(kind))
        } else if l.peek::<kw::module>() {
            parser.parse::<kw::module>()?;
            Ok(AliasKind::Module)
        } else if l.peek::<kw::component>() {
            parser.parse::<kw::component>()?;
            Ok(AliasKind::Component)
        } else if l.peek::<kw::instance>() {
            parser.parse::<kw::instance>()?;
            Ok(AliasKind::Instance)
        } else if l.peek::<kw::value>() {
            parser.parse::<kw::value>()?;
            Ok(AliasKind::Value)
        } else {
            Err(l.error())
        }
    }
}

/// aliastarget ::= export <instanceidx> <name>
///               | outer <outeridx> <idx>
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum AliasTarget<'a> {
    Export {
        instance: ast::Index<'a>,
        export: &'a str,
    },
    Outer {
        /// The number of enclosing components to skip.
        outer: ast::Index<'a>,
        /// An index into the target component's `aliaskind` index space.
        index: ast::Index<'a>,
    },
}

impl<'a> Parse<'a> for Alias<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::alias>()?.0;
        let target = if parser.parse::<Option<kw::outer>>()?.is_some() {
            AliasTarget::Outer {
                outer: parser.parse()?,
                index: parser.parse()?,
            }
        } else {
            parser.parse::<kw::export>()?;
            AliasTarget::Export {
                instance: parser.parse::<ast::Index<'a>>()?,
                export: parser.parse()?,
            }
        };
        let (kind, id, name) = parser.parens(|p| Ok((p.parse()?, p.parse()?, p.parse()?)))?;

        Ok(Alias {
            span,
            id,
            name,
            kind,
            target,
        })
    }
}
