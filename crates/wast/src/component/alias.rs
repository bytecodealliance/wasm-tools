use crate::kw;
use crate::parser::{Parse, Parser, Result};
use crate::token::{Id, Index, NameAnnotation, Span};

/// A inline alias for component exported items.
#[derive(Debug)]
pub struct InlineExportAlias<'a> {
    /// The instance to alias the export from.
    pub instance: Index<'a>,
    /// The name of the export to alias.
    pub name: &'a str,
}

impl<'a> Parse<'a> for InlineExportAlias<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::alias>()?;
        parser.parse::<kw::export>()?;
        let instance = parser.parse()?;
        let name = parser.parse()?;

        Ok(Self { instance, name })
    }
}

/// An alias to a core item.
#[derive(Debug)]
pub struct CoreAlias<'a> {
    /// Where this `core alias` was defined.
    pub span: Span,
    /// An identifier that this alias is resolved with (optionally) for name
    /// resolution.
    pub id: Option<Id<'a>>,
    /// An optional name for this alias stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// The target of the alias.
    pub target: CoreAliasTarget<'a>,
    /// The kind of item being aliased.
    pub kind: CoreAliasKind,
}

impl<'a> Parse<'a> for CoreAlias<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::core>()?.0;
        parser.parse::<kw::alias>()?;
        let target = parser.parse()?;
        let (kind, id, name) = parser.parens(|parser| {
            let kind = parser.parse()?;
            let id = parser.parse()?;
            let name = parser.parse()?;
            Ok((kind, id, name))
        })?;

        Ok(Self {
            span,
            target,
            id,
            name,
            kind,
        })
    }
}

/// The target of a core alias.
#[derive(Debug)]
pub enum CoreAliasTarget<'a> {
    /// The alias is to an export of a core module instance.
    Export {
        /// The core module index exporting the item.
        instance: Index<'a>,
        /// The name of the exported item being aliased.
        name: &'a str,
    },
}

impl<'a> Parse<'a> for CoreAliasTarget<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        // Right now only export aliases are supported.
        parser.parse::<kw::export>()?;
        Ok(Self::Export {
            instance: parser.parse()?,
            name: parser.parse()?,
        })
    }
}

/// Represents the kind of a core alias.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CoreAliasKind {
    /// The alias is to a core function.
    Func,
    /// The alias is to a table.
    Table,
    /// The alias is to a module.
    Memory,
    /// The alias is to a global.
    Global,
    /// The alias is to a tag.
    Tag,
    /// The alias is to a core type.
    Type,
    /// The alias is to a module.
    Module,
    /// The alias is to a module instance.
    Instance,
}

impl<'a> Parse<'a> for CoreAliasKind {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        // Note: this intentionally does not parse
        // type, module, or instance; those are only
        // valid in `AliasKind` and they are parsed there
        // instead.
        if l.peek::<kw::func>() {
            parser.parse::<kw::func>()?;
            Ok(Self::Func)
        } else if l.peek::<kw::table>() {
            parser.parse::<kw::table>()?;
            Ok(Self::Table)
        } else if l.peek::<kw::memory>() {
            parser.parse::<kw::memory>()?;
            Ok(Self::Memory)
        } else if l.peek::<kw::global>() {
            parser.parse::<kw::global>()?;
            Ok(Self::Global)
        } else if l.peek::<kw::tag>() {
            parser.parse::<kw::tag>()?;
            Ok(Self::Tag)
        } else {
            Err(l.error())
        }
    }
}

/// An alias to a component item.
#[derive(Debug)]
pub struct Alias<'a> {
    /// Where this `alias` was defined.
    pub span: Span,
    /// An identifier that this alias is resolved with (optionally) for name
    /// resolution.
    pub id: Option<Id<'a>>,
    /// An optional name for this alias stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// The target of this alias.
    pub target: AliasTarget<'a>,
    /// The kind of item that's being aliased.
    pub kind: AliasKind,
}

impl<'a> Alias<'a> {
    /// Parses only an outer alias.
    pub fn parse_outer(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::alias>()?.0;
        let target = AliasTarget::parse_outer(parser)?;
        let (kind, id, name) =
            parser.parens(|parser| Ok((parser.parse()?, parser.parse()?, parser.parse()?)))?;

        Ok(Self {
            span,
            target,
            id,
            name,
            kind,
        })
    }
}

impl<'a> Parse<'a> for Alias<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::alias>()?.0;
        let target = parser.parse()?;
        let (kind, id, name) =
            parser.parens(|parser| Ok((parser.parse()?, parser.parse()?, parser.parse()?)))?;

        Ok(Self {
            span,
            target,
            id,
            name,
            kind,
        })
    }
}

/// The target of a component alias.
#[derive(Debug)]
pub enum AliasTarget<'a> {
    /// The alias is to an export of a component instance.
    Export {
        /// The component instance exporting the item.
        instance: Index<'a>,
        /// The name of the exported item to alias.
        name: &'a str,
    },
    /// The alias is to an item from an outer component.
    Outer {
        /// The number of enclosing components to skip.
        outer: Index<'a>,
        /// The index of the item being aliased.
        index: Index<'a>,
    },
}

impl<'a> AliasTarget<'a> {
    /// Parses only an outer alias target.
    pub fn parse_outer(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::outer>()?;
        Ok(Self::Outer {
            outer: parser.parse()?,
            index: parser.parse()?,
        })
    }
}

impl<'a> Parse<'a> for AliasTarget<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<kw::outer>() {
            Self::parse_outer(parser)
        } else {
            parser.parse::<kw::export>()?;
            Ok(Self::Export {
                instance: parser.parse()?,
                name: parser.parse()?,
            })
        }
    }
}

/// Represents the kind of item being aliased.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AliasKind {
    /// The item is a core item.
    Core(CoreAliasKind),
    /// The item is a function.
    Func,
    /// The item is a value.
    Value,
    /// The item is a type.
    Type,
    /// The item is a component.
    Component,
    /// The item is a component instance.
    Instance,
}

impl<'a> Parse<'a> for AliasKind {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::core>() {
            parser.parse::<kw::core>()?;
            if parser.peek::<kw::module>() {
                parser.parse::<kw::module>()?;
                Ok(Self::Core(CoreAliasKind::Module))
            } else if parser.peek::<kw::instance>() {
                parser.parse::<kw::instance>()?;
                Ok(Self::Core(CoreAliasKind::Instance))
            } else if parser.peek::<kw::r#type>() {
                parser.parse::<kw::r#type>()?;
                Ok(Self::Core(CoreAliasKind::Type))
            } else {
                Ok(Self::Core(parser.parse()?))
            }
        } else if l.peek::<kw::func>() {
            parser.parse::<kw::func>()?;
            Ok(Self::Func)
        } else if l.peek::<kw::value>() {
            parser.parse::<kw::value>()?;
            Ok(Self::Value)
        } else if l.peek::<kw::r#type>() {
            parser.parse::<kw::r#type>()?;
            Ok(Self::Type)
        } else if l.peek::<kw::component>() {
            parser.parse::<kw::component>()?;
            Ok(Self::Component)
        } else if l.peek::<kw::instance>() {
            parser.parse::<kw::instance>()?;
            Ok(Self::Instance)
        } else {
            Err(l.error())
        }
    }
}
