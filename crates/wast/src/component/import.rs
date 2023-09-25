use crate::component::*;
use crate::kw;
use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use crate::token::{Id, Index, LParen, NameAnnotation, Span};

/// An `import` statement and entry in a WebAssembly component.
#[derive(Debug)]
pub struct ComponentImport<'a> {
    /// Where this `import` was defined
    pub span: Span,
    /// The name of the item being imported.
    pub name: ComponentImportName<'a>,
    /// The item that's being imported.
    pub item: ItemSig<'a>,
}

///Info About where to find import
#[derive(Debug, Copy, Clone)]
pub struct ImportMetadata<'a> {
    /// Name of import
    pub name: &'a str,
    /// Url for import
    pub location: Option<&'a str>,
    /// Content Integrity Hash
    pub integrity: Option<&'a str>,
}

impl<'a> Parse<'a> for ComponentImport<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::import>()?.0;
        let name = parser.parse()?;
        match name {
            ComponentImportName::Kebab(_) | ComponentImportName::Interface(_) => {
                let item = parser.parens(|p| p.parse())?;
                return Ok(ComponentImport { span, name, item });
            }
            ComponentImportName::Url(..)
            | ComponentImportName::Relative(..)
            | ComponentImportName::Naked(..)
            | ComponentImportName::Locked(..)
            | ComponentImportName::Unlocked(_) => {
                let item = parser.parens(|p| p.parse())?;
                Ok(ComponentImport { span, name, item })
            }
        }
    }
}

/// The different ways an import can be named.
#[derive(Debug, Copy, Clone)]
pub enum ComponentImportName<'a> {
    /// This is a kebab-named import where a top-level name is assigned.
    Kebab(&'a str),
    /// This is an interface import where the string is an ID.
    Interface(&'a str),
    /// External Url
    Url(&'a str, &'a str, Option<&'a str>),
    /// Relative path
    Relative(&'a str, &'a str, Option<&'a str>),
    /// Just Integrity
    Naked(&'a str, &'a str),
    /// Locked Registry Import
    Locked(&'a str, &'a str),
    /// Unocked Registry Import
    Unlocked(&'a str),
}

/// The different ways an export can be named.
#[derive(Debug, Copy, Clone)]
pub enum ComponentExportName<'a> {
    /// This is a kebab-named import where a top-level name is assigned.
    Kebab(&'a str),
    /// This is an interface import where the string is an ID.
    Interface(&'a str),
}

/// Kinds of Implementation Imports
pub enum ComponentImportKinds {
    /// Url
    Url,
    /// Relative
    Relative,
    /// Naked
    Naked,
    /// Locked
    Locked,
    /// Unlocked
    Unlocked,
    /// Unknown
    Unknown,
}

impl Peek for ComponentImportName<'_> {
    fn peek(cursor: Cursor) -> Result<bool> {
        match cursor.keyword() {
            Ok(Some(("relative-url" | "url" | "locked-dep" | "unlocked-dep" | "integrity", _))) => {
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn display() -> &'static str {
        "implementation import"
    }
}

impl<'a> Parse<'a> for ComponentImportName<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<LParen>()? {
            if parser.peek2::<kw::interface>()? {
                return Ok(ComponentImportName::Interface(parser.parens(|p| {
                    p.parse::<kw::interface>()?;
                    p.parse()
                })?));
            } else if parser.peek2::<kw::locked_dep>()? {
                parser.parens(|p| {
                    p.parse::<kw::locked_dep>()?;
                    let name = p.parse()?;
                    p.parse::<kw::integrity>()?;
                    let integrity = p.parse()?;
                    return Ok(ComponentImportName::Locked(name, integrity));
                })
            } else if parser.peek2::<kw::unlocked_dep>()? {
                let name = parser.parens(|p| {
                    if p.peek::<kw::unlocked_dep>()? {
                        p.parse::<kw::unlocked_dep>()?;
                        let parsed_name = p.parse();
                        parsed_name
                    } else {
                        Err(p.error("Unknown Implementation Import"))
                    }
                })?;
                return Ok(ComponentImportName::Unlocked(name));
            } else {
                return Err(parser.error("Unknown Import Kind"));
            }
        } else {
            if parser.peek2::<LParen>()? {
                if parser.peek3::<ComponentImportName>()? {
                    let name = parser.parse()?;
                    return parser.parens(|p| {
                        if p.peek::<kw::url>()? {
                            p.parse::<kw::url>()?;
                            let location = p.parse()?;
                            let integrity = if p.peek::<kw::integrity>()? {
                                p.parse::<kw::integrity>()?;
                                Some(p.parse()?)
                            } else {
                                None
                            };
                            return Ok(ComponentImportName::Url(name, location, integrity));
                        } else if p.peek::<kw::relative_url>()? {
                            p.parse::<kw::relative_url>()?;
                            let location = p.parse()?;
                            let integrity = if p.peek::<kw::integrity>()? {
                                p.parse::<kw::integrity>()?;
                                Some(p.parse()?)
                            } else {
                                None
                            };
                            return Ok(ComponentImportName::Relative(name, location, integrity));
                        } else {
                            Err(p.error("Unknown Implementation Import"))
                        }
                    });
                }
            };
            let name = parser.parse()?;
            if parser.peek::<kw::integrity>()? {
                parser.parse::<kw::integrity>()?;
                let integrity = parser.parse()?;
                return Ok(ComponentImportName::Naked(name, integrity));
            } else {
                return Ok(ComponentImportName::Kebab(name));
            }
        }
    }
}

impl<'a> Parse<'a> for ComponentExportName<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<LParen>()? {
            if parser.peek2::<kw::interface>()? {
                return Ok(ComponentExportName::Interface(parser.parens(|p| {
                    p.parse::<kw::interface>()?;
                    p.parse()
                })?));
            } else {
                return Err(parser.error("Unknown Import Kind"));
            }
        } else {
            return Ok(ComponentExportName::Kebab(parser.parse()?));
        }
    }
}

/// An item signature for imported items.
#[derive(Debug)]
pub struct ItemSig<'a> {
    /// Where this item is defined in the source.
    pub span: Span,
    /// An optional identifier used during name resolution to refer to this item
    /// from the rest of the component.
    pub id: Option<Id<'a>>,
    /// An optional name which, for functions, will be stored in the
    /// custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// What kind of item this is.
    pub kind: ItemSigKind<'a>,
}

impl<'a> Parse<'a> for ItemSig<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parse_item_sig(parser, true)
    }
}

/// An item signature for imported items.
#[derive(Debug)]
pub struct ItemSigNoName<'a>(pub ItemSig<'a>);

impl<'a> Parse<'a> for ItemSigNoName<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ItemSigNoName(parse_item_sig(parser, false)?))
    }
}

fn parse_item_sig<'a>(parser: Parser<'a>, name: bool) -> Result<ItemSig<'a>> {
    let mut l = parser.lookahead1();
    let (span, parse_kind): (_, fn(Parser<'a>) -> Result<ItemSigKind>) = if l.peek::<kw::core>()? {
        let span = parser.parse::<kw::core>()?.0;
        parser.parse::<kw::module>()?;
        (span, |parser| Ok(ItemSigKind::CoreModule(parser.parse()?)))
    } else if l.peek::<kw::func>()? {
        let span = parser.parse::<kw::func>()?.0;
        (span, |parser| Ok(ItemSigKind::Func(parser.parse()?)))
    } else if l.peek::<kw::component>()? {
        let span = parser.parse::<kw::component>()?.0;
        (span, |parser| Ok(ItemSigKind::Component(parser.parse()?)))
    } else if l.peek::<kw::instance>()? {
        let span = parser.parse::<kw::instance>()?.0;
        (span, |parser| Ok(ItemSigKind::Instance(parser.parse()?)))
    } else if l.peek::<kw::value>()? {
        let span = parser.parse::<kw::value>()?.0;
        (span, |parser| Ok(ItemSigKind::Value(parser.parse()?)))
    } else if l.peek::<kw::r#type>()? {
        let span = parser.parse::<kw::r#type>()?.0;
        (span, |parser| {
            Ok(ItemSigKind::Type(parser.parens(|parser| parser.parse())?))
        })
    } else {
        return Err(l.error());
    };
    Ok(ItemSig {
        span,
        id: if name { parser.parse()? } else { None },
        name: if name { parser.parse()? } else { None },
        kind: parse_kind(parser)?,
    })
}

/// The kind of signatures for imported items.
#[derive(Debug)]
pub enum ItemSigKind<'a> {
    /// The item signature is for a core module.
    CoreModule(CoreTypeUse<'a, ModuleType<'a>>),
    /// The item signature is for a function.
    Func(ComponentTypeUse<'a, ComponentFunctionType<'a>>),
    /// The item signature is for a component.
    Component(ComponentTypeUse<'a, ComponentType<'a>>),
    /// The item signature is for an instance.
    Instance(ComponentTypeUse<'a, InstanceType<'a>>),
    /// The item signature is for a value.
    Value(ComponentValTypeUse<'a>),
    /// The item signature is for a type.
    Type(TypeBounds<'a>),
}

/// Represents the bounds applied to types being imported.
#[derive(Debug)]
pub enum TypeBounds<'a> {
    /// The equality type bounds.
    Eq(Index<'a>),
    /// A resource type is imported/exported,
    SubResource,
}

impl<'a> Parse<'a> for TypeBounds<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::eq>()? {
            parser.parse::<kw::eq>()?;
            Ok(Self::Eq(parser.parse()?))
        } else if l.peek::<kw::sub>()? {
            parser.parse::<kw::sub>()?;
            parser.parse::<kw::resource>()?;
            Ok(Self::SubResource)
        } else {
            Err(l.error())
        }
    }
}

/// A listing of a inline `(import "foo")` statement.
///
/// This is the same as `core::InlineImport` except only one string import is
/// required.
#[derive(Debug, Clone)]
pub struct InlineImport<'a> {
    /// The name of the item being imported.
    pub name: ComponentImportName<'a>,
}

impl<'a> Parse<'a> for InlineImport<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|p| {
            p.parse::<kw::import>()?;
            Ok(InlineImport { name: p.parse()? })
        })
    }
}

impl Peek for InlineImport<'_> {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        let cursor = match cursor.lparen()? {
            Some(cursor) => cursor,
            None => return Ok(false),
        };
        let cursor = match cursor.keyword()? {
            Some(("import", cursor)) => cursor,
            _ => return Ok(false),
        };

        // (import "foo")
        if let Some((_, cursor)) = cursor.string()? {
            return Ok(cursor.rparen()?.is_some());
        }

        // (import (interface "foo"))
        let cursor = match cursor.lparen()? {
            Some(cursor) => cursor,
            None => return Ok(false),
        };
        let cursor = match cursor.keyword()? {
            Some(("interface", cursor)) => cursor,
            _ => return Ok(false),
        };
        let cursor = match cursor.string()? {
            Some((_, cursor)) => cursor,
            _ => return Ok(false),
        };
        let cursor = match cursor.rparen()? {
            Some(cursor) => cursor,
            _ => return Ok(false),
        };
        Ok(cursor.rparen()?.is_some())
    }

    fn display() -> &'static str {
        "inline import"
    }
}
