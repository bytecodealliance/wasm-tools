use crate::core::*;
use crate::kw;
use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use crate::token::{Id, LParen, NameAnnotation, Span};

/// An `import` statement and entry in a WebAssembly module.
#[derive(Debug, Clone)]
pub struct Imports<'a> {
    /// Where this `import` statement was defined
    pub span: Span,
    /// All items inside the `import` statement
    pub items: ImportItems<'a>,
}

#[derive(Debug, Clone)]
pub enum ImportItems<'a> {
    Single(Import<'a>),
    Group1 {
        module: &'a str,
        items: Vec<ImportGroupItem<'a>>,
    },
    Group2 {
        module: &'a str,
        sig: ItemSig<'a>,
        items: Vec<&'a str>,
    },
}

#[derive(Debug, Clone)]
pub struct ImportGroupItem<'a> {
    /// Where this `item` was defined
    pub span: Span,
    pub name: &'a str,
    pub sig: Option<ItemSig<'a>>,
}

/// A single fully-qualified import. May not correspond to a single (import)
/// statement.
#[derive(Debug, Clone)]
pub struct Import<'a> {
    /// Where this `import` was defined
    pub span: Span,
    /// The module that this statement is importing from
    pub module: &'a str,
    /// The name of the field in the module this statement imports from.
    pub field: &'a str,
    /// The item that's being imported.
    pub item: ItemSig<'a>,
}

enum CompactImportEncoding {
    Unknown,
    Encoding1,
    Encoding2,
}

impl<'a> Parse<'a> for Imports<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::import>()?.0;
        let module = parser.parse()?;
        if parser.peek::<LParen>()? {
            let mut encoding = CompactImportEncoding::Unknown;
            let mut items = Vec::new();
            while parser.peek2::<kw::item>()? {
                let item: ImportGroupItem = parser.parens(|p| p.parse())?;
                match item.sig {
                    Some(_) => {
                        // Compact encoding 1 (name / type pairs)
                        match encoding {
                            CompactImportEncoding::Unknown => {
                                encoding = CompactImportEncoding::Encoding1
                            }
                            CompactImportEncoding::Encoding1 => {}
                            CompactImportEncoding::Encoding2 => {
                                return Err(parser.error("unexpected import type"));
                            }
                        }
                    }
                    None => {
                        // Compact encoding 2 (names only)
                        match encoding {
                            CompactImportEncoding::Unknown => {
                                encoding = CompactImportEncoding::Encoding2
                            }
                            CompactImportEncoding::Encoding1 => {
                                return Err(parser.error("unexpected `)`"));
                            }
                            CompactImportEncoding::Encoding2 => {}
                        }
                    }
                }
                items.push(item);
            }

            match encoding {
                CompactImportEncoding::Unknown => Err(parser.error("expected import items")),
                CompactImportEncoding::Encoding1 => Ok(Imports {
                    span,
                    items: ImportItems::Group1 { module, items },
                }),
                CompactImportEncoding::Encoding2 => {
                    let sig: ItemSig = parser.parens(|p| p.parse())?;
                    let names = items
                        .iter()
                        .map(|item| {
                            if item.sig.is_some() {
                                unreachable!();
                            }
                            item.name
                        })
                        .collect();
                    Ok(Imports {
                        span,
                        items: ImportItems::Group2 {
                            module,
                            sig,
                            items: names,
                        },
                    })
                }
            }
        } else {
            // Single item
            let field = parser.parse()?;
            let item = parser.parens(|p| p.parse())?;
            Ok(Imports {
                span,
                items: ImportItems::Single(Import {
                    span,
                    module,
                    field,
                    item,
                }),
            })
        }
    }
}

impl<'a> Parse<'a> for ImportGroupItem<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::item>()?.0;
        Ok(ImportGroupItem {
            span,
            name: parser.parse()?,
            sig: if parser.is_empty() {
                None
            } else {
                Some(parser.parens(|p| p.parse())?)
            },
        })
    }
}

impl<'a> IntoIterator for &'a Imports<'a> {
    type Item = &'a Import<'a>;
    type IntoIter = Box<dyn Iterator<Item = Self::Item> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        match &self.items {
            ImportItems::Single(import) => Box::new(std::iter::once(import)),
            ImportItems::Group1 { module, items } => todo!(),
            ImportItems::Group2 { module, sig, items } => todo!(),
        }
    }
}

impl<'a> IntoIterator for &'a mut Imports<'a> {
    type Item = &'a mut Import<'a>;
    type IntoIter = Box<dyn Iterator<Item = Self::Item> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        match &mut self.items {
            ImportItems::Single(import) => Box::new(std::iter::once(import)),
            ImportItems::Group1 { module, items } => todo!(),
            ImportItems::Group2 { module, sig, items } => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ItemSig<'a> {
    /// Where this item is defined in the source.
    pub span: Span,
    /// An optional identifier used during name resolution to refer to this item
    /// from the rest of the module.
    pub id: Option<Id<'a>>,
    /// An optional name which, for functions, will be stored in the
    /// custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// What kind of item this is.
    pub kind: ItemKind<'a>,
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum ItemKind<'a> {
    Func(TypeUse<'a, FunctionType<'a>>),
    Table(TableType<'a>),
    Memory(MemoryType),
    Global(GlobalType<'a>),
    Tag(TagType<'a>),
    FuncExact(TypeUse<'a, FunctionType<'a>>),
}

impl<'a> Parse<'a> for ItemSig<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::func>()? {
            let span = parser.parse::<kw::func>()?.0;
            let id = parser.parse()?;
            let name = parser.parse()?;
            let kind = if parser.peek2::<kw::exact>()? {
                ItemKind::FuncExact(parser.parens(|p| {
                    p.parse::<kw::exact>()?;
                    p.parse()
                })?)
            } else {
                ItemKind::Func(parser.parse()?)
            };
            Ok(ItemSig {
                span,
                id,
                name,
                kind,
            })
        } else if l.peek::<kw::table>()? {
            let span = parser.parse::<kw::table>()?.0;
            Ok(ItemSig {
                span,
                id: parser.parse()?,
                name: None,
                kind: ItemKind::Table(parser.parse()?),
            })
        } else if l.peek::<kw::memory>()? {
            let span = parser.parse::<kw::memory>()?.0;
            Ok(ItemSig {
                span,
                id: parser.parse()?,
                name: None,
                kind: ItemKind::Memory(parser.parse()?),
            })
        } else if l.peek::<kw::global>()? {
            let span = parser.parse::<kw::global>()?.0;
            Ok(ItemSig {
                span,
                id: parser.parse()?,
                name: None,
                kind: ItemKind::Global(parser.parse()?),
            })
        } else if l.peek::<kw::tag>()? {
            let span = parser.parse::<kw::tag>()?.0;
            Ok(ItemSig {
                span,
                id: parser.parse()?,
                name: None,
                kind: ItemKind::Tag(parser.parse()?),
            })
        } else {
            Err(l.error())
        }
    }
}

/// A listing of a inline `(import "foo")` statement.
///
/// Note that when parsing this type it is somewhat unconventional that it
/// parses its own surrounding parentheses. This is typically an optional type,
/// so it's so far been a bit nicer to have the optionality handled through
/// `Peek` rather than `Option<T>`.
#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub struct InlineImport<'a> {
    pub module: &'a str,
    pub field: &'a str,
}

impl<'a> Parse<'a> for InlineImport<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|p| {
            p.parse::<kw::import>()?;
            Ok(InlineImport {
                module: p.parse()?,
                field: p.parse()?,
            })
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
        let cursor = match cursor.string()? {
            Some((_, cursor)) => cursor,
            None => return Ok(false),
        };
        let cursor = match cursor.string()? {
            Some((_, cursor)) => cursor,
            None => return Ok(false),
        };

        Ok(cursor.rparen()?.is_some())
    }

    fn display() -> &'static str {
        "inline import"
    }
}
