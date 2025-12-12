use crate::core::*;
use crate::kw;
use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use crate::token::{Id, LParen, NameAnnotation, Span};

/// An `import` statement and entry in a WebAssembly module.
#[derive(Debug, Clone)]
pub struct Imports<'a> {
    /// Where this `import` statement was defined.
    pub span: Span,
    /// All items inside the `import` statement.
    pub items: ImportItems<'a>,
}

/// The individual items inside an `import` statement, possibly in one of the
/// "compact" forms.
#[derive(Debug, Clone)]
pub enum ImportItems<'a> {
    /// A single import item:
    ///
    /// ```text
    /// (import "mod" "name" <type>)
    /// ```
    Single {
        /// The name of the module being importing from.
        module: &'a str,
        /// The name of the field in the module being imported from.
        name: &'a str,
        /// The type of the item being imported.
        sig: ItemSig<'a>,
    },

    /// A group of import items with a common module name:
    ///
    /// ```text
    /// (import "mod" (item "foo" <type>) (item "bar" <type>))
    /// ```
    Group1 {
        /// The name of the module being imported from.
        module: &'a str,
        /// The individual items being imported (name/type).
        items: Vec<ImportGroup1Item<'a>>,
    },

    /// A group of import items with a common module name and type:
    ///
    /// ```text
    /// (import "mod" (item "foo") (item "bar") <type>)
    /// ```
    Group2 {
        /// The name of the module being imported from.
        module: &'a str,
        /// The type of all the items being imported.
        sig: ItemSig<'a>,
        /// The individual items being imported (names only).
        items: Vec<ImportGroup2Item<'a>>,
    },
}

/// An item in a group of compact imports sharing a module name. Used with [`ImportItems::Group1`].
///
/// ```text
/// (item "foo" <type>)
/// ```
#[derive(Debug, Clone)]
pub struct ImportGroup1Item<'a> {
    /// Where this `item` was defined.
    pub span: Span,
    /// The name of the item being imported.
    pub name: &'a str,
    /// The type of the item being imported.
    pub sig: ItemSig<'a>,
}

/// An item in a group of compact imports sharing a module name and type. Used with [`ImportItems::Group2`].
#[derive(Debug, Clone)]
pub struct ImportGroup2Item<'a> {
    /// Where this `item` was defined.
    pub span: Span,
    /// The name of the item being imported.
    pub name: &'a str,
}

enum CompactImportEncoding {
    Unknown,
    Encoding1,
    Encoding2,
}

impl<'a> Imports<'a> {
    /// Constructs an Imports object for a single import item.
    pub fn single(span: Span, module: &'a str, name: &'a str, sig: ItemSig<'a>) -> Self {
        Self {
            span,
            items: ImportItems::Single { module, name, sig },
        }
    }

    /// Returns the number of import items defined in the group.
    pub fn num_items(&self) -> usize {
        match &self.items {
            ImportItems::Single {
                module: _,
                name: _,
                sig: _,
            } => 1,
            ImportItems::Group1 { module: _, items } => items.len(),
            ImportItems::Group2 {
                module: _,
                sig: _,
                items,
            } => items.len(),
        }
    }

    /// Returns the ItemSig for each defined import in the group. Items using
    /// compact encoding 2 will share an ItemSig.
    pub fn item_sigs(&self) -> Vec<&ItemSig<'a>> {
        let res = match &self.items {
            ImportItems::Single {
                module: _,
                name: _,
                sig,
            } => vec![sig],
            ImportItems::Group1 { module: _, items } => {
                items.iter().map(|item| &item.sig).collect()
            }
            ImportItems::Group2 {
                module: _,
                sig,
                items,
            } => vec![sig; items.len()],
        };
        debug_assert!(res.len() == self.num_items());
        res
    }

    /// Returns mutable references to each ItemSig defined in the group. This
    /// may be less than the number of imports defined in the group, if items
    /// share a sig.
    pub fn unique_sigs_mut(&mut self) -> Vec<&mut ItemSig<'a>> {
        match &mut self.items {
            ImportItems::Single {
                module: _,
                name: _,
                sig: item,
            } => vec![item],
            ImportItems::Group1 { module: _, items } => {
                items.iter_mut().map(|item| &mut item.sig).collect()
            }
            ImportItems::Group2 {
                module: _,
                sig,
                items: _,
            } => vec![sig],
        }
    }
}

struct ImportGroupItemCommon<'a> {
    span: Span,
    name: &'a str,
    sig: Option<ItemSig<'a>>,
}

impl<'a> Parse<'a> for Imports<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::import>()?.0;
        let module = parser.parse()?;
        if parser.peek::<LParen>()? {
            let mut encoding = CompactImportEncoding::Unknown;
            let mut items = Vec::new();
            while parser.peek2::<kw::item>()? {
                let item: ImportGroupItemCommon = parser.parens(|p| p.parse())?;
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
                    items: ImportItems::Group1 {
                        module,
                        items: items
                            .into_iter()
                            .map(|item| ImportGroup1Item {
                                span: item.span,
                                name: item.name,
                                sig: item.sig.unwrap(),
                            })
                            .collect(),
                    },
                }),
                CompactImportEncoding::Encoding2 => {
                    let sig: ItemSig = parser.parens(|p| p.parse())?;
                    if let Some(id) = sig.id {
                        return Err(parser.error_at(id.span(), "identifier not allowed"));
                    }
                    Ok(Imports {
                        span,
                        items: ImportItems::Group2 {
                            module,
                            sig,
                            items: items
                                .into_iter()
                                .map(|item| ImportGroup2Item {
                                    span: item.span,
                                    name: item.name,
                                })
                                .collect(),
                        },
                    })
                }
            }
        } else {
            // Single item
            let field = parser.parse()?;
            let sig = parser.parens(|p| p.parse())?;
            Ok(Imports::single(span, module, field, sig))
        }
    }
}

impl<'a> Parse<'a> for ImportGroupItemCommon<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::item>()?.0;
        Ok(ImportGroupItemCommon {
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
