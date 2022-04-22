use crate::ast::{self, kw};
use crate::parser::{Cursor, Parse, Parser, Peek, Result};

/// A entry in a WebAssembly module's export section.
#[derive(Debug)]
pub struct Export<'a> {
    /// Where this export was defined.
    pub span: ast::Span,
    /// The name of this export from the module.
    pub name: &'a str,
    /// What's being exported from the module.
    pub index: ast::ItemRef<'a, ExportKind>,
}

/// A entry in a WebAssembly component's export section.
///
/// export       ::= (export <name> <componentarg>)
#[derive(Debug)]
pub struct ComponentExport<'a> {
    /// Where this export was defined.
    pub span: ast::Span,
    /// The name of this export from the component.
    pub name: &'a str,
    /// What's being exported from the component.
    pub arg: ast::ComponentTypeUse<'a, ast::ComponentArg<'a>>,
}

/// Different kinds of elements that can be exported from a WebAssembly module,
/// contained in an [`Export`].
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum ExportKind {
    Func,
    Table,
    Memory,
    Global,
    Tag,
    Type,
}

impl<'a> Parse<'a> for Export<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::export>()?.0;
        let name = parser.parse()?;
        let index = parser.parse()?;
        Ok(Export { span, name, index })
    }
}

impl<'a> Parse<'a> for ComponentExport<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::export>()?.0;
        let name = parser.parse()?;
        let arg = parser.parse()?;
        Ok(ComponentExport { span, name, arg })
    }
}

impl<'a> Parse<'a> for ExportKind {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::func>() {
            parser.parse::<kw::func>()?;
            Ok(ExportKind::Func)
        } else if l.peek::<kw::table>() {
            parser.parse::<kw::table>()?;
            Ok(ExportKind::Table)
        } else if l.peek::<kw::memory>() {
            parser.parse::<kw::memory>()?;
            Ok(ExportKind::Memory)
        } else if l.peek::<kw::global>() {
            parser.parse::<kw::global>()?;
            Ok(ExportKind::Global)
        } else if l.peek::<kw::tag>() {
            parser.parse::<kw::tag>()?;
            Ok(ExportKind::Tag)
        } else if l.peek::<kw::r#type>() {
            parser.parse::<kw::r#type>()?;
            Ok(ExportKind::Type)
        } else {
            Err(l.error())
        }
    }
}

impl Peek for ExportKind {
    fn peek(cursor: Cursor<'_>) -> bool {
        kw::func::peek(cursor)
            || kw::table::peek(cursor)
            || kw::memory::peek(cursor)
            || kw::global::peek(cursor)
            || kw::tag::peek(cursor)
            || kw::r#type::peek(cursor)
    }
    fn display() -> &'static str {
        "export kind"
    }
}

macro_rules! kw_defaults {
    ($($kw:ident)*) => ($(
        impl Default for kw::$kw {
            fn default() -> kw::$kw {
                kw::$kw(ast::Span::from_offset(0))
            }
        }
    )*);
}

kw_defaults! {
    instance
    module
    component
    func
    table
    global
    tag
    memory
    r#type
}

macro_rules! kw_conversions {
    ($($kw:ident => $kind:ident)*) => ($(
        impl From<kw::$kw> for ExportKind {
            fn from(_: kw::$kw) -> ExportKind {
                ExportKind::$kind
            }
        }
    )*);
}

kw_conversions! {
    func => Func
    table => Table
    global => Global
    tag => Tag
    memory => Memory
    r#type => Type
}

/// A listing of inline `(export "foo")` statements on a WebAssembly item in
/// its textual format.
#[derive(Debug)]
pub struct InlineExport<'a> {
    /// The extra names to export an item as, if any.
    pub names: Vec<&'a str>,
}

impl<'a> Parse<'a> for InlineExport<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut names = Vec::new();
        while parser.peek::<Self>() {
            names.push(parser.parens(|p| {
                p.parse::<kw::export>()?;
                p.parse::<&str>()
            })?);
        }
        Ok(InlineExport { names })
    }
}

impl Peek for InlineExport<'_> {
    fn peek(cursor: Cursor<'_>) -> bool {
        let cursor = match cursor.lparen() {
            Some(cursor) => cursor,
            None => return false,
        };
        let cursor = match cursor.keyword() {
            Some(("export", cursor)) => cursor,
            _ => return false,
        };
        let cursor = match cursor.string() {
            Some((_, cursor)) => cursor,
            None => return false,
        };
        cursor.rparen().is_some()
    }

    fn display() -> &'static str {
        "inline export"
    }
}
