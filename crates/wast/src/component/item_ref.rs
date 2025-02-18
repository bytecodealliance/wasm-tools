use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use crate::token::{Index, Span};

fn peek<K: Peek>(cursor: Cursor) -> Result<bool> {
    // This is a little fancy because when parsing something like:
    //
    //      (type (component (type $foo)))
    //
    // we need to disambiguate that from
    //
    //      (type (component (type $foo (func))))
    //
    // where the first is a type reference and the second is an inline
    // component type defining a type internally. The peek here not only
    // peeks for `K` but also for the index and possibly trailing
    // strings.

    // Peek for the given keyword type
    if !K::peek(cursor)? {
        return Ok(false);
    }

    // Move past the given keyword
    let cursor = match cursor.keyword()? {
        Some((_, c)) => c,
        _ => return Ok(false),
    };

    // Peek an id or integer index, followed by `)` or string to disambiguate
    let cursor = match cursor.id()? {
        Some((_, cursor)) => Some(cursor),
        None => cursor.integer()?.map(|p| p.1),
    };
    Ok(match cursor {
        Some(cursor) => cursor.rparen()?.is_some() || cursor.string()?.is_some(),
        None => false,
    })
}

/// Parses core item references.
#[derive(Clone, Debug)]
pub struct CoreItemRef<'a, K, I> {
    /// The item kind being parsed.
    pub kind: K,
    /// The item or instance reference.
    pub idx: Index<'a, I>,
    /// Export name to resolve the item from.
    pub export_name: Option<&'a str>,
}

impl<'a, K: Parse<'a>, I> Parse<'a> for CoreItemRef<'a, K, I>
where
    (I, Span): Parse<'a>,
{
    fn parse(parser: Parser<'a>) -> Result<Self> {
        // This does not parse the surrounding `(` and `)` because
        // core prefix is context dependent and only the caller knows if it should be
        // present for core references; therefore, the caller parses the parens and any core prefix
        let kind = parser.parse::<K>()?;
        let idx = parser.parse()?;
        let export_name = parser.parse()?;
        Ok(Self {
            kind,
            idx,
            export_name,
        })
    }
}

impl<'a, K: Peek, I> Peek for CoreItemRef<'a, K, I> {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        peek::<K>(cursor)
    }

    fn display() -> &'static str {
        "a core item reference"
    }
}

/// Parses component item references.
#[derive(Clone, Debug)]
pub struct ItemRef<'a, K, I> {
    /// The item kind being parsed.
    pub kind: K,
    /// The item or instance reference.
    pub idx: Index<'a, I>,
    /// Export names to resolve the item from.
    pub export_names: Vec<&'a str>,
}

impl<'a, K: Parse<'a>, I> Parse<'a> for ItemRef<'a, K, I>
where
    (I, Span): Parse<'a>,
{
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let kind = parser.parse::<K>()?;
        let idx = parser.parse()?;
        let mut export_names = Vec::new();
        while !parser.is_empty() {
            export_names.push(parser.parse()?);
        }
        Ok(Self {
            kind,
            idx,
            export_names,
        })
    }
}

impl<'a, K: Peek, I> Peek for ItemRef<'a, K, I> {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        peek::<K>(cursor)
    }

    fn display() -> &'static str {
        "a component item reference"
    }
}

/// Convenience structure to parse `$f` or `(item $f)`.
#[derive(Clone, Debug)]
pub struct IndexOrRef<'a, K, I>(pub ItemRef<'a, K, I>);

impl<'a, K, I> Parse<'a> for IndexOrRef<'a, K, I>
where
    K: Parse<'a> + Default,
    (I, Span): Parse<'a>,
{
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<Index<'_, I>>()? {
            Ok(IndexOrRef(ItemRef {
                kind: K::default(),
                idx: parser.parse()?,
                export_names: Vec::new(),
            }))
        } else {
            Ok(IndexOrRef(parser.parens(|p| p.parse())?))
        }
    }
}

/// Convenience structure to parse `$f` or `(item $f)`.
#[derive(Clone, Debug)]
pub struct IndexOrCoreRef<'a, K, I>(pub CoreItemRef<'a, K, I>);

impl<'a, K, I> Parse<'a> for IndexOrCoreRef<'a, K, I>
where
    K: Parse<'a> + Default,
    (I, Span): Parse<'a>,
{
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<Index<'_, I>>()? {
            Ok(IndexOrCoreRef(CoreItemRef {
                kind: K::default(),
                idx: parser.parse()?,
                export_name: None,
            }))
        } else {
            Ok(IndexOrCoreRef(parser.parens(|p| p.parse())?))
        }
    }
}
