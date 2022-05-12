use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use crate::token::Index;

/// Parses `(func $foo)`
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct ItemRef<'a, K> {
    pub kind: K,
    pub idx: Index<'a>,
    pub export_names: Vec<&'a str>,
}

impl<'a, K: Parse<'a>> Parse<'a> for ItemRef<'a, K> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            let kind = parser.parse::<K>()?;
            let idx = parser.parse()?;
            let mut export_names = Vec::new();
            while !parser.is_empty() {
                export_names.push(parser.parse()?);
            }
            Ok(ItemRef {
                kind,
                idx,
                export_names,
            })
        })
    }
}

impl<'a, K: Peek> Peek for ItemRef<'a, K> {
    fn peek(cursor: Cursor<'_>) -> bool {
        match cursor.lparen() {
            Some(remaining) => K::peek(remaining),
            None => false,
        }
    }

    fn display() -> &'static str {
        "an item reference"
    }
}

/// Convenience structure to parse `$f` or `(item $f)`.
#[derive(Clone, Debug)]
pub struct IndexOrRef<'a, K>(pub ItemRef<'a, K>);

impl<'a, K> Parse<'a> for IndexOrRef<'a, K>
where
    K: Parse<'a> + Default,
{
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<Index<'_>>() {
            Ok(IndexOrRef(ItemRef {
                kind: K::default(),
                idx: parser.parse()?,
                export_names: Vec::new(),
            }))
        } else {
            Ok(IndexOrRef(parser.parse()?))
        }
    }
}

impl<'a, K: Peek> Peek for IndexOrRef<'a, K> {
    fn peek(cursor: Cursor<'_>) -> bool {
        Index::peek(cursor) || ItemRef::<K>::peek(cursor)
    }

    fn display() -> &'static str {
        "an item reference"
    }
}
