use crate::kw;
use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use crate::token::Index;

/// Returns whether the text parser accepts the legacy syntax for component
/// index references that predates WebAssembly/component-model#655.
///
/// The component-model text format historically accepted references to core
/// items without a `core` prefix on the sort, such as `(realloc (func $f))`,
/// and additionally accepted an export name directly after some indices, such
/// as `(memory $i "mem")`. Upstream WebAssembly/component-model#655
/// regularized the grammar such that these must now be written
/// `(realloc (core func $f))` and `(memory (core memory $i "mem"))` for
/// example.
///
/// To smooth over this transition the legacy syntax is still accepted, by
/// default, in addition to the new syntax. Setting the environment variable
/// `WAST_STRICT_COMPONENT_INDICES` to `1` rejects the legacy syntax. The
/// default will be flipped to strict in the future, and eventually support
/// for the legacy syntax will be removed.
pub(crate) fn allow_legacy_indices() -> bool {
    const DEFAULT: bool = true;
    static ALLOW: std::sync::LazyLock<bool> = std::sync::LazyLock::new(|| {
        match std::env::var("WAST_STRICT_COMPONENT_INDICES").as_deref() {
            Ok("0") => true,
            Ok(_) => false,
            Err(_) => DEFAULT,
        }
    });
    *ALLOW
}

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
pub struct CoreItemRef<'a, K> {
    /// The item kind being parsed.
    pub kind: K,
    /// The item or instance reference.
    pub idx: Index<'a>,
    /// Export name to resolve the item from.
    pub export_name: Option<&'a str>,
}

impl<'a, K: Parse<'a>> Parse<'a> for CoreItemRef<'a, K> {
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

impl<'a, K: Peek> Peek for CoreItemRef<'a, K> {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        peek::<K>(cursor)
    }

    fn display() -> &'static str {
        "a core item reference"
    }
}

/// Parses component item references.
#[derive(Clone, Debug)]
pub struct ItemRef<'a, K> {
    /// The item kind being parsed.
    pub kind: K,
    /// The item or instance reference.
    pub idx: Index<'a>,
    /// Export names to resolve the item from.
    pub export_names: Vec<&'a str>,
}

impl<'a, K: Parse<'a>> Parse<'a> for ItemRef<'a, K> {
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

impl<'a, K: Peek> Peek for ItemRef<'a, K> {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        peek::<K>(cursor)
    }

    fn display() -> &'static str {
        "a component item reference"
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
        if parser.peek::<Index<'_>>()? {
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

/// Convenience structure to parse `core-prefix(<core:{X}idx>)` from the
/// component-model specification.
///
/// This parses either:
///
/// * a bare index: `$f` or `0`
/// * a parenthesized reference with a `core` prefix: `(core func $f)` or
///   `(core func $i "name")`
///
/// When the `LEGACY` type parameter is `true`, and legacy syntax is enabled
/// (see the `allow_legacy_indices`) this additionally accepts the legacy syntax
/// which omits the `core` prefix: `(func $f)` or `(func $i "name")`.
#[derive(Clone, Debug)]
pub struct CorePrefixedRef<'a, K, const LEGACY: bool>(pub CoreItemRef<'a, K>);

impl<'a, K, const LEGACY: bool> Parse<'a> for CorePrefixedRef<'a, K, LEGACY>
where
    K: Parse<'a> + Peek + Default,
{
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<Index<'_>>()? {
            return Ok(CorePrefixedRef(CoreItemRef {
                kind: K::default(),
                idx: parser.parse()?,
                export_name: None,
            }));
        }
        parser.parens(|parser| {
            if LEGACY && parser.peek::<K>()? {
                if !allow_legacy_indices() {
                    let name = K::display().trim_matches('`');
                    return Err(parser.error(format!(
                        "the `core` keyword is required in this reference: \
                         `({name} ...)` should be written `(core {name} ...)` \
                         (or set WAST_STRICT_COMPONENT_INDICES=0 to accept \
                         the legacy syntax)"
                    )));
                }
            } else {
                parser.parse::<kw::core>()?;
            }
            let item = parser.parse::<CoreItemRef<'a, K>>()?;
            Ok(CorePrefixedRef(item))
        })
    }
}

/// Parses the contents of a reference to a core item where the sort keyword
/// `kind` doubles as the enclosing s-expression's keyword and has already
/// been consumed by the caller, e.g. the `(memory ...)` canonical ABI option.
///
/// This parses either:
///
/// * a bare index: `$m` or `0`
/// * a nested parenthesized reference with a `core` prefix:
///   `(core memory $m)` or `(core memory $i "name")`
///
/// When legacy syntax is enabled (see `allow_legacy_indices`) this additionally
/// accepts the legacy syntax of an export name directly following the index:
/// `$i "name"`.
pub(crate) fn parse_core_prefixed_contents<'a, K>(
    parser: Parser<'a>,
    kind: K,
) -> Result<CoreItemRef<'a, K>>
where
    K: Parse<'a> + Peek + Default,
{
    if parser.peek::<Index<'_>>()? {
        let idx = parser.parse()?;
        let export_name = if parser.peek::<&str>()? {
            if !allow_legacy_indices() {
                let name = K::display().trim_matches('`');
                return Err(parser.error(format!(
                    "an export name must be written inside a nested \
                     reference: `({name} $i \"name\")` should be written \
                     `({name} (core {name} $i \"name\"))` \
                     (or set WAST_STRICT_COMPONENT_INDICES=0 to accept \
                     the legacy syntax)"
                )));
            }
            Some(parser.parse()?)
        } else {
            None
        };
        return Ok(CoreItemRef {
            kind,
            idx,
            export_name,
        });
    }
    parser.parens(|parser| {
        parser.parse::<kw::core>()?;
        parser.parse::<CoreItemRef<'a, K>>()
    })
}
