//! Definitions of name-related helpers and newtypes, primarily for the
//! component model.

use crate::Result;
use semver::Version;
use std::borrow::Borrow;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

/// Represents a kebab string slice used in validation.
///
/// This is a wrapper around `str` that ensures the slice is
/// a valid kebab case string according to the component model
/// specification.
///
/// It also provides an equality and hashing implementation
/// that ignores ASCII case.
#[derive(Debug, Eq)]
#[repr(transparent)]
pub struct KebabStr(str);

impl KebabStr {
    /// Creates a new kebab string slice.
    ///
    /// Returns `None` if the given string is not a valid kebab string.
    pub fn new<'a>(s: impl AsRef<str> + 'a) -> Option<&'a Self> {
        let s = Self::new_unchecked(s);
        if s.is_kebab_case() {
            Some(s)
        } else {
            None
        }
    }

    pub(crate) fn new_unchecked<'a>(s: impl AsRef<str> + 'a) -> &'a Self {
        // Safety: `KebabStr` is a transparent wrapper around `str`
        // Therefore transmuting `&str` to `&KebabStr` is safe.
        unsafe { std::mem::transmute::<_, &Self>(s.as_ref()) }
    }

    /// Gets the underlying string slice.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Converts the slice to an owned string.
    pub fn to_kebab_string(&self) -> KebabString {
        KebabString(self.to_string())
    }

    fn is_kebab_case(&self) -> bool {
        let mut lower = false;
        let mut upper = false;
        for c in self.chars() {
            match c {
                'a'..='z' if !lower && !upper => lower = true,
                'A'..='Z' if !lower && !upper => upper = true,
                'a'..='z' if lower => {}
                'A'..='Z' if upper => {}
                '0'..='9' if lower || upper => {}
                '-' if lower || upper => {
                    lower = false;
                    upper = false;
                }
                _ => return false,
            }
        }

        !self.is_empty() && !self.ends_with('-')
    }
}

impl Deref for KebabStr {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl PartialEq for KebabStr {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        self.chars()
            .zip(other.chars())
            .all(|(a, b)| a.to_ascii_lowercase() == b.to_ascii_lowercase())
    }
}

impl PartialEq<KebabString> for KebabStr {
    fn eq(&self, other: &KebabString) -> bool {
        self.eq(other.as_kebab_str())
    }
}

impl Hash for KebabStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.len().hash(state);

        for b in self.chars() {
            b.to_ascii_lowercase().hash(state);
        }
    }
}

impl fmt::Display for KebabStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self as &str).fmt(f)
    }
}

impl ToOwned for KebabStr {
    type Owned = KebabString;

    fn to_owned(&self) -> Self::Owned {
        self.to_kebab_string()
    }
}

/// Represents an owned kebab string for validation.
///
/// This is a wrapper around `String` that ensures the string is
/// a valid kebab case string according to the component model
/// specification.
///
/// It also provides an equality and hashing implementation
/// that ignores ASCII case.
#[derive(Debug, Clone, Eq)]
pub struct KebabString(String);

impl KebabString {
    /// Creates a new kebab string.
    ///
    /// Returns `None` if the given string is not a valid kebab string.
    pub fn new(s: impl Into<String>) -> Option<Self> {
        let s = s.into();
        if KebabStr::new(&s).is_some() {
            Some(Self(s))
        } else {
            None
        }
    }

    /// Gets the underlying string.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Converts the kebab string to a kebab string slice.
    pub fn as_kebab_str(&self) -> &KebabStr {
        // Safety: internal string is always valid kebab-case
        KebabStr::new_unchecked(self.as_str())
    }
}

impl Deref for KebabString {
    type Target = KebabStr;

    fn deref(&self) -> &Self::Target {
        self.as_kebab_str()
    }
}

impl Borrow<KebabStr> for KebabString {
    fn borrow(&self) -> &KebabStr {
        self.as_kebab_str()
    }
}

impl PartialEq for KebabString {
    fn eq(&self, other: &Self) -> bool {
        self.as_kebab_str().eq(other.as_kebab_str())
    }
}

impl PartialEq<KebabStr> for KebabString {
    fn eq(&self, other: &KebabStr) -> bool {
        self.as_kebab_str().eq(other)
    }
}

impl Hash for KebabString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_kebab_str().hash(state)
    }
}

impl fmt::Display for KebabString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_kebab_str().fmt(f)
    }
}

impl From<KebabString> for String {
    fn from(s: KebabString) -> String {
        s.0
    }
}

/// An import or export name in the component model which is backed by `T`,
/// which defaults to `String`.
///
/// This name can be either:
///
/// * a plain label or "kebab string": `a-b-c`
/// * a plain method name : `[method]a-b.c-d`
/// * a plain static method name : `[static]a-b.c-d`
/// * a plain constructor: `[constructor]a-b`
/// * an interface name: `wasi:cli/reactor@0.1.0`
/// * a dependency name: `locked-dep=foo:bar/baz`
/// * a URL name: `url=https://..`
/// * a hash name: `integrity=sha256:...`
///
/// # Equality and hashing
///
/// Note that this type the `[method]...` and `[static]...` variants are
/// considered equal and hash to the same value. This enables disallowing
/// clashes between the two where method name overlap cannot happen.
#[derive(Clone)]
pub struct ComponentName {
    raw: String,
    kind: ParsedComponentNameKind,
}

#[derive(Copy, Clone)]
enum ParsedComponentNameKind {
    Label,
    Constructor,
    Method,
    Static,
    Interface,
}

/// Created via [`ComponentName::kind`] and classifies a name.
#[derive(Debug, Clone)]
pub enum ComponentNameKind<'a> {
    /// `a-b-c`
    Label(&'a KebabStr),
    /// `[constructor]a-b`
    Constructor(&'a KebabStr),
    /// `[method]a-b.c-d`
    #[allow(missing_docs)]
    Method(ResourceFunc<'a>),
    /// `[static]a-b.c-d`
    #[allow(missing_docs)]
    Static(ResourceFunc<'a>),
    /// `wasi:http/types@2.0`
    #[allow(missing_docs)]
    Interface(InterfaceName<'a>),
}

const CONSTRUCTOR: &str = "[constructor]";
const METHOD: &str = "[method]";
const STATIC: &str = "[static]";

impl ComponentName {
    /// Attempts to parse `name` as a kebab name, returning `None` if it's not
    /// valid.
    pub fn new(name: &str, offset: usize) -> Result<ComponentName> {
        let kind = ComponentName::parse_kind(name, offset)?;
        Ok(ComponentName {
            raw: name.to_string(),
            kind,
        })
    }

    fn parse_kind(name: &str, offset: usize) -> Result<ParsedComponentNameKind> {
        let validate_kebab = |s: &str| {
            if KebabStr::new(s).is_none() {
                bail!(offset, "`{s}` is not in kebab case")
            } else {
                Ok(())
            }
        };
        let find = |s: &str, c: char| match s.find(c) {
            Some(i) => Ok(i),
            None => bail!(offset, "failed to find `{c}` character"),
        };

        if let Some(name) = name.strip_prefix(CONSTRUCTOR) {
            validate_kebab(name)?;
            return Ok(ParsedComponentNameKind::Constructor);
        }
        if let Some(s) = name.strip_prefix(METHOD) {
            let dot = find(s, '.')?;
            validate_kebab(&s[..dot])?;
            validate_kebab(&s[dot + 1..])?;
            return Ok(ParsedComponentNameKind::Method);
        }
        if let Some(s) = name.strip_prefix(STATIC) {
            let dot = find(s, '.')?;
            validate_kebab(&s[..dot])?;
            validate_kebab(&s[dot + 1..])?;
            return Ok(ParsedComponentNameKind::Static);
        }

        match name.find(':') {
            Some(colon) => {
                validate_kebab(&name[..colon])?;
                let slash = find(name, '/')?;
                let at = name[slash..].find('@').map(|i| i + slash);
                validate_kebab(&name[colon + 1..slash])?;
                validate_kebab(&name[slash + 1..at.unwrap_or(name.len())])?;
                if let Some(at) = at {
                    let version = &name[at + 1..];
                    if let Err(e) = version.parse::<Version>() {
                        bail!(offset, "failed to parse version: {e}")
                    }
                }
                Ok(ParsedComponentNameKind::Interface)
            }
            None => {
                validate_kebab(name)?;
                Ok(ParsedComponentNameKind::Label)
            }
        }
    }

    /// Returns the [`ComponentNameKind`] corresponding to this name.
    pub fn kind(&self) -> ComponentNameKind<'_> {
        use ComponentNameKind::*;
        use ParsedComponentNameKind as PK;
        match self.kind {
            PK::Label => Label(KebabStr::new_unchecked(&self.raw)),
            PK::Constructor => Constructor(KebabStr::new_unchecked(&self.raw[CONSTRUCTOR.len()..])),
            PK::Method => Method(ResourceFunc(&self.raw[METHOD.len()..])),
            PK::Static => Static(ResourceFunc(&self.raw[STATIC.len()..])),
            PK::Interface => Interface(InterfaceName(&self.raw)),
        }
    }

    /// Returns the raw underlying name as a string.
    pub fn as_str(&self) -> &str {
        &self.raw
    }
}

impl From<ComponentName> for String {
    fn from(name: ComponentName) -> String {
        name.raw
    }
}

impl Hash for ComponentName {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.kind().hash(hasher)
    }
}

impl PartialEq for ComponentName {
    fn eq(&self, other: &ComponentName) -> bool {
        self.kind().eq(&other.kind())
    }
}

impl Eq for ComponentName {}

impl fmt::Display for ComponentName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.raw.fmt(f)
    }
}

impl fmt::Debug for ComponentName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.raw.fmt(f)
    }
}

impl Hash for ComponentNameKind<'_> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        use ComponentNameKind::*;
        match self {
            Label(name) => (0u8, name).hash(hasher),
            Constructor(name) => (1u8, name).hash(hasher),
            // for hashing method == static
            Method(name) | Static(name) => (2u8, name).hash(hasher),
            Interface(name) => (3u8, name).hash(hasher),
        }
    }
}

impl PartialEq for ComponentNameKind<'_> {
    fn eq(&self, other: &ComponentNameKind<'_>) -> bool {
        use ComponentNameKind::*;
        match (self, other) {
            (Label(a), Label(b)) => a == b,
            (Label(_), _) => false,
            (Constructor(a), Constructor(b)) => a == b,
            (Constructor(_), _) => false,

            // method == static for the purposes of hashing so equate them here
            // as well.
            (Method(a), Method(b))
            | (Static(a), Static(b))
            | (Method(a), Static(b))
            | (Static(a), Method(b)) => a == b,

            (Method(_), _) => false,
            (Static(_), _) => false,

            (Interface(a), Interface(b)) => a == b,
            (Interface(_), _) => false,
        }
    }
}

impl Eq for ComponentNameKind<'_> {}

/// TODO
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ResourceFunc<'a>(&'a str);

impl<'a> ResourceFunc<'a> {
    /// TODO
    pub fn resource(&self) -> &'a KebabStr {
        let dot = self.0.find('.').unwrap();
        KebabStr::new_unchecked(&self.0[..dot])
    }
}

/// TODO
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct InterfaceName<'a>(&'a str);

impl<'a> InterfaceName<'a> {
    /// TODO
    pub fn as_str(&self) -> &'a str {
        self.0
    }

    /// TODO
    pub fn namespace(&self) -> &'a KebabStr {
        let colon = self.0.find(':').unwrap();
        KebabStr::new_unchecked(&self.0[..colon])
    }

    /// TODO
    pub fn package(&self) -> &'a KebabStr {
        let colon = self.0.find(':').unwrap();
        let slash = self.0.find('/').unwrap();
        KebabStr::new_unchecked(&self.0[colon + 1..slash])
    }

    /// TODO
    pub fn interface(&self) -> &'a KebabStr {
        let slash = self.0.find('/').unwrap();
        let at = self.0.find('@').unwrap_or(self.0.len());
        KebabStr::new_unchecked(&self.0[slash + 1..at])
    }

    /// TODO
    pub fn version(&self) -> Option<Version> {
        let at = self.0.find('@')?;
        Some(Version::parse(&self.0[at + 1..]).unwrap())
    }
}

/// TODO
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct DependencyName<'a>(&'a str);

/// TODO
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct HashName<'a>(&'a str);

/// TODO
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct UrlName<'a>(&'a str);

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn parse_kebab_name(s: &str) -> Option<ComponentName> {
        ComponentName::new(s, 0).ok()
    }

    #[test]
    fn kebab_smoke() {
        assert!(KebabStr::new("").is_none());
        assert!(KebabStr::new("a").is_some());
        assert!(KebabStr::new("aB").is_none());
        assert!(KebabStr::new("a-B").is_some());
        assert!(KebabStr::new("a-").is_none());
        assert!(KebabStr::new("-").is_none());
        assert!(KebabStr::new("¶").is_none());
        assert!(KebabStr::new("0").is_none());
        assert!(KebabStr::new("a0").is_some());
        assert!(KebabStr::new("a-0").is_none());
    }

    #[test]
    fn name_smoke() {
        assert!(parse_kebab_name("a").is_some());
        assert!(parse_kebab_name("[foo]a").is_none());
        assert!(parse_kebab_name("[constructor]a").is_some());
        assert!(parse_kebab_name("[method]a").is_none());
        assert!(parse_kebab_name("[method]a.b").is_some());
        assert!(parse_kebab_name("[method]a.b.c").is_none());
        assert!(parse_kebab_name("[static]a.b").is_some());
        assert!(parse_kebab_name("[static]a").is_none());
    }

    #[test]
    fn name_equality() {
        assert_eq!(parse_kebab_name("a"), parse_kebab_name("a"));
        assert_ne!(parse_kebab_name("a"), parse_kebab_name("b"));
        assert_eq!(
            parse_kebab_name("[constructor]a"),
            parse_kebab_name("[constructor]a")
        );
        assert_ne!(
            parse_kebab_name("[constructor]a"),
            parse_kebab_name("[constructor]b")
        );
        assert_eq!(
            parse_kebab_name("[method]a.b"),
            parse_kebab_name("[method]a.b")
        );
        assert_ne!(
            parse_kebab_name("[method]a.b"),
            parse_kebab_name("[method]b.b")
        );
        assert_eq!(
            parse_kebab_name("[static]a.b"),
            parse_kebab_name("[static]a.b")
        );
        assert_ne!(
            parse_kebab_name("[static]a.b"),
            parse_kebab_name("[static]b.b")
        );

        assert_eq!(
            parse_kebab_name("[static]a.b"),
            parse_kebab_name("[method]a.b")
        );
        assert_eq!(
            parse_kebab_name("[method]a.b"),
            parse_kebab_name("[static]a.b")
        );

        assert_ne!(
            parse_kebab_name("[method]b.b"),
            parse_kebab_name("[static]a.b")
        );

        let mut s = HashSet::new();
        assert!(s.insert(parse_kebab_name("a")));
        assert!(s.insert(parse_kebab_name("[constructor]a")));
        assert!(s.insert(parse_kebab_name("[method]a.b")));
        assert!(!s.insert(parse_kebab_name("[static]a.b")));
        assert!(s.insert(parse_kebab_name("[static]b.b")));
    }
}
