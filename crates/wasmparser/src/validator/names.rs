//! Definitions of name-related helpers and newtypes, primarily for the
//! component model.

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

/// A "kebab name" in the component model which is backed by `T`, which defaults
/// to `String`.
///
/// This name can be either:
///
/// * a `KebabStr`: `a-b-c`
/// * a method name : `[method]a-b.c-d`
/// * a static method name : `[static]a-b.c-d`
/// * a constructor: `[constructor]a-b`
///
/// # Equality and hashing
///
/// Note that this type the `Method` and `Static` variants are considered equal
/// and hash to the same value. This enables disallowing clashes between the two
/// where method name overlap cannot happen.
#[derive(Copy, Clone)]
pub struct KebabName<T = String> {
    raw: T,
    dot: Option<u32>,
}

/// Created via [`KebabName::kind`] and classifies a name.
#[derive(Debug, Copy, Clone)]
pub enum KebabNameKind<'a> {
    /// `a-b-c`
    Normal(&'a KebabStr),
    /// `[constructor]a-b`
    Constructor(&'a KebabStr),
    /// `[method]a-b.c-d`
    #[allow(missing_docs)]
    Method {
        resource: &'a KebabStr,
        name: &'a KebabStr,
    },
    /// `[static]a-b.c-d`
    #[allow(missing_docs)]
    Static {
        resource: &'a KebabStr,
        name: &'a KebabStr,
    },
}

impl<T> KebabName<T>
where
    T: AsRef<str>,
{
    /// Attempts to parse `name` as a kebab name, returning `None` if it's not
    /// valid.
    pub fn new(name: T) -> Option<KebabName<T>> {
        let s = name.as_ref();
        let dot = if let Some(s) = s.strip_prefix("[constructor]") {
            KebabStr::new(s)?;
            None
        } else if let Some(s) = s.strip_prefix("[method]") {
            let dot = s.find('.')?;
            KebabStr::new(&s[..dot])?;
            KebabStr::new(&s[dot + 1..])?;
            Some(u32::try_from(dot).ok()?)
        } else if let Some(s) = s.strip_prefix("[static]") {
            let dot = s.find('.')?;
            KebabStr::new(&s[..dot])?;
            KebabStr::new(&s[dot + 1..])?;
            Some(u32::try_from(dot).ok()?)
        } else {
            KebabStr::new(s)?;
            None
        };
        Some(KebabName { raw: name, dot })
    }

    /// Returns the [`KebabNameKind`] corresponding to this name.
    pub fn kind(&self) -> KebabNameKind<'_> {
        let s = self.raw.as_ref();
        if !s.starts_with('[') {
            KebabNameKind::Normal(KebabStr::new_unchecked(s))
        } else if s.as_bytes()[1] == b'c' {
            let prefix = "[constructor]";
            KebabNameKind::Constructor(KebabStr::new_unchecked(&s[prefix.len()..]))
        } else {
            let prefix1 = "[method]";
            let prefix2 = "[static]";
            let dot = self.dot.unwrap() as usize;
            assert_eq!(prefix1.len(), prefix2.len());
            let rest = &s[prefix1.len()..];
            let resource = KebabStr::new_unchecked(&rest[..dot]);
            let name = KebabStr::new_unchecked(&rest[dot + 1..]);
            if s.as_bytes()[1] == b'm' {
                KebabNameKind::Method { resource, name }
            } else {
                KebabNameKind::Static { resource, name }
            }
        }
    }

    /// Returns the raw underlying name as a string.
    pub fn as_str(&self) -> &str {
        self.raw.as_ref()
    }
}

impl<T> Hash for KebabName<T>
where
    T: AsRef<str>,
{
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.kind().hash(hasher)
    }
}

impl<T> PartialEq for KebabName<T>
where
    T: AsRef<str>,
{
    fn eq(&self, other: &KebabName<T>) -> bool {
        self.kind().eq(&other.kind())
    }
}

impl<T> Eq for KebabName<T> where T: AsRef<str> {}

impl<T> fmt::Display for KebabName<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.raw.fmt(f)
    }
}

impl<T> fmt::Debug for KebabName<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.raw.fmt(f)
    }
}

impl Hash for KebabNameKind<'_> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            KebabNameKind::Normal(name) => {
                hasher.write_u8(0);
                name.hash(hasher);
            }
            KebabNameKind::Constructor(name) => {
                hasher.write_u8(1);
                name.hash(hasher);
            }
            // for hashing method == static
            KebabNameKind::Method { resource, name } | KebabNameKind::Static { resource, name } => {
                hasher.write_u8(2);
                resource.hash(hasher);
                name.hash(hasher);
            }
        }
    }
}

impl PartialEq for KebabNameKind<'_> {
    fn eq(&self, other: &KebabNameKind<'_>) -> bool {
        match (self, other) {
            (KebabNameKind::Normal(a), KebabNameKind::Normal(b)) => a == b,
            (KebabNameKind::Normal(_), _) => false,
            (KebabNameKind::Constructor(a), KebabNameKind::Constructor(b)) => a == b,
            (KebabNameKind::Constructor(_), _) => false,

            // method == static for the purposes of hashing so equate them here
            // as well.
            (
                KebabNameKind::Method {
                    resource: ar,
                    name: an,
                },
                KebabNameKind::Method {
                    resource: br,
                    name: bn,
                },
            )
            | (
                KebabNameKind::Static {
                    resource: ar,
                    name: an,
                },
                KebabNameKind::Static {
                    resource: br,
                    name: bn,
                },
            )
            | (
                KebabNameKind::Method {
                    resource: ar,
                    name: an,
                },
                KebabNameKind::Static {
                    resource: br,
                    name: bn,
                },
            )
            | (
                KebabNameKind::Static {
                    resource: ar,
                    name: an,
                },
                KebabNameKind::Method {
                    resource: br,
                    name: bn,
                },
            ) => ar == br && an == bn,

            (KebabNameKind::Method { .. }, _) => false,
            (KebabNameKind::Static { .. }, _) => false,
        }
    }
}

impl Eq for KebabNameKind<'_> {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

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
        assert!(KebabName::new("a").is_some());
        assert!(KebabName::new("[foo]a").is_none());
        assert!(KebabName::new("[constructor]a").is_some());
        assert!(KebabName::new("[method]a").is_none());
        assert!(KebabName::new("[method]a.b").is_some());
        assert!(KebabName::new("[method]a.b.c").is_none());
        assert!(KebabName::new("[static]a.b").is_some());
        assert!(KebabName::new("[static]a").is_none());
    }

    #[test]
    fn name_equality() {
        assert_eq!(KebabName::new("a"), KebabName::new("a"));
        assert_ne!(KebabName::new("a"), KebabName::new("b"));
        assert_eq!(
            KebabName::new("[constructor]a"),
            KebabName::new("[constructor]a")
        );
        assert_ne!(
            KebabName::new("[constructor]a"),
            KebabName::new("[constructor]b")
        );
        assert_eq!(KebabName::new("[method]a.b"), KebabName::new("[method]a.b"));
        assert_ne!(KebabName::new("[method]a.b"), KebabName::new("[method]b.b"));
        assert_eq!(KebabName::new("[static]a.b"), KebabName::new("[static]a.b"));
        assert_ne!(KebabName::new("[static]a.b"), KebabName::new("[static]b.b"));

        assert_eq!(KebabName::new("[static]a.b"), KebabName::new("[method]a.b"));
        assert_eq!(KebabName::new("[method]a.b"), KebabName::new("[static]a.b"));

        assert_ne!(KebabName::new("[method]b.b"), KebabName::new("[static]a.b"));

        let mut s = HashSet::new();
        assert!(s.insert(KebabName::new("a")));
        assert!(s.insert(KebabName::new("[constructor]a")));
        assert!(s.insert(KebabName::new("[method]a.b")));
        assert!(!s.insert(KebabName::new("[static]a.b")));
        assert!(s.insert(KebabName::new("[static]b.b")));
    }
}
