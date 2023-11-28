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
    Dependency,
    Url,
    Hash,
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
    /// `locked-dep=foo:bar/baz`
    #[allow(missing_docs)]
    Dependency(DependencyName<'a>),
    /// `url=https://...`
    #[allow(missing_docs)]
    Url(UrlName<'a>),
    /// `integrity=sha256:...`
    #[allow(missing_docs)]
    Hash(HashName<'a>),
}

const CONSTRUCTOR: &str = "[constructor]";
const METHOD: &str = "[method]";
const STATIC: &str = "[static]";

impl ComponentName {
    /// Attempts to parse `name` as a valid component name, returning `Err` if
    /// it's not valid.
    pub fn new(name: &str, offset: usize) -> Result<ComponentName> {
        let mut parser = ComponentNameParser { next: name, offset };
        let kind = parser.parse()?;
        if !parser.next.is_empty() {
            bail!(offset, "trailing characters found: `{}`", parser.next);
        }
        Ok(ComponentName {
            raw: name.to_string(),
            kind,
        })
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
            PK::Dependency => Dependency(DependencyName(&self.raw)),
            PK::Url => Url(UrlName(&self.raw)),
            PK::Hash => Hash(HashName(&self.raw)),
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
            Dependency(name) => (4u8, name).hash(hasher),
            Url(name) => (5u8, name).hash(hasher),
            Hash(name) => (6u8, name).hash(hasher),
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
            (Dependency(a), Dependency(b)) => a == b,
            (Dependency(_), _) => false,
            (Url(a), Url(b)) => a == b,
            (Url(_), _) => false,
            (Hash(a), Hash(b)) => a == b,
            (Hash(_), _) => false,
        }
    }
}

impl Eq for ComponentNameKind<'_> {}

/// A resource name and its function, stored as `a.b`.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ResourceFunc<'a>(&'a str);

impl<'a> ResourceFunc<'a> {
    /// Returns the the underlying string as `a.b`
    pub fn as_str(&self) -> &'a str {
        self.0
    }

    /// Returns the resource name or the `a` in `a.b`
    pub fn resource(&self) -> &'a KebabStr {
        let dot = self.0.find('.').unwrap();
        KebabStr::new_unchecked(&self.0[..dot])
    }
}

/// An interface name, stored as `a:b/c@1.2.3`
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct InterfaceName<'a>(&'a str);

impl<'a> InterfaceName<'a> {
    /// Returns the entire underlying string.
    pub fn as_str(&self) -> &'a str {
        self.0
    }

    /// Returns the `a` in `a:b/c`
    pub fn namespace(&self) -> &'a KebabStr {
        let colon = self.0.find(':').unwrap();
        KebabStr::new_unchecked(&self.0[..colon])
    }

    /// Returns the `b` in `a:b/c`
    pub fn package(&self) -> &'a KebabStr {
        let colon = self.0.find(':').unwrap();
        let slash = self.0.find('/').unwrap();
        KebabStr::new_unchecked(&self.0[colon + 1..slash])
    }

    /// Returns the `c` in `a:b/c`
    pub fn interface(&self) -> &'a KebabStr {
        let slash = self.0.find('/').unwrap();
        let at = self.0.find('@').unwrap_or(self.0.len());
        KebabStr::new_unchecked(&self.0[slash + 1..at])
    }

    /// Returns the `1.2.3` in `a:b/c@1.2.3`
    pub fn version(&self) -> Option<Version> {
        let at = self.0.find('@')?;
        Some(Version::parse(&self.0[at + 1..]).unwrap())
    }
}

/// A dependency on an implementation either as `locked-dep=...` or
/// `unlocked-dep=...`
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct DependencyName<'a>(&'a str);

impl<'a> DependencyName<'a> {
    /// Returns entire underlying import string
    pub fn as_str(&self) -> &'a str {
        self.0
    }
}

/// A dependency on an implementation either as `url=...` or
/// `relative-url=...`
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct UrlName<'a>(&'a str);

impl<'a> UrlName<'a> {
    /// Returns entire underlying import string
    pub fn as_str(&self) -> &'a str {
        self.0
    }
}

/// A dependency on an implementation either as `integrity=...`.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct HashName<'a>(&'a str);

impl<'a> HashName<'a> {
    /// Returns entire underlying import string.
    pub fn as_str(&self) -> &'a str {
        self.0
    }
}

// A small helper structure to parse `self.next` which is an import or export
// name.
//
// Methods will update `self.next` as they go along and `self.offset` is used
// for error messages.
struct ComponentNameParser<'a> {
    next: &'a str,
    offset: usize,
}

impl<'a> ComponentNameParser<'a> {
    fn parse(&mut self) -> Result<ParsedComponentNameKind> {
        if self.eat_str(CONSTRUCTOR) {
            self.expect_kebab()?;
            return Ok(ParsedComponentNameKind::Constructor);
        }
        if self.eat_str(METHOD) {
            let resource = self.take_until('.')?;
            self.kebab(resource)?;
            self.expect_kebab()?;
            return Ok(ParsedComponentNameKind::Method);
        }
        if self.eat_str(STATIC) {
            let resource = self.take_until('.')?;
            self.kebab(resource)?;
            self.expect_kebab()?;
            return Ok(ParsedComponentNameKind::Static);
        }

        // 'unlocked-dep=<' <pkgidset> '>'
        if self.eat_str("unlocked-dep=") {
            self.expect_str("<")?;
            self.pkgidset_up_to('>')?;
            self.expect_str(">")?;
            return Ok(ParsedComponentNameKind::Dependency);
        }

        // 'locked-dep=<' <pkgid> '>' ( ',' <hashname> )?
        if self.eat_str("locked-dep=") {
            self.expect_str("<")?;
            self.pkgid_up_to('>')?;
            self.expect_str(">")?;
            self.eat_optional_hash()?;
            return Ok(ParsedComponentNameKind::Dependency);
        }

        // 'url=<' <nonbrackets> '>' (',' <hashname>)?
        if self.eat_str("url=") {
            self.expect_str("<")?;
            let _url = self.take_up_to('>')?;
            self.expect_str(">")?;
            self.eat_optional_hash()?;
            return Ok(ParsedComponentNameKind::Url);
        }
        // 'relative-url=<' <nonbrackets> '>' (',' <hashname>)?
        if self.eat_str("relative-url=") {
            self.expect_str("<")?;
            let _url = self.take_up_to('>')?;
            self.expect_str(">")?;
            self.eat_optional_hash()?;
            return Ok(ParsedComponentNameKind::Url);
        }

        // 'integrity=<' <integrity-metadata> '>'
        if self.eat_str("integrity=") {
            self.expect_str("<")?;
            let _hash = self.parse_hash()?;
            self.expect_str(">")?;
            return Ok(ParsedComponentNameKind::Hash);
        }

        match self.eat_until(':') {
            // interfacename ::= <namespace> <label> <projection> <version>?
            Some(namespace) => {
                self.kebab(namespace)?;
                let pkg = self.take_until('/')?;
                self.kebab(pkg)?;
                match self.eat_until('@') {
                    Some(interface) => {
                        self.kebab(interface)?;
                        let version = self.take_rest();
                        self.semver(version)?;
                    }
                    None => {
                        self.expect_kebab()?;
                    }
                }
                Ok(ParsedComponentNameKind::Interface)
            }
            None => {
                self.expect_kebab()?;
                Ok(ParsedComponentNameKind::Label)
            }
        }
    }

    // pkgidset      ::= <pkgname> <verrange>?
    // pkgname       ::= <namespace> <label>
    // verrange      ::= '@*'
    //                 | '@{' <verlower> '}'
    //                 | '@{' <verupper> '}'
    //                 | '@{' <verlower> ' ' <verupper> '}'
    // verlower      ::= '>=' <valid semver>
    // verupper      ::= '<' <valid semver>
    fn pkgidset_up_to(&mut self, end: char) -> Result<()> {
        let namespace = self.take_until(':')?;
        self.kebab(namespace)?;
        let name = match self.eat_until('@') {
            Some(name) => name,
            // a:b
            None => {
                let name = self.take_up_to(end)?;
                self.kebab(name)?;
                return Ok(());
            }
        };
        self.kebab(name)?;

        // a:b@*
        if self.eat_str("*") {
            return Ok(());
        }
        self.expect_str("{")?;
        if self.eat_str(">=") {
            match self.eat_until(' ') {
                Some(lower) => {
                    self.semver(lower)?;
                }
                // a:b@{>=1.2.3}
                None => {
                    let lower = self.take_until('}')?;
                    self.semver(lower)?;
                    return Ok(());
                }
            }
        }

        // a:b@{<1.2.3}
        // .. or
        // a:b@{<1.2.3 >=1.2.3}
        self.expect_str("<")?;
        let version = self.take_until('}')?;
        self.semver(version)?;
        Ok(())
    }

    // pkgid         ::= <pkgname> <version>?
    fn pkgid_up_to(&mut self, end: char) -> Result<()> {
        let namespace = self.take_until(':')?;
        self.kebab(namespace)?;
        match self.eat_until('@') {
            // a:b@1.2.3
            Some(name) => {
                self.kebab(name)?;
                let version = self.take_up_to(end)?;
                self.semver(version)?;
            }
            // a:b
            None => {
                let name = self.take_up_to(end)?;
                self.kebab(name)?;
            }
        }
        Ok(())
    }

    fn parse_hash(&mut self) -> Result<&'a str> {
        let integrity = self.take_up_to('>')?;
        let mut any = false;
        for hash in integrity.split_whitespace() {
            any = true;
            let rest = hash
                .strip_prefix("sha256")
                .or_else(|| hash.strip_prefix("sha384"))
                .or_else(|| hash.strip_prefix("sha512"));
            let rest = match rest {
                Some(s) => s,
                None => bail!(self.offset, "unrecognized hash algorithm: `{hash}`"),
            };
            let rest = match rest.strip_prefix('-') {
                Some(s) => s,
                None => bail!(self.offset, "expected `-` after hash algorithm: {hash}"),
            };
            let (base64, _options) = match rest.find('?') {
                Some(i) => (&rest[..i], Some(&rest[i + 1..])),
                None => (rest, None),
            };
            if !is_base64(base64) {
                bail!(self.offset, "not valid base64: `{base64}`");
            }
        }
        if !any {
            bail!(self.offset, "integrity hash cannot be empty");
        }
        Ok(integrity)
    }

    fn eat_optional_hash(&mut self) -> Result<Option<&'a str>> {
        if !self.eat_str(",") {
            return Ok(None);
        }
        self.expect_str("integrity=<")?;
        let ret = self.parse_hash()?;
        self.expect_str(">")?;
        Ok(Some(ret))
    }

    fn eat_str(&mut self, prefix: &str) -> bool {
        match self.next.strip_prefix(prefix) {
            Some(rest) => {
                self.next = rest;
                true
            }
            None => false,
        }
    }

    fn expect_str(&mut self, prefix: &str) -> Result<()> {
        if self.eat_str(prefix) {
            Ok(())
        } else {
            bail!(self.offset, "expected `{prefix}` at `{}`", self.next);
        }
    }

    fn eat_until(&mut self, c: char) -> Option<&'a str> {
        let ret = self.eat_up_to(c);
        if ret.is_some() {
            self.next = &self.next[c.len_utf8()..];
        }
        ret
    }

    fn eat_up_to(&mut self, c: char) -> Option<&'a str> {
        let i = self.next.find(c)?;
        let (a, b) = self.next.split_at(i);
        self.next = b;
        Some(a)
    }

    fn kebab(&self, s: &'a str) -> Result<&'a KebabStr> {
        match KebabStr::new(s) {
            Some(name) => Ok(name),
            None => bail!(self.offset, "`{s}` is not in kebab case"),
        }
    }

    fn semver(&self, s: &str) -> Result<Version> {
        match Version::parse(s) {
            Ok(v) => Ok(v),
            Err(e) => bail!(self.offset, "`{s}` is not a valid semver: {e}"),
        }
    }

    fn take_until(&mut self, c: char) -> Result<&'a str> {
        match self.eat_until(c) {
            Some(s) => Ok(s),
            None => bail!(self.offset, "failed to find `{c}` character"),
        }
    }

    fn take_up_to(&mut self, c: char) -> Result<&'a str> {
        match self.eat_up_to(c) {
            Some(s) => Ok(s),
            None => bail!(self.offset, "failed to find `{c}` character"),
        }
    }

    fn take_rest(&mut self) -> &'a str {
        let ret = self.next;
        self.next = "";
        ret
    }

    fn expect_kebab(&mut self) -> Result<&'a KebabStr> {
        let s = self.take_rest();
        self.kebab(s)
    }
}

fn is_base64(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let mut equals = 0;
    for (i, byte) in s.as_bytes().iter().enumerate() {
        match byte {
            b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'+' | b'/' if equals == 0 => {}
            b'=' if i > 0 && equals < 2 => equals += 1,
            _ => return false,
        }
    }
    true
}

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
