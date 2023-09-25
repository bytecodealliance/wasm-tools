//! Definitions of name-related helpers and newtypes, primarily for the
//! component model.

use crate::{ComponentExportName, ComponentImportName, Result};
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
#[derive(Clone)]
pub struct KebabName {
    raw: String,
    parsed: ParsedKebabName,
}

#[derive(Debug, Clone, Hash)]
struct At {
    index: u32,
    semver: Semver,
}

#[derive(Debug, Clone, Hash)]
/// Pinned version or semver range
pub enum Semver {
    /// pinned version
    Semver(Version),
    /// semver range
    SemverRange(SemverRange),
}

impl PartialEq for Semver {
    fn eq(&self, other: &Semver) -> bool {
        match self {
            Self::Semver(version) => match other {
                Semver::Semver(other_version) => version == other_version,
                Semver::SemverRange(_) => false,
            },
            Self::SemverRange(version_range) => match other {
                Semver::Semver(_) => false,
                Semver::SemverRange(other_range) => version_range == other_range,
            },
        }
    }
}

#[derive(Debug, Clone, Hash)]
/// Potential semver range specifications
pub enum SemverRange {
    /// All versions
    All,
    /// Upper Bound
    Upper(Version),
    /// Lower Bound
    Lower(Version),
    /// Upper and Lower Bound
    Both((Version, Version)),
}

impl PartialEq for SemverRange {
    fn eq(&self, other: &SemverRange) -> bool {
        match self {
            SemverRange::All => match other {
                SemverRange::All => true,
                _ => false,
            },
            SemverRange::Upper(ua) => match other {
                SemverRange::Upper(ub) => ua == ub,
                _ => false,
            },
            SemverRange::Lower(la) => match other {
                SemverRange::Lower(lb) => la == lb,
                _ => false,
            },
            SemverRange::Both((la, ua)) => match other {
                SemverRange::Both((lb, ub)) => la == lb && ua == ub,
                _ => false,
            },
        }
    }
}

#[derive(Clone)]
enum ParsedKebabName {
    Normal,
    Constructor,
    Method {
        dot: u32,
    },
    Static {
        dot: u32,
    },
    RegistryId {
        colon: u32,
        slash: Option<u32>,
        at: Option<At>,
    },
}

/// Created via [`KebabName::kind`] and classifies a name.
#[derive(Debug, Clone)]
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
    /// `wasi:http/types@2.0`
    #[allow(missing_docs)]
    RegistryId {
        namespace: &'a KebabStr,
        package: &'a KebabStr,
        interface: Option<&'a KebabStr>,
        version: Option<Semver>,
    },
}

const CONSTRUCTOR: &str = "[constructor]";
const METHOD: &str = "[method]";
const STATIC: &str = "[static]";

impl KebabName {
    /// Attempts to parse `name` as a kebab name, returning `None` if it's not
    /// valid.
    pub fn from_export(name: ComponentExportName<'_>, offset: usize) -> Result<KebabName> {
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

        let parsed = match name {
            ComponentExportName::Kebab(s) => {
                if let Some(s) = s.strip_prefix(CONSTRUCTOR) {
                    validate_kebab(s)?;
                    ParsedKebabName::Constructor
                } else if let Some(s) = s.strip_prefix(METHOD) {
                    let dot = find(s, '.')?;
                    validate_kebab(&s[..dot])?;
                    validate_kebab(&s[dot + 1..])?;
                    ParsedKebabName::Method { dot: dot as u32 }
                } else if let Some(s) = s.strip_prefix(STATIC) {
                    let dot = find(s, '.')?;
                    validate_kebab(&s[..dot])?;
                    validate_kebab(&s[dot + 1..])?;
                    ParsedKebabName::Static { dot: dot as u32 }
                } else {
                    validate_kebab(s)?;
                    ParsedKebabName::Normal
                }
            }
            ComponentExportName::Interface(s) => {
                let colon = find(s, ':')?;
                validate_kebab(&s[..colon])?;
                let slash = find(s, '/')?;
                let at = s[slash..].find('@').map(|i| i + slash);
                validate_kebab(&s[colon + 1..slash])?;
                validate_kebab(&s[slash + 1..at.unwrap_or(s.len())])?;
                if let Some(at) = at {
                    let version = &s[at + 1..];
                    let version = version.parse::<Version>();
                    match version {
                        Ok(ver) => ParsedKebabName::RegistryId {
                            colon: colon as u32,
                            slash: Some(slash as u32),
                            at: Some(At {
                                index: at as u32,
                                semver: Semver::Semver(ver),
                            }),
                        },
                        Err(e) => {
                            bail!(offset, "failed to parse version: {e}")
                        }
                    }
                } else {
                    ParsedKebabName::RegistryId {
                        colon: colon as u32,
                        slash: Some(slash as u32),
                        at: None,
                    }
                }
            }
        };
        Ok(KebabName {
            raw: name.as_str().to_string(),
            parsed,
        })
    }

    /// Attempts to parse `name` as a kebab name, returning `None` if it's not
    /// valid.
    pub fn from_import(name: ComponentImportName<'_>, offset: usize) -> Result<KebabName> {
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
        let maybe_find = |s: &str, c: char| match s.find(c) {
            Some(i) => Some(i),
            None => None,
        };
        let validate_version_range = |s: &str| match s.chars().nth(0).unwrap() {
            '*' => Ok(SemverRange::All),
            '{' => match s.chars().nth(1).unwrap() {
                '>' => {
                    if s.chars().nth(2).unwrap() == '=' {
                        let space = maybe_find(s, ' ');
                        if let Some(sp) = space {
                            match s[3..sp].parse::<Version>() {
                                Ok(lower_ver) => {
                                    let upper = &s[sp + 1..s.len() - 1];
                                    match upper.chars().nth(0).unwrap() {
                                        '<' => {
                                            match upper[1..].parse::<Version>() {
                                                Ok(upper_ver) => {
                                                    return Ok(SemverRange::Both((
                                                        lower_ver, upper_ver,
                                                    )))
                                                }
                                                Err(_) => {
                                                    bail!(offset, "`{s}` multiple bounds but terminal bound is not upper bound")
                                                }
                                            }
                                          }
                                          _ => bail!(offset, "`{s}` multiple bounds but terminal bound is not upper bound"),
                                    }
                                }
                                Err(_) => bail!(offset, "`{s}` lower bound is not valid semver"),
                            }
                        } else {
                            let close = find(s, '}')?;
                            match s[3..close].parse::<Version>() {
                                Ok(lower) => return Ok(SemverRange::Lower(lower)),
                                Err(_) => bail!(offset, "`{s}` range has no closing brace"),
                            }
                        }
                    } else {
                        bail!(offset, "`{s}` lower range bound must be inclusive")
                    }
                }
                '<' => {
                    let close = find(s, '}')?;
                    match s[2..close].parse::<Version>() {
                        Ok(upper) => return Ok(SemverRange::Upper(upper)),
                        Err(_) => bail!(offset, "`{s}` is not a valid semver range"),
                    }
                }
                _ => bail!(offset, "`{s}` is not a valid semver range"),
            },
            _ => bail!(offset, "`{s}` is not a valid semver range"),
        };
        let parsed = match name {
            ComponentImportName::Kebab(s) => {
                if let Some(s) = s.strip_prefix(CONSTRUCTOR) {
                    validate_kebab(s)?;
                    ParsedKebabName::Constructor
                } else if let Some(s) = s.strip_prefix(METHOD) {
                    let dot = find(s, '.')?;
                    validate_kebab(&s[..dot])?;
                    validate_kebab(&s[dot + 1..])?;
                    ParsedKebabName::Method { dot: dot as u32 }
                } else if let Some(s) = s.strip_prefix(STATIC) {
                    let dot = find(s, '.')?;
                    validate_kebab(&s[..dot])?;
                    validate_kebab(&s[dot + 1..])?;
                    ParsedKebabName::Static { dot: dot as u32 }
                } else {
                    validate_kebab(s)?;
                    ParsedKebabName::Normal
                }
            }
            ComponentImportName::Interface(s) => {
                let colon = find(s, ':')?;
                validate_kebab(&s[..colon])?;
                let slash = find(s, '/')?;
                let at = s[slash..].find('@').map(|i| i + slash);
                validate_kebab(&s[colon + 1..slash])?;
                validate_kebab(&s[slash + 1..at.unwrap_or(s.len())])?;
                if let Some(at) = at {
                    let version = &s[at + 1..];
                    let version = version.parse::<Version>();
                    match version {
                        Ok(ver) => ParsedKebabName::RegistryId {
                            colon: colon as u32,
                            slash: Some(slash as u32),
                            at: Some(At {
                                index: at as u32,
                                semver: Semver::Semver(ver),
                            }),
                        },
                        Err(e) => {
                            bail!(offset, "failed to parse version: {e}")
                        }
                    }
                } else {
                    ParsedKebabName::RegistryId {
                        colon: colon as u32,
                        slash: Some(slash as u32),
                        at: None,
                    }
                }
            }
            ComponentImportName::Url(name, _, _) => {
                validate_kebab(name)?;
                ParsedKebabName::Normal
            }
            ComponentImportName::Relative(name, _, _) => {
                validate_kebab(name)?;
                ParsedKebabName::Normal
            }
            ComponentImportName::Naked(name, _) => {
                validate_kebab(name)?;
                ParsedKebabName::Normal
            }
            ComponentImportName::Locked(name, _) => {
                let colon = find(name, ':')?;
                validate_kebab(&name[..colon])?;
                let slash = maybe_find(name, '/');
                if let Some(sl) = slash {
                    let at = name[sl..].find('@').map(|i| i + sl);
                    validate_kebab(&name[colon + 1..sl])?;
                    if let Some(at) = at {
                        validate_kebab(&name[sl + 1..at])?;
                        let version_string = &name[at + 1..];
                        let version = version_string.parse::<Version>();
                        match version {
                            Ok(ver) => ParsedKebabName::RegistryId {
                                colon: colon as u32,
                                slash: Some(sl as u32),
                                at: Some(At {
                                    index: at as u32,
                                    semver: Semver::Semver(ver),
                                }),
                            },
                            Err(e) => {
                                bail!(offset, "failed to parse version: {e}")
                            }
                        }
                    } else {
                        ParsedKebabName::RegistryId {
                            colon: colon as u32,
                            slash: Some(sl as u32),
                            at: None,
                        }
                    }
                } else {
                    let at = name[colon..].find('@').map(|i| i + colon);
                    if let Some(at) = at {
                        validate_kebab(&name[colon + 1..at])?;
                        let version_string = &name[at + 1..];
                        let version = version_string.parse::<Version>();
                        match version {
                            Ok(ver) => ParsedKebabName::RegistryId {
                                colon: colon as u32,
                                slash: None,
                                at: Some(At {
                                    index: at as u32,
                                    semver: Semver::Semver(ver),
                                }),
                            },
                            Err(e) => {
                                bail!(offset, "failed to parse version: {e}")
                            }
                        }
                    } else {
                        ParsedKebabName::RegistryId {
                            colon: colon as u32,
                            slash: None,
                            at: None,
                        }
                    }
                }
            }
            ComponentImportName::Unlocked(name) => {
                let colon = find(name, ':')?;
                validate_kebab(&name[..colon])?;
                let slash = maybe_find(name, '/');
                if let Some(sl) = slash {
                    let at = name[sl..].find('@').map(|i| i + sl);
                    validate_kebab(&name[colon + 1..sl])?;
                    if let Some(at) = at {
                        validate_kebab(&name[sl + 1..at])?;
                        let version_string = validate_version_range(&name[at + 1..])?;
                        ParsedKebabName::RegistryId {
                            colon: colon as u32,
                            slash: Some(sl as u32),
                            at: Some(At {
                                index: at as u32,
                                semver: Semver::SemverRange(version_string),
                            }),
                        }
                    } else {
                        ParsedKebabName::RegistryId {
                            colon: colon as u32,
                            slash: Some(sl as u32),
                            at: None,
                        }
                    }
                } else {
                    let at = name[colon..].find('@').map(|i| i + colon);
                    if let Some(at) = at {
                        validate_kebab(&name[colon + 1..at])?;
                        let version_string = validate_version_range(&name[at + 1..])?;
                        ParsedKebabName::RegistryId {
                            colon: colon as u32,
                            slash: None,
                            at: Some(At {
                                index: at as u32,
                                semver: Semver::SemverRange(version_string),
                            }),
                        }
                    } else {
                        ParsedKebabName::RegistryId {
                            colon: colon as u32,
                            slash: None,
                            at: None,
                        }
                    }
                }
            }
        };
        Ok(KebabName {
            raw: name.as_str().to_string(),
            parsed,
        })
    }

    /// Returns the [`KebabNameKind`] corresponding to this name.
    pub fn kind(&self) -> KebabNameKind<'_> {
        match &self.parsed {
            ParsedKebabName::Normal => KebabNameKind::Normal(KebabStr::new_unchecked(&self.raw)),
            ParsedKebabName::Constructor => {
                let kebab = &self.raw[CONSTRUCTOR.len()..];
                KebabNameKind::Constructor(KebabStr::new_unchecked(kebab))
            }
            ParsedKebabName::Method { dot } => {
                let dotted = &self.raw[METHOD.len()..];
                let resource = KebabStr::new_unchecked(&dotted[..*dot as usize]);
                let name = KebabStr::new_unchecked(&dotted[*dot as usize + 1..]);
                KebabNameKind::Method { resource, name }
            }
            ParsedKebabName::Static { dot } => {
                let dotted = &self.raw[METHOD.len()..];
                let resource = KebabStr::new_unchecked(&dotted[..*dot as usize]);
                let name = KebabStr::new_unchecked(&dotted[*dot as usize + 1..]);
                KebabNameKind::Static { resource, name }
            }
            ParsedKebabName::RegistryId { colon, slash, at } => {
                let colon = *colon as usize;
                let package = if let Some(sl) = slash {
                    KebabStr::new_unchecked(&self.raw[colon + 1..*sl as usize])
                } else if let Some(at) = at {
                    KebabStr::new_unchecked(&self.raw[colon + 1..at.index as usize])
                } else {
                    KebabStr::new_unchecked(&self.raw[colon + 1..])
                };
                let interface = if let Some(sl) = slash {
                    Some(KebabStr::new_unchecked(&self.raw[*sl as usize + 1..]))
                } else {
                    None
                };
                let version = if let Some(at) = at {
                    Some(at.semver.clone())
                } else {
                    None
                };
                let namespace = KebabStr::new_unchecked(&self.raw[..colon]);
                KebabNameKind::RegistryId {
                    namespace,
                    package,
                    interface,
                    version,
                }
            }
        }
    }

    /// Returns the raw underlying name as a string.
    pub fn as_str(&self) -> &str {
        &self.raw
    }
}

impl From<KebabName> for String {
    fn from(name: KebabName) -> String {
        name.raw
    }
}

impl Hash for KebabName {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.kind().hash(hasher)
    }
}

impl PartialEq for KebabName {
    fn eq(&self, other: &KebabName) -> bool {
        self.kind().eq(&other.kind())
    }
}

impl Eq for KebabName {}

impl fmt::Display for KebabName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.raw.fmt(f)
    }
}

impl fmt::Debug for KebabName {
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
            KebabNameKind::RegistryId {
                namespace,
                package,
                interface,
                version,
            } => {
                hasher.write_u8(3);
                namespace.hash(hasher);
                package.hash(hasher);
                interface.hash(hasher);
                version.hash(hasher);
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
            (
                KebabNameKind::RegistryId {
                    namespace: an,
                    package: ap,
                    interface: ai,
                    version: av,
                },
                KebabNameKind::RegistryId {
                    namespace: bn,
                    package: bp,
                    interface: bi,
                    version: bv,
                },
            ) => an == bn && ap == bp && ai == bi && av == bv,
            (KebabNameKind::RegistryId { .. }, _) => false,
        }
    }
}

impl Eq for KebabNameKind<'_> {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn parse_kebab_name(s: &str) -> Option<KebabName> {
        KebabName::from_import(ComponentImportName::Kebab(s), 0).ok()
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
