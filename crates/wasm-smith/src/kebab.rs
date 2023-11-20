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
