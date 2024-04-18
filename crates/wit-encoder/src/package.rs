use std::fmt;

use semver::Version;

use crate::{Docs, Interface, World};

/// A WIT package.
///
/// A package is a collection of interfaces and worlds. Packages additionally
/// have a unique identifier that affects generated components and uniquely
/// identifiers this particular package.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Package {
    /// A unique name corresponding to this package.
    pub name: PackageName,

    /// Documentation associated with this package.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,

    /// All interfaces contained in this packaged, keyed by the interface's
    /// name.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id_map"))]
    pub interfaces: Vec<Interface>,

    /// All worlds contained in this package, keyed by the world's name.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id_map"))]
    pub worlds: Vec<World>,
}

/// A structure used to keep track of the name of a package, containing optional
/// information such as a namespace and version information.
///
/// This is directly encoded as an "ID" in the binary component representation
/// with an interfaced tacked on as well.
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(into = "String"))]
pub struct PackageName {
    /// A namespace such as `wasi` in `wasi:foo/bar`
    pub namespace: String,
    /// The kebab-name of this package, which is always specified.
    pub name: String,
    /// Optional major/minor version information.
    pub version: Option<Version>,
}

impl From<PackageName> for String {
    fn from(name: PackageName) -> String {
        name.to_string()
    }
}

impl PackageName {
    /// Returns the ID that this package name would assign the `interface` name
    /// specified.
    pub fn interface_id(&self, interface: &str) -> String {
        let mut s = String::new();
        s.push_str(&format!("{}:{}/{interface}", self.namespace, self.name));
        if let Some(version) = &self.version {
            s.push_str(&format!("@{version}"));
        }
        s
    }
}

impl fmt::Display for PackageName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.namespace, self.name)?;
        if let Some(version) = &self.version {
            write!(f, "@{version}")?;
        }
        Ok(())
    }
}
