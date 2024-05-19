use std::fmt;

use semver::Version;

use crate::{ident::Ident, Interface, Render, RenderOpts, World};

/// A WIT package.
///
/// A package is a collection of interfaces and worlds. Packages additionally
/// have a unique identifier that affects generated components and uniquely
/// identifiers this particular package.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Package {
    /// A unique name corresponding to this package.
    name: PackageName,

    /// All interfaces contained in this packaged, keyed by the interface's
    /// name.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id_map"))]
    interfaces: Vec<Interface>,

    /// All worlds contained in this package, keyed by the world's name.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id_map"))]
    worlds: Vec<World>,
}

impl Package {
    /// Create a new instance of `Package`.
    pub fn new(name: PackageName) -> Self {
        Self {
            name,
            interfaces: vec![],
            worlds: vec![],
        }
    }

    /// Add an `Interface` to the package
    pub fn interface(&mut self, interface: Interface) {
        self.interfaces.push(interface)
    }

    /// Add a `World` to the package
    pub fn world(&mut self, world: World) {
        self.worlds.push(world)
    }
}

impl Render for Package {
    fn render_opts(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: usize,
        opts: RenderOpts,
    ) -> fmt::Result {
        write!(
            f,
            "{:depth$}package {};\n",
            "",
            self.name,
            depth = opts.indent(depth)
        )?;
        write!(f, "\n")?;
        for interface in &self.interfaces {
            interface.render(f, depth)?;
        }
        Ok(())
    }
}

impl fmt::Display for Package {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.render(f, 0)
    }
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
    namespace: String,
    /// The kebab-name of this package, which is always specified.
    name: Ident,
    /// Optional major/minor version information.
    version: Option<Version>,
}

impl PackageName {
    /// Create a new instance of `PackageName`
    pub fn new(
        namespace: impl Into<String>,
        name: impl Into<Ident>,
        version: Option<Version>,
    ) -> Self {
        Self {
            namespace: namespace.into(),
            name: name.into(),
            version,
        }
    }
}

impl From<PackageName> for String {
    fn from(name: PackageName) -> String {
        name.to_string()
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
