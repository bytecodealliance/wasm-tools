use crate::{Docs, Function, Interface, Type};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct World {
    /// The WIT identifier name of this world.
    pub name: String,

    /// All imported items into this interface, both worlds and functions.
    pub imports: Vec<(WorldKey, WorldItem)>,

    /// All exported items from this interface, both worlds and functions.
    pub exports: Vec<(WorldKey, WorldItem)>,

    /// Documentation associated with this world declaration.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,

    /// All the included worlds from this world. Empty if this is fully resolved
    #[cfg_attr(feature = "serde", serde(skip))]
    pub includes: Vec<WorldKey>,

    /// All the included worlds names. Empty if this is fully resolved
    #[cfg_attr(feature = "serde", serde(skip))]
    pub include_names: Vec<Vec<IncludeName>>,
}

#[derive(Debug, Clone)]
pub struct IncludeName {
    /// The name of the item
    pub name: String,

    /// The name to be replaced with
    pub as_: String,
}

/// The key to the import/export maps of a world. Either a kebab-name or a
/// unique interface.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(into = "String"))]
pub enum WorldKey {
    /// A kebab-name.
    Name(String),
    /// An interface which is assigned no kebab-name.
    Interface(String),
}

impl WorldKey {
    /// Asserts that this is `WorldKey::Name` and returns the name.
    #[track_caller]
    pub fn unwrap_name(self) -> String {
        match self {
            WorldKey::Name(name) => name,
            WorldKey::Interface(_) => panic!("expected a name, found interface"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum WorldItem {
    /// An interface is being imported or exported from a world, indicating that
    /// it's a namespace of functions.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id"))]
    Interface(Interface),

    /// A function is being directly imported or exported from this world.
    Function(Function),

    /// A type is being exported from this world.
    ///
    /// Note that types are never imported into worlds at this time.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id"))]
    Type(Type),
}