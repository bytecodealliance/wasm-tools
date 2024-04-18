use crate::{Docs, Function, Resource, Type};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Interface {
    /// Optionally listed name of this interface.
    ///
    /// This is `None` for inline interfaces in worlds.
    pub name: Option<String>,

    /// Exported types from this interface.
    ///
    /// Export names are listed within the types themselves. Note that the
    /// export name here matches the name listed in the `TypeDef`.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id_map"))]
    pub types: Vec<Type>,

    /// Exported functions from this interface.
    pub functions: Vec<Function>,

    /// Exported resources from this interface.
    pub resources: Vec<Resource>,

    /// Documentation associated with this interface.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,
}
