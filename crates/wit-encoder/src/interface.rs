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
    types: Vec<Type>,

    /// Exported functions from this interface.
    functions: Vec<Function>,

    /// Exported resources from this interface.
    resources: Vec<Resource>,

    /// Documentation associated with this interface.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    docs: Docs,
}

impl Interface {
    /// Create a new instance of `Interface`.
    pub fn new() -> Self {
        Self {
            name: None,
            types: vec![],
            functions: vec![],
            resources: vec![],
            docs: Docs::default(),
        }
    }

    /// Add a `Type` to the interface
    pub fn name(&mut self, name: Option<String>) {
        self.name = name;
    }

    /// Add a `Type` to the interface
    pub fn type_(&mut self, type_: Type) {
        self.types.push(type_)
    }

    /// Add an `Function` to the interface
    pub fn function(&mut self, function: Function) {
        self.functions.push(function)
    }

    /// Add a `Resource` to the interface
    pub fn resource(&mut self, resource: Resource) {
        self.resources.push(resource)
    }

    /// Set the documentation
    pub fn docs(&mut self, docs: Docs) {
        self.docs = docs;
    }
}
