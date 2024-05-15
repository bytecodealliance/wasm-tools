use std::fmt;

use crate::{Docs, Function, Resource, ToWitSyntax, TypeDef};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Interface {
    /// Optionally listed name of this interface.
    ///
    /// This is `None` for inline interfaces in worlds.
    name: Option<String>,

    /// Exported types from this interface.
    ///
    /// Export names are listed within the types themselves. Note that the
    /// export name here matches the name listed in the `TypeDef`.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id_map"))]
    type_defs: Vec<TypeDef>,

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
    pub fn new(name: Option<impl Into<String>>) -> Self {
        Self {
            name: name.map(|n| n.into()),
            type_defs: vec![],
            functions: vec![],
            resources: vec![],
            docs: Docs::default(),
        }
    }

    /// Add a `Type` to the interface
    pub fn name(&mut self, name: Option<impl Into<String>>) {
        self.name = name.map(|n| n.into());
    }

    /// Add a type-def to the interface
    pub fn type_def(&mut self, type_def: TypeDef) {
        self.type_defs.push(type_def);
    }

    /// Add an `Function` to the interface
    pub fn function(&mut self, function: Function) {
        self.functions.push(function);
    }

    /// Add a `Resource` to the interface
    pub fn resource(&mut self, resource: Resource) {
        self.resources.push(resource);
    }

    /// Set the documentation
    pub fn docs(&mut self, docs: Docs) {
        self.docs = docs;
    }
}
