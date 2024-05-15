use std::fmt;

use crate::{Docs, Function, Resource, Render, RenderOpts, TypeDef};

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

impl Render for Interface {
    fn render_opts(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: usize,
        opts: RenderOpts,
    ) -> fmt::Result {
        write!(
            f,
            "{:depth$}interface {} {{\n",
            "",
            self.name.as_ref().unwrap_or(&String::new()),
            depth = opts.indent(depth),
        )?;
        for function in &self.functions {
            function.render(f, depth + 1)?;
        }
        for type_def in &self.type_defs {
            type_def.render(f, depth + 1)?;
        }
        // TODO: handle types and resources.
        write!(f, "{:depth$}}}\n", "", depth = opts.indent(depth))?;
        Ok(())
    }
}
