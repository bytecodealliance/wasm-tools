use std::fmt;

use crate::{Docs, Render, RenderOpts, StandaloneFunction, TypeDef};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Interface {
    /// Optionally listed name of this interface.
    ///
    /// This is `None` for inline interfaces in worlds.
    name: Option<String>,

    /// Exported types from this interface.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id_map"))]
    type_defs: Vec<TypeDef>,

    /// Exported functions from this interface.
    functions: Vec<StandaloneFunction>,

    /// Documentation associated with this interface.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    docs: Option<Docs>,
}

impl Interface {
    /// Create a new instance of `Interface`.
    pub fn new(name: Option<impl Into<String>>) -> Self {
        Self {
            name: name.map(|n| n.into()),
            type_defs: vec![],
            functions: vec![],
            docs: None,
        }
    }

    /// Add a `Type` to the interface
    pub fn name(&mut self, name: Option<impl Into<String>>) {
        self.name = name.map(|n| n.into());
    }

    /// Add a `TypeDef` to the interface
    pub fn type_def(&mut self, type_def: TypeDef) {
        self.type_defs.push(type_def);
    }

    pub fn type_defs(&self) -> &[TypeDef] {
        &self.type_defs
    }

    pub fn type_defs_mut(&mut self) -> &mut Vec<TypeDef> {
        &mut self.type_defs
    }

    /// Add an `Function` to the interface
    pub fn function(&mut self, function: StandaloneFunction) {
        self.functions.push(function);
    }

    pub fn functions(&self) -> &[StandaloneFunction] {
        &self.functions
    }

    pub fn functions_mut(&mut self) -> &mut Vec<StandaloneFunction> {
        &mut self.functions
    }

    /// Set the documentation of this interface.
    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

impl Render for Interface {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
        if let Some(docs) = &self.docs {
            docs.render(f, opts)?;
        }
        write!(
            f,
            "{}interface {} {{\n",
            opts.spaces(),
            self.name.as_ref().unwrap_or(&String::new()),
        )?;
        for function in &self.functions {
            function.render(f, &opts.indent())?;
        }
        for type_def in &self.type_defs {
            type_def.render(f, &opts.indent())?;
        }
        write!(f, "{}}}\n", opts.spaces())?;
        Ok(())
    }
}
