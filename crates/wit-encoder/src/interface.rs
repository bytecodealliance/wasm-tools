use std::fmt;

use crate::{Docs, Ident, Render, RenderOpts, StandaloneFunction, TypeDef};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Interface {
    /// Name of this interface.
    pub(crate) name: Ident,

    // Interface items
    pub(crate) items: Vec<InterfaceItem>,

    /// Documentation associated with this interface.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub(crate) docs: Option<Docs>,
}

impl Interface {
    /// Create a new instance of `Interface`.
    pub fn new(name: impl Into<Ident>) -> Self {
        Self {
            name: name.into(),
            items: vec![],
            docs: None,
        }
    }

    /// Add a `TypeDef` to the interface
    pub fn type_def(&mut self, type_def: TypeDef) {
        self.items.push(InterfaceItem::TypeDef(type_def));
    }

    /// Add an `Function` to the interface
    pub fn function(&mut self, function: StandaloneFunction) {
        self.items.push(InterfaceItem::Function(function));
    }

    pub fn items(&self) -> &[InterfaceItem] {
        &self.items
    }

    pub fn functions_mut(&mut self) -> &mut Vec<InterfaceItem> {
        &mut self.items
    }

    /// Set the documentation of this interface.
    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum InterfaceItem {
    TypeDef(TypeDef),
    Function(StandaloneFunction),
}

pub type InterfaceItems = Vec<InterfaceItem>;

impl Render for InterfaceItems {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
        for item in self {
            match item {
                InterfaceItem::TypeDef(type_def) => {
                    type_def.render(f, opts)?;
                }
                InterfaceItem::Function(function) => {
                    function.render(f, opts)?;
                }
            }
        }
        Ok(())
    }
}
