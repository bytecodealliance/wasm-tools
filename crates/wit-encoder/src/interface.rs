use std::fmt;

use crate::{Docs, Ident, Render, RenderOpts, StandaloneFunc, TypeDef, Use};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Interface {
    /// Name of this interface.
    pub(crate) name: Ident,

    // Interface items
    pub(crate) items: Vec<InterfaceItem>,

    /// Documentation associated with this interface.
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
    pub fn function(&mut self, function: StandaloneFunc) {
        self.items.push(InterfaceItem::Function(function));
    }

    /// Add a `Use` to the interface
    pub fn use_(&mut self, use_: Use) {
        self.items.push(InterfaceItem::Use(use_));
    }

    pub fn item(&mut self, item: impl Into<InterfaceItem>) {
        self.items.push(item.into());
    }

    pub fn items(&self) -> &[InterfaceItem] {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut Vec<InterfaceItem> {
        &mut self.items
    }

    /// Set the documentation of this interface.
    pub fn set_docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }

    pub fn docs(&self) -> &Option<Docs> {
        &self.docs
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum InterfaceItem {
    TypeDef(TypeDef),
    Use(Use),
    Function(StandaloneFunc),
}

pub type InterfaceItems = Vec<InterfaceItem>;

impl Render for InterfaceItems {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
        for item in self {
            match item {
                InterfaceItem::TypeDef(type_def) => {
                    type_def.render(f, opts)?;
                }
                InterfaceItem::Function(func) => {
                    if let Some(docs) = &func.docs {
                        docs.render(f, opts)?;
                    }
                    write!(f, "{}{}: func({})", opts.spaces(), func.name, func.params,)?;
                    if !func.results.is_empty() {
                        write!(f, " -> {}", func.results)?;
                    }
                    write!(f, ";\n")?;
                }
                InterfaceItem::Use(use_) => {
                    use_.render(f, opts)?;
                }
            }
        }
        Ok(())
    }
}
