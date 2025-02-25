use std::fmt;

use crate::{Docs, Ident, Render, RenderOpts, StandaloneFunc, TypeDef, Use};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Interface {
    /// Name of this interface.
    pub(crate) name: Ident,

    // Interface uses
    pub(crate) uses: Vec<Use>,

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
            uses: vec![],
            items: vec![],
            docs: None,
        }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn set_name(&mut self, name: impl Into<Ident>) {
        self.name = name.into();
    }

    /// Add a `TypeDef` to the interface
    pub fn type_def(&mut self, type_def: TypeDef) {
        self.items.push(InterfaceItem::TypeDef(type_def));
    }

    /// Add an `Function` to the interface
    pub fn function(&mut self, function: StandaloneFunc) {
        self.items.push(InterfaceItem::Function(function));
    }

    pub fn uses(&self) -> &[Use] {
        &self.uses
    }

    pub fn uses_mut(&mut self) -> &mut [Use] {
        &mut self.uses
    }

    /// Add a `Use` to the interface
    pub fn use_(&mut self, use_: Use) {
        self.uses.push(use_);
    }

    /// Use a type in the interface.
    pub fn use_type(
        &mut self,
        target: impl Into<Ident>,
        item: impl Into<Ident>,
        rename: Option<Ident>,
    ) {
        let target = target.into();
        let use_ = self.uses.iter_mut().find(|u| u.target() == &target);
        match use_ {
            Some(use_) => use_.item(item, rename),
            None => {
                self.use_({
                    let mut use_ = Use::new(target);
                    use_.item(item, rename);
                    use_
                });
            }
        }
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
                    let opt_async = if func.async_ { "async " } else { "" };
                    write!(
                        f,
                        "{}{}: {opt_async}func({})",
                        opts.spaces(),
                        func.name,
                        func.params,
                    )?;
                    if let Some(ty) = &func.result {
                        write!(f, " -> {ty}")?;
                    }
                    write!(f, ";\n")?;
                }
            }
        }
        Ok(())
    }
}

pub type InterfaceUses = Vec<Use>;

impl Render for InterfaceUses {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
        for use_ in self {
            use_.render(f, opts)?;
        }
        Ok(())
    }
}
