use std::fmt;

use crate::{Docs, Include, Interface, Render, RenderOpts, StandaloneFunc, Use, ident::Ident};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct World {
    /// The WIT identifier name of this world.
    name: Ident,

    /// Interface uses
    uses: Vec<Use>,

    /// All imported and exported items into this world.
    items: Vec<WorldItem>,

    /// Documentation associated with this world declaration.
    docs: Option<Docs>,
}

impl World {
    /// Create a new world.
    pub fn new(name: impl Into<Ident>) -> Self {
        Self {
            name: name.into(),
            uses: vec![],
            items: vec![],
            docs: None,
        }
    }

    /// Add a `name` to the world
    pub fn set_name(&mut self, name: impl Into<Ident>) {
        self.name = name.into();
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn items(&self) -> &[WorldItem] {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut Vec<WorldItem> {
        &mut self.items
    }

    /// Add an import or export to the world
    pub fn item(&mut self, item: WorldItem) {
        self.items.push(item);
    }

    pub fn inline_interface_import(&mut self, value: Interface) {
        self.item(WorldItem::inline_interface_import(value));
    }
    pub fn inline_interface_export(&mut self, value: Interface) {
        self.item(WorldItem::inline_interface_export(value));
    }
    pub fn named_interface_import(&mut self, value: impl Into<WorldNamedInterface>) {
        self.item(WorldItem::named_interface_import(value));
    }
    pub fn named_interface_export(&mut self, value: impl Into<WorldNamedInterface>) {
        self.item(WorldItem::named_interface_export(value));
    }
    pub fn function_import(&mut self, value: StandaloneFunc) {
        self.item(WorldItem::function_import(value));
    }
    pub fn function_export(&mut self, value: StandaloneFunc) {
        self.item(WorldItem::function_export(value));
    }
    pub fn include(&mut self, include: Include) {
        self.item(WorldItem::Include(include));
    }

    pub fn uses(&self) -> &[Use] {
        &self.uses
    }
    pub fn uses_mut(&mut self) -> &mut [Use] {
        &mut self.uses
    }
    pub fn use_(&mut self, use_: Use) {
        self.uses.push(use_);
    }
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

    pub fn docs(&self) -> Option<&Docs> {
        self.docs.as_ref()
    }

    /// Set the documentation
    pub fn set_docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

impl Render for World {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
        fn import(f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
            write!(f, "{}import ", opts.spaces())
        }
        fn export(f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
            write!(f, "{}export ", opts.spaces())
        }
        fn render_function(
            f: &mut fmt::Formatter<'_>,
            _opts: &RenderOpts,
            func: &StandaloneFunc,
        ) -> fmt::Result {
            let opt_async = if func.async_ { "async " } else { "" };
            write!(f, "{}: {opt_async}func({})", func.name, func.params)?;
            if let Some(ty) = &func.result {
                write!(f, " -> {ty}")?;
            }
            write!(f, ";\n")?;
            Ok(())
        }
        write!(f, "{}world {} {{\n", opts.spaces(), self.name)?;
        let opts = &opts.indent();
        self.uses.render(f, opts)?;
        for item in &self.items {
            match item {
                WorldItem::InlineInterfaceImport(interface) => {
                    if let Some(docs) = &interface.docs {
                        docs.render(f, opts)?;
                    }
                    import(f, opts)?;
                    write!(f, "{}: interface {{", interface.name)?;
                    if !interface.uses.is_empty() || !interface.items.is_empty() {
                        write!(f, "\n")?;
                        interface.uses.render(f, &opts.indent())?;
                        interface.items.render(f, &opts.indent())?;
                        write!(f, "{}}}\n", opts.spaces())?;
                    } else {
                        write!(f, "}}\n")?;
                    }
                }
                WorldItem::InlineInterfaceExport(interface) => {
                    if let Some(docs) = &interface.docs {
                        docs.render(f, opts)?;
                    }
                    export(f, opts)?;
                    write!(f, "{}: interface {{", interface.name)?;
                    if !interface.items.is_empty() {
                        write!(f, "\n")?;
                        interface.items.render(f, &opts.indent())?;
                        write!(f, "{}}}\n", opts.spaces())?;
                    } else {
                        write!(f, "}}\n")?;
                    }
                }
                WorldItem::NamedInterfaceImport(interface) => {
                    if let Some(docs) = &interface.docs {
                        docs.render(f, opts)?;
                    }
                    import(f, opts)?;
                    write!(f, "{};\n", interface.name)?;
                }
                WorldItem::NamedInterfaceExport(interface) => {
                    if let Some(docs) = &interface.docs {
                        docs.render(f, opts)?;
                    }
                    export(f, opts)?;
                    write!(f, "{};\n", interface.name)?;
                }
                WorldItem::FunctionImport(function) => {
                    if let Some(docs) = &function.docs {
                        docs.render(f, opts)?;
                    }
                    import(f, opts)?;
                    render_function(f, opts, function)?;
                }
                WorldItem::FunctionExport(function) => {
                    if let Some(docs) = &function.docs {
                        docs.render(f, opts)?;
                    }
                    export(f, opts)?;
                    render_function(f, opts, function)?;
                }
                WorldItem::Include(include) => include.render(f, opts)?,
            }
        }
        let opts = &opts.outdent();
        write!(f, "{}}}\n", opts.spaces())?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub enum WorldItem {
    /// An imported inline interface
    InlineInterfaceImport(Interface),

    /// An exported inline interface
    InlineInterfaceExport(Interface),

    /// Refers to a named interface import
    NamedInterfaceImport(WorldNamedInterface),

    /// Refers to a named interface export
    NamedInterfaceExport(WorldNamedInterface),

    /// A function is being directly imported from this world.
    FunctionImport(StandaloneFunc),

    /// A function is being directly exported from this world.
    FunctionExport(StandaloneFunc),

    /// Include type
    Include(Include),
}

impl WorldItem {
    pub fn inline_interface_import(value: Interface) -> Self {
        Self::InlineInterfaceImport(value)
    }
    pub fn inline_interface_export(value: Interface) -> Self {
        Self::InlineInterfaceExport(value)
    }
    pub fn named_interface_import(value: impl Into<WorldNamedInterface>) -> Self {
        Self::NamedInterfaceImport(value.into())
    }
    pub fn named_interface_export(value: impl Into<WorldNamedInterface>) -> Self {
        Self::NamedInterfaceExport(value.into())
    }
    pub fn function_import(value: StandaloneFunc) -> Self {
        Self::FunctionImport(value)
    }
    pub fn function_export(value: StandaloneFunc) -> Self {
        Self::FunctionExport(value)
    }
    pub fn include(value: impl Into<Ident>) -> Self {
        Self::Include(Include::new(value))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct WorldNamedInterface {
    /// Name of this interface.
    pub(crate) name: Ident,

    /// Documentation associated with this interface.
    pub(crate) docs: Option<Docs>,
}

impl<N> From<N> for WorldNamedInterface
where
    N: Into<Ident>,
{
    fn from(name: N) -> Self {
        Self::new(name)
    }
}

impl WorldNamedInterface {
    pub fn new(name: impl Into<Ident>) -> Self {
        Self {
            name: name.into(),
            docs: None,
        }
    }

    pub fn set_name(&mut self, name: impl Into<Ident>) {
        self.name = name.into();
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn set_docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }

    pub fn docs(&self) -> Option<&Docs> {
        self.docs.as_ref()
    }
}
