use crate::{
    encode_section, ComponentExportKind, ComponentSection, ComponentSectionId, Encode,
    CORE_FUNCTION_SORT, CORE_GLOBAL_SORT, CORE_INSTANCE_SORT, CORE_MEMORY_SORT, CORE_MODULE_SORT,
    CORE_TABLE_SORT, CORE_TYPE_SORT,
};

use super::{COMPONENT_SORT, CORE_SORT, FUNCTION_SORT, INSTANCE_SORT, TYPE_SORT, VALUE_SORT};

/// Represents the kinds of aliasable core items.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CoreAliasKind {
    /// The alias is to a core function.
    Func,
    /// The alias is to a table.
    Table,
    /// The alias is to a memory.
    Memory,
    /// The alias is to a global.
    Global,
    /// The alias is to a core type.
    Type,
    /// The alias is to a core module.
    Module,
    /// The alias is to a core instance.
    Instance,
}

impl Encode for CoreAliasKind {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Self::Func => sink.push(CORE_FUNCTION_SORT),
            Self::Table => sink.push(CORE_TABLE_SORT),
            Self::Memory => sink.push(CORE_MEMORY_SORT),
            Self::Global => sink.push(CORE_GLOBAL_SORT),
            Self::Type => sink.push(CORE_TYPE_SORT),
            Self::Module => sink.push(CORE_MODULE_SORT),
            Self::Instance => sink.push(CORE_INSTANCE_SORT),
        }
    }
}

/// Represents the kinds of aliasable component items.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ComponentAliasKind {
    /// The alias is to a core item.
    Core(CoreAliasKind),
    /// The alias is to a function.
    Func,
    /// The alias is to a value.
    Value,
    /// The alias is to a type.
    Type,
    /// The alias is to a component.
    Component,
    /// The alias is to an instance.
    Instance,
}

impl Encode for ComponentAliasKind {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Self::Core(kind) => {
                sink.push(CORE_SORT);
                kind.encode(sink);
            }
            Self::Func => sink.push(FUNCTION_SORT),
            Self::Value => sink.push(VALUE_SORT),
            Self::Type => sink.push(TYPE_SORT),
            Self::Component => sink.push(COMPONENT_SORT),
            Self::Instance => sink.push(INSTANCE_SORT),
        }
    }
}

/// An encoder for the alias section of WebAssembly component.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, ComponentAliasSection, ComponentExportKind, ComponentAliasKind};
///
/// let mut aliases = ComponentAliasSection::new();
/// aliases.instance_export(0, ComponentExportKind::Func, "f");
/// aliases.outer(0, ComponentAliasKind::Instance, 1);
///
/// let mut component = Component::new();
/// component.section(&aliases);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ComponentAliasSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ComponentAliasSection {
    /// Create a new alias section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of aliases in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define an alias to an instance's export.
    pub fn instance_export(
        &mut self,
        instance_index: u32,
        kind: ComponentExportKind,
        name: &str,
    ) -> &mut Self {
        kind.encode(&mut self.bytes);
        self.bytes.push(0x00);
        instance_index.encode(&mut self.bytes);
        name.encode(&mut self.bytes);
        self
    }

    /// Define an alias to an outer component item.
    ///
    /// The count starts at 0 to indicate the current component, 1 indicates the direct
    /// parent, 2 the grandparent, etc.
    pub fn outer(&mut self, count: u32, kind: ComponentAliasKind, index: u32) -> &mut Self {
        kind.encode(&mut self.bytes);
        self.bytes.push(0x01);
        count.encode(&mut self.bytes);
        index.encode(&mut self.bytes);
        self
    }
}

impl Encode for ComponentAliasSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl ComponentSection for ComponentAliasSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Alias.into()
    }
}
