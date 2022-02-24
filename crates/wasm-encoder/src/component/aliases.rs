use crate::{encoders, ComponentSection, ComponentSectionId};

const ALIAS_KIND_INSTANCE_EXPORT: u8 = 0x00;
const ALIAS_KIND_INSTANCE_CORE_EXPORT: u8 = 0x01;
pub(crate) const ALIAS_KIND_OUTER: u8 = 0x02;
const ALIAS_KIND_OUTER_MODULE: u8 = 0x00;
const ALIAS_KIND_OUTER_COMPONENT: u8 = 0x01;
pub(crate) const ALIAS_KIND_OUTER_TYPE: u8 = 0x05;

/// Represents the expected export kind for an alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AliasExportKind {
    /// The alias is to a module.
    Module,
    /// The alias is to a component.
    Component,
    /// The alias is to an instance.
    Instance,
    /// The alias is to a function.
    Function,
    /// The alias is to a value.
    Value,
    /// The alias is to a table.
    Table,
    /// The alias is to a memory.
    Memory,
    /// The alias is to a global.
    Global,
    /// The alias is to a tag.
    Tag,
}

impl AliasExportKind {
    fn encode(&self, bytes: &mut Vec<u8>) {
        let (preamble, value) = match self {
            AliasExportKind::Module => (ALIAS_KIND_INSTANCE_EXPORT, 0x00),
            AliasExportKind::Component => (ALIAS_KIND_INSTANCE_EXPORT, 0x01),
            AliasExportKind::Instance => (ALIAS_KIND_INSTANCE_EXPORT, 0x02),
            AliasExportKind::Function => (ALIAS_KIND_INSTANCE_EXPORT, 0x03),
            AliasExportKind::Value => (ALIAS_KIND_INSTANCE_EXPORT, 0x04),
            AliasExportKind::Table => (ALIAS_KIND_INSTANCE_CORE_EXPORT, 0x01),
            AliasExportKind::Memory => (ALIAS_KIND_INSTANCE_CORE_EXPORT, 0x02),
            AliasExportKind::Global => (ALIAS_KIND_INSTANCE_CORE_EXPORT, 0x03),
            AliasExportKind::Tag => (ALIAS_KIND_INSTANCE_CORE_EXPORT, 0x04),
        };

        bytes.push(preamble);
        bytes.push(value);
    }
}

/// An encoder for the alias section of WebAssembly component.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, AliasSection, AliasExportKind};
///
/// let mut aliases = AliasSection::new();
/// aliases.outer_type(0, 2);
/// aliases.instance_export(0, AliasExportKind::Function, "foo");
///
/// let mut component = Component::new();
/// component.section(&aliases);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct AliasSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl AliasSection {
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

    /// Define an alias that references the export of a defined instance.
    pub fn instance_export(
        &mut self,
        instance: u32,
        kind: AliasExportKind,
        name: &str,
    ) -> &mut Self {
        kind.encode(&mut self.bytes);
        self.bytes.extend(encoders::u32(instance));
        self.bytes.extend(encoders::str(name));
        self.num_added += 1;
        self
    }

    /// Define an alias to an outer type.
    ///
    /// The count starts at 0 to represent the current component.
    pub fn outer_type(&mut self, count: u32, index: u32) -> &mut Self {
        self.bytes.push(ALIAS_KIND_OUTER);
        self.bytes.push(ALIAS_KIND_OUTER_TYPE);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(index));
        self.num_added += 1;
        self
    }

    /// Define an alias to an outer module.
    ///
    /// The count starts at 0 to represent the current component.
    pub fn outer_module(&mut self, count: u32, index: u32) -> &mut Self {
        self.bytes.push(ALIAS_KIND_OUTER);
        self.bytes.push(ALIAS_KIND_OUTER_MODULE);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(index));
        self.num_added += 1;
        self
    }

    /// Define an alias to an outer component.
    ///
    /// The count starts at 0 to represent the current component.
    pub fn outer_component(&mut self, count: u32, index: u32) -> &mut Self {
        self.bytes.push(ALIAS_KIND_OUTER);
        self.bytes.push(ALIAS_KIND_OUTER_COMPONENT);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(index));
        self.num_added += 1;
        self
    }
}

impl ComponentSection for AliasSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Alias.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}
