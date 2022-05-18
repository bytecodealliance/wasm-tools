use crate::{encode_section, encoders, ComponentSection, ComponentSectionId, Encode};

/// Represents the expected export kind for an alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AliasExportKind {
    /// The alias is to a module.
    Module,
    /// The alias is to a component.
    Component,
    /// The alias is to an instance.
    Instance,
    /// The alias is to a component function.
    ComponentFunction,
    /// The alias is to a value.
    Value,
    /// The alias is to a core WebAssembly function.
    Function,
    /// The alias is to a table.
    Table,
    /// The alias is to a memory.
    Memory,
    /// The alias is to a global.
    Global,
    /// The alias is to a tag.
    Tag,
}

impl Encode for AliasExportKind {
    fn encode(&self, sink: &mut Vec<u8>) {
        let (preamble, value) = match self {
            AliasExportKind::Module => (0x00, 0x00),
            AliasExportKind::Component => (0x00, 0x01),
            AliasExportKind::Instance => (0x00, 0x02),
            AliasExportKind::ComponentFunction => (0x00, 0x03),
            AliasExportKind::Value => (0x00, 0x04),
            AliasExportKind::Function => (0x01, 0x00),
            AliasExportKind::Table => (0x01, 0x01),
            AliasExportKind::Memory => (0x01, 0x02),
            AliasExportKind::Global => (0x01, 0x03),
            AliasExportKind::Tag => (0x01, 0x04),
        };

        sink.push(preamble);
        sink.push(value);
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
        self.bytes.push(0x02);
        self.bytes.push(0x05);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(index));
        self.num_added += 1;
        self
    }

    /// Define an alias to an outer module.
    ///
    /// The count starts at 0 to represent the current component.
    pub fn outer_module(&mut self, count: u32, index: u32) -> &mut Self {
        self.bytes.push(0x02);
        self.bytes.push(0x00);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(index));
        self.num_added += 1;
        self
    }

    /// Define an alias to an outer component.
    ///
    /// The count starts at 0 to represent the current component.
    pub fn outer_component(&mut self, count: u32, index: u32) -> &mut Self {
        self.bytes.push(0x02);
        self.bytes.push(0x01);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(index));
        self.num_added += 1;
        self
    }
}

impl Encode for AliasSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, ComponentSectionId::Alias, self.num_added, &self.bytes);
    }
}

impl ComponentSection for AliasSection {}
