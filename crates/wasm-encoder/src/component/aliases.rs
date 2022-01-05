use super::{ComponentSection, SectionId};
use crate::encoders;

const ALIAS_KIND_INSTANCE_EXPORT: u8 = 0x00;
pub(crate) const ALIAS_KIND_OUTER: u8 = 0x01;
pub(crate) const ALIAS_KIND_OUTER_MODULE: u8 = 0x01;
pub(crate) const ALIAS_KIND_OUTER_TYPE: u8 = 0x06;

/// Represents the expected export kind for an alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportKind {
    /// The alias is to an instance.
    Instance = 0x00,
    /// The alias is to a module.
    Module = 0x01,
    /// The alias is to a function.
    Function = 0x02,
    /// The alias is to a table.
    Table = 0x03,
    /// The alias is to a memory.
    Memory = 0x04,
    /// The alias is to a global.
    Global = 0x05,
    /// The alias is to an adapter function.
    AdapterFunction = 0x06,
}

/// An encoder for the component alias section.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::component::{Component, AliasSection, ExportKind};
///
/// let mut aliases = AliasSection::new();
/// aliases.outer_type(0, 2);
/// aliases.instance_export(0, ExportKind::Function, "foo");
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
    /// Create a new component alias section encoder.
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
    pub fn instance_export(&mut self, instance: u32, kind: ExportKind, name: &str) -> &mut Self {
        self.bytes.push(ALIAS_KIND_INSTANCE_EXPORT);
        self.bytes.extend(encoders::u32(instance));
        self.bytes.extend(encoders::str(name));
        self.bytes.push(kind as u8);
        self.num_added += 1;
        self
    }

    /// Define an alias that references an outer module's type.
    pub fn outer_type(&mut self, count: u32, ty: u32) -> &mut Self {
        self.bytes.push(ALIAS_KIND_OUTER);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(ty));
        self.bytes.push(ALIAS_KIND_OUTER_TYPE);
        self.num_added += 1;
        self
    }

    /// Define an alias that references an outer module's module.
    pub fn outer_module(&mut self, count: u32, module: u32) -> &mut Self {
        self.bytes.push(ALIAS_KIND_OUTER);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(module));
        self.bytes.push(ALIAS_KIND_OUTER_MODULE);
        self.num_added += 1;
        self
    }
}

impl ComponentSection for AliasSection {
    fn id(&self) -> u8 {
        SectionId::Alias.into()
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
