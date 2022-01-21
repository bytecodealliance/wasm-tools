use crate::{encoders, ComponentSection, ComponentSectionId, EncodingFormat, Section, SectionId};

/// Represents an export of a local item (by index).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Export {
    /// The export is a function.
    Function(u32),
    /// The export is a table.
    Table(u32),
    /// The export is a memory.
    Memory(u32),
    /// The export is a global.
    Global(u32),
    /// The export is a tag.
    ///
    /// This variant is used with the exception handling proposal.
    Tag(u32),
    /// The export is an instance.
    ///
    /// This variant is used for components.
    Instance(u32),
    /// The export is a module.
    ///
    /// This variant is used for components.
    Module(u32),
    /// The export is an adapter function.
    ///
    /// This variant is used for components.
    AdapterFunction(u32),
}

impl Export {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        let (ty, index) = match self {
            Self::Function(i) => (0x00, *i),
            Self::Table(i) => (0x01, *i),
            Self::Memory(i) => (0x02, *i),
            Self::Global(i) => (0x03, *i),
            Self::Tag(i) => (0x04, *i),
            Self::Instance(i) => (0x05, *i),
            Self::Module(i) => (0x06, *i),
            Self::AdapterFunction(i) => (0x07, *i),
        };

        bytes.push(ty);
        bytes.extend(encoders::u32(index));
    }
}

/// An encoder for the export section.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Module, ExportSection, Export};
///
/// // This assumes there is a function at index 0 to export
/// let mut exports = ExportSection::new();
/// exports.export("foo", Export::Function(0));
///
/// let mut module = Module::new();
/// module.section(&exports);
///
/// let bytes = module.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ExportSection {
    bytes: Vec<u8>,
    num_added: u32,
    uses_component_exports: bool,
}

impl ExportSection {
    /// Create a new export section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of exports in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define an export in the export section.
    pub fn export(&mut self, name: &str, export: Export) -> &mut Self {
        self.uses_component_exports |= match export {
            Export::Function(_)
            | Export::Table(_)
            | Export::Memory(_)
            | Export::Global(_)
            | Export::Tag(_) => false,
            Export::Instance(_) | Export::Module(_) | Export::AdapterFunction(_) => true,
        };

        self.bytes.extend(encoders::str(name));
        export.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    fn encode(&self, expected: EncodingFormat, sink: &mut impl Extend<u8>) {
        if self.uses_component_exports {
            assert_eq!(
                expected,
                EncodingFormat::Component,
                "export section format mismatch"
            );
        }

        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}

impl Section for ExportSection {
    fn id(&self) -> u8 {
        SectionId::Export.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(EncodingFormat::Module, sink);
    }
}

impl ComponentSection for ExportSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Export.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(EncodingFormat::Component, sink);
    }
}
