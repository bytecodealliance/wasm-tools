use crate::{encode_section, Encode, Section, SectionId};

/// Represents an export from a WebAssembly module.
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
}

impl Encode for Export {
    fn encode(&self, sink: &mut Vec<u8>) {
        let (kind, index) = match self {
            Self::Function(i) => (0x00, *i),
            Self::Table(i) => (0x01, *i),
            Self::Memory(i) => (0x02, *i),
            Self::Global(i) => (0x03, *i),
            Self::Tag(i) => (0x04, *i),
        };

        sink.push(kind);
        index.encode(sink);
    }
}

/// An encoder for the export section of WebAssembly module.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Module, ExportSection, Export};
///
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
        name.encode(&mut self.bytes);
        export.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for ExportSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, SectionId::Export, self.num_added, &self.bytes);
    }
}

impl Section for ExportSection {}
