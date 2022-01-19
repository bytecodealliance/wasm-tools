use crate::{
    encoders, AdapterModuleSectionId, ComponentSectionId, ModuleSectionId, Section,
    SectionEncodingFormat,
};

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
    /// This variant is used for adapter modules and components.
    Instance(u32),
    /// The export is a module.
    ///
    /// This variant is used for adapter modules and components.
    Module(u32),
    /// The export is an adapter function.
    ///
    /// This variant is used for components.
    AdapterFunction(u32),
}

impl Export {
    pub(crate) fn encode(&self, format: SectionEncodingFormat, bytes: &mut Vec<u8>) {
        let (ty, index) = match format {
            SectionEncodingFormat::Module => match self {
                Self::Function(i) => (0x00, *i),
                Self::Table(i) => (0x01, *i),
                Self::Memory(i) => (0x02, *i),
                Self::Global(i) => (0x03, *i),
                Self::Tag(i) => (0x04, *i),
                _ => panic!("cannot encode {:?} for a WebAssembly module", self),
            },
            SectionEncodingFormat::AdapterModule => match self {
                Self::Instance(i) => (0x00, *i),
                Self::Module(i) => (0x01, *i),
                Self::Function(i) => (0x02, *i),
                Self::Table(i) => (0x03, *i),
                Self::Memory(i) => (0x04, *i),
                Self::Global(i) => (0x05, *i),
                _ => panic!("cannot encode {:?} for a WebAssembly adapter module", self),
            },
            SectionEncodingFormat::Component => match self {
                Self::Instance(i) => (0x00, *i),
                Self::Module(i) => (0x01, *i),
                Self::Function(i) => (0x02, *i),
                Self::Table(i) => (0x03, *i),
                Self::Memory(i) => (0x04, *i),
                Self::Global(i) => (0x05, *i),
                Self::AdapterFunction(i) => (0x06, *i),
                _ => panic!("cannot encode {:?} for a WebAssembly component", self),
            },
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
/// use wasm_encoder::{Module, ExportSection, SectionEncodingFormat, Export};
///
/// // This assumes there is a function at index 0 to export
/// let mut exports = ExportSection::new(SectionEncodingFormat::Module);
/// exports.export("foo", Export::Function(0));
///
/// let mut module = Module::new();
/// module.section(&exports);
///
/// let bytes = module.finish();
/// ```
#[derive(Clone, Debug)]
pub struct ExportSection {
    bytes: Vec<u8>,
    num_added: u32,
    format: SectionEncodingFormat,
}

impl ExportSection {
    /// Create a new export section encoder.
    pub fn new(format: SectionEncodingFormat) -> Self {
        Self {
            bytes: Vec::new(),
            num_added: 0,
            format,
        }
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
        self.bytes.extend(encoders::str(name));
        export.encode(self.format, &mut self.bytes);
        self.num_added += 1;
        self
    }

    fn encode(&self, format: SectionEncodingFormat, sink: &mut impl Extend<u8>) {
        assert_eq!(self.format, format, "export section format mismatch");
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}

impl Section<ModuleSectionId> for ExportSection {
    fn id(&self) -> ModuleSectionId {
        ModuleSectionId::Export
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::Module, sink);
    }
}

impl Section<AdapterModuleSectionId> for ExportSection {
    fn id(&self) -> AdapterModuleSectionId {
        AdapterModuleSectionId::Export
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::AdapterModule, sink);
    }
}

impl Section<ComponentSectionId> for ExportSection {
    fn id(&self) -> ComponentSectionId {
        ComponentSectionId::Export
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::Component, sink);
    }
}
