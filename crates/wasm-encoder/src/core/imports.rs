use crate::{
    encode_section, encoders, Encode, GlobalType, MemoryType, Section, SectionId, TableType,
    TagType,
};

/// The type of an entity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntityType {
    /// A function type.
    ///
    /// The value is an index into the types section.
    Function(u32),
    /// A table type.
    Table(TableType),
    /// A memory type.
    Memory(MemoryType),
    /// A global type.
    Global(GlobalType),
    /// A tag type.
    ///
    /// This variant is used with the exception handling proposal.
    Tag(TagType),
}

impl Encode for EntityType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Self::Function(i) => {
                sink.push(0x00);
                sink.extend(encoders::u32(*i));
            }
            Self::Table(t) => {
                sink.push(0x01);
                t.encode(sink);
            }
            Self::Memory(t) => {
                sink.push(0x02);
                t.encode(sink);
            }
            Self::Global(t) => {
                sink.push(0x03);
                t.encode(sink);
            }
            Self::Tag(t) => {
                sink.push(0x04);
                t.encode(sink);
            }
        }
    }
}

impl From<TableType> for EntityType {
    fn from(t: TableType) -> Self {
        Self::Table(t)
    }
}

impl From<MemoryType> for EntityType {
    fn from(t: MemoryType) -> Self {
        Self::Memory(t)
    }
}

impl From<GlobalType> for EntityType {
    fn from(t: GlobalType) -> Self {
        Self::Global(t)
    }
}

impl From<TagType> for EntityType {
    fn from(t: TagType) -> Self {
        Self::Tag(t)
    }
}

/// An encoder for the import section of WebAssembly modules.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{MemoryType, Module, ImportSection};
///
/// let mut imports = ImportSection::new();
/// imports.import(
///     "env",
///     "memory",
///     MemoryType {
///         minimum: 1,
///         maximum: None,
///         memory64: false,
///     }
/// );
///
/// let mut module = Module::new();
/// module.section(&imports);
///
/// let bytes = module.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ImportSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ImportSection {
    /// Create a new import section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of imports in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define an import in the import section.
    pub fn import(&mut self, module: &str, field: &str, ty: impl Into<EntityType>) -> &mut Self {
        self.bytes.extend(encoders::str(module));
        self.bytes.extend(encoders::str(field));
        ty.into().encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for ImportSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, SectionId::Import, self.num_added, &self.bytes);
    }
}

impl Section for ImportSection {}
