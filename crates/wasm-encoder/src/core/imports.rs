use crate::{
    encoders, ComponentSection, ComponentSectionId, GlobalType, MemoryType, Section, SectionId,
    TableType, TagType,
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

impl EntityType {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::Function(i) => {
                bytes.push(0x00);
                bytes.extend(encoders::u32(*i));
            }
            Self::Table(t) => {
                bytes.push(0x01);
                t.encode(bytes);
            }
            Self::Memory(t) => {
                bytes.push(0x02);
                t.encode(bytes);
            }
            Self::Global(t) => {
                bytes.push(0x03);
                t.encode(bytes);
            }
            Self::Tag(t) => {
                bytes.push(0x04);
                t.encode(bytes);
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

impl Section for ImportSection {
    fn id(&self) -> u8 {
        SectionId::Import.into()
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

/// An encoder for the import section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, ComponentImportSection};
///
/// let mut imports = ComponentImportSection::new();
/// imports.import("f", 0);
///
/// let mut component = Component::new();
/// component.section(&imports);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ComponentImportSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ComponentImportSection {
    /// Create a new component import section encoder.
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

    /// Define an import in the component import section.
    pub fn import(&mut self, name: &str, ty: u32) -> &mut Self {
        self.bytes.extend(encoders::str(name));
        self.bytes.extend(encoders::u32(ty));
        self.num_added += 1;
        self
    }
}

impl ComponentSection for ComponentImportSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Import.into()
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
