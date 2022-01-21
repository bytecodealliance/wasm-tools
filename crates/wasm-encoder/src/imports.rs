use crate::{encoders, ComponentSection, ComponentSectionId, Section, SectionId, TypeRef};

/// An encoder for the import section.
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
    ///
    /// This method is only supported for encoding modules.
    pub fn import(&mut self, module: &str, field: &str, ty: impl Into<TypeRef>) -> &mut Self {
        self.bytes.extend(encoders::str(module));
        self.bytes.extend(encoders::str(field));
        ty.into().encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Define an import in the import section.
    ///
    /// This method is only supported for encoding components.
    pub fn import_by_name(&mut self, name: &str, ty: impl Into<TypeRef>) -> &mut Self {
        self.bytes.extend(encoders::str(name));
        ty.into().encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    fn encode(&self, sink: &mut impl Extend<u8>) {
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
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
        self.encode(sink);
    }
}

impl ComponentSection for ImportSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Import.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(sink);
    }
}
