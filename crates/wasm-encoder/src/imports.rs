use crate::{
    encoders, AdapterModuleSectionId, ComponentSectionId, ModuleSectionId, Section,
    SectionEncodingFormat, TypeRef,
};

/// An encoder for the import section.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{MemoryType, Module, ImportSection, SectionEncodingFormat};
///
/// let mut imports = ImportSection::new(SectionEncodingFormat::Module);
/// imports.import(
///     Some("env"),
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
#[derive(Clone, Debug)]
pub struct ImportSection {
    bytes: Vec<u8>,
    num_added: u32,
    format: SectionEncodingFormat,
}

impl ImportSection {
    /// Create a new import section encoder.
    pub fn new(format: SectionEncodingFormat) -> Self {
        Self {
            bytes: Vec::new(),
            num_added: 0,
            format,
        }
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
    /// When encoding modules, the import's module name is required.
    /// When encoding adapter modules and components, the import's module name must be `None`.
    pub fn import(
        &mut self,
        module: Option<&str>,
        name: &str,
        ty: impl Into<TypeRef>,
    ) -> &mut Self {
        match self.format {
            SectionEncodingFormat::Module => {
                // The module name is required
                let module = module.expect("module name is required for module encoding");
                self.bytes.extend(encoders::str(module));
                self.bytes.extend(encoders::str(name));
            }
            SectionEncodingFormat::AdapterModule | SectionEncodingFormat::Component => {
                assert!(
                    module.is_none(),
                    "module name cannot be provided for adapter module or component encoding"
                );
                self.bytes.extend(encoders::str(name));
            }
        }
        ty.into().encode(self.format, &mut self.bytes);
        self.num_added += 1;
        self
    }

    fn encode(&self, format: SectionEncodingFormat, sink: &mut impl Extend<u8>) {
        assert_eq!(self.format, format, "import section format mismatch");
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}

impl Section<ModuleSectionId> for ImportSection {
    fn id(&self) -> ModuleSectionId {
        ModuleSectionId::Import
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::Module, sink);
    }
}

impl Section<AdapterModuleSectionId> for ImportSection {
    fn id(&self) -> AdapterModuleSectionId {
        AdapterModuleSectionId::Import
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::AdapterModule, sink);
    }
}

impl Section<ComponentSectionId> for ImportSection {
    fn id(&self) -> ComponentSectionId {
        ComponentSectionId::Import
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::Component, sink);
    }
}
