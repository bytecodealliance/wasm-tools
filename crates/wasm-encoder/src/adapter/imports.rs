use super::{AdapterModuleSection, SectionId, TypeRef};
use crate::encoders;

/// An encoder for the adapter module import section.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{MemoryType, adapter::{AdapterModule, ImportSection}};
///
/// let mut imports = ImportSection::new();
/// imports.import(
///     "memory",
///     MemoryType {
///         minimum: 1,
///         maximum: None,
///         memory64: false,
///     }
/// );
///
/// let mut module = AdapterModule::new();
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
    /// Create a new adapter module import section encoder.
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
    pub fn import(&mut self, name: &str, ty: impl Into<TypeRef>) -> &mut Self {
        self.bytes.extend(encoders::str(name));
        ty.into().encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl AdapterModuleSection for ImportSection {
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
