use super::{AdapterModuleSection, IndexRef, SectionId};
use crate::encoders;

/// An encoder for the adapter module export section.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::adapter::{AdapterModule, ExportSection, IndexRef};
///
/// // This assumes there is a function at index 0 to export
/// let mut exports = ExportSection::new();
/// exports.export("foo", IndexRef::Function(0));
///
/// let mut module = AdapterModule::new();
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
    /// Create a new adapter module export section encoder.
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
    pub fn export(&mut self, name: &str, index_ref: IndexRef) -> &mut Self {
        self.bytes.extend(encoders::str(name));
        index_ref.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl AdapterModuleSection for ExportSection {
    fn id(&self) -> u8 {
        SectionId::Export.into()
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
