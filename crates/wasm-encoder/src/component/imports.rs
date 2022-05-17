use crate::{encode_section, encoders, ComponentSection, ComponentSectionId, Encode};

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

impl Encode for ComponentImportSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(
            sink,
            ComponentSectionId::Import,
            self.num_added,
            &self.bytes,
        );
    }
}

impl ComponentSection for ComponentImportSection {}
