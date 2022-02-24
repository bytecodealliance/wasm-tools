use crate::{encoders, ComponentArg, ComponentSection, ComponentSectionId};

/// Represents an export for a WebAssembly component.
pub type ComponentExport<'a> = ComponentArg<'a>;

/// An encoder for the export section of WebAssembly component.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, ComponentExportSection, ComponentExport};
///
/// // This exports an instance named "foo" that exports a function named "bar".
/// let mut exports = ComponentExportSection::new();
/// exports.export("foo", [("bar", ComponentExport::Function(0))]);
///
/// let mut component = Component::new();
/// component.section(&exports);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ComponentExportSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ComponentExportSection {
    /// Create a new component export section encoder.
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
    pub fn export<'a>(&mut self, name: &str, export: impl Into<ComponentExport<'a>>) -> &mut Self {
        self.bytes.extend(encoders::str(name));
        export.into().encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl ComponentSection for ComponentExportSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Export.into()
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
