use crate::{encode_section, Encode, ExportKind};

/// An encoder for the core alias section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, AliasSection, ExportKind};
///
/// let mut aliases = AliasSection::new();
/// aliases.instance_export(0, ExportKind::Func, "f");
///
/// let mut component = Component::new();
/// component.section(&aliases);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct AliasSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl AliasSection {
    /// Create a new core alias section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of aliases in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define an alias to an instance's export.
    pub fn instance_export(
        &mut self,
        instance_index: u32,
        kind: ExportKind,
        name: &str,
    ) -> &mut Self {
        kind.encode(&mut self.bytes);
        self.bytes.push(0x00);
        instance_index.encode(&mut self.bytes);
        name.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for AliasSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}
