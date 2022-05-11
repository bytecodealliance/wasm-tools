use crate::{encode_section, ComponentExport, ComponentSection, ComponentSectionId, Encode};

/// An encoder for the instance section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, ComponentInstanceSection, ComponentExport};
///
/// let mut instances = ComponentInstanceSection::new();
/// instances.export_items([("foo", ComponentExport::Func(0))]);
/// instances.instantiate(1, [("foo", ComponentExport::Instance(0))]);
///
/// let mut component = Component::new();
/// component.section(&instances);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ComponentInstanceSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ComponentInstanceSection {
    /// Create a new instance section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of instances in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define an instance by instantiating a component.
    pub fn instantiate<'a, A>(&mut self, component_index: u32, args: A) -> &mut Self
    where
        A: IntoIterator<Item = (&'a str, ComponentExport)>,
        A::IntoIter: ExactSizeIterator,
    {
        let args = args.into_iter();
        self.bytes.push(0x00);
        component_index.encode(&mut self.bytes);
        args.len().encode(&mut self.bytes);
        for (name, export) in args {
            name.encode(&mut self.bytes);
            export.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Define an instance by exporting items.
    pub fn export_items<'a, E>(&mut self, exports: E) -> &mut Self
    where
        E: IntoIterator<Item = (&'a str, ComponentExport)>,
        E::IntoIter: ExactSizeIterator,
    {
        let exports = exports.into_iter();
        self.bytes.push(0x01);
        exports.len().encode(&mut self.bytes);
        for (name, export) in exports {
            name.encode(&mut self.bytes);
            export.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }
}

impl Encode for ComponentInstanceSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl ComponentSection for ComponentInstanceSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Instance.into()
    }
}
