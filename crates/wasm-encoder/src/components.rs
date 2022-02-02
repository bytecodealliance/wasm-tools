use crate::{encoders, Component, ComponentSection, ComponentSectionId};

/// An encoder for the component section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, NestedComponentSection};
///
/// let mut components = NestedComponentSection::new();
/// components.component(&Component::new());
///
/// let mut component = Component::new();
/// component.section(&components);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct NestedComponentSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl NestedComponentSection {
    /// Create a new component section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of components in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Writes a component into this component section.
    ///
    /// The given component must be fully formed.
    pub fn component(&mut self, component: &Component) -> &mut Self {
        self.bytes.extend(
            encoders::u32(u32::try_from(component.bytes.len()).unwrap())
                .chain(component.bytes.iter().copied()),
        );
        self.num_added += 1;
        self
    }
}

impl ComponentSection for NestedComponentSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Component.into()
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
