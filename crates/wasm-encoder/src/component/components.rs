use crate::{Component, ComponentSection, ComponentSectionId, Encode};

/// An encoder for the component section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, NestedComponentSection};
///
/// let mut nested = Component::new();
/// let mut component = Component::new();
/// component.section(&NestedComponentSection(&nested));
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug)]
pub struct NestedComponentSection<'a>(pub &'a Component);

impl Encode for NestedComponentSection<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        ComponentSectionId::Component.encode(sink);
        self.0.bytes.encode(sink);
    }
}

impl ComponentSection for NestedComponentSection<'_> {}
