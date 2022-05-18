use crate::{encoders, Component, ComponentSection, ComponentSectionId, Encode};

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
        sink.push(ComponentSectionId::Component.into());
        sink.extend(encoders::u32(u32::try_from(self.0.bytes.len()).unwrap()));
        sink.extend(&self.0.bytes);
    }
}

impl ComponentSection for NestedComponentSection<'_> {}
