use crate::{encoders, Component, ComponentSection, ComponentSectionId};

/// An encoder for the component section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, NestedComponentSection};
///
/// let mut component = Component::new();
/// component.section(&NestedComponentSection(Component::new()));
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct NestedComponentSection(pub Component);

impl ComponentSection for NestedComponentSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Component.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        sink.extend(
            encoders::u32(u32::try_from(self.0.bytes.len()).unwrap())
                .chain(self.0.bytes.iter().copied()),
        );
    }
}
