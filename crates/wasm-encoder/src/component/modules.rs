use crate::{encoders, ComponentSection, ComponentSectionId, Module};

/// An encoder for the module section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Module, Component, ModuleSection};
///
/// let mut component = Component::new();
/// component.section(&ModuleSection(Module::new()));
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ModuleSection(pub Module);

impl ComponentSection for ModuleSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Module.into()
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
