use crate::{encoders, ComponentSection, ComponentSectionId, Encode, Module};

/// An encoder for the module section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Module, Component, ModuleSection};
///
/// let mut module = Module::new();
/// let mut component = Component::new();
/// component.section(&ModuleSection(&module));
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug)]
pub struct ModuleSection<'a>(pub &'a Module);

impl Encode for ModuleSection<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(ComponentSectionId::Module.into());
        sink.extend(encoders::u32(u32::try_from(self.0.bytes.len()).unwrap()));
        sink.extend(&self.0.bytes);
    }
}

impl ComponentSection for ModuleSection<'_> {}
