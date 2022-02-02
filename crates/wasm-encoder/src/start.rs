use super::*;

/// An encoder for the start section of WebAssembly modules.
///
/// # Example
///
/// Note: this doesn't actually define the function at index 0, its type, or its
/// code body, so the resulting Wasm module will be invalid. See `TypeSection`,
/// `FunctionSection`, and `CodeSection` for details on how to generate those
/// things.
///
/// ```
/// use wasm_encoder::{Module, StartSection};
///
/// let start = StartSection { function_index: 0 };
///
/// let mut module = Module::new();
/// module.section(&start);
///
/// let wasm_bytes = module.finish();
/// ```
#[derive(Clone, Copy, Debug)]
pub struct StartSection {
    /// The index of the start function.
    pub function_index: u32,
}

impl Section for StartSection {
    fn id(&self) -> u8 {
        SectionId::Start.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        let f = encoders::u32(self.function_index);
        let n = f.len();
        sink.extend(encoders::u32(n as u32).chain(f));
    }
}

/// An encoder for the start section of WebAssembly components.
///
/// # Example
///
/// ```
/// use wasm_encoder::{Component, ComponentStartSection};
///
/// let start = ComponentStartSection { function_index: 0, args: [] };
///
/// let mut component = Component::new();
/// component.section(&start);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug)]
pub struct ComponentStartSection<A> {
    /// The index to the start function.
    pub function_index: u32,
    /// The arguments to pass to the start function.
    ///
    /// An argument is an index to a value.
    pub args: A,
}

impl<A> ComponentSection for ComponentStartSection<A>
where
    A: AsRef<[u32]>,
{
    fn id(&self) -> u8 {
        ComponentSectionId::Start.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        let args = self.args.as_ref();

        let mut bytes = Vec::new();
        bytes.extend(encoders::u32(self.function_index));

        bytes.extend(encoders::u32(u32::try_from(args.len()).unwrap()));
        for arg in args {
            bytes.extend(encoders::u32(*arg));
        }

        let n = bytes.len();
        sink.extend(encoders::u32(n as u32).chain(bytes));
    }
}
