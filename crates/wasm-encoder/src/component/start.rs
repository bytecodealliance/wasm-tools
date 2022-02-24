use crate::{encoders, ComponentSection, ComponentSectionId};

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
