use super::{AdapterModuleSection, IndexRef, SectionId};
use crate::encoders;

const INSTANCE_KIND_INSTANTIATION: u8 = 0x00;
const INSTANCE_KIND_EXPORTS: u8 = 0x01;

/// An encoder for the adapter module instance section.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::adapter::{AdapterModule, InstanceSection, IndexRef};
///
/// // This assumes there is a module with index 0, a function with index 0,
/// // a module with index 2, and a global with index 0.
/// let mut instances = InstanceSection::new();
/// instances.instantiate(0, [
///     ("x", IndexRef::Function(0)),
///     ("", IndexRef::Module(2)),
///     ("foo", IndexRef::Global(0)),
/// ]);
///
/// let mut module = AdapterModule::new();
/// module.section(&instances);
///
/// let bytes = module.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct InstanceSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl InstanceSection {
    /// Create a new adapter module instance section encoder.
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

    /// Define an instantiation of the given module with the given
    /// arguments to the instantiation.
    pub fn instantiate<'a, I>(&mut self, module: u32, args: I) -> &mut Self
    where
        I: IntoIterator<Item = (&'a str, IndexRef)>,
        I::IntoIter: ExactSizeIterator,
    {
        let args = args.into_iter();

        self.bytes.push(INSTANCE_KIND_INSTANTIATION);
        self.bytes.extend(encoders::u32(module));

        self.bytes
            .extend(encoders::u32(u32::try_from(args.len()).unwrap()));
        for (name, index) in args {
            self.bytes.extend(encoders::str(name));
            index.encode(&mut self.bytes);
        }

        self
    }

    /// Define an instance by exporting the given exports.
    pub fn exports<'a, E>(&mut self, exports: E) -> &mut Self
    where
        E: IntoIterator<Item = (&'a str, IndexRef)>,
        E::IntoIter: ExactSizeIterator,
    {
        let exports = exports.into_iter();

        self.bytes.push(INSTANCE_KIND_EXPORTS);

        self.bytes
            .extend(encoders::u32(u32::try_from(exports.len()).unwrap()));
        for (name, index) in exports {
            self.bytes.extend(encoders::str(name));
            index.encode(&mut self.bytes);
        }

        self
    }
}

impl AdapterModuleSection for InstanceSection {
    fn id(&self) -> u8 {
        SectionId::Instance.into()
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
