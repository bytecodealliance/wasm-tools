use super::*;

/// An encoder for the memory section.
///
/// # Example
///
/// ```
/// use wasm_encoder::{Module, MemorySection, MemoryType, Limits};
///
/// let mut memories = MemorySection::new();
/// memories.memory(MemoryType {
///     limits: Limits {
///         min: 1,
///         max: None,
///     },
/// });
///
/// let mut module = Module::new();
/// module.section(&memories);
///
/// let wasm_bytes = module.finish();
/// ```
#[derive(Clone, Debug)]
pub struct MemorySection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl MemorySection {
    /// Create a new memory section encoder.
    pub fn new() -> MemorySection {
        MemorySection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Define a memory.
    pub fn memory(&mut self, memory_type: MemoryType) -> &mut Self {
        memory_type.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Section for MemorySection {
    fn id(&self) -> u8 {
        SectionId::Memory.into()
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

/// A memory's type.
#[derive(Clone, Copy, Debug)]
pub struct MemoryType {
    /// This memory's limits (in units of pages).
    pub limits: Limits,
}

impl MemoryType {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        self.limits.encode(bytes);
    }
}
