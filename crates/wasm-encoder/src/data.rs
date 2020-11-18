use super::*;

/// An encoder for the data section.
///
/// # Example
///
/// ```
/// use wasm_encoder::{
///     DataSection, Instruction, Limits, MemorySection, MemoryType,
///     Module,
/// };
///
/// let mut memory = MemorySection::new();
/// memory.memory(MemoryType {
///     limits: Limits {
///         min: 1,
///         max: None,
///     },
/// });
///
/// let mut data = DataSection::new();
/// let memory_index = 0;
/// let offset = Instruction::I32Const(42);
/// let segment_data = b"hello";
/// data.active(memory_index, offset, segment_data.iter().copied());
///
/// let mut module = Module::new();
/// module
///     .section(&memory)
///     .section(&data);
///
/// let wasm_bytes = module.finish();
/// ```
pub struct DataSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl DataSection {
    /// Create a new data section encoder.
    pub fn new() -> DataSection {
        DataSection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Define an active data segment.
    pub fn active<D>(&mut self, memory_index: u32, offset: Instruction, data: D) -> &mut Self
    where
        D: IntoIterator<Item = u8>,
        D::IntoIter: ExactSizeIterator,
    {
        self.bytes.extend(encoders::u32(memory_index));

        offset.encode(&mut self.bytes);
        Instruction::End.encode(&mut self.bytes);

        let data = data.into_iter();
        self.bytes
            .extend(encoders::u32(u32::try_from(data.len()).unwrap()));
        self.bytes.extend(data);

        self.num_added += 1;
        self
    }
}

impl Section for DataSection {
    fn id(&self) -> u8 {
        SectionId::Data as u8
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
