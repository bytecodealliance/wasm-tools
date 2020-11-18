use super::*;

/// An encoder for the element section.
///
/// # Example
///
/// ```
/// use wasm_encoder::{
///     ElementSection, Instruction, Module, TableSection, TableType, Limits,
/// };
///
/// let mut tables = TableSection::new();
/// tables.table(TableType {
///     limits: Limits {
///         min: 128,
///         max: None,
///     },
/// });
///
/// let mut elements = ElementSection::new();
/// let table_index = 0;
/// let offset = Instruction::I32Const(42);
/// let segment_elems = vec![
///     // Function indices...
/// ];
/// elements.active(table_index, offset, segment_elems);
///
/// let mut module = Module::new();
/// module
///     .section(&tables)
///     .section(&elements);
///
/// let wasm_bytes = module.finish();
/// ```
pub struct ElementSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ElementSection {
    /// Create a new element section encoder.
    pub fn new() -> ElementSection {
        ElementSection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Define an active element segment.
    pub fn active<E>(&mut self, table_index: u32, offset: Instruction, elements: E) -> &mut Self
    where
        E: IntoIterator<Item = u32>,
        E::IntoIter: ExactSizeIterator,
    {
        self.bytes.extend(encoders::u32(table_index));

        offset.encode(&mut self.bytes);
        Instruction::End.encode(&mut self.bytes);

        let elements = elements.into_iter();
        self.bytes
            .extend(encoders::u32(u32::try_from(elements.len()).unwrap()));
        for elem in elements {
            self.bytes.extend(encoders::u32(elem));
        }

        self.num_added += 1;
        self
    }
}

impl Section for ElementSection {
    fn id(&self) -> u8 {
        SectionId::Element as u8
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
