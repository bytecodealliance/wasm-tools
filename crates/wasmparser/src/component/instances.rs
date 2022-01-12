use super::IndexRef;
use crate::{
    BinaryReader, BinaryReaderError, Range, Result, SectionIteratorLimited, SectionReader,
    SectionWithLimitedItems,
};

const INSTANCE_KIND_INSTANTIATION: u32 = 0x00;
const INSTANCE_KIND_EXPORTED: u32 = 0x01;

/// Represents an instance in the component instance section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instance<'a> {
    /// The instance is the result of instantiating a module.
    Instantiation {
        /// The index of the module being instantiated.
        module: u32,
        /// The imports to instantiate the module with.
        imports: Box<[(&'a str, IndexRef)]>,
    },
    /// The instance is the result of exporting local items.
    Exported(Box<[(&'a str, IndexRef)]>),
}

impl<'a> Instance<'a> {
    fn new(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read_u8()? {
            INSTANCE_KIND_INSTANTIATION => Instance::Instantiation {
                module: reader.read_var_u32()?,
                imports: (0..reader.read_var_u32()?)
                    .map(|_| Ok((reader.read_string()?, IndexRef::new(reader)?)))
                    .collect::<Result<_>>()?,
            },
            INSTANCE_KIND_EXPORTED => Instance::Exported(
                (0..reader.read_var_u32()?)
                    .map(|_| Ok((reader.read_string()?, IndexRef::new(reader)?)))
                    .collect::<Result<_>>()?,
            ),
            x => {
                return Err(BinaryReaderError::new(
                    format!("invalid byte (0x{:x}) in instance kind", x),
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// The instance section reader for a WebAssembly component.
#[derive(Clone)]
pub struct InstanceSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> InstanceSectionReader<'a> {
    /// Creates a new instance section reader for the given data and initial offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<Self> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(Self { reader, count })
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets the number of instances in the section.
    pub fn len(&self) -> u32 {
        self.count
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Reads instances from the instance section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::component::InstanceSectionReader;
    /// # let data: &[u8] = &[0x1, 0x0, 0x0, 0x1, 0x3, b'f', b'o', b'o', 0x2, 0x1];
    /// let instances = InstanceSectionReader::new(data, 0).unwrap();
    /// for instance in instances {
    ///     let instance = instance.expect("instance");
    ///     println!("{:?}", instance);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<Instance<'a>> {
        Instance::new(&mut self.reader)
    }
}

impl<'a> SectionReader for InstanceSectionReader<'a> {
    type Item = Instance<'a>;

    fn read(&mut self) -> Result<Self::Item> {
        InstanceSectionReader::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        InstanceSectionReader::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for InstanceSectionReader<'a> {
    fn get_count(&self) -> u32 {
        InstanceSectionReader::len(self)
    }
}

impl<'a> IntoIterator for InstanceSectionReader<'a> {
    type Item = Result<Instance<'a>>;
    type IntoIter = SectionIteratorLimited<InstanceSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::component::{
        INDEX_REF_ADAPTER_FUNCTION, INDEX_REF_FUNCTION, INDEX_REF_GLOBAL, INDEX_REF_INSTANCE,
        INDEX_REF_MEMORY, INDEX_REF_MODULE, INDEX_REF_TABLE,
    };
    use anyhow::Result;

    #[test]
    fn it_parses_an_empty_section() -> Result<()> {
        let data: &[u8] = &[0];
        let reader = InstanceSectionReader::new(data, 0)?;
        assert!(reader.eof());
        assert_eq!(reader.len(), 0);
        Ok(())
    }

    #[test]
    fn it_parses_an_instantiation() -> Result<()> {
        let data: &[u8] = &[
            1,
            INSTANCE_KIND_INSTANTIATION as u8,
            101,
            7,
            3,
            b'f',
            b'o',
            b'o',
            INDEX_REF_INSTANCE as u8,
            0,
            3,
            b'b',
            b'a',
            b'r',
            INDEX_REF_MODULE as u8,
            1,
            3,
            b'b',
            b'a',
            b'z',
            INDEX_REF_FUNCTION as u8,
            2,
            3,
            b'j',
            b'a',
            b'm',
            INDEX_REF_TABLE as u8,
            3,
            1,
            b'a',
            INDEX_REF_MEMORY as u8,
            4,
            1,
            b'b',
            INDEX_REF_GLOBAL as u8,
            5,
            1,
            b'c',
            INDEX_REF_ADAPTER_FUNCTION as u8,
            6,
        ];
        let reader = InstanceSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        let instances: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;

        assert_eq!(
            instances,
            [Instance::Instantiation {
                module: 101,
                imports: [
                    ("foo", IndexRef::Instance(0)),
                    ("bar", IndexRef::Module(1)),
                    ("baz", IndexRef::Function(2)),
                    ("jam", IndexRef::Table(3)),
                    ("a", IndexRef::Memory(4)),
                    ("b", IndexRef::Global(5)),
                    ("c", IndexRef::AdapterFunction(6)),
                ]
                .into()
            }]
        );

        Ok(())
    }

    #[test]
    fn it_parses_an_export() -> Result<()> {
        let data: &[u8] = &[
            1,
            INSTANCE_KIND_EXPORTED as u8,
            7,
            3,
            b'f',
            b'o',
            b'o',
            INDEX_REF_INSTANCE as u8,
            0,
            3,
            b'b',
            b'a',
            b'r',
            INDEX_REF_MODULE as u8,
            1,
            3,
            b'b',
            b'a',
            b'z',
            INDEX_REF_FUNCTION as u8,
            2,
            3,
            b'j',
            b'a',
            b'm',
            INDEX_REF_TABLE as u8,
            3,
            1,
            b'a',
            INDEX_REF_MEMORY as u8,
            4,
            1,
            b'b',
            INDEX_REF_GLOBAL as u8,
            5,
            1,
            b'c',
            INDEX_REF_ADAPTER_FUNCTION as u8,
            6,
        ];
        let reader = InstanceSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        let instances: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;

        assert_eq!(
            instances,
            [Instance::Exported(
                [
                    ("foo", IndexRef::Instance(0)),
                    ("bar", IndexRef::Module(1)),
                    ("baz", IndexRef::Function(2)),
                    ("jam", IndexRef::Table(3)),
                    ("a", IndexRef::Memory(4)),
                    ("b", IndexRef::Global(5)),
                    ("c", IndexRef::AdapterFunction(6)),
                ]
                .into()
            )]
        );

        Ok(())
    }
}
