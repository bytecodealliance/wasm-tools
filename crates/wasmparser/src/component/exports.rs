use super::IndexRef;
use crate::{
    BinaryReader, Range, Result, SectionIteratorLimited, SectionReader, SectionWithLimitedItems,
};

/// Represents an export in the component export section.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Export<'a> {
    /// The name of the export.
    pub name: &'a str,
    /// The index of the item being exported.
    pub index: IndexRef,
}

impl<'a> Export<'a> {
    fn new(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(Self {
            name: reader.read_string()?,
            index: IndexRef::new(reader)?,
        })
    }
}

/// The export section reader for a WebAssembly component.
#[derive(Clone)]
pub struct ExportSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> ExportSectionReader<'a> {
    /// Creates a new export section reader for the given data and initial offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<Self> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(Self { reader, count })
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets the number of exports in the section.
    pub fn len(&self) -> u32 {
        self.count
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Reads exports from the export section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::component::ExportSectionReader;
    /// # let data: &[u8] = &[0x1, 0x3, b'f', b'o', b'o', 0x2, 0x1];
    /// let exports = ExportSectionReader::new(data, 0).unwrap();
    /// for export in exports {
    ///     let export = export.expect("export");
    ///     println!("{:?}", export);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<Export<'a>> {
        Export::new(&mut self.reader)
    }
}

impl<'a> SectionReader for ExportSectionReader<'a> {
    type Item = Export<'a>;

    fn read(&mut self) -> Result<Self::Item> {
        ExportSectionReader::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        ExportSectionReader::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for ExportSectionReader<'a> {
    fn get_count(&self) -> u32 {
        ExportSectionReader::len(self)
    }
}

impl<'a> IntoIterator for ExportSectionReader<'a> {
    type Item = Result<Export<'a>>;
    type IntoIter = SectionIteratorLimited<ExportSectionReader<'a>>;

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
        let reader = ExportSectionReader::new(data, 0)?;
        assert!(reader.eof());
        assert_eq!(reader.len(), 0);
        Ok(())
    }

    #[test]
    fn it_parses_exports() -> Result<()> {
        let data: &[u8] = &[
            7,
            1,
            b'a',
            INDEX_REF_INSTANCE as u8,
            0,
            1,
            b'b',
            INDEX_REF_MODULE as u8,
            1,
            1,
            b'c',
            INDEX_REF_FUNCTION as u8,
            2,
            1,
            b'd',
            INDEX_REF_TABLE as u8,
            3,
            1,
            b'e',
            INDEX_REF_MEMORY as u8,
            4,
            1,
            b'f',
            INDEX_REF_GLOBAL as u8,
            5,
            1,
            b'g',
            INDEX_REF_ADAPTER_FUNCTION as u8,
            6,
        ];

        let reader = ExportSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 7);

        let exports: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;

        assert_eq!(
            exports,
            &[
                Export {
                    name: "a",
                    index: IndexRef::Instance(0)
                },
                Export {
                    name: "b",
                    index: IndexRef::Module(1)
                },
                Export {
                    name: "c",
                    index: IndexRef::Function(2)
                },
                Export {
                    name: "d",
                    index: IndexRef::Table(3)
                },
                Export {
                    name: "e",
                    index: IndexRef::Memory(4)
                },
                Export {
                    name: "f",
                    index: IndexRef::Global(5)
                },
                Export {
                    name: "g",
                    index: IndexRef::AdapterFunction(6)
                },
            ]
        );

        Ok(())
    }
}
