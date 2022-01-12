use crate::{
    component::TypeRef, BinaryReader, Range, Result, SectionIteratorLimited, SectionReader,
    SectionWithLimitedItems,
};

/// Represents an import in the component import section.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Import<'a> {
    /// The name of the import.
    pub name: &'a str,
    /// The type of the import.
    pub ty: TypeRef,
}

impl<'a> Import<'a> {
    fn new(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(Self {
            name: reader.read_string()?,
            ty: TypeRef::new(reader)?,
        })
    }
}

/// The import section reader for a WebAssembly component.
#[derive(Clone)]
pub struct ImportSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> ImportSectionReader<'a> {
    /// Creates a new import section reader for the given data and initial offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<Self> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(Self { reader, count })
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets the number of imports in the section.
    pub fn len(&self) -> u32 {
        self.count
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Reads imports from the import section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::component::ImportSectionReader;
    /// # let data: &[u8] = &[0x1, 0x3, b'f', b'o', b'o', 0x2, 0x1];
    /// let imports = ImportSectionReader::new(data, 0).unwrap();
    /// for import in imports {
    ///     let import = import.expect("import");
    ///     println!("{:?}", import);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<Import<'a>> {
        Import::new(&mut self.reader)
    }
}

impl<'a> SectionReader for ImportSectionReader<'a> {
    type Item = Import<'a>;

    fn read(&mut self) -> Result<Self::Item> {
        ImportSectionReader::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        ImportSectionReader::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for ImportSectionReader<'a> {
    fn get_count(&self) -> u32 {
        ImportSectionReader::len(self)
    }
}

impl<'a> IntoIterator for ImportSectionReader<'a> {
    type Item = Result<Import<'a>>;
    type IntoIter = SectionIteratorLimited<ImportSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        component::{
            TYPE_REF_ADAPTER_FUNCTION, TYPE_REF_FUNCTION, TYPE_REF_GLOBAL, TYPE_REF_INSTANCE,
            TYPE_REF_MEMORY, TYPE_REF_MODULE, TYPE_REF_TABLE,
        },
        GlobalType, MemoryType, TableType, Type,
    };
    use anyhow::Result;

    #[test]
    fn it_parses_an_empty_section() -> Result<()> {
        let data: &[u8] = &[0];
        let reader = ImportSectionReader::new(data, 0)?;
        assert!(reader.eof());
        assert_eq!(reader.len(), 0);
        Ok(())
    }

    #[test]
    fn it_parses_imports() -> Result<()> {
        let data: &[u8] = &[
            7,
            1,
            b'a',
            TYPE_REF_INSTANCE as u8,
            0,
            1,
            b'b',
            TYPE_REF_MODULE as u8,
            1,
            1,
            b'c',
            TYPE_REF_FUNCTION as u8,
            2,
            1,
            b'd',
            TYPE_REF_TABLE as u8,
            0x7f, // i32
            0x1,  // has max
            0x0,  // min
            0x10, // max
            1,
            b'e',
            TYPE_REF_MEMORY as u8,
            0x1,  // flags (has max)
            0x0,  // min
            0x10, // max
            1,
            b'f',
            TYPE_REF_GLOBAL as u8,
            0x7e, // i64
            0x1,  // mutable
            1,
            b'g',
            TYPE_REF_ADAPTER_FUNCTION as u8,
            3,
        ];

        let reader = ImportSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 7);

        let imports: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;

        assert_eq!(
            imports,
            &[
                Import {
                    name: "a",
                    ty: TypeRef::Instance(0)
                },
                Import {
                    name: "b",
                    ty: TypeRef::Module(1)
                },
                Import {
                    name: "c",
                    ty: TypeRef::Function(2)
                },
                Import {
                    name: "d",
                    ty: TypeRef::Table(TableType {
                        element_type: Type::I32,
                        initial: 0,
                        maximum: Some(16)
                    })
                },
                Import {
                    name: "e",
                    ty: TypeRef::Memory(MemoryType {
                        initial: 0,
                        maximum: Some(16),
                        shared: false,
                        memory64: false,
                    })
                },
                Import {
                    name: "f",
                    ty: TypeRef::Global(GlobalType {
                        content_type: Type::I64,
                        mutable: true,
                    })
                },
                Import {
                    name: "g",
                    ty: TypeRef::AdapterFunction(3)
                }
            ]
        );

        Ok(())
    }
}
