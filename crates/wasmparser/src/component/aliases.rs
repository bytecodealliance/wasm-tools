use crate::{
    BinaryReader, BinaryReaderError, Range, Result, SectionIteratorLimited, SectionReader,
    SectionWithLimitedItems,
};

const ALIAS_KIND_INSTANCE: u32 = 0x00;
pub(crate) const ALIAS_KIND_OUTER: u32 = 0x01;
pub(crate) const ALIAS_KIND_OUTER_MODULE: u32 = 0x01;
pub(crate) const ALIAS_KIND_OUTER_TYPE: u32 = 0x06;

const ALIAS_EXPORT_KIND_INSTANCE: u32 = 0x0;
const ALIAS_EXPORT_KIND_MODULE: u32 = 0x1;
const ALIAS_EXPORT_KIND_FUNCTION: u32 = 0x2;
const ALIAS_EXPORT_KIND_TABLE: u32 = 0x3;
const ALIAS_EXPORT_KIND_MEMORY: u32 = 0x4;
const ALIAS_EXPORT_KIND_GLOBAL: u32 = 0x5;
const ALIAS_EXPORT_KIND_ADAPTER_FUNCTION: u32 = 0x6;

/// Represents the expected export kind for an alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportKind {
    /// The alias is to an instance.
    Instance,
    /// The alias is to a module.
    Module,
    /// The alias is to a function.
    Function,
    /// The alias is to a table.
    Table,
    /// The alias is to a memory.
    Memory,
    /// The alias is to a global.
    Global,
    /// The alias is to an adapter function.
    AdapterFunction,
}

impl ExportKind {
    fn new(reader: &mut BinaryReader) -> Result<Self> {
        Ok(match reader.read_u8()? {
            ALIAS_EXPORT_KIND_INSTANCE => ExportKind::Instance,
            ALIAS_EXPORT_KIND_MODULE => ExportKind::Module,
            ALIAS_EXPORT_KIND_FUNCTION => ExportKind::Function,
            ALIAS_EXPORT_KIND_TABLE => ExportKind::Table,
            ALIAS_EXPORT_KIND_MEMORY => ExportKind::Memory,
            ALIAS_EXPORT_KIND_GLOBAL => ExportKind::Global,
            ALIAS_EXPORT_KIND_ADAPTER_FUNCTION => ExportKind::AdapterFunction,
            x => {
                return Err(BinaryReaderError::new(
                    format!("invalid byte (0x{:x}) in alias export kind", x),
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// Represents an alias in the component alias section.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Alias<'a> {
    /// The alias is to an instance export.
    Instance {
        /// The index of the instance being aliased.
        index: u32,
        /// The name of the export being aliased.
        name: &'a str,
        /// The export kind.
        kind: ExportKind,
    },
    /// The alias is to an outer module type.
    OuterType {
        /// The outward count from the current module (count 0).
        count: u32,
        /// The type index within the outer module.
        index: u32,
    },
    OuterModule {
        /// The outward count from the current module (count 0).
        count: u32,
        /// The type index within the outer module.
        index: u32,
    },
}

impl<'a> Alias<'a> {
    fn new(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read_u8()? {
            ALIAS_KIND_INSTANCE => Self::Instance {
                index: reader.read_var_u32()?,
                name: reader.read_string()?,
                kind: ExportKind::new(reader)?,
            },
            ALIAS_KIND_OUTER => {
                let count = reader.read_var_u32()?;
                let index = reader.read_var_u32()?;
                match reader.read_u8()? {
                    ALIAS_KIND_OUTER_MODULE => Alias::OuterModule { count, index },
                    ALIAS_KIND_OUTER_TYPE => Alias::OuterType { count, index },
                    x => {
                        return Err(BinaryReaderError::new(
                            format!("invalid byte (0x{:x}) in outer alias kind", x),
                            reader.original_position() - 1,
                        ))
                    }
                }
            }
            x => {
                return Err(BinaryReaderError::new(
                    format!("invalid byte (0x{:x}) in alias kind", x),
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// The alias section reader for a WebAssembly component.
#[derive(Clone)]
pub struct AliasSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> AliasSectionReader<'a> {
    /// Creates a new alias section reader for the given data and initial offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<Self> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(Self { reader, count })
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets the number of aliases in the section.
    pub fn len(&self) -> u32 {
        self.count
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Reads aliases from the alias section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::component::AliasSectionReader;
    /// # let data: &[u8] = &[0x1, 0x0, 0x0, 0x3, b'f', b'o', b'o', 0x2];
    /// let aliases = AliasSectionReader::new(data, 0).unwrap();
    /// for alias in aliases {
    ///     let alias = alias.expect("alias");
    ///     println!("{:?}", alias);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<Alias<'a>> {
        Alias::new(&mut self.reader)
    }
}

impl<'a> SectionReader for AliasSectionReader<'a> {
    type Item = Alias<'a>;

    fn read(&mut self) -> Result<Self::Item> {
        AliasSectionReader::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        AliasSectionReader::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for AliasSectionReader<'a> {
    fn get_count(&self) -> u32 {
        AliasSectionReader::len(self)
    }
}

impl<'a> IntoIterator for AliasSectionReader<'a> {
    type Item = Result<Alias<'a>>;
    type IntoIter = SectionIteratorLimited<AliasSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;

    #[test]
    fn it_parses_an_empty_section() -> Result<()> {
        let data: &[u8] = &[0];
        let reader = AliasSectionReader::new(data, 0)?;
        assert!(reader.eof());
        assert_eq!(reader.len(), 0);
        Ok(())
    }

    #[test]
    fn it_parses_an_instance_aliases() -> Result<()> {
        let data: &[u8] = &[
            7,
            ALIAS_KIND_INSTANCE as u8,
            0,
            1,
            b'a',
            ALIAS_EXPORT_KIND_INSTANCE as u8,
            ALIAS_KIND_INSTANCE as u8,
            1,
            1,
            b'b',
            ALIAS_EXPORT_KIND_MODULE as u8,
            ALIAS_KIND_INSTANCE as u8,
            2,
            1,
            b'c',
            ALIAS_EXPORT_KIND_FUNCTION as u8,
            ALIAS_KIND_INSTANCE as u8,
            3,
            1,
            b'd',
            ALIAS_EXPORT_KIND_TABLE as u8,
            ALIAS_KIND_INSTANCE as u8,
            4,
            1,
            b'e',
            ALIAS_EXPORT_KIND_MEMORY as u8,
            ALIAS_KIND_INSTANCE as u8,
            5,
            1,
            b'f',
            ALIAS_EXPORT_KIND_GLOBAL as u8,
            ALIAS_KIND_INSTANCE as u8,
            6,
            1,
            b'g',
            ALIAS_EXPORT_KIND_ADAPTER_FUNCTION as u8,
        ];
        let reader = AliasSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 7);

        let aliases: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;

        assert_eq!(
            aliases,
            [
                Alias::Instance {
                    index: 0,
                    name: "a".into(),
                    kind: ExportKind::Instance,
                },
                Alias::Instance {
                    index: 1,
                    name: "b".into(),
                    kind: ExportKind::Module,
                },
                Alias::Instance {
                    index: 2,
                    name: "c".into(),
                    kind: ExportKind::Function,
                },
                Alias::Instance {
                    index: 3,
                    name: "d".into(),
                    kind: ExportKind::Table,
                },
                Alias::Instance {
                    index: 4,
                    name: "e".into(),
                    kind: ExportKind::Memory,
                },
                Alias::Instance {
                    index: 5,
                    name: "f".into(),
                    kind: ExportKind::Global,
                },
                Alias::Instance {
                    index: 6,
                    name: "g".into(),
                    kind: ExportKind::AdapterFunction,
                },
            ]
        );
        Ok(())
    }

    #[test]
    fn it_parses_an_outer_module_alias() -> Result<()> {
        let data: &[u8] = &[
            1,
            ALIAS_KIND_OUTER as u8,
            3,
            101,
            ALIAS_KIND_OUTER_MODULE as u8,
        ];
        let reader = AliasSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        let aliases: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;

        assert_eq!(
            aliases,
            [Alias::OuterModule {
                count: 3,
                index: 101
            }]
        );
        Ok(())
    }

    #[test]
    fn it_parses_an_outer_type_alias() -> Result<()> {
        let data: &[u8] = &[
            1,
            ALIAS_KIND_OUTER as u8,
            3,
            101,
            ALIAS_KIND_OUTER_TYPE as u8,
        ];
        let reader = AliasSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        let aliases: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;

        assert_eq!(
            aliases,
            [Alias::OuterType {
                count: 3,
                index: 101
            }]
        );
        Ok(())
    }
}
