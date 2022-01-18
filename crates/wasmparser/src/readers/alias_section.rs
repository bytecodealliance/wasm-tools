use crate::{
    BinaryReader, BinaryReaderError, ExternalKind, Range, Result, SectionIteratorLimited,
    SectionReader, SectionWithLimitedItems,
};

/// A reader for a core WebAssembly module's alias section.
#[derive(Clone)]
pub struct AliasSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

/// Represents an alias in a core WebAssembly module's alias section.
#[derive(Clone, Debug)]
pub enum Alias<'a> {
    /// The alias is to an outer module's type.
    OuterType {
        /// The relative depth of the outer module.
        relative_depth: u32,
        /// The index of the type in the outer module.
        index: u32,
    },
    /// The alias is to an outer module's module.
    OuterModule {
        /// The relative depth of the outer module.
        relative_depth: u32,
        /// The index of the module in the outer module.
        index: u32,
    },
    /// The alias is to an export from an instance.
    InstanceExport {
        /// The index of the instance.
        instance: u32,
        /// The export kind being aliased.
        kind: ExternalKind,
        /// The name of the export being aliased.
        export: &'a str,
    },
}

impl<'a> AliasSectionReader<'a> {
    /// Constructs a new `AliasSectionReader` for the given data and offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<AliasSectionReader<'a>> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(AliasSectionReader { reader, count })
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets the count of items in the section.
    pub fn get_count(&self) -> u32 {
        self.count
    }

    /// Reads an item from the section.
    pub fn read(&mut self) -> Result<Alias<'a>> {
        Ok(match self.reader.read_u8()? {
            0x00 => Alias::InstanceExport {
                instance: self.reader.read_var_u32()?,
                kind: self.reader.read_external_kind()?,
                export: self.reader.read_string()?,
            },
            0x01 => {
                let relative_depth = self.reader.read_var_u32()?;
                match self.reader.read_external_kind()? {
                    ExternalKind::Type => Alias::OuterType {
                        relative_depth,
                        index: self.reader.read_var_u32()?,
                    },
                    ExternalKind::Module => Alias::OuterModule {
                        relative_depth,
                        index: self.reader.read_var_u32()?,
                    },
                    _ => {
                        return Err(BinaryReaderError::new(
                            "invalid external kind in alias",
                            self.original_position() - 1,
                        ))
                    }
                }
            }
            _ => {
                return Err(BinaryReaderError::new(
                    "invalid byte in alias",
                    self.original_position() - 1,
                ))
            }
        })
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
        AliasSectionReader::get_count(self)
    }
}

impl<'a> IntoIterator for AliasSectionReader<'a> {
    type Item = Result<Alias<'a>>;
    type IntoIter = SectionIteratorLimited<AliasSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}
