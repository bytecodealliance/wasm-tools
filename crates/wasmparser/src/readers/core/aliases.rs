use crate::{
    BinaryReader, ExternalKind, Result, SectionIteratorLimited, SectionReader,
    SectionWithLimitedItems,
};
use std::ops::Range;

/// Represents a core alias for a WebAssembly module.
#[derive(Debug, Clone)]
pub enum Alias<'a> {
    /// The alias is to an export of a module instance.
    InstanceExport {
        /// The alias kind.
        kind: ExternalKind,
        /// The instance index.
        instance_index: u32,
        /// The export name.
        name: &'a str,
    },
}

/// A reader for the core alias section of a WebAssembly component.
#[derive(Clone)]
pub struct AliasSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> AliasSectionReader<'a> {
    /// Constructs a new `AliasSectionReader` for the given data and offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<Self> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(Self { reader, count })
    }

    /// Gets the original position of the section reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets the count of items in the section.
    pub fn get_count(&self) -> u32 {
        self.count
    }

    /// Reads content of the core alias section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::AliasSectionReader;
    /// let data: &[u8] = &[0x01, 0x00, 0x00, 0x00, 0x03, b'f', b'o', b'o'];
    /// let mut reader = AliasSectionReader::new(data, 0).unwrap();
    /// for _ in 0..reader.get_count() {
    ///     let alias = reader.read().expect("alias");
    ///     println!("Alias: {:?}", alias);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<Alias<'a>> {
        self.reader.read_alias()
    }
}

impl<'a> SectionReader for AliasSectionReader<'a> {
    type Item = Alias<'a>;

    fn read(&mut self) -> Result<Self::Item> {
        Self::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        Self::original_position(self)
    }

    fn range(&self) -> Range<usize> {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for AliasSectionReader<'a> {
    fn get_count(&self) -> u32 {
        Self::get_count(self)
    }
}

impl<'a> IntoIterator for AliasSectionReader<'a> {
    type Item = Result<Alias<'a>>;
    type IntoIter = SectionIteratorLimited<Self>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}
