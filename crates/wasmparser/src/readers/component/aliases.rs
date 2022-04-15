use crate::{BinaryReader, Result, SectionIteratorLimited, SectionReader, SectionWithLimitedItems};
use std::ops::Range;

/// Represents a kind of alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AliasKind {
    /// The alias is to a module.
    Module,
    /// The alias is to a component.
    Component,
    /// The alias is to an instance.
    Instance,
    /// The alias is to a component function.
    ComponentFunc,
    /// The alias is to a value.
    Value,
    /// The alias is to a core function.
    Func,
    /// The alias is to a table.
    Table,
    /// The alias is to a memory.
    Memory,
    /// The alias is to a global.
    Global,
    /// The alias is to a tag.
    Tag,
}

/// Represents an alias in a WebAssembly component.
#[derive(Debug, Clone)]
pub enum Alias<'a> {
    /// The alias is to an export of an instance.
    InstanceExport {
        /// The alias kind.
        kind: AliasKind,
        /// The instance identifier.
        instance: u32,
        /// The export name.
        name: &'a str,
    },
    /// The alias is to an outer module.
    OuterModule {
        /// The outward count, starting at zero for the current component.
        count: u32,
        /// The index of the module within the outer component.
        index: u32,
    },
    /// The alias is to an outer component.
    OuterComponent {
        /// The outward count, starting at zero for the current component.
        count: u32,
        /// The index of the component within the outer component.
        index: u32,
    },
    /// The alias is to an outer type.
    OuterType {
        /// The outward count, starting at zero for the current component.
        count: u32,
        /// The index of the type within the outer component.
        index: u32,
    },
}

/// A reader for the alias section of a WebAssembly component.
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

    /// Reads content of the alias section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::AliasSectionReader;
    /// let data: &[u8] = &[0x01, 0x02, 0x00, 0x03, b'f', b'o', b'o'];
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
