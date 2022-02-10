use crate::{
    BinaryReader, ComponentExport, ComponentExportKind, Export, Range, Result,
    SectionIteratorLimited, SectionReader, SectionWithLimitedItems,
};

/// Represents the kind of argument when instantiating a WebAssembly module.
#[derive(Debug, Clone)]
pub enum ModuleArgKind<'a> {
    /// The argument is an instance.
    Instance(u32),
    /// The argument is an instance based on exports of local items.
    Exports(Box<[Export<'a>]>),
}

/// Represents an argument to instantiating a WebAssembly component.
#[derive(Debug, Clone)]
pub struct ModuleArg<'a> {
    /// The name of the module argument.
    pub name: &'a str,
    /// The kind of the module argument.
    pub kind: ModuleArgKind<'a>,
}

/// Represents an argument to instantiating a WebAssembly component.
pub type ComponentArg<'a> = ComponentExport<'a>;

/// Represents the argument type for instantiating a WebAssembly component.
pub type ComponentArgKind<'a> = ComponentExportKind<'a>;

/// Represents an instance in a WebAssembly component.
#[derive(Debug, Clone)]
pub enum Instance<'a> {
    /// The instance is from instantiating a WebAssembly module.
    Module {
        /// The module index.
        index: u32,
        /// The module's instantiation arguments.
        args: Box<[ModuleArg<'a>]>,
    },
    /// The instance is from instantiating a WebAssembly component.
    Component {
        /// The component index.
        index: u32,
        /// The component's instantiation arguments.
        args: Box<[ComponentArg<'a>]>,
    },
    /// The instance is from exporting local items.
    Exports(Box<[ComponentExport<'a>]>),
}

/// A reader for the instance section of a WebAssembly component.
#[derive(Clone)]
pub struct InstanceSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> InstanceSectionReader<'a> {
    /// Constructs a new `InstanceSectionReader` for the given data and offset.
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

    /// Reads content of the instance section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::InstanceSectionReader;
    /// # let data: &[u8] = &[0x01, 0x00, 0x00, 0x00, 0x01, 0x03, b'f', b'o', b'o', 0x00, 0x02, 0x00];
    /// let mut reader = InstanceSectionReader::new(data, 0).unwrap();
    /// for _ in 0..reader.get_count() {
    ///     let instance = reader.read().expect("instance");
    ///     println!("Instance: {:?}", instance);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<Instance<'a>> {
        self.reader.read_instance()
    }
}

impl<'a> SectionReader for InstanceSectionReader<'a> {
    type Item = Instance<'a>;

    fn read(&mut self) -> Result<Self::Item> {
        Self::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        Self::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for InstanceSectionReader<'a> {
    fn get_count(&self) -> u32 {
        Self::get_count(self)
    }
}

impl<'a> IntoIterator for InstanceSectionReader<'a> {
    type Item = Result<Instance<'a>>;
    type IntoIter = SectionIteratorLimited<Self>;

    /// Implements iterator over the instance section.
    ///
    /// # Examples
    ///
    /// ```
    /// use wasmparser::InstanceSectionReader;
    /// # let data: &[u8] = &[0x01, 0x00, 0x00, 0x00, 0x01, 0x03, b'f', b'o', b'o', 0x00, 0x02, 0x00];
    /// let mut reader = InstanceSectionReader::new(data, 0).unwrap();
    /// for inst in reader {
    ///     println!("Instance {:?}", inst.expect("instance"));
    /// }
    /// ```
    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}
