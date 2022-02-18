use crate::{
    BinaryReader, ComponentArgKind, InterfaceType, Range, Result, SectionIteratorLimited,
    SectionReader, SectionWithLimitedItems,
};

/// Represents the kind of export in a WebAssembly component.
#[derive(Debug, Clone)]
pub enum ComponentExportKind<'a> {
    /// The export is a module.
    Module(u32),
    /// The export is a component.
    Component(u32),
    /// The export is an instance.
    Instance(u32),
    /// The export is a function.
    Function(u32),
    /// The export is a value.
    Value(u32),
    /// The export is an interface type.
    Type(InterfaceType),
    /// The export is an instance based on exports of local items.
    InstanceFromExports(Box<[ComponentExport<'a>]>),
}

impl<'a> From<ComponentArgKind<'a>> for ComponentExportKind<'a> {
    fn from(arg: ComponentArgKind<'a>) -> Self {
        match arg {
            ComponentArgKind::Module(idx) => Self::Module(idx),
            ComponentArgKind::Component(idx) => Self::Component(idx),
            ComponentArgKind::Instance(idx) => Self::Instance(idx),
            ComponentArgKind::Function(idx) => Self::Function(idx),
            ComponentArgKind::Value(idx) => Self::Value(idx),
            ComponentArgKind::InstanceFromExports(exports) => Self::InstanceFromExports(exports),
        }
    }
}

/// Represents an export in a WebAssembly component.
#[derive(Debug, Clone)]
pub struct ComponentExport<'a> {
    /// The name of the exported item.
    pub name: &'a str,
    /// The kind of the export.
    pub kind: ComponentExportKind<'a>,
}

/// A reader for the export section of a WebAssembly component.
#[derive(Clone)]
pub struct ComponentExportSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> ComponentExportSectionReader<'a> {
    /// Constructs a new `ComponentExportSectionReader` for the given data and offset.
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

    /// Reads content of the export section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::ComponentExportSectionReader;
    ///
    /// # let data: &[u8] = &[0x01, 0x03, b'f', b'o', b'o', 0x00, 0x03, 0x00];
    /// let mut reader = ComponentExportSectionReader::new(data, 0).unwrap();
    /// for _ in 0..reader.get_count() {
    ///     let export = reader.read().expect("export");
    ///     println!("Export: {:?}", export);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<ComponentExport<'a>> {
        self.reader.read_component_export()
    }
}

impl<'a> SectionReader for ComponentExportSectionReader<'a> {
    type Item = ComponentExport<'a>;

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

impl<'a> SectionWithLimitedItems for ComponentExportSectionReader<'a> {
    fn get_count(&self) -> u32 {
        Self::get_count(self)
    }
}

impl<'a> IntoIterator for ComponentExportSectionReader<'a> {
    type Item = Result<ComponentExport<'a>>;
    type IntoIter = SectionIteratorLimited<Self>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}
