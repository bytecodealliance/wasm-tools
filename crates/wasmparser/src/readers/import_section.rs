/* Copyright 2018 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use crate::{
    BinaryReader, ImportSectionEntryType, Range, Result, SectionIteratorLimited, SectionReader,
    SectionWithLimitedItems,
};

/// Represents a core WebAssembly import.
#[derive(Debug, Copy, Clone)]
pub struct Import<'a> {
    /// The module being imported from.
    pub module: &'a str,
    /// The name of the imported item.
    pub field: Option<&'a str>,
    /// The type of the imported item.
    pub ty: ImportSectionEntryType,
}

/// A reader for a core WebAssembly's import section.
#[derive(Clone)]
pub struct ImportSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> ImportSectionReader<'a> {
    /// Constructs a new `ImportSectionReader` for the given data and offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<ImportSectionReader<'a>> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(ImportSectionReader { reader, count })
    }

    /// Gets the original position of the section reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets the count of items in the section.
    pub fn get_count(&self) -> u32 {
        self.count
    }

    /// Reads content of the import section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::ImportSectionReader;
    /// # let data: &[u8] = &[0x01, 0x01, 0x41, 0x01, 0x66, 0x00, 0x00];
    /// let mut import_reader = ImportSectionReader::new(data, 0).unwrap();
    /// for _ in 0..import_reader.get_count() {
    ///     let import = import_reader.read().expect("import");
    ///     println!("Import: {:?}", import);
    /// }
    /// ```
    pub fn read<'b>(&mut self) -> Result<Import<'b>>
    where
        'a: 'b,
    {
        self.reader.read_import()
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
        ImportSectionReader::get_count(self)
    }
}

impl<'a> IntoIterator for ImportSectionReader<'a> {
    type Item = Result<Import<'a>>;
    type IntoIter = SectionIteratorLimited<ImportSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}
