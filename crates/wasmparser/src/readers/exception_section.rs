/* Copyright 2020 Mozilla Foundation
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

use super::{
    BinaryReader, ExceptionType, Range, Result, SectionIteratorLimited, SectionReader,
    SectionWithLimitedItems,
};

#[derive(Clone)]
pub struct ExceptionSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> ExceptionSectionReader<'a> {
    pub fn new(data: &'a [u8], offset: usize) -> Result<ExceptionSectionReader<'a>> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(ExceptionSectionReader { reader, count })
    }

    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    pub fn get_count(&self) -> u32 {
        self.count
    }

    /// Reads content of the global section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::ExceptionSectionReader;
    /// # let data: &[u8] = &[0x01, 0x00, 0x01];
    /// let mut exn_reader = ExceptionSectionReader::new(data, 0).unwrap();
    /// for _ in 0..exn_reader.get_count() {
    ///     let exn = exn_reader.read().expect("exception_type");
    ///     println!("Exception: {:?}", exn);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<ExceptionType> {
        self.reader.read_exception_type()
    }
}

impl<'a> SectionReader for ExceptionSectionReader<'a> {
    type Item = ExceptionType;
    fn read(&mut self) -> Result<Self::Item> {
        ExceptionSectionReader::read(self)
    }
    fn eof(&self) -> bool {
        self.reader.eof()
    }
    fn original_position(&self) -> usize {
        ExceptionSectionReader::original_position(self)
    }
    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for ExceptionSectionReader<'a> {
    fn get_count(&self) -> u32 {
        ExceptionSectionReader::get_count(self)
    }
}

impl<'a> IntoIterator for ExceptionSectionReader<'a> {
    type Item = Result<ExceptionType>;
    type IntoIter = SectionIteratorLimited<ExceptionSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}
