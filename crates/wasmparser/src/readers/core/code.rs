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
    BinaryReader, BinaryReaderError, FromReader, OperatorsReader, Result, SectionLimited, ValType,
};
use std::ops::Range;

/// A reader for the code section of a WebAssembly module.
pub type CodeSectionReader<'a> = SectionLimited<'a, FunctionBody<'a>>;

/// Represents a WebAssembly function body.
#[derive(Debug, Clone, Copy)]
pub struct FunctionBody<'a> {
    offset: usize,
    data: &'a [u8],
    allow_memarg64: bool,
}

impl<'a> FunctionBody<'a> {
    /// Constructs a new `FunctionBody` for the given data and offset.
    pub fn new(offset: usize, data: &'a [u8]) -> Self {
        Self {
            offset,
            data,
            allow_memarg64: false,
        }
    }

    /// Whether or not to allow 64-bit memory arguments in the
    /// function body.
    ///
    /// This is intended to be `true` when support for the memory64
    /// WebAssembly proposal is also enabled.
    pub fn allow_memarg64(&mut self, allow: bool) {
        self.allow_memarg64 = allow;
    }

    /// Gets a binary reader for this function body.
    pub fn get_binary_reader<'b>(&self) -> BinaryReader<'b>
    where
        'a: 'b,
    {
        let mut reader = BinaryReader::new_with_offset(self.data, self.offset);
        reader.allow_memarg64(self.allow_memarg64);
        reader
    }

    fn skip_locals(reader: &mut BinaryReader) -> Result<()> {
        let count = reader.read_var_u32()?;
        for _ in 0..count {
            reader.read_var_u32()?;
            reader.read_var_u32()?;
        }
        Ok(())
    }

    /// Gets the locals reader for this function body.
    pub fn get_locals_reader<'b>(&self) -> Result<LocalsReader<'b>>
    where
        'a: 'b,
    {
        let mut reader = BinaryReader::new_with_offset(self.data, self.offset);
        let count = reader.read_var_u32()?;
        Ok(LocalsReader { reader, count })
    }

    /// Gets the operators reader for this function body.
    pub fn get_operators_reader<'b>(&self) -> Result<OperatorsReader<'b>>
    where
        'a: 'b,
    {
        let mut reader = BinaryReader::new_with_offset(self.data, self.offset);
        Self::skip_locals(&mut reader)?;
        let pos = reader.position;
        let mut reader = OperatorsReader::new(&self.data[pos..], self.offset + pos);
        reader.allow_memarg64(self.allow_memarg64);
        Ok(reader)
    }

    /// Gets the range of the function body.
    pub fn range(&self) -> Range<usize> {
        self.offset..self.offset + self.data.len()
    }
}

impl<'a> FromReader<'a> for FunctionBody<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let size = reader.read_var_u32()? as usize;
        let body_start = reader.position;
        let body_end = body_start + size;
        if reader.buffer.len() < body_end {
            return Err(BinaryReaderError::new(
                "function body extends past end of the code section",
                reader.original_offset + reader.buffer.len(),
            ));
        }
        reader.skip_to(body_end);
        Ok(FunctionBody {
            offset: reader.original_offset + body_start,
            data: &reader.buffer[body_start..body_end],
            allow_memarg64: false,
        })
    }
}

/// A reader for a function body's locals.
pub struct LocalsReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> LocalsReader<'a> {
    /// Gets the count of locals in the function body.
    pub fn get_count(&self) -> u32 {
        self.count
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Reads an item from the reader.
    pub fn read(&mut self) -> Result<(u32, ValType)> {
        let count = self.reader.read_var_u32()?;
        let value_type = self.reader.read_val_type()?;
        Ok((count, value_type))
    }
}

impl<'a> IntoIterator for LocalsReader<'a> {
    type Item = Result<(u32, ValType)>;
    type IntoIter = LocalsIterator<'a>;
    fn into_iter(self) -> Self::IntoIter {
        let count = self.count;
        LocalsIterator {
            reader: self,
            left: count,
            err: false,
        }
    }
}

/// An iterator over locals in a function body.
pub struct LocalsIterator<'a> {
    reader: LocalsReader<'a>,
    left: u32,
    err: bool,
}

impl<'a> Iterator for LocalsIterator<'a> {
    type Item = Result<(u32, ValType)>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.err || self.left == 0 {
            return None;
        }
        let result = self.reader.read();
        self.err = result.is_err();
        self.left -= 1;
        Some(result)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.reader.get_count() as usize;
        (count, Some(count))
    }
}
