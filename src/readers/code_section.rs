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

use super::{BinaryReader, BinaryReaderError, Result};

#[derive(Debug)]
pub struct FunctionBody<'a> {
    offset: usize,
    data: &'a [u8],
}

impl<'a> FunctionBody<'a> {
    pub fn get_binary_reader<'b>(&self) -> BinaryReader<'b>
    where
        'a: 'b,
    {
        BinaryReader::new_with_offset(self.data, self.offset)
    }
}

pub struct CodeSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> CodeSectionReader<'a> {
    pub fn new(data: &'a [u8], offset: usize) -> Result<CodeSectionReader<'a>> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(CodeSectionReader { reader, count })
    }

    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    pub fn get_count(&self) -> u32 {
        self.count
    }

    fn verify_body_end(&self, end: usize) -> Result<()> {
        if self.reader.buffer.len() < end {
            return Err(BinaryReaderError {
                message: "Function body extends past end of the code section",
                offset: self.reader.original_offset + self.reader.buffer.len(),
            });
        }
        Ok(())
    }

    /// Reads content of the code section.
    ///
    /// # Examples
    /// ```
    /// # let data: &[u8] = &[0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
    /// #     0x01, 0x4, 0x01, 0x60, 0x00, 0x00, 0x03, 0x02, 0x01, 0x00,
    /// #     0x0a, 0x05, 0x01, 0x03, 0x00, 0x01, 0x0b];
    /// use wasmparser::ModuleReader;
    /// let mut reader = ModuleReader::new(data).expect("module reader");
    /// let section = reader.read().expect("type section");
    /// let section = reader.read().expect("function section");
    /// let section = reader.read().expect("code section");
    /// let mut code_reader = section.get_code_section_reader().expect("code section reader");
    /// for _ in 0..code_reader.get_count() {
    ///     let body = code_reader.read().expect("function body");
    ///     let mut binary_reader = body.get_binary_reader();
    ///     assert!(binary_reader.read_local_count().expect("local count") == 0);
    ///     let op = binary_reader.read_operator().expect("first operator");
    ///     println!("First operator: {:?}", op);
    /// }
    /// ```
    pub fn read<'b>(&mut self) -> Result<FunctionBody<'b>>
    where
        'a: 'b,
    {
        let size = self.reader.read_var_u32()? as usize;
        let body_start = self.reader.position;
        let body_end = body_start + size;
        self.verify_body_end(body_end)?;
        self.reader.skip_to(body_end);
        Ok(FunctionBody {
            offset: self.reader.original_offset + body_start,
            data: &self.reader.buffer[body_start..body_end],
        })
    }
}
