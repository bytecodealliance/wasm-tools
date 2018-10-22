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

use super::{BinaryReader, InitExpr, Result};

#[derive(Debug, Copy, Clone)]
pub struct Element<'a> {
    pub table_index: u32,
    pub init_expr: InitExpr<'a>,
    pub items: ElementItems<'a>,
}

#[derive(Debug, Copy, Clone)]
pub struct ElementItems<'a> {
    offset: usize,
    data: &'a [u8],
}

impl<'a> ElementItems<'a> {
    pub fn get_items_reader<'b>(&self) -> Result<ElementItemsReader<'b>>
    where
        'a: 'b,
    {
        ElementItemsReader::new(self.data, self.offset)
    }
}

pub struct ElementItemsReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> ElementItemsReader<'a> {
    pub fn new(data: &[u8], offset: usize) -> Result<ElementItemsReader> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(ElementItemsReader { reader, count })
    }

    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    pub fn get_count(&self) -> u32 {
        self.count
    }

    pub fn read(&mut self) -> Result<u32> {
        self.reader.read_var_u32()
    }
}

pub struct ElementSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> ElementSectionReader<'a> {
    pub fn new(data: &'a [u8], offset: usize) -> Result<ElementSectionReader<'a>> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(ElementSectionReader { reader, count })
    }

    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    pub fn get_count(&self) -> u32 {
        self.count
    }

    /// Reads content of the element section.
    ///
    /// # Examples
    /// ```
    /// # let data: &[u8] = &[0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
    /// #     0x01, 0x4, 0x01, 0x60, 0x00, 0x00, 0x03, 0x02, 0x01, 0x00,
    /// #     0x05, 0x03, 0x01, 0x00, 0x02,
    /// #     0x09, 0x07, 0x01, 0x00, 0x41, 0x00, 0x0B, 0x01, 0x00,
    /// #     0x0a, 0x05, 0x01, 0x03, 0x00, 0x01, 0x0b];
    /// use wasmparser::ModuleReader;
    /// let mut reader = ModuleReader::new(data).expect("module reader");
    /// let section = reader.read().expect("type section");
    /// let section = reader.read().expect("function section");
    /// let section = reader.read().expect("table section");
    /// let section = reader.read().expect("element section");
    /// let mut element_reader = section.get_element_section_reader().expect("element section reader");
    /// for _ in 0..element_reader.get_count() {
    ///     let element = element_reader.read().expect("element");
    ///     println!("Element: {:?}", element);
    ///     let mut init_expr_reader = element.init_expr.get_binary_reader();
    ///     let op = init_expr_reader.read_operator().expect("op");
    ///     println!("Init const: {:?}", op);
    ///     let mut items_reader = element.items.get_items_reader().expect("items reader");
    ///     for _ in 0..items_reader.get_count() {
    ///         let item = items_reader.read().expect("item");
    ///         println!("  Item: {}", item);
    ///     }
    /// }
    /// ```
    pub fn read<'b>(&mut self) -> Result<Element<'b>>
    where
        'a: 'b,
    {
        let table_index = self.reader.read_var_u32()?;
        let init_expr = {
            let expr_offset = self.reader.position;
            self.reader.skip_init_expr()?;
            let data = &self.reader.buffer[expr_offset..self.reader.position];
            InitExpr::new(data, self.reader.original_offset + expr_offset)
        };
        let data_start = self.reader.position;
        let items_count = self.reader.read_var_u32()?;
        for _ in 0..items_count {
            self.reader.skip_var_32()?;
        }
        let data_end = self.reader.position;
        let items = ElementItems {
            offset: self.reader.original_offset + data_start,
            data: &self.reader.buffer[data_start..data_end],
        };
        Ok(Element {
            table_index,
            init_expr,
            items,
        })
    }
}
