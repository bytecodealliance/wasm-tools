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

use super::{
    BinaryReader, BinaryReaderError, InitExpr, OperatorsReader, Result, SectionIteratorLimited,
    SectionReader, SectionWithLimitedItems, Type,
};
use crate::Operator;

#[derive(Clone)]
pub struct Element<'a> {
    pub kind: ElementKind<'a>,
}

#[derive(Clone)]
pub enum ElementKind<'a> {
    Passive {
        ty: Type,
        items: PassiveElementItems<'a>,
    },
    Active {
        table_index: u32,
        init_expr: InitExpr<'a>,
        items: ActiveElementItems<'a>,
    },
}

#[derive(Debug, Copy, Clone)]
pub struct ActiveElementItems<'a> {
    offset: usize,
    data: &'a [u8],
}

impl<'a> ActiveElementItems<'a> {
    pub fn get_items_reader<'b>(&self) -> Result<ActiveElementItemsReader<'b>>
    where
        'a: 'b,
    {
        ActiveElementItemsReader::new(self.data, self.offset)
    }
}

pub struct ActiveElementItemsReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> ActiveElementItemsReader<'a> {
    pub fn new(data: &[u8], offset: usize) -> Result<ActiveElementItemsReader> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(ActiveElementItemsReader { reader, count })
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

impl<'a> IntoIterator for ActiveElementItemsReader<'a> {
    type Item = Result<u32>;
    type IntoIter = ActiveElementItemsIterator<'a>;
    fn into_iter(self) -> Self::IntoIter {
        let count = self.count;
        ActiveElementItemsIterator {
            reader: self,
            left: count,
            err: false,
        }
    }
}

pub struct ActiveElementItemsIterator<'a> {
    reader: ActiveElementItemsReader<'a>,
    left: u32,
    err: bool,
}

impl<'a> Iterator for ActiveElementItemsIterator<'a> {
    type Item = Result<u32>;
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

#[derive(Clone)]
pub struct PassiveElementItems<'a> {
    amt: u32,
    reader: OperatorsReader<'a>,
}

pub enum PassiveElementItem {
    Null,
    Func(u32),
}

impl<'a> PassiveElementItems<'a> {
    pub fn get_count(&self) -> u32 {
        self.amt
    }

    pub fn read(&mut self) -> Result<PassiveElementItem> {
        let ret = match self.reader.read_with_offset()? {
            (Operator::RefNull, _) => PassiveElementItem::Null,
            (Operator::RefFunc { function_index }, _) => PassiveElementItem::Func(function_index),
            (_, offset) => {
                return Err(BinaryReaderError {
                    message: "invalid passive segment",
                    offset,
                })
            }
        };
        match self.reader.read_with_offset()? {
            (Operator::End, _) => {}
            (_, offset) => {
                return Err(BinaryReaderError {
                    message: "invalid passive segment",
                    offset,
                })
            }
        }
        self.amt -= 1;
        if self.amt == 0 {
            self.reader.ensure_end()?;
        }
        Ok(ret)
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
    /// use wasmparser::{ModuleReader, ElementKind};
    ///use wasmparser::Result;
    /// let mut reader = ModuleReader::new(data).expect("module reader");
    /// let section = reader.read().expect("type section");
    /// let section = reader.read().expect("function section");
    /// let section = reader.read().expect("table section");
    /// let section = reader.read().expect("element section");
    /// let mut element_reader = section.get_element_section_reader().expect("element section reader");
    /// for _ in 0..element_reader.get_count() {
    ///     let element = element_reader.read().expect("element");
    ///     if let ElementKind::Active { items, init_expr, .. } = element.kind {
    ///         let mut init_expr_reader = init_expr.get_binary_reader();
    ///         let op = init_expr_reader.read_operator().expect("op");
    ///         println!("Init const: {:?}", op);
    ///         let mut items_reader = items.get_items_reader().expect("items reader");
    ///         for _ in 0..items_reader.get_count() {
    ///             let item = items_reader.read().expect("item");
    ///             println!("  Item: {}", item);
    ///         }
    ///     }
    /// }
    /// ```
    pub fn read<'b>(&mut self) -> Result<Element<'b>>
    where
        'a: 'b,
    {
        let flags = self.reader.read_var_u32()?;
        let kind = if flags == 1 {
            let ty = self.reader.read_type()?;
            let amt = self.reader.read_var_u32()?;
            let data_start = self.reader.position;
            let mut reader = OperatorsReader {
                reader: self.reader.clone(),
            };
            for _ in 0..amt {
                loop {
                    match reader.read()? {
                        Operator::End => break,
                        _ => {}
                    }
                }
            }
            self.reader = reader.reader;
            let data_end = self.reader.position;
            let items = PassiveElementItems {
                amt,
                reader: OperatorsReader::new(
                    &self.reader.buffer[data_start..data_end],
                    self.reader.original_offset + data_start,
                ),
            };
            ElementKind::Passive { ty, items }
        } else {
            let table_index = match flags {
                0 => 0,
                2 => self.reader.read_var_u32()?,
                _ => {
                    return Err(BinaryReaderError {
                        message: "invalid flags byte in element segment",
                        offset: self.reader.original_position() - 1,
                    });
                }
            };
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
            let items = ActiveElementItems {
                offset: self.reader.original_offset + data_start,
                data: &self.reader.buffer[data_start..data_end],
            };
            ElementKind::Active {
                table_index,
                init_expr,
                items,
            }
        };
        Ok(Element { kind })
    }
}

impl<'a> SectionReader for ElementSectionReader<'a> {
    type Item = Element<'a>;
    fn read(&mut self) -> Result<Self::Item> {
        ElementSectionReader::read(self)
    }
    fn eof(&self) -> bool {
        self.reader.eof()
    }
    fn original_position(&self) -> usize {
        ElementSectionReader::original_position(self)
    }
}

impl<'a> SectionWithLimitedItems for ElementSectionReader<'a> {
    fn get_count(&self) -> u32 {
        ElementSectionReader::get_count(self)
    }
}

impl<'a> IntoIterator for ElementSectionReader<'a> {
    type Item = Result<Element<'a>>;
    type IntoIter = SectionIteratorLimited<ElementSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}
