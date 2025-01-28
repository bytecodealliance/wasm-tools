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

use wasm_types::{
    AbsoluteLabelIdx, DataIdx, ElemIdx, FieldIdx, FuncIdx, GlobalIdx, LocalIdx, MemIdx, TableIdx,
    TagIdx, TypeIdx,
};

use crate::{
    BinaryReader, BinaryReaderError, FromReader, Result, SectionLimited, Subsection, Subsections,
};
use core::ops::Range;

/// Represents a name map from the names custom section.
pub type NameMap<'a, I> = SectionLimited<'a, Naming<'a, I>>;

/// Represents a name for an index from the names section.
#[derive(Debug, Copy, Clone)]
pub struct Naming<'a, I> {
    /// The index being named.
    pub index: I,
    /// The name for the index.
    pub name: &'a str,
}

impl<'a, I: FromReader<'a>> FromReader<'a> for Naming<'a, I> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let index = reader.read()?;
        // This seems to match what browsers do where they don't limit the
        // length of names in the `name` section while they do limit the names
        // in the import and export section for example.
        let name = reader.read_unlimited_string()?;
        Ok(Naming { index, name })
    }
}

/// Represents a reader for indirect names from the names custom section.
pub type IndirectNameMap<'a, I, J> = SectionLimited<'a, IndirectNaming<'a, I, J>>;

/// Represents an indirect name in the names custom section.
#[derive(Debug, Clone)]
pub struct IndirectNaming<'a, I, J> {
    /// The indirect index of the name.
    pub index: I,
    /// The map of names within the `index` prior.
    pub names: NameMap<'a, J>,
}

impl<'a, I: FromReader<'a>, J: FromReader<'a>> FromReader<'a> for IndirectNaming<'a, I, J> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let index = reader.read()?;

        // Skip the `NameMap` manually here.
        //
        // FIXME(#188) shouldn't need to skip here
        let names = reader.skip(|reader| {
            let count = reader.read_var_u32()?;
            for _ in 0..count {
                reader.read_var_u32()?;
                reader.skip_string()?;
            }
            Ok(())
        })?;

        Ok(IndirectNaming {
            index,
            names: NameMap::new(names)?,
        })
    }
}

/// Represents a name read from the names custom section.
#[derive(Clone)]
pub enum Name<'a> {
    /// The name is for the module.
    Module {
        /// The specified name.
        name: &'a str,
        /// The byte range that `name` occupies in the original binary.
        name_range: Range<usize>,
    },
    /// The name is for the functions.
    Function(NameMap<'a, FuncIdx>),
    /// The name is for the function locals.
    Local(IndirectNameMap<'a, FuncIdx, LocalIdx>),
    /// The name is for the function labels.
    Label(IndirectNameMap<'a, FuncIdx, AbsoluteLabelIdx>),
    /// The name is for the types.
    Type(NameMap<'a, TypeIdx>),
    /// The name is for the tables.
    Table(NameMap<'a, TableIdx>),
    /// The name is for the memories.
    Memory(NameMap<'a, MemIdx>),
    /// The name is for the globals.
    Global(NameMap<'a, GlobalIdx>),
    /// The name is for the element segments.
    Element(NameMap<'a, ElemIdx>),
    /// The name is for the data segments.
    Data(NameMap<'a, DataIdx>),
    /// The name is for fields.
    Field(IndirectNameMap<'a, TypeIdx, FieldIdx>),
    /// The name is for tags.
    Tag(NameMap<'a, TagIdx>),
    /// An unknown [name subsection](https://webassembly.github.io/spec/core/appendix/custom.html#subsections).
    Unknown {
        /// The identifier for this subsection.
        ty: u8,
        /// The contents of this subsection.
        data: &'a [u8],
        /// The range of bytes, relative to the start of the original data
        /// stream, that the contents of this subsection reside in.
        range: Range<usize>,
    },
}

/// A reader for the name custom section of a WebAssembly module.
pub type NameSectionReader<'a> = Subsections<'a, Name<'a>>;

impl<'a> Subsection<'a> for Name<'a> {
    fn from_reader(id: u8, mut reader: BinaryReader<'a>) -> Result<Self> {
        let data = reader.remaining_buffer();
        let offset = reader.original_position();
        Ok(match id {
            0 => {
                let name = reader.read_string()?;
                if !reader.eof() {
                    return Err(BinaryReaderError::new(
                        "trailing data at the end of a name",
                        reader.original_position(),
                    ));
                }
                Name::Module {
                    name,
                    name_range: offset..reader.original_position(),
                }
            }
            1 => Name::Function(NameMap::new(reader)?),
            2 => Name::Local(IndirectNameMap::new(reader)?),
            3 => Name::Label(IndirectNameMap::new(reader)?),
            4 => Name::Type(NameMap::new(reader)?),
            5 => Name::Table(NameMap::new(reader)?),
            6 => Name::Memory(NameMap::new(reader)?),
            7 => Name::Global(NameMap::new(reader)?),
            8 => Name::Element(NameMap::new(reader)?),
            9 => Name::Data(NameMap::new(reader)?),
            10 => Name::Field(IndirectNameMap::new(reader)?),
            11 => Name::Tag(NameMap::new(reader)?),
            ty => Name::Unknown {
                ty,
                data,
                range: offset..offset + data.len(),
            },
        })
    }
}
