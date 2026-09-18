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
    BinaryReader, Error, FromReader, Result, SectionLimited, SectionLimitedIntoIter, Subsection,
    Subsections,
};
use core::ops::Range;

/// Represents a name map from the names custom section.
#[derive(Debug, Clone)]
pub struct NameMap<'a> {
    /// The raw section that's being read.
    pub names: SectionLimitedIntoIter<'a, Naming<'a>>,
    last_index: Option<u32>,
}

impl<'a> NameMap<'a> {
    /// Creates a new name map parser from the given data.
    pub fn new(data: BinaryReader<'a>) -> Result<Self> {
        let names = SectionLimited::new(data)?;
        Ok(NameMap {
            names: names.into_iter(),
            last_index: None,
        })
    }
}

impl<'a> Iterator for NameMap<'a> {
    type Item = Result<Naming<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.names.original_position();
        let name = self.names.next()?;
        if let Ok(name) = &name {
            let index = name.index;
            if let Some(prev) = self.last_index {
                if index <= prev {
                    return Some(Err(Error::new("names out of order", start)));
                }
            }
            self.last_index = Some(index);
        }
        Some(name)
    }
}

/// Represents a name for an index from the names section.
#[derive(Debug, Copy, Clone)]
pub struct Naming<'a> {
    /// The index being named.
    pub index: u32,
    /// The name for the index.
    pub name: &'a str,
}

impl<'a> FromReader<'a> for Naming<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let index = reader.read_var_u32()?;
        // This seems to match what browsers do where they don't limit the
        // length of names in the `name` section while they do limit the names
        // in the import and export section for example.
        let name = reader.read_unlimited_string()?;
        Ok(Naming { index, name })
    }
}

/// Represents a reader for indirect names from the names custom section.
#[derive(Clone)]
pub struct IndirectNameMap<'a> {
    /// The names that are being iterated over.
    pub names: SectionLimitedIntoIter<'a, IndirectNaming<'a>>,
    last_index: Option<u32>,
}

impl<'a> IndirectNameMap<'a> {
    /// Creates a new `IndirectNameMap` from the given data.
    pub fn new(data: BinaryReader<'a>) -> Result<Self> {
        let names = SectionLimited::new(data)?.into_iter();
        Ok(IndirectNameMap {
            names,
            last_index: None,
        })
    }
}

impl<'a> Iterator for IndirectNameMap<'a> {
    type Item = Result<IndirectNaming<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.names.original_position();
        let name = self.names.next()?;
        if let Ok(name) = &name {
            let index = name.index;
            if let Some(prev) = self.last_index {
                if index <= prev {
                    return Some(Err(Error::new("indirect names out of order", start)));
                }
            }
            self.last_index = Some(index);
        }
        Some(name)
    }
}

/// Represents an indirect name in the names custom section.
#[derive(Debug, Clone)]
pub struct IndirectNaming<'a> {
    /// The indirect index of the name.
    pub index: u32,
    /// The map of names within the `index` prior.
    pub names: NameMap<'a>,
}

impl<'a> FromReader<'a> for IndirectNaming<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let index = reader.read_var_u32()?;

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
        name_range: Range<u64>,
    },
    /// The name is for the functions.
    Function(NameMap<'a>),
    /// The name is for the function locals.
    Local(IndirectNameMap<'a>),
    /// The name is for the function labels.
    Label(IndirectNameMap<'a>),
    /// The name is for the types.
    Type(NameMap<'a>),
    /// The name is for the tables.
    Table(NameMap<'a>),
    /// The name is for the memories.
    Memory(NameMap<'a>),
    /// The name is for the globals.
    Global(NameMap<'a>),
    /// The name is for the element segments.
    Element(NameMap<'a>),
    /// The name is for the data segments.
    Data(NameMap<'a>),
    /// The name is for fields.
    Field(IndirectNameMap<'a>),
    /// The name is for tags.
    Tag(NameMap<'a>),
    /// The name is for parameters of function types.
    Parameter(IndirectNameMap<'a>),
    /// The name is for parameters of tag types.
    TagParameter(IndirectNameMap<'a>),
    /// An unknown [name subsection](https://webassembly.github.io/spec/core/appendix/custom.html#subsections).
    Unknown {
        /// The identifier for this subsection.
        ty: u8,
        /// The contents of this subsection.
        data: &'a [u8],
        /// The range of bytes, relative to the start of the original data
        /// stream, that the contents of this subsection reside in.
        range: Range<u64>,
    },
}

impl Name<'_> {
    fn id(&self) -> u8 {
        match self {
            Name::Module { .. } => 0,
            Name::Function(_) => 1,
            Name::Local(_) => 2,
            Name::Label(_) => 3,
            Name::Type(_) => 4,
            Name::Table(_) => 5,
            Name::Memory(_) => 6,
            Name::Global(_) => 7,
            Name::Element(_) => 8,
            Name::Data(_) => 9,
            Name::Field(_) => 10,
            Name::Tag(_) => 11,
            Name::Parameter(_) => 12,
            Name::TagParameter(_) => 13,
            Name::Unknown { ty, .. } => *ty,
        }
    }
}

/// A reader for the name custom section of a WebAssembly module.
#[derive(Clone)]
pub struct NameSectionReader<'a> {
    /// The raw list of sections that are being parsed.
    pub sections: Subsections<'a, Name<'a>>,
    last_id: Option<u8>,
}

impl<'a> NameSectionReader<'a> {
    /// Creates a new `NameSectionReader` from the given data.
    pub fn new(data: BinaryReader<'a>) -> Self {
        NameSectionReader {
            sections: Subsections::new(data),
            last_id: None,
        }
    }
}

impl<'a> Iterator for NameSectionReader<'a> {
    type Item = Result<Name<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.sections.original_position();
        let section = self.sections.next()?;
        if let Ok(section) = &section {
            let id = section.id();
            if let Some(prev) = self.last_id {
                if id <= prev {
                    return Some(Err(Error::new("name subsection out of order", start)));
                }
            }
            self.last_id = Some(id);
        }
        Some(section)
    }
}

impl<'a> Subsection<'a> for Name<'a> {
    fn from_reader(id: u8, mut reader: BinaryReader<'a>) -> Result<Self> {
        Ok(match id {
            0 => {
                let offset = reader.original_position();
                let name = reader.read_string()?;
                if !reader.eof() {
                    return Err(Error::new(
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
            12 => Name::Parameter(IndirectNameMap::new(reader)?),
            13 => Name::TagParameter(IndirectNameMap::new(reader)?),
            ty => Name::Unknown {
                ty,
                data: reader.remaining_buffer(),
                range: reader.remaining_range(),
            },
        })
    }
}
