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

use core::mem;

use crate::{
    BinaryReader, BinaryReaderError, ExternalKind, FromReader, GlobalType, MemoryType, Result,
    SectionLimited, SectionLimitedIntoIterWithOffsets, TableType, TagType,
};

/// Represents a reference to a type definition in a WebAssembly module.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TypeRef {
    /// The type is a function.
    Func(u32),
    /// The type is a table.
    Table(TableType),
    /// The type is a memory.
    Memory(MemoryType),
    /// The type is a global.
    Global(GlobalType),
    /// The type is a tag.
    ///
    /// This variant is only used for the exception handling proposal.
    ///
    /// The value is an index in the types index space.
    Tag(TagType),
    /// The type is a function.
    FuncExact(u32),
}

/// Represents a group of imports in a WebAssembly module.
#[derive(Debug, Clone)]
pub enum Imports<'a> {
    /// The group contains a single import.
    Single(usize, Import<'a>),
    /// The group contains many imports that share the same module name, but have different types.
    Compact1(ImportGroup1<'a>),
    /// The group contains many imports that share the same module name and type.
    Compact2(ImportGroup2<'a>),
}

/// Represents an import in a WebAssembly module.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Import<'a> {
    /// The module being imported from.
    pub module: &'a str,
    /// The name of the imported item.
    pub name: &'a str,
    /// The type of the imported item.
    pub ty: TypeRef,
}

/// A group of imports that share a common module name, but have different types.
#[derive(Debug, Clone)]
pub struct ImportGroup1<'a> {
    /// The module being imported from.
    pub module: &'a str,
    /// The imported items.
    pub items: SectionLimited<'a, ImportItemCompact<'a>>,
}

/// A single compact import item.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ImportItemCompact<'a> {
    /// The name of the imported item.
    pub name: &'a str,
    /// The type of the imported item.
    pub ty: TypeRef,
}

/// A group of imports that share a common module name and type.
#[derive(Debug, Clone)]
pub struct ImportGroup2<'a> {
    /// The module each item will be imported from.
    pub module: &'a str,
    /// The type of the imported items.
    pub ty: TypeRef,
    /// The imported item names.
    pub items: SectionLimited<'a, &'a str>,
}

/// A reader for the import section of a WebAssembly module.
pub type ImportSectionReader<'a> = SectionLimited<'a, Imports<'a>>;

impl<'a> FromReader<'a> for Imports<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let start = reader.original_position();
        let module = reader.read_string()?;
        let single_item_name = reader.read_string()?;
        let discriminator = reader.peek_bytes(1)?[0];
        match (single_item_name, discriminator) {
            ("", 0x7F) => {
                // Compact encoding 1: one module name, many item names / types
                reader.read_bytes(1)?;
                // FIXME(#188) shouldn't need to skip here
                let items = reader.skip(|reader| {
                    let count = reader.read_var_u32()?;
                    for _ in 0..count {
                        reader.skip_string()?;
                        reader.read::<TypeRef>()?;
                    }
                    Ok(())
                })?;
                Ok(Imports::Compact1(ImportGroup1 {
                    module,
                    items: SectionLimited::new(items)?,
                }))
            }
            ("", 0x7E) => {
                // Compact encoding 2: one module name / type, many item names
                reader.read_bytes(1)?;
                let ty: TypeRef = reader.read()?;
                // FIXME(#188) shouldn't need to skip here
                let items = reader.skip(|reader| {
                    let count = reader.read_var_u32()?;
                    for _ in 0..count {
                        reader.skip_string()?;
                    }
                    Ok(())
                })?;
                Ok(Imports::Compact2(ImportGroup2 {
                    module,
                    ty,
                    items: SectionLimited::new(items)?,
                }))
            }
            _ => Ok(Imports::Single(
                start,
                Import {
                    module: module,
                    name: single_item_name,
                    ty: reader.read()?,
                },
            )),
        }
    }
}

impl<'a> FromReader<'a> for Import<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(Import {
            module: reader.read()?,
            name: reader.read()?,
            ty: reader.read()?,
        })
    }
}

impl<'a> FromReader<'a> for ImportItemCompact<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(ImportItemCompact {
            name: reader.read()?,
            ty: reader.read()?,
        })
    }
}

impl<'a> FromReader<'a> for TypeRef {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read()? {
            ExternalKind::Func => TypeRef::Func(reader.read_var_u32()?),
            ExternalKind::FuncExact => TypeRef::FuncExact(reader.read_var_u32()?),
            ExternalKind::Table => TypeRef::Table(reader.read()?),
            ExternalKind::Memory => TypeRef::Memory(reader.read()?),
            ExternalKind::Global => TypeRef::Global(reader.read()?),
            ExternalKind::Tag => TypeRef::Tag(reader.read()?),
        })
    }
}

// Iterator implementations to streamline usage of the Imports type in its
// various possible encodings

impl<'a> SectionLimited<'a, Imports<'a>> {
    /// TODO
    pub fn into_imports(self) -> impl Iterator<Item = Result<Import<'a>>> {
        self.into_imports_with_offsets()
            .map(|res| res.map(|(_, import)| import))
    }

    /// TODO
    pub fn into_imports_with_offsets(self) -> impl Iterator<Item = Result<(usize, Import<'a>)>> {
        self.into_iter().flat_map(|res| match res {
            Ok(imports) => imports.into_iter(),
            Err(e) => ImportsIter {
                state: ImportsIterState::Error(e),
            },
        })
    }
}

impl<'a> IntoIterator for Imports<'a> {
    type Item = Result<(usize, Import<'a>)>;
    type IntoIter = ImportsIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ImportsIter {
            state: match self {
                Imports::Single(start, import) => ImportsIterState::Single(start, import),
                Imports::Compact1(group) => ImportsIterState::Compact1 {
                    module: group.module,
                    iter: group.items.into_iter_with_offsets(),
                },
                Imports::Compact2(group) => ImportsIterState::Compact2 {
                    module: group.module,
                    ty: group.ty,
                    iter: group.items.into_iter_with_offsets(),
                },
            },
        }
    }
}

/// TODO
pub struct ImportsIter<'a> {
    state: ImportsIterState<'a>,
}

enum ImportsIterState<'a> {
    Done,
    Error(BinaryReaderError),
    Single(usize, Import<'a>),
    Compact1 {
        module: &'a str,
        iter: SectionLimitedIntoIterWithOffsets<'a, ImportItemCompact<'a>>,
    },
    Compact2 {
        module: &'a str,
        ty: TypeRef,
        iter: SectionLimitedIntoIterWithOffsets<'a, &'a str>,
    },
}

impl<'a> Iterator for ImportsIter<'a> {
    type Item = Result<(usize, Import<'a>)>;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.state {
            ImportsIterState::Done => None,
            ImportsIterState::Error(_) => {
                let ImportsIterState::Error(e) =
                    mem::replace(&mut self.state, ImportsIterState::Done)
                else {
                    unreachable!()
                };
                Some(Err(e))
            }

            ImportsIterState::Single(offset, i) => {
                let ret = Some(Ok((*offset, i.clone())));
                self.state = ImportsIterState::Done;
                ret
            }
            ImportsIterState::Compact1 { module, iter } => {
                let item = iter.next()?;
                Some(item.map(|(offset, item)| {
                    (
                        offset,
                        Import {
                            module,
                            name: item.name,
                            ty: item.ty,
                        },
                    )
                }))
            }
            ImportsIterState::Compact2 { module, ty, iter } => {
                let item = iter.next()?;
                Some(item.map(|(offset, name)| {
                    (
                        offset,
                        Import {
                            module,
                            name,
                            ty: *ty,
                        },
                    )
                }))
            }
        }
    }
}
