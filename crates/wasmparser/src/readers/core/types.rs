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

use std::fmt::{self, Debug, Write};

use crate::limits::{
    MAX_WASM_FUNCTION_PARAMS, MAX_WASM_FUNCTION_RETURNS, MAX_WASM_STRUCT_FIELDS,
    MAX_WASM_SUPERTYPES, MAX_WASM_TYPES,
};
use crate::{BinaryReader, BinaryReaderError, FromReader, Result, SectionLimited};

pub(crate) trait Matches {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType;
}

define_core_wasm_types!(u32);

impl ValType {
    pub(crate) fn is_valtype_byte(byte: u8) -> bool {
        match byte {
            0x7F | 0x7E | 0x7D | 0x7C | 0x7B | 0x70 | 0x6F | 0x64 | 0x63 | 0x6E | 0x71 | 0x72
            | 0x73 | 0x6D | 0x6B | 0x6A | 0x6C => true,
            _ => false,
        }
    }
}

impl Matches for ValType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        match (self, other) {
            (Self::Ref(r1), Self::Ref(r2)) => r1.matches(r2, type_at),
            (a, b) => a == b,
        }
    }
}

impl<'a> FromReader<'a> for StorageType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        match reader.peek()? {
            0x78 => {
                reader.position += 1;
                Ok(StorageType::I8)
            }
            0x77 => {
                reader.position += 1;
                Ok(StorageType::I16)
            }
            _ => Ok(StorageType::Val(reader.read()?)),
        }
    }
}

impl<'a> FromReader<'a> for ValType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        match reader.peek()? {
            0x7F => {
                reader.position += 1;
                Ok(ValType::I32)
            }
            0x7E => {
                reader.position += 1;
                Ok(ValType::I64)
            }
            0x7D => {
                reader.position += 1;
                Ok(ValType::F32)
            }
            0x7C => {
                reader.position += 1;
                Ok(ValType::F64)
            }
            0x7B => {
                reader.position += 1;
                Ok(ValType::V128)
            }
            0x70 | 0x6F | 0x64 | 0x63 | 0x6E | 0x71 | 0x72 | 0x73 | 0x6D | 0x6B | 0x6A | 0x6C => {
                Ok(ValType::Ref(reader.read()?))
            }
            _ => bail!(reader.original_position(), "invalid value type"),
        }
    }
}

impl Matches for RefType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        self == other
            || ((other.is_nullable() || !self.is_nullable())
                && self.heap_type().matches(&other.heap_type(), type_at))
    }
}

impl<'a> FromReader<'a> for RefType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        match reader.read()? {
            0x70 => Ok(RefType::FUNC.nullable()),
            0x6F => Ok(RefType::EXTERN.nullable()),
            0x6E => Ok(RefType::ANY.nullable()),
            0x71 => Ok(RefType::NONE.nullable()),
            0x72 => Ok(RefType::NOEXTERN.nullable()),
            0x73 => Ok(RefType::NOFUNC.nullable()),
            0x6D => Ok(RefType::EQ.nullable()),
            0x6B => Ok(RefType::STRUCT.nullable()),
            0x6A => Ok(RefType::ARRAY.nullable()),
            0x6C => Ok(RefType::I31.nullable()),
            byte @ (0x63 | 0x64) => {
                let nullable = byte == 0x63;
                let pos = reader.original_position();
                RefType::new(nullable, reader.read()?)
                    .ok_or_else(|| crate::BinaryReaderError::new("type index too large", pos))
            }
            _ => bail!(reader.original_position(), "malformed reference type"),
        }
    }
}

impl Matches for HeapType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        if self == other {
            return true;
        }

        use HeapType as HT;
        match (self, other) {
            (HT::Eq | HT::I31 | HT::Struct | HT::Array | HT::None, HT::Any) => true,
            (HT::I31 | HT::Struct | HT::Array | HT::None, HT::Eq) => true,
            (HT::NoExtern, HT::Extern) => true,
            (HT::NoFunc, HT::Func) => true,
            (HT::None, HT::I31 | HT::Array | HT::Struct) => true,

            (HT::Concrete(a), HT::Eq | HT::Any) => matches!(
                type_at(*a).composite_type,
                CompositeType::Array(_) | CompositeType::Struct(_)
            ),

            (HT::Concrete(a), HT::Struct) => {
                matches!(type_at(*a).composite_type, CompositeType::Struct(_))
            }

            (HT::Concrete(a), HT::Array) => {
                matches!(type_at(*a).composite_type, CompositeType::Array(_))
            }

            (HT::Concrete(a), HT::Func) => {
                matches!(type_at(*a).composite_type, CompositeType::Func(_))
            }

            (HT::Concrete(a), HT::Concrete(b)) => type_at(*a)
                .composite_type
                .matches(&type_at(*b).composite_type, type_at),

            (HT::None, HT::Concrete(b)) => matches!(
                type_at(*b).composite_type,
                CompositeType::Array(_) | CompositeType::Struct(_)
            ),

            (HT::NoFunc, HT::Concrete(b)) => {
                matches!(type_at(*b).composite_type, CompositeType::Func(_))
            }

            _ => false,
        }
    }
}

impl<'a> FromReader<'a> for HeapType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        match reader.peek()? {
            0x70 => {
                reader.position += 1;
                Ok(HeapType::Func)
            }
            0x6F => {
                reader.position += 1;
                Ok(HeapType::Extern)
            }
            0x6E => {
                reader.position += 1;
                Ok(HeapType::Any)
            }
            0x71 => {
                reader.position += 1;
                Ok(HeapType::None)
            }
            0x72 => {
                reader.position += 1;
                Ok(HeapType::NoExtern)
            }
            0x73 => {
                reader.position += 1;
                Ok(HeapType::NoFunc)
            }
            0x6D => {
                reader.position += 1;
                Ok(HeapType::Eq)
            }
            0x6B => {
                reader.position += 1;
                Ok(HeapType::Struct)
            }
            0x6A => {
                reader.position += 1;
                Ok(HeapType::Array)
            }
            0x6C => {
                reader.position += 1;
                Ok(HeapType::I31)
            }
            _ => {
                let idx = match u32::try_from(reader.read_var_s33()?) {
                    Ok(idx) => idx,
                    Err(_) => {
                        bail!(reader.original_position(), "invalid indexed ref heap type");
                    }
                };
                Ok(HeapType::Concrete(idx))
            }
        }
    }
}

impl Matches for SubType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        !other.is_final && self.composite_type.matches(&other.composite_type, type_at)
    }
}

impl Matches for CompositeType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        match (self, other) {
            (CompositeType::Func(a), CompositeType::Func(b)) => a.matches(b, type_at),
            (CompositeType::Array(a), CompositeType::Array(b)) => a.matches(b, type_at),
            (CompositeType::Struct(a), CompositeType::Struct(b)) => a.matches(b, type_at),
            _ => false,
        }
    }
}

impl Matches for FuncType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        self.params().len() == other.params().len()
            && self.results().len() == other.results().len()
            // Note: per GC spec, function subtypes are contravariant in their parameter types.
            // Also see https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)
            && self
                .params()
                .iter()
                .zip(other.params())
                .all(|(a, b)| b.matches(a, type_at))
            && self
                .results()
                .iter()
                .zip(other.results())
                .all(|(a, b)| a.matches(b, type_at))
    }
}

impl Matches for ArrayType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        self.0.matches(&other.0, type_at)
    }
}

impl Matches for FieldType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        (other.mutable || !self.mutable) && self.element_type.matches(&other.element_type, type_at)
    }
}

impl Matches for StorageType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        match (self, other) {
            (Self::Val(a), Self::Val(b)) => a.matches(b, type_at),
            (a @ (Self::I8 | Self::I16 | Self::Val(_)), b) => a == b,
        }
    }
}

impl Matches for StructType {
    fn matches<'a, F>(&self, other: &Self, type_at: &F) -> bool
    where
        F: Fn(u32) -> &'a SubType,
    {
        // Note: Structure types support width and depth subtyping.
        self.fields.len() >= other.fields.len()
            && self
                .fields
                .iter()
                .zip(other.fields.iter())
                .all(|(a, b)| a.matches(b, type_at))
    }
}

/// Represents a table's type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TableType {
    /// The table's element type.
    pub element_type: RefType,
    /// Initial size of this table, in elements.
    pub initial: u32,
    /// Optional maximum size of the table, in elements.
    pub maximum: Option<u32>,
}

/// Represents a memory's type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MemoryType {
    /// Whether or not this is a 64-bit memory, using i64 as an index. If this
    /// is false it's a 32-bit memory using i32 as an index.
    ///
    /// This is part of the memory64 proposal in WebAssembly.
    pub memory64: bool,

    /// Whether or not this is a "shared" memory, indicating that it should be
    /// send-able across threads and the `maximum` field is always present for
    /// valid types.
    ///
    /// This is part of the threads proposal in WebAssembly.
    pub shared: bool,

    /// Initial size of this memory, in wasm pages.
    ///
    /// For 32-bit memories (when `memory64` is `false`) this is guaranteed to
    /// be at most `u32::MAX` for valid types.
    pub initial: u64,

    /// Optional maximum size of this memory, in wasm pages.
    ///
    /// For 32-bit memories (when `memory64` is `false`) this is guaranteed to
    /// be at most `u32::MAX` for valid types. This field is always present for
    /// valid wasm memories when `shared` is `true`.
    pub maximum: Option<u64>,
}

impl MemoryType {
    /// Gets the index type for the memory.
    pub fn index_type(&self) -> ValType {
        if self.memory64 {
            ValType::I64
        } else {
            ValType::I32
        }
    }
}

/// Represents a global's type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GlobalType {
    /// The global's type.
    pub content_type: ValType,
    /// Whether or not the global is mutable.
    pub mutable: bool,
}

/// Represents a tag kind.
#[derive(Clone, Copy, Debug)]
pub enum TagKind {
    /// The tag is an exception type.
    Exception,
}

/// A tag's type.
#[derive(Clone, Copy, Debug)]
pub struct TagType {
    /// The kind of tag
    pub kind: TagKind,
    /// The function type this tag uses.
    pub func_type_idx: u32,
}

/// A reader for the type section of a WebAssembly module.
pub type TypeSectionReader<'a> = SectionLimited<'a, RecGroup>;

impl<'a> TypeSectionReader<'a> {
    /// Returns an iterator over this type section which will only yield
    /// function types and any usage of GC types from the GC proposal will
    /// be translated into an error.
    pub fn into_iter_err_on_gc_types(self) -> impl Iterator<Item = Result<FuncType>> + 'a {
        self.into_iter_with_offsets().map(|item| {
            let (offset, group) = item?;
            let mut types = group.into_types();
            let ty = match (types.next(), types.next()) {
                (Some(ty), None) => ty,
                _ => bail!(offset, "gc proposal not supported"),
            };
            if !ty.is_final || ty.supertype_idx.is_some() {
                bail!(offset, "gc proposal not supported");
            }
            match ty.composite_type {
                CompositeType::Func(f) => Ok(f),
                CompositeType::Array(_) | CompositeType::Struct(_) => {
                    bail!(offset, "gc proposal not supported");
                }
            }
        })
    }
}

impl<'a> FromReader<'a> for CompositeType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        read_composite_type(reader.read_u8()?, reader)
    }
}

fn read_composite_type(
    opcode: u8,
    reader: &mut BinaryReader,
) -> Result<CompositeType, BinaryReaderError> {
    Ok(match opcode {
        0x60 => CompositeType::Func(reader.read()?),
        0x5e => CompositeType::Array(reader.read()?),
        0x5f => CompositeType::Struct(reader.read()?),
        x => return reader.invalid_leading_byte(x, "type"),
    })
}

impl<'a> FromReader<'a> for RecGroup {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        match reader.peek()? {
            0x4e => {
                reader.read_u8()?;
                let types = reader.read_iter(MAX_WASM_TYPES, "rec group types")?;
                Ok(RecGroup::explicit(types.collect::<Result<_>>()?))
            }
            _ => Ok(RecGroup::implicit(reader.read()?)),
        }
    }
}

impl<'a> FromReader<'a> for SubType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let pos = reader.original_position();
        Ok(match reader.read_u8()? {
            opcode @ (0x4f | 0x50) => {
                let idx_iter = reader.read_iter(MAX_WASM_SUPERTYPES, "supertype idxs")?;
                let idxs = idx_iter.collect::<Result<Vec<u32>>>()?;
                if idxs.len() > 1 {
                    return Err(BinaryReaderError::new(
                        "multiple supertypes not supported",
                        pos,
                    ));
                }
                SubType {
                    is_final: opcode == 0x4f,
                    supertype_idx: idxs.first().copied(),
                    composite_type: read_composite_type(reader.read_u8()?, reader)?,
                }
            }
            opcode => SubType {
                is_final: true,
                supertype_idx: None,
                composite_type: read_composite_type(opcode, reader)?,
            },
        })
    }
}

impl<'a> FromReader<'a> for FuncType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let mut params_results = reader
            .read_iter(MAX_WASM_FUNCTION_PARAMS, "function params")?
            .collect::<Result<Vec<_>>>()?;
        let len_params = params_results.len();
        let results = reader.read_iter(MAX_WASM_FUNCTION_RETURNS, "function returns")?;
        params_results.reserve(results.size_hint().0);
        for result in results {
            params_results.push(result?);
        }
        Ok(FuncType::from_raw_parts(params_results.into(), len_params))
    }
}

impl<'a> FromReader<'a> for FieldType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let element_type = reader.read()?;
        let mutable = reader.read_u8()?;
        Ok(FieldType {
            element_type,
            mutable: match mutable {
                0 => false,
                1 => true,
                _ => bail!(
                    reader.original_position(),
                    "invalid mutability byte for array type"
                ),
            },
        })
    }
}

impl<'a> FromReader<'a> for ArrayType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(ArrayType(FieldType::from_reader(reader)?))
    }
}

impl<'a> FromReader<'a> for StructType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let fields = reader.read_iter(MAX_WASM_STRUCT_FIELDS, "struct fields")?;
        Ok(StructType {
            fields: fields.collect::<Result<_>>()?,
        })
    }
}
