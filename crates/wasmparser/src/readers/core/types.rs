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

use crate::limits::{MAX_WASM_FUNCTION_PARAMS, MAX_WASM_FUNCTION_RETURNS};
use crate::{BinaryReader, FromReader, Result, SectionLimited};
use std::fmt::Debug;

/// The type of a value in a WebAssembly module.
//
// This is a bitpacked enum that fits in a `u32` with a three bit discriminant
// and has the following variants:
//
//                       ValType::Ref: [ 000:i3 RefType:i29 ]
//                       ValType::I32: [ 001:i3       _:i29 ]
//                       ValType::I64: [ 010:i3       _:i29 ]
//                       ValType::F32: [ 011:i3       _:i29 ]
//                       ValType::F64: [ 100:i3       _:i29 ]
//                      ValType::V128: [ 101:i3       _:i29 ]
//      (reserved for MaybeType::BOT): [ 110:i3       _:i29 ]
// (reserved for MaybeType::HEAP_BOT): [ 111:i3       _:i29 ]
//
// Note that the last two variants should never appear within a `ValType`, but
// because they are reserved for `MaybeType`'s use, `MaybeType` doesn't need to
// add any of its own discriminant bits. Additionally, the `MaybeType`
// discriminants must be last (with greatest values) so that checking for a
// valid `ValType` discriminant can be a simple unsigned less than comparison.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ValType(u32);

impl std::fmt::Debug for ValType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::V128 => write!(f, "v128"),
            ty if ty.is_ref_type() => {
                let ref_ty = ty.as_ref_type().unwrap();
                std::fmt::Debug::fmt(&ref_ty, f)
            }
            _ => unreachable!("Invalid bit pattern for ValType: ValType({:032b})", self.0),
        }
    }
}

impl ValType {
    pub(crate) const DISCRIMINANT_MASK: u32 = 0b111 << 29;

    const REF_DISCRIMINANT: u32 = 0b000 << 29;
    const I32_DISCRIMINANT: u32 = 0b001 << 29;
    const I64_DISCRIMINANT: u32 = 0b010 << 29;
    const F32_DISCRIMINANT: u32 = 0b011 << 29;
    const F64_DISCRIMINANT: u32 = 0b100 << 29;
    const V128_DISCRIMINANT: u32 = 0b101 << 29;

    pub(crate) const MAYBE_TYPE_BOT_DISCRIMINANT: u32 = 0b110 << 29;
    pub(crate) const MAYBE_TYPE_HEAP_BOT_DISCRIMINANT: u32 = 0b111 << 29;

    /// The `i32` value type.
    pub const I32: Self = Self(Self::I32_DISCRIMINANT);

    /// The `i64` value type.
    pub const I64: Self = Self(Self::I64_DISCRIMINANT);

    /// The `f32` value type.
    pub const F32: Self = Self(Self::F32_DISCRIMINANT);

    /// The `f64` value type.
    pub const F64: Self = Self(Self::F64_DISCRIMINANT);

    /// The `v128` value type.
    pub const V128: Self = Self(Self::V128_DISCRIMINANT);

    /// The `funcref` aka `anyfunc` aka `(ref null func)` type.
    pub const FUNCREF: ValType = Self(RefType::FUNCREF.0);

    /// The `externref` aka `(ref null extern)` type.
    pub const EXTERNREF: ValType = Self(RefType::EXTERNREF.0);

    #[inline]
    pub(crate) fn as_u32(&self) -> u32 {
        self.0
    }

    #[inline]
    pub(crate) fn from_u32(x: u32) -> Self {
        debug_assert!(
            x & Self::DISCRIMINANT_MASK < Self::MAYBE_TYPE_BOT_DISCRIMINANT,
            "should be a `ValType`, not a `MaybeType`"
        );
        if x & Self::DISCRIMINANT_MASK == Self::REF_DISCRIMINANT {
            RefType::from_u32(x).into()
        } else {
            debug_assert!(
                x & !Self::DISCRIMINANT_MASK == 0,
                "should not have any undefined bits set"
            );
            Self(x)
        }
    }

    /// Create a value type from the given reference type.
    #[inline]
    pub fn ref_type(ref_type: RefType) -> Self {
        let inner = ref_type.as_u32();
        debug_assert_eq!(
            inner & Self::DISCRIMINANT_MASK,
            Self::REF_DISCRIMINANT,
            "ref type does not have expected discriminant"
        );
        ValType(inner)
    }

    #[inline]
    fn discriminant(&self) -> u32 {
        self.0 & Self::DISCRIMINANT_MASK
    }

    /// Is this a reference type?
    #[inline]
    pub fn is_ref_type(&self) -> bool {
        self.discriminant() == Self::REF_DISCRIMINANT
    }

    /// If this value type is a reference type, get it; otherwise returns
    /// `None`.
    #[inline]
    pub fn as_ref_type(&self) -> Option<RefType> {
        if self.is_ref_type() {
            debug_assert_eq!(self.0 & !Self::DISCRIMINANT_MASK, self.0);
            Some(RefType::from_u32(self.0))
        } else {
            None
        }
    }

    /// Whether the type is defaultable, i.e. it is not a non-nullable reference
    /// type.
    pub fn is_defaultable(&self) -> bool {
        match *self {
            Self::I32 | Self::I64 | Self::F32 | Self::F64 | Self::V128 => true,
            _ if self.is_ref_type() => self.as_ref_type().unwrap().is_nullable(),
            _ => unreachable!(),
        }
    }

    pub(crate) fn is_valtype_byte(byte: u8) -> bool {
        match byte {
            0x7F | 0x7E | 0x7D | 0x7C | 0x7B | 0x70 | 0x6F | 0x6B | 0x6C => true,
            _ => false,
        }
    }
}

impl From<RefType> for ValType {
    #[inline]
    fn from(ref_ty: RefType) -> ValType {
        Self::ref_type(ref_ty)
    }
}

/// A reference type.
//
// This is a bitpacked enum that fits in a `u32`, always has zeroes for the
// three high bits so that it aligns with the `ValType::Ref` discriminant, and
// after that has a two bit discriminant distinguishing the following variants:
//
// `(ref null? <type_index>)`: [ 000:i3 00:i2 nullable:i1 type_index:i26 ]
//         `(ref null? func)`: [ 000:i3 01:i2 nullable:i1                ]
//       `(ref null? extern)`: [ 000:i3 10:i2 nullable:i1                ]
//                     unused: [ 000:i3 11:i2                            ]
//
// Note that we only technically need 20 bits for the type index to fit every
// type index less than or equal to `crate::limits::MAX_WASM_TYPES`. So if we
// ever need them, we have 6 bits available in that first variant.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct RefType(u32);

impl std::fmt::Debug for RefType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self.is_nullable(), self.heap_type()) {
            (true, HeapType::Extern) => write!(f, "externref"),
            (false, HeapType::Extern) => write!(f, "(ref extern)"),
            (true, HeapType::Func) => write!(f, "funcref"),
            (false, HeapType::Func) => write!(f, "(ref func)"),
            (true, HeapType::TypedFunc(idx)) => write!(f, "(ref null {idx})"),
            (false, HeapType::TypedFunc(idx)) => write!(f, "(ref {idx})"),
        }
    }
}

impl RefType {
    const DISCRIMINANT_MASK: u32 = 0b11 << 27;

    const TYPED_FUNC_DISCRIMINANT: u32 = 0b00 << 27;
    const ANY_FUNC_DISCRIMINANT: u32 = 0b01 << 27;
    const EXTERN_DISCRIMINANT: u32 = 0b10 << 27;

    const NULLABLE_MASK: u32 = 1 << 26;
    const INDEX_MASK: u32 = u32::MAX >> 6;

    /// An nullable untyped function reference aka `(ref null func)` aka
    /// `funcref` aka `anyfunc`.
    pub const FUNCREF: Self = RefType(Self::ANY_FUNC_DISCRIMINANT | Self::NULLABLE_MASK);

    /// A nullable reference to an extern object aka `(ref null extern)` aka
    /// `externref`.
    pub const EXTERNREF: Self = RefType(Self::EXTERN_DISCRIMINANT | Self::NULLABLE_MASK);

    const fn can_represent_type_index(index: u32) -> bool {
        const _CAN_REPRESENT_ALL_TYPES_WITHIN_OUR_IMPL_LIMITS: () = {
            assert!(RefType::can_represent_type_index(
                crate::limits::MAX_WASM_TYPES as u32
            ));
        };
        index & Self::INDEX_MASK == index
    }

    /// Create a reference to a typed function with the type at the given index.
    ///
    /// Returns `None` when the type index is beyond this crate's implementation
    /// limits and therfore is not representable.
    pub fn typed_func(nullable: bool, index: u32) -> Option<Self> {
        if Self::can_represent_type_index(index) {
            let nullable = if nullable { Self::NULLABLE_MASK } else { 0 };
            Some(RefType(Self::TYPED_FUNC_DISCRIMINANT | nullable | index))
        } else {
            None
        }
    }

    /// Create a new `RefType`.
    ///
    /// Returns `None` when the heap type's type index (if any) is beyond this
    /// crate's implementation limits and therfore is not representable.
    pub fn new(nullable: bool, heap_type: HeapType) -> Option<Self> {
        let nullable32 = if nullable { Self::NULLABLE_MASK } else { 0 };
        match heap_type {
            HeapType::TypedFunc(index) => Self::typed_func(nullable, index),
            HeapType::Func => Some(Self(Self::ANY_FUNC_DISCRIMINANT | nullable32)),
            HeapType::Extern => Some(Self(Self::EXTERN_DISCRIMINANT | nullable32)),
        }
    }

    fn discriminant(&self) -> u32 {
        self.0 & Self::DISCRIMINANT_MASK
    }

    /// Is this a reference to a typed function?
    pub fn is_typed_func_ref(&self) -> bool {
        self.discriminant() == Self::TYPED_FUNC_DISCRIMINANT
    }

    /// If this is a reference to a typed function, get its type index.
    pub fn type_index(&self) -> Option<u32> {
        if self.is_typed_func_ref() {
            Some(self.0 & Self::INDEX_MASK)
        } else {
            None
        }
    }

    /// Is this an untyped function reference aka `(ref null func)` aka `funcref` aka `anyfunc`?
    pub fn is_func_ref(&self) -> bool {
        self.discriminant() == Self::ANY_FUNC_DISCRIMINANT
    }

    /// Is this a `(ref null extern)` aka `externref`?
    pub fn is_extern_ref(&self) -> bool {
        self.discriminant() == Self::EXTERN_DISCRIMINANT
    }

    /// Is this ref type nullable?
    pub fn is_nullable(&self) -> bool {
        self.0 & Self::NULLABLE_MASK != 0
    }

    /// Get the non-nullable version of this ref type.
    pub fn as_non_null(&self) -> Self {
        Self(self.0 & !Self::NULLABLE_MASK)
    }

    /// Get the heap type that this is a reference to.
    pub fn heap_type(&self) -> HeapType {
        match self.discriminant() {
            Self::TYPED_FUNC_DISCRIMINANT => HeapType::TypedFunc(self.type_index().unwrap()),
            Self::ANY_FUNC_DISCRIMINANT => HeapType::Func,
            Self::EXTERN_DISCRIMINANT => HeapType::Extern,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn as_u32(&self) -> u32 {
        self.0
    }

    #[inline]
    fn from_u32(x: u32) -> Self {
        debug_assert_eq!(x & (0b111 << 29), 0);
        debug_assert!(matches!(
            x & Self::DISCRIMINANT_MASK,
            Self::ANY_FUNC_DISCRIMINANT | Self::TYPED_FUNC_DISCRIMINANT | Self::EXTERN_DISCRIMINANT
        ));
        RefType(x)
    }
}

/// A heap type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum HeapType {
    /// Function of the type at the given index.
    TypedFunc(u32),
    /// Untyped (any) function.
    Func,
    /// Extern heap type.
    Extern,
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
            0x70 | 0x6F | 0x6B | 0x6C => Ok(ValType::ref_type(reader.read()?)),
            _ => bail!(reader.original_position(), "invalid value type"),
        }
    }
}

impl<'a> FromReader<'a> for RefType {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        match reader.read()? {
            0x70 => Ok(RefType::FUNCREF),
            0x6F => Ok(RefType::EXTERNREF),
            byte @ (0x6B | 0x6C) => {
                let nullable = byte == 0x6C;
                // NB: `FromReader for HeapType` only succeeds when
                // `RefType::new` will succeed, so we can unwrap here.
                Ok(RefType::new(nullable, reader.read()?).unwrap())
            }
            _ => bail!(reader.original_position(), "malformed reference type"),
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
            _ => {
                let idx = match u32::try_from(reader.read_var_s33()?) {
                    Ok(idx) if RefType::can_represent_type_index(idx) => idx,
                    Ok(_) => bail!(
                        reader.original_position(),
                        "heap type's index is beyond `wasmparser`'s implementation limits"
                    ),
                    Err(_) => {
                        bail!(reader.original_position(), "invalid function heap type",);
                    }
                };
                match idx.try_into() {
                    Ok(packed) => Ok(HeapType::TypedFunc(packed)),
                    Err(_) => {
                        bail!(reader.original_position(), "function index too large");
                    }
                }
            }
        }
    }
}

/// Represents a type in a WebAssembly module.
#[derive(Debug, Clone)]
pub enum Type {
    /// The type is for a function.
    Func(FuncType),
}

/// Represents a type of a function in a WebAssembly module.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct FuncType {
    /// The combined parameters and result types.
    params_results: Box<[ValType]>,
    /// The number of parameter types.
    len_params: usize,
}

impl Debug for FuncType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FuncType")
            .field("params", &self.params())
            .field("returns", &self.results())
            .finish()
    }
}

impl FuncType {
    /// Creates a new [`FuncType`] from the given `params` and `results`.
    pub fn new<P, R>(params: P, results: R) -> Self
    where
        P: IntoIterator<Item = ValType>,
        R: IntoIterator<Item = ValType>,
    {
        let mut buffer = params.into_iter().collect::<Vec<_>>();
        let len_params = buffer.len();
        buffer.extend(results);
        Self {
            params_results: buffer.into(),
            len_params,
        }
    }

    /// Creates a new [`FuncType`] fom its raw parts.
    ///
    /// # Panics
    ///
    /// If `len_params` is greater than the length of `params_results` combined.
    pub(crate) fn from_raw_parts(params_results: Box<[ValType]>, len_params: usize) -> Self {
        assert!(len_params <= params_results.len());
        Self {
            params_results,
            len_params,
        }
    }

    /// Returns a shared slice to the parameter types of the [`FuncType`].
    #[inline]
    pub fn params(&self) -> &[ValType] {
        &self.params_results[..self.len_params]
    }

    /// Returns a shared slice to the result types of the [`FuncType`].
    #[inline]
    pub fn results(&self) -> &[ValType] {
        &self.params_results[self.len_params..]
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
pub type TypeSectionReader<'a> = SectionLimited<'a, Type>;

impl<'a> FromReader<'a> for Type {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read_u8()? {
            0x60 => Type::Func(reader.read()?),
            x => return reader.invalid_leading_byte(x, "type"),
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
