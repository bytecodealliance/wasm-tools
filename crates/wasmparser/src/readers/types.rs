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
    BinaryReader, ComponentImport, Export, Import, Range, Result, SectionIteratorLimited,
    SectionReader, SectionWithLimitedItems,
};

/// Represents the types of values in a WebAssembly module.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// The type is i32.
    I32,
    /// The type is i64.
    I64,
    /// The type is f32.
    F32,
    /// The type is f64.
    F64,
    /// The type is v128.
    V128,
    /// The type is a function reference.
    FuncRef,
    /// The type is an extern reference.
    ExternRef,
}

/// Represents a type defined in a WebAssembly module.
#[derive(Debug, Clone)]
pub enum TypeDef {
    /// The type is a function.
    Func(FuncType),
}

/// Represents a type of a function in a WebAssembly module.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FuncType {
    /// The function parameter types.
    pub params: Box<[Type]>,
    /// The function result types.
    pub returns: Box<[Type]>,
}

/// Represents a table's type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TableType {
    /// The table's element type.
    pub element_type: Type,
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
    pub fn index_type(&self) -> Type {
        if self.memory64 {
            Type::I64
        } else {
            Type::I32
        }
    }
}

/// Represents a global's type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GlobalType {
    /// The global's type.
    pub content_type: Type,
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
#[derive(Clone)]
pub struct TypeSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> TypeSectionReader<'a> {
    /// Constructs a new `TypeSectionReader` for the given data and offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<Self> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(Self { reader, count })
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets a count of items in the section.
    pub fn get_count(&self) -> u32 {
        self.count
    }

    /// Reads content of the type section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::TypeSectionReader;
    /// let data: &[u8] = &[0x01, 0x60, 0x00, 0x00];
    /// let mut reader = TypeSectionReader::new(data, 0).unwrap();
    /// for _ in 0..reader.get_count() {
    ///     let ty = reader.read().expect("type");
    ///     println!("Type {:?}", ty);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<TypeDef> {
        self.reader.read_type_def()
    }
}

impl<'a> SectionReader for TypeSectionReader<'a> {
    type Item = TypeDef;

    fn read(&mut self) -> Result<Self::Item> {
        Self::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        Self::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for TypeSectionReader<'a> {
    fn get_count(&self) -> u32 {
        Self::get_count(self)
    }
}

impl<'a> IntoIterator for TypeSectionReader<'a> {
    type Item = Result<TypeDef>;
    type IntoIter = SectionIteratorLimited<Self>;

    /// Implements iterator over the type section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::TypeSectionReader;
    /// # let data: &[u8] = &[0x01, 0x60, 0x00, 0x00];
    /// let mut reader = TypeSectionReader::new(data, 0).unwrap();
    /// for ty in reader {
    ///     println!("Type {:?}", ty);
    /// }
    /// ```
    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}

/// Represents a type defined in a WebAssembly component.
#[derive(Debug, Clone)]
pub enum ComponentTypeDef<'a> {
    /// The type is a module type.
    Module(Box<[ModuleType<'a>]>),
    /// The type is a component type.
    Component(Box<[ComponentType<'a>]>),
    /// The type is an instance type.
    Instance(Box<[InstanceType<'a>]>),
    /// The type is a function type.
    Function(ComponentFuncType<'a>),
    /// The type is for a value type.
    Value(InterfaceType),
    /// The type is for a compound type.
    Compound(CompoundType<'a>),
}

/// Represents a module type definition in a WebAssembly component.
#[derive(Debug, Clone)]
pub enum ModuleType<'a> {
    /// The module type definition is for a type.
    Type(TypeDef),
    /// The module type definition is for an export.
    Export(Export<'a>),
    /// The module type definition is for an import.
    Import(Import<'a>),
}

/// Represents a component type definition in a WebAssembly component.
#[derive(Debug, Clone)]
pub enum ComponentType<'a> {
    /// The component type definition is for a type.
    Type(ComponentTypeDef<'a>),
    /// The component type definition is for an alias to an outer type.
    OuterType {
        /// The enclosing module count, starting at zero for current module.
        count: u32,
        /// The outer type index being aliased.
        index: u32,
    },
    /// The component type definition is for an export.
    ///
    /// The value is the type index in the component type's type index space.
    Export(u32),
    /// The component type definition is for an import.
    Import(ComponentImport<'a>),
}

/// Represents an instance type definition in a WebAssembly component.
#[derive(Debug, Clone)]
pub enum InstanceType<'a> {
    /// The instance type definition is for a type.
    Type(ComponentTypeDef<'a>),
    /// The instance type definition is for an alias to an outer type.
    OuterType {
        /// The enclosing module count, starting at zero for current module.
        count: u32,
        /// The outer type index being aliased.
        index: u32,
    },
    /// The instance type definition is for an export.
    ///
    /// The value is the type index in the instance type's type index space.
    Export(u32),
}

/// Represents a type of a function in a WebAssembly component.
#[derive(Debug, Clone)]
pub struct ComponentFuncType<'a> {
    /// The function parameter types.
    pub params: Box<[(&'a str, InterfaceType)]>,
    /// The function result type.
    pub result: InterfaceType,
}

/// Represents an interface type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterfaceType {
    /// The type is the unit type.
    Unit,
    /// The type is a boolean.
    Bool,
    /// The type is a signed 8-bit integer.
    S8,
    /// The type is an unsigned 8-bit integer.
    U8,
    /// The type is a signed 16-bit integer.
    S16,
    /// The type is an unsigned 16-bit integer.
    U16,
    /// The type is a signed 32-bit integer.
    S32,
    /// The type is an unsigned 32-bit integer.
    U32,
    /// The type is a signed 64-bit integer.
    S64,
    /// The type is an unsigned 64-bit integer.
    U64,
    /// The type is a 32-bit floating point number.
    F32,
    /// The type is a 64-bit floating point number.
    F64,
    /// The type is a Unicode character.
    Char,
    /// The type is a string.
    String,
    /// The type is a compound interface type.
    ///
    /// The value is a type index to a compound type.
    Compound(u32),
}

/// Represents a compound interface type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompoundType<'a> {
    /// The type is a record with the given fields.
    Record(Box<[(&'a str, InterfaceType)]>),
    /// The type is a variant with the given cases.
    Variant {
        /// The cases of the variant.
        cases: Box<[(&'a str, InterfaceType)]>,
        /// The index of the default case of the variant.
        default: Option<u32>,
    },
    /// The type is a list of the given interface type.
    List(InterfaceType),
    /// The type is a tuple of the given interface types.
    Tuple(Box<[InterfaceType]>),
    /// The type is flags with the given names.
    Flags(Box<[&'a str]>),
    /// The type is an enum with the given tags.
    Enum(Box<[&'a str]>),
    /// The type is a union of the given interface types.
    Union(Box<[InterfaceType]>),
    /// The type is an optional of the given interface type.
    Optional(InterfaceType),
    /// The type is an expected type.
    Expected {
        /// The type returned for success.
        ok: InterfaceType,
        /// The type returned for failure.
        error: InterfaceType,
    },
}

/// A reader for the type section of a WebAssembly component.
#[derive(Clone)]
pub struct ComponentTypeSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> ComponentTypeSectionReader<'a> {
    /// Constructs a new `ComponentTypeSectionReader` for the given data and offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<Self> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(Self { reader, count })
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets a count of items in the section.
    pub fn get_count(&self) -> u32 {
        self.count
    }

    /// Reads content of the type section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::ComponentTypeSectionReader;
    /// let data: &[u8] = &[0x01, 0x4c, 0x01, 0x03, b'f', b'o', b'o', 0x72, 0x72];
    /// let mut reader = ComponentTypeSectionReader::new(data, 0).unwrap();
    /// for _ in 0..reader.get_count() {
    ///     let ty = reader.read().expect("type");
    ///     println!("Type {:?}", ty);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<ComponentTypeDef<'a>> {
        self.reader.read_component_type_def()
    }
}

impl<'a> SectionReader for ComponentTypeSectionReader<'a> {
    type Item = ComponentTypeDef<'a>;

    fn read(&mut self) -> Result<Self::Item> {
        Self::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        Self::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for ComponentTypeSectionReader<'a> {
    fn get_count(&self) -> u32 {
        Self::get_count(self)
    }
}

impl<'a> IntoIterator for ComponentTypeSectionReader<'a> {
    type Item = Result<ComponentTypeDef<'a>>;
    type IntoIter = SectionIteratorLimited<Self>;

    /// Implements iterator over the type section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::TypeSectionReader;
    /// # let data: &[u8] = &[0x01, 0x4c, 0x01, 0x03, b'f', b'o', b'o', 0x72, 0x72];
    /// let mut reader = TypeSectionReader::new(data, 0).unwrap();
    /// for ty in reader {
    ///     println!("Type {:?}", ty);
    /// }
    /// ```
    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}
