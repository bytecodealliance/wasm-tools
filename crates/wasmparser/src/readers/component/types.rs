use crate::{
    BinaryReader, ComponentImport, Import, Range, Result, SectionIteratorLimited, SectionReader,
    SectionWithLimitedItems, TypeDef, TypeRef,
};

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
    Export {
        /// The name of the exported item.
        name: &'a str,
        /// The type of the exported
        ty: TypeRef,
    },
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
    Export {
        /// The name of the exported item.
        name: &'a str,
        /// The type index of the exported item.
        ty: u32,
    },
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
    Export {
        /// The name of the exported item.
        name: &'a str,
        /// The type index of the exported item.
        ty: u32,
    },
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
    /// use wasmparser::ComponentTypeSectionReader;
    /// # let data: &[u8] = &[0x01, 0x4c, 0x01, 0x03, b'f', b'o', b'o', 0x72, 0x72];
    /// let mut reader = ComponentTypeSectionReader::new(data, 0).unwrap();
    /// for ty in reader {
    ///     println!("Type {:?}", ty.expect("type"));
    /// }
    /// ```
    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}
