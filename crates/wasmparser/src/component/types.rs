use crate::{
    BinaryReader, BinaryReaderError, FuncType, GlobalType, MemoryType, Range, Result,
    SectionIteratorLimited, SectionReader, SectionWithLimitedItems, TableType,
};

use super::{ALIAS_KIND_OUTER, ALIAS_KIND_OUTER_TYPE};

const TYPE_INSTANCE: u32 = 0x7f;
const TYPE_MODULE: u32 = 0x7e;
const TYPE_FUNCTION: u32 = 0x7d;
const TYPE_ADAPTER_FUNCTION: u32 = 0x7c;

const COMPOUND_TYPE_LIST: u32 = 0x7b;
const COMPOUND_TYPE_RECORD: u32 = 0x7a;
const COMPOUND_TYPE_VARIANT: u32 = 0x79;
const COMPOUND_TYPE_TUPLE: u32 = 0x78;
const COMPOUND_TYPE_FLAGS: u32 = 0x77;
const COMPOUND_TYPE_ENUM: u32 = 0x76;
const COMPOUND_TYPE_UNION: u32 = 0x75;
const COMPOUND_TYPE_OPTIONAL: u32 = 0x74;
const COMPOUND_TYPE_EXPECTED: u32 = 0x73;
const COMPOUND_TYPE_NAMED: u32 = 0x72;

const INTERFACE_TYPE_BOOL: u32 = 0x71;
const INTERFACE_TYPE_S8: u32 = 0x70;
const INTERFACE_TYPE_U8: u32 = 0x6f;
const INTERFACE_TYPE_S16: u32 = 0x6e;
const INTERFACE_TYPE_U16: u32 = 0x6d;
const INTERFACE_TYPE_S32: u32 = 0x6c;
const INTERFACE_TYPE_U32: u32 = 0x6b;
const INTERFACE_TYPE_S64: u32 = 0x6a;
const INTERFACE_TYPE_U64: u32 = 0x69;
const INTERFACE_TYPE_F32: u32 = 0x68;
const INTERFACE_TYPE_F64: u32 = 0x67;
const INTERFACE_TYPE_CHAR: u32 = 0x66;
const INTERFACE_TYPE_STRING: u32 = 0x65;

const INSTANCE_TYPEDEF_TYPE: u32 = 0x01;
const INSTANCE_TYPEDEF_ALIAS: u32 = 0x05;
const INSTANCE_TYPEDEF_EXPORT: u32 = 0x06;

const MODULE_TYPEDEF_TYPE: u32 = 0x01;
const MODULE_TYPEDEF_ALIAS: u32 = 0x05;
const MODULE_TYPEDEF_EXPORT: u32 = 0x06;
const MODULE_TYPEDEF_IMPORT: u32 = 0x02;

pub(crate) const TYPE_REF_INSTANCE: u32 = 0x00;
pub(crate) const TYPE_REF_MODULE: u32 = 0x01;
pub(crate) const TYPE_REF_FUNCTION: u32 = 0x02;
pub(crate) const TYPE_REF_TABLE: u32 = 0x03;
pub(crate) const TYPE_REF_MEMORY: u32 = 0x04;
pub(crate) const TYPE_REF_GLOBAL: u32 = 0x05;
pub(crate) const TYPE_REF_ADAPTER_FUNCTION: u32 = 0x06;

/// Represents a reference to a type definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeRef {
    /// The definition is an instance.
    ///
    /// The value is an index in the types index space.
    Instance(u32),
    /// The definition is a module.
    ///
    /// The value is an index in the types index space.
    Module(u32),
    /// The definition is a core wasm function.
    ///
    /// The value is an index in the types index space.
    Function(u32),
    /// The definition is a core wasm table.
    Table(TableType),
    /// The definition is a core wasm memory.
    Memory(MemoryType),
    /// The definition is a core wasm global.
    Global(GlobalType),
    /// The definition is an adapter function.
    ///
    /// The value is an index in the types index space.
    AdapterFunction(u32),
}

impl TypeRef {
    pub(crate) fn new(reader: &mut BinaryReader) -> Result<Self> {
        Ok(match reader.read_u8()? {
            TYPE_REF_INSTANCE => TypeRef::Instance(reader.read_var_u32()?),
            TYPE_REF_MODULE => TypeRef::Module(reader.read_var_u32()?),
            TYPE_REF_FUNCTION => TypeRef::Function(reader.read_var_u32()?),
            TYPE_REF_TABLE => TypeRef::Table(reader.read_table_type()?),
            TYPE_REF_MEMORY => TypeRef::Memory(reader.read_memory_type()?),
            TYPE_REF_GLOBAL => TypeRef::Global(reader.read_global_type()?),
            TYPE_REF_ADAPTER_FUNCTION => TypeRef::AdapterFunction(reader.read_var_u32()?),
            x => {
                return Err(BinaryReaderError::new(
                    format!("invalid byte (0x{:x}) in type reference", x),
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// Represents an index for an outer alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OuterAliasIndex {
    /// The index is a type index.
    Type(u32),
}

impl OuterAliasIndex {
    fn new(reader: &mut BinaryReader) -> Result<Self> {
        let index = reader.read_var_u32()?;
        Ok(match reader.read_u8()? {
            ALIAS_KIND_OUTER_TYPE => Self::Type(index),
            _ => {
                return Err(BinaryReaderError::new(
                    "expected an alias to a type for type definition",
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// Represents an instance type definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstanceTypeDef<'a> {
    /// The instance type definition is for a type.
    Type(TypeDef<'a>),
    /// The instance type definition is for an outer alias.
    Alias {
        /// The enclosing module count, starting at zero for current module.
        count: u32,
        /// The outer index being aliased.
        index: OuterAliasIndex,
    },
    /// The instance type definition is for an instance export.
    Export {
        /// The export's name.
        name: &'a str,
        /// The export's type.
        ty: TypeRef,
    },
}

impl<'a> InstanceTypeDef<'a> {
    fn new(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read_u8()? {
            INSTANCE_TYPEDEF_TYPE => Self::Type(TypeDef::new(reader)?),
            INSTANCE_TYPEDEF_ALIAS => match reader.read_u8()? {
                ALIAS_KIND_OUTER => Self::Alias {
                    count: reader.read_var_u32()?,
                    index: OuterAliasIndex::new(reader)?,
                },
                x => {
                    return Err(BinaryReaderError::new(
                        format!("unexpected alias kind (0x{:x}) encountered in instance type definition", x),
                        reader.original_position() - 1,
                    ));
                }
            },
            INSTANCE_TYPEDEF_EXPORT => Self::Export {
                name: reader.read_string()?,
                ty: TypeRef::new(reader)?,
            },
            x => {
                return Err(BinaryReaderError::new(
                    format!(
                        "unexpected kind (0x{:x}) encountered in instance type definition",
                        x
                    ),
                    reader.original_position() - 1,
                ));
            }
        })
    }
}

/// Represents a module type definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleTypeDef<'a> {
    /// The module type definition is for a type.
    Type(TypeDef<'a>),
    /// The module type definition is for an outer alias.
    Alias {
        /// The enclosing module count, starting at zero for current module.
        count: u32,
        /// The outer index being aliased.
        index: OuterAliasIndex,
    },
    /// The module type definition is for an export.
    Export {
        /// The export's name.
        name: &'a str,
        /// The export's type.
        ty: TypeRef,
    },
    /// The module type definition is for an import.
    Import {
        /// The import's name.
        name: &'a str,
        /// The import's type.
        ty: TypeRef,
    },
}

impl<'a> ModuleTypeDef<'a> {
    fn new(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read_u8()? {
            MODULE_TYPEDEF_TYPE => Self::Type(TypeDef::new(reader)?),
            MODULE_TYPEDEF_ALIAS => match reader.read_u8()? {
                ALIAS_KIND_OUTER => Self::Alias {
                    count: reader.read_var_u32()?,
                    index: OuterAliasIndex::new(reader)?,
                },
                _ => {
                    return Err(BinaryReaderError::new(
                        "expected an outer alias for module type definition",
                        reader.original_position() - 1,
                    ));
                }
            },
            MODULE_TYPEDEF_EXPORT => Self::Export {
                name: reader.read_string()?,
                ty: TypeRef::new(reader)?,
            },
            MODULE_TYPEDEF_IMPORT => Self::Import {
                name: reader.read_string()?,
                ty: TypeRef::new(reader)?,
            },
            _ => {
                return Err(BinaryReaderError::new(
                    "invalid module type definition",
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// Represents an interface type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterfaceType {
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

impl InterfaceType {
    fn new(reader: &mut BinaryReader) -> Result<Self> {
        Ok(match reader.read_var_u32()? {
            INTERFACE_TYPE_BOOL => Self::Bool,
            INTERFACE_TYPE_S8 => Self::S8,
            INTERFACE_TYPE_U8 => Self::U8,
            INTERFACE_TYPE_S16 => Self::S16,
            INTERFACE_TYPE_U16 => Self::U16,
            INTERFACE_TYPE_S32 => Self::S32,
            INTERFACE_TYPE_U32 => Self::U32,
            INTERFACE_TYPE_S64 => Self::S64,
            INTERFACE_TYPE_U64 => Self::U64,
            INTERFACE_TYPE_F32 => Self::F32,
            INTERFACE_TYPE_F64 => Self::F64,
            INTERFACE_TYPE_CHAR => Self::Char,
            INTERFACE_TYPE_STRING => Self::String,
            i => Self::Compound(i),
        })
    }

    fn maybe(reader: &mut BinaryReader) -> Result<Option<Self>> {
        Ok(match reader.read_u8()? {
            0 => None,
            1 => Some(Self::new(reader)?),
            x => {
                return Err(BinaryReaderError::new(
                    format!("invalid byte (0x{:x}) in type definition", x),
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// Represents a compound interface type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompoundType<'a> {
    /// The type is a list of the given interface type.
    List(InterfaceType),
    /// The type is a record with the given fields.
    Record(Box<[(&'a str, InterfaceType)]>),
    /// The type is a variant with the given cases.
    Variant(Box<[(&'a str, Option<InterfaceType>)]>),
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
        ok: Option<InterfaceType>,
        /// The type returned for failure.
        error: Option<InterfaceType>,
    },
    /// The type is a named type.
    Named {
        /// The name of the type.
        name: &'a str,
        /// The interface type being named.
        ty: InterfaceType,
    },
}

impl<'a> CompoundType<'a> {
    fn new(leading: u32, reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match leading {
            COMPOUND_TYPE_LIST => CompoundType::List(InterfaceType::new(reader)?),
            COMPOUND_TYPE_RECORD => CompoundType::Record(
                (0..reader.read_var_u32()?)
                    .map(|_| {
                        let name = reader.read_string()?;
                        let ty = InterfaceType::new(reader)?;
                        Ok((name, ty))
                    })
                    .collect::<Result<_>>()?,
            ),
            COMPOUND_TYPE_VARIANT => CompoundType::Variant(
                (0..reader.read_var_u32()?)
                    .map(|_| {
                        let name = reader.read_string()?;
                        let ty = InterfaceType::maybe(reader)?;
                        Ok((name, ty))
                    })
                    .collect::<Result<_>>()?,
            ),
            COMPOUND_TYPE_TUPLE => CompoundType::Tuple(
                (0..reader.read_var_u32()?)
                    .map(|_| InterfaceType::new(reader))
                    .collect::<Result<_>>()?,
            ),
            COMPOUND_TYPE_FLAGS => CompoundType::Flags(
                (0..reader.read_var_u32()?)
                    .map(|_| reader.read_string())
                    .collect::<Result<_>>()?,
            ),
            COMPOUND_TYPE_ENUM => CompoundType::Enum(
                (0..reader.read_var_u32()?)
                    .map(|_| reader.read_string())
                    .collect::<Result<_>>()?,
            ),
            COMPOUND_TYPE_UNION => CompoundType::Union(
                (0..reader.read_var_u32()?)
                    .map(|_| InterfaceType::new(reader))
                    .collect::<Result<_>>()?,
            ),
            COMPOUND_TYPE_OPTIONAL => CompoundType::Optional(InterfaceType::new(reader)?),
            COMPOUND_TYPE_EXPECTED => CompoundType::Expected {
                ok: InterfaceType::maybe(reader)?,
                error: InterfaceType::maybe(reader)?,
            },
            COMPOUND_TYPE_NAMED => CompoundType::Named {
                name: reader.read_string()?,
                ty: InterfaceType::new(reader)?,
            },
            x => {
                return Err(BinaryReaderError::new(
                    format!("invalid leading byte (0x{:x}) in type definition", x),
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// Represents a name-type pair.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameTypePair<'a> {
    /// The optional name.
    pub name: Option<&'a str>,
    /// The type.
    pub ty: InterfaceType,
}

/// Represents a type definition in the types section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDef<'a> {
    /// The type is an instance.
    Instance(Box<[InstanceTypeDef<'a>]>),
    /// The type is a module.
    Module(Box<[ModuleTypeDef<'a>]>),
    /// The type is a core WebAssembly function.
    Function(FuncType),
    /// The type is a function adapter.
    AdapterFunction {
        /// The function's parameter types.
        params: Box<[NameTypePair<'a>]>,
        /// The function's result types.
        results: Box<[NameTypePair<'a>]>,
    },
    /// The type is a compound interface type.
    Compound(CompoundType<'a>),
}

impl<'a> TypeDef<'a> {
    fn new(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read_u8()? {
            TYPE_INSTANCE => TypeDef::Instance(
                (0..reader.read_var_u32()?)
                    .map(|_| InstanceTypeDef::new(reader))
                    .collect::<Result<_>>()?,
            ),
            TYPE_MODULE => TypeDef::Module(
                (0..reader.read_var_u32()?)
                    .map(|_| ModuleTypeDef::new(reader))
                    .collect::<Result<_>>()?,
            ),
            TYPE_FUNCTION => TypeDef::Function(reader.read_func_type()?),
            TYPE_ADAPTER_FUNCTION => TypeDef::AdapterFunction {
                params: Self::maybe_named_types(reader)?,
                results: Self::maybe_named_types(reader)?,
            },
            // All other supported leading bytes should be compound types
            leading => TypeDef::Compound(CompoundType::new(leading, reader)?),
        })
    }

    fn maybe_named_types(reader: &mut BinaryReader<'a>) -> Result<Box<[NameTypePair<'a>]>> {
        Ok(match reader.read_u8()? {
            0 => (0..reader.read_var_u32()?)
                .map(|_| {
                    Ok(NameTypePair {
                        name: None,
                        ty: InterfaceType::new(reader)?,
                    })
                })
                .collect::<Result<_>>()?,
            1 => (0..reader.read_var_u32()?)
                .map(|_| {
                    Ok(NameTypePair {
                        name: Some(reader.read_string()?),
                        ty: InterfaceType::new(reader)?,
                    })
                })
                .collect::<Result<_>>()?,
            x => {
                return Err(BinaryReaderError::new(
                    format!("invalid byte (0x{:x}) in type definition", x),
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// The type section reader for a WebAssembly component.
#[derive(Clone)]
pub struct TypeSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> TypeSectionReader<'a> {
    /// Creates a new type section reader for the given data and initial offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<Self> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(Self { reader, count })
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets the number of types in the section.
    pub fn len(&self) -> u32 {
        self.count
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Reads types from the type section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::component::TypeSectionReader;
    /// # let data: &[u8] = &[0x01, 0x7b, 0x65];
    /// let types = TypeSectionReader::new(data, 0).unwrap();
    /// for ty in types {
    ///     let ty = ty.expect("type");
    ///     println!("{:?}", ty);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<TypeDef<'a>> {
        TypeDef::new(&mut self.reader)
    }
}

impl<'a> SectionReader for TypeSectionReader<'a> {
    type Item = TypeDef<'a>;

    fn read(&mut self) -> Result<Self::Item> {
        TypeSectionReader::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        TypeSectionReader::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for TypeSectionReader<'a> {
    fn get_count(&self) -> u32 {
        TypeSectionReader::len(self)
    }
}

impl<'a> IntoIterator for TypeSectionReader<'a> {
    type Item = Result<TypeDef<'a>>;
    type IntoIter = SectionIteratorLimited<TypeSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Type;
    use anyhow::Result;

    #[test]
    fn it_parses_an_empty_section() -> Result<()> {
        let data: &[u8] = &[0];
        let reader = TypeSectionReader::new(data, 0)?;
        assert!(reader.eof());
        assert_eq!(reader.len(), 0);
        Ok(())
    }

    #[test]
    fn it_parses_an_instance_definition() -> Result<()> {
        let data: &[u8] = &[
            1,
            TYPE_INSTANCE as u8,
            3,
            INSTANCE_TYPEDEF_TYPE as u8,
            COMPOUND_TYPE_LIST as u8,
            INTERFACE_TYPE_STRING as u8,
            INSTANCE_TYPEDEF_ALIAS as u8,
            ALIAS_KIND_OUTER as u8,
            6,
            101,
            ALIAS_KIND_OUTER_TYPE as u8,
            INSTANCE_TYPEDEF_EXPORT as u8,
            3,
            b'f',
            b'o',
            b'o',
            TYPE_REF_FUNCTION as u8,
            7,
        ];
        let mut reader = TypeSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        match reader.read()? {
            TypeDef::Instance(defs) => {
                assert_eq!(defs.len(), 3);
                assert_eq!(
                    defs[0],
                    InstanceTypeDef::Type(TypeDef::Compound(CompoundType::List(
                        InterfaceType::String
                    )))
                );
                assert_eq!(
                    defs[1],
                    InstanceTypeDef::Alias {
                        count: 6,
                        index: OuterAliasIndex::Type(101)
                    }
                );
                assert_eq!(
                    defs[2],
                    InstanceTypeDef::Export {
                        name: "foo",
                        ty: TypeRef::Function(7)
                    }
                );
            }
            _ => panic!("unexpected type"),
        }

        assert!(reader.eof());
        Ok(())
    }

    #[test]
    fn it_parses_a_module_definition() -> Result<()> {
        let data: &[u8] = &[
            1,
            TYPE_MODULE as u8,
            4,
            MODULE_TYPEDEF_TYPE as u8,
            COMPOUND_TYPE_OPTIONAL as u8,
            INTERFACE_TYPE_STRING as u8,
            MODULE_TYPEDEF_ALIAS as u8,
            ALIAS_KIND_OUTER as u8,
            10,
            42,
            ALIAS_KIND_OUTER_TYPE as u8,
            MODULE_TYPEDEF_EXPORT as u8,
            3,
            b'f',
            b'o',
            b'o',
            TYPE_REF_FUNCTION as u8,
            7,
            MODULE_TYPEDEF_IMPORT as u8,
            3,
            b'b',
            b'a',
            b'r',
            TYPE_REF_FUNCTION as u8,
            8,
        ];
        let mut reader = TypeSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        match reader.read()? {
            TypeDef::Module(defs) => {
                assert_eq!(defs.len(), 4);
                assert_eq!(
                    defs[0],
                    ModuleTypeDef::Type(TypeDef::Compound(CompoundType::Optional(
                        InterfaceType::String
                    )))
                );
                assert_eq!(
                    defs[1],
                    ModuleTypeDef::Alias {
                        count: 10,
                        index: OuterAliasIndex::Type(42)
                    }
                );
                assert_eq!(
                    defs[2],
                    ModuleTypeDef::Export {
                        name: "foo",
                        ty: TypeRef::Function(7)
                    }
                );
                assert_eq!(
                    defs[3],
                    ModuleTypeDef::Import {
                        name: "bar",
                        ty: TypeRef::Function(8)
                    }
                )
            }
            _ => panic!("unexpected type"),
        }

        assert!(reader.eof());
        Ok(())
    }

    #[test]
    fn it_parses_a_func_type() -> Result<()> {
        let data: &[u8] = &[
            1,
            TYPE_FUNCTION as u8,
            4,
            0x7f, // i32
            0x7e, // i64
            0x7d, // f32
            0x7c, // f64
            2,
            0x7e, // i64
            0x7f, // i32
        ];
        let mut reader = TypeSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        match reader.read()? {
            TypeDef::Function(ty) => {
                assert_eq!(*ty.params, [Type::I32, Type::I64, Type::F32, Type::F64]);
                assert_eq!(*ty.returns, [Type::I64, Type::I32]);
            }
            _ => panic!("unexpected type"),
        }

        assert!(reader.eof());
        Ok(())
    }

    #[test]
    fn it_parses_a_nameless_adapter_func_type() -> Result<()> {
        let data: &[u8] = &[
            0x01,
            TYPE_ADAPTER_FUNCTION as u8,
            0,
            4,
            INTERFACE_TYPE_STRING as u8,
            INTERFACE_TYPE_BOOL as u8,
            INTERFACE_TYPE_S64 as u8,
            INTERFACE_TYPE_CHAR as u8,
            0,
            2,
            INTERFACE_TYPE_F64 as u8,
            INTERFACE_TYPE_S16 as u8,
        ];
        let mut reader = TypeSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        match reader.read()? {
            TypeDef::AdapterFunction { params, results } => {
                assert_eq!(
                    *params,
                    [
                        NameTypePair {
                            name: None,
                            ty: InterfaceType::String
                        },
                        NameTypePair {
                            name: None,
                            ty: InterfaceType::Bool
                        },
                        NameTypePair {
                            name: None,
                            ty: InterfaceType::S64
                        },
                        NameTypePair {
                            name: None,
                            ty: InterfaceType::Char
                        }
                    ]
                );
                assert_eq!(
                    *results,
                    [
                        NameTypePair {
                            name: None,
                            ty: InterfaceType::F64
                        },
                        NameTypePair {
                            name: None,
                            ty: InterfaceType::S16
                        }
                    ]
                );
            }
            _ => panic!("unexpected type"),
        }

        assert!(reader.eof());
        Ok(())
    }

    #[test]
    fn it_parses_a_named_adapter_func_type() -> Result<()> {
        let data: &[u8] = &[
            0x01,
            TYPE_ADAPTER_FUNCTION as u8,
            1,
            4,
            3,
            b'a',
            b'b',
            b'c',
            INTERFACE_TYPE_STRING as u8,
            1,
            b'd',
            INTERFACE_TYPE_S16 as u8,
            2,
            b'e',
            b'f',
            INTERFACE_TYPE_S32 as u8,
            4,
            b'g',
            b'h',
            b'i',
            b'j',
            INTERFACE_TYPE_U8 as u8,
            1,
            2,
            3,
            b'k',
            b'l',
            b'm',
            INTERFACE_TYPE_F32 as u8,
            1,
            b'n',
            INTERFACE_TYPE_S8 as u8,
        ];
        let mut reader = TypeSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        match reader.read()? {
            TypeDef::AdapterFunction { params, results } => {
                assert_eq!(
                    *params,
                    [
                        NameTypePair {
                            name: Some("abc"),
                            ty: InterfaceType::String
                        },
                        NameTypePair {
                            name: Some("d"),
                            ty: InterfaceType::S16
                        },
                        NameTypePair {
                            name: Some("ef"),
                            ty: InterfaceType::S32
                        },
                        NameTypePair {
                            name: Some("ghij"),
                            ty: InterfaceType::U8
                        },
                    ]
                );
                assert_eq!(
                    *results,
                    [
                        NameTypePair {
                            name: Some("klm"),
                            ty: InterfaceType::F32
                        },
                        NameTypePair {
                            name: Some("n"),
                            ty: InterfaceType::S8
                        },
                    ]
                );
            }
            _ => panic!("unexpected type"),
        }

        assert!(reader.eof());
        Ok(())
    }

    #[test]
    fn it_parses_compound_types() -> Result<()> {
        let data: &[u8] = &[
            10,
            COMPOUND_TYPE_LIST as u8,
            INTERFACE_TYPE_STRING as u8,
            COMPOUND_TYPE_RECORD as u8,
            2,
            3,
            b'f',
            b'o',
            b'o',
            INTERFACE_TYPE_S8 as u8,
            3,
            b'b',
            b'a',
            b'r',
            INTERFACE_TYPE_U8 as u8,
            COMPOUND_TYPE_VARIANT as u8,
            2,
            3,
            b'b',
            b'a',
            b'z',
            0,
            3,
            b'j',
            b'a',
            b'm',
            1,
            INTERFACE_TYPE_STRING as u8,
            COMPOUND_TYPE_TUPLE as u8,
            4,
            INTERFACE_TYPE_CHAR as u8,
            INTERFACE_TYPE_STRING as u8,
            INTERFACE_TYPE_S16 as u8,
            INTERFACE_TYPE_S32 as u8,
            COMPOUND_TYPE_FLAGS as u8,
            3,
            1,
            b'a',
            1,
            b'b',
            1,
            b'c',
            COMPOUND_TYPE_ENUM as u8,
            3,
            1,
            b'd',
            1,
            b'e',
            1,
            b'f',
            COMPOUND_TYPE_UNION as u8,
            2,
            INTERFACE_TYPE_CHAR as u8,
            INTERFACE_TYPE_STRING as u8,
            COMPOUND_TYPE_OPTIONAL as u8,
            INTERFACE_TYPE_BOOL as u8,
            COMPOUND_TYPE_EXPECTED as u8,
            0,
            1,
            INTERFACE_TYPE_STRING as u8,
            COMPOUND_TYPE_NAMED as u8,
            3,
            b'f',
            b'o',
            b'o',
            INTERFACE_TYPE_S8 as u8,
        ];
        let reader = TypeSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 10);

        let types: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;
        assert_eq!(
            types,
            [
                TypeDef::Compound(CompoundType::List(InterfaceType::String)),
                TypeDef::Compound(CompoundType::Record(
                    [("foo", InterfaceType::S8), ("bar", InterfaceType::U8)].into()
                )),
                TypeDef::Compound(CompoundType::Variant(
                    [("baz", None), ("jam", Some(InterfaceType::String))].into()
                )),
                TypeDef::Compound(CompoundType::Tuple(
                    [
                        InterfaceType::Char,
                        InterfaceType::String,
                        InterfaceType::S16,
                        InterfaceType::S32,
                    ]
                    .into()
                )),
                TypeDef::Compound(CompoundType::Flags(["a", "b", "c"].into())),
                TypeDef::Compound(CompoundType::Enum(["d", "e", "f"].into())),
                TypeDef::Compound(CompoundType::Union(
                    [InterfaceType::Char, InterfaceType::String].into()
                )),
                TypeDef::Compound(CompoundType::Optional(InterfaceType::Bool)),
                TypeDef::Compound(CompoundType::Expected {
                    ok: None,
                    error: Some(InterfaceType::String)
                }),
                TypeDef::Compound(CompoundType::Named {
                    name: "foo",
                    ty: InterfaceType::S8
                }),
            ]
        );
        Ok(())
    }
}
