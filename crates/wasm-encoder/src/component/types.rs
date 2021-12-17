use super::{ComponentSection, SectionId, TypeRef};
use crate::{encoders, ValType};

const TYPE_INSTANCE: u8 = 0x7f;
const TYPE_MODULE: u8 = 0x7e;
const TYPE_FUNCTION: u8 = 0x7d;
const TYPE_ADAPTER_FUNCTION: u8 = 0x7c;

const COMPOUND_INTERFACE_TYPE_LIST: u8 = 0x7b;
const COMPOUND_INTERFACE_TYPE_RECORD: u8 = 0x7a;
const COMPOUND_INTERFACE_TYPE_VARIANT: u8 = 0x79;
const COMPOUND_INTERFACE_TYPE_TUPLE: u8 = 0x78;
const COMPOUND_INTERFACE_TYPE_FLAGS: u8 = 0x77;
const COMPOUND_INTERFACE_TYPE_ENUM: u8 = 0x76;
const COMPOUND_INTERFACE_TYPE_UNION: u8 = 0x75;
const COMPOUND_INTERFACE_TYPE_OPTIONAL: u8 = 0x74;
const COMPOUND_INTERFACE_TYPE_EXPECTED: u8 = 0x73;
const COMPOUND_INTERFACE_TYPE_NAMED: u8 = 0x72;

const INTERFACE_TYPE_BOOL: u8 = 0x71;
const INTERFACE_TYPE_S8: u8 = 0x70;
const INTERFACE_TYPE_U8: u8 = 0x6f;
const INTERFACE_TYPE_S16: u8 = 0x6e;
const INTERFACE_TYPE_U16: u8 = 0x6d;
const INTERFACE_TYPE_S32: u8 = 0x6c;
const INTERFACE_TYPE_U32: u8 = 0x6b;
const INTERFACE_TYPE_S64: u8 = 0x6a;
const INTERFACE_TYPE_U64: u8 = 0x69;
const INTERFACE_TYPE_F32: u8 = 0x68;
const INTERFACE_TYPE_F64: u8 = 0x67;
const INTERFACE_TYPE_CHAR: u8 = 0x66;
const INTERFACE_TYPE_STRING: u8 = 0x65;
const INTERFACE_TYPE_UNIT: u8 = 0x64;

const ALIAS_TYPE_INSTANCE_EXPORT: u8 = 0x00;
const ALIAS_TYPE_OUTER: u8 = 0x01;

const OUTER_ALIAS_MODULE: u8 = 0x01;
const OUTER_ALIAS_TYPE: u8 = 0x06;

const INSTANCE_TYPEDEF_TYPE: u8 = 0x01;
const INSTANCE_TYPEDEF_ALIAS: u8 = 0x05;
const INSTANCE_TYPEDEF_EXPORT: u8 = 0x06;

const MODULE_TYPEDEF_TYPE: u8 = 0x01;
const MODULE_TYPEDEF_ALIAS: u8 = 0x05;
const MODULE_TYPEDEF_EXPORT: u8 = 0x06;
const MODULE_TYPEDEF_IMPORT: u8 = 0x02;

/// Represents a definition kind.
///
/// This is used in aliases to specify the kind of the definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefinitionKind {
    /// The definition is an instance.
    Instance,
    /// The definition is a module.
    Module,
    /// The definition is a function.
    Function,
    /// The definition is a table.
    Table,
    /// The definition is a memory.
    Memory,
    /// The definition is a global.
    Global,
}

/// Represents an index for an outer alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OuterAliasIndex {
    /// The index is a module index.
    Module(u32),
    /// The index is a type index.
    Type(u32),
}

impl OuterAliasIndex {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::Module(index) => {
                bytes.extend(encoders::u32(*index));
                bytes.push(OUTER_ALIAS_MODULE);
            }
            Self::Type(index) => {
                bytes.extend(encoders::u32(*index));
                bytes.push(OUTER_ALIAS_TYPE);
            }
        }
    }
}

/// Represents an alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Alias<'a> {
    /// The alias is to an instance export.
    InstanceExport {
        /// The index of the instance being aliased.
        instance: u32,
        /// The name of the export.
        name: &'a str,
        /// The kind of the definition being aliased.
        kind: DefinitionKind,
    },
    /// The alias is to an index in an outer module.
    Outer {
        /// The count of the outer module.
        ///
        /// The count starts at 0 which denotes the current module.
        count: u32,
        /// The index from the outer module's index space being aliased.
        index: OuterAliasIndex,
    },
}

impl Alias<'_> {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::InstanceExport {
                instance,
                name,
                kind,
            } => {
                bytes.push(ALIAS_TYPE_INSTANCE_EXPORT);
                bytes.extend(encoders::u32(*instance));
                bytes.extend(encoders::str(name));
                bytes.push(*kind as u8);
            }
            Self::Outer { count, index } => {
                bytes.push(ALIAS_TYPE_OUTER);
                bytes.extend(encoders::u32(*count));
                index.encode(bytes);
            }
        }
    }
}

/// Represents a named interface type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NamedInterfaceType<'a> {
    name: &'a str,
    ty: InterfaceType,
}

impl NamedInterfaceType<'_> {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.extend(encoders::str(self.name));
        self.ty.encode(bytes);
    }
}

/// Represents results of an adapter function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AdapterFunctionResults<'a> {
    /// The adapter function returns zero results.
    None,
    /// The adapter function returns one unnamed result.
    Single(InterfaceType),
    /// The function returns multiple named results.
    ///
    /// The length of the slice must be greater than one.
    Multiple(&'a [NamedInterfaceType<'a>]),
}

impl AdapterFunctionResults<'_> {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::None => bytes.push(0x00),
            Self::Single(ty) => {
                bytes.push(0x01);
                ty.encode(bytes);
            }
            Self::Multiple(types) => {
                assert!(types.len() > 1);
                bytes.extend(encoders::u32(types.len() as u32));
                for ty in *types {
                    ty.encode(bytes);
                }
            }
        }
    }
}

/// Represents a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type<'a> {
    /// The type is an instance.
    Instance(&'a [InstanceTypeDef<'a>]),
    /// The type is a module.
    Module(&'a [ModuleTypeDef<'a>]),
    /// The type is a core wasm function.
    Function {
        /// The parameters of the function.
        params: &'a [ValType],
        /// The results of the function.
        results: &'a [ValType],
    },
    /// The type is an adapter function.
    AdapterFunction {
        /// The parameters of the function.
        params: &'a [NamedInterfaceType<'a>],
        /// The results of the function.
        results: AdapterFunctionResults<'a>,
    },
    /// The type is a compound interface type.
    CompoundInterfaceType(CompoundInterfaceType<'a>),
}

impl Type<'_> {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::Instance(defs) => {
                bytes.push(TYPE_INSTANCE);
                bytes.extend(encoders::u32(u32::try_from(defs.len()).unwrap()));
                for def in *defs {
                    def.encode(bytes);
                }
            }
            Self::Module(defs) => {
                bytes.push(TYPE_MODULE);
                bytes.extend(encoders::u32(u32::try_from(defs.len()).unwrap()));
                for def in *defs {
                    def.encode(bytes);
                }
            }
            Self::Function { params, results } => {
                bytes.push(TYPE_FUNCTION);

                bytes.extend(encoders::u32(u32::try_from(params.len()).unwrap()));
                bytes.extend(params.iter().map(|ty| u8::from(*ty)));

                bytes.extend(encoders::u32(u32::try_from(results.len()).unwrap()));
                bytes.extend(results.iter().map(|ty| u8::from(*ty)));
            }
            Self::AdapterFunction { params, results } => {
                bytes.push(TYPE_ADAPTER_FUNCTION);

                bytes.extend(encoders::u32(u32::try_from(params.len()).unwrap()));
                for param in *params {
                    param.encode(bytes);
                }

                results.encode(bytes);
            }
            Self::CompoundInterfaceType(ty) => {
                ty.encode(bytes);
            }
        }
    }
}

/// Represents an instance type definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstanceTypeDef<'a> {
    /// The definition is for a type.
    Type(Type<'a>),
    /// The definition is for an alias.
    Alias(Alias<'a>),
    /// The definition is for an export.
    Export(&'a str, TypeRef),
}

impl InstanceTypeDef<'_> {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::Type(ty) => {
                bytes.push(INSTANCE_TYPEDEF_TYPE);
                ty.encode(bytes)
            }
            Self::Alias(alias) => {
                bytes.push(INSTANCE_TYPEDEF_ALIAS);
                alias.encode(bytes);
            }
            Self::Export(name, ty) => {
                bytes.push(INSTANCE_TYPEDEF_EXPORT);
                bytes.extend(encoders::str(name));
                ty.encode(bytes);
            }
        }
    }
}

/// Represents a module type definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleTypeDef<'a> {
    /// The definition is for a type.
    Type(Type<'a>),
    /// The definition is for an alias.
    Alias(Alias<'a>),
    /// The definition is for an export.
    Export(&'a str, TypeRef),
    /// The definition is for an import.
    Import(&'a str, TypeRef),
}

impl ModuleTypeDef<'_> {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::Type(ty) => {
                bytes.push(MODULE_TYPEDEF_TYPE);
                ty.encode(bytes)
            }
            Self::Alias(alias) => {
                bytes.push(MODULE_TYPEDEF_ALIAS);
                alias.encode(bytes);
            }
            Self::Export(name, ty) => {
                bytes.push(MODULE_TYPEDEF_EXPORT);
                bytes.extend(encoders::str(name));
                ty.encode(bytes);
            }
            Self::Import(name, ty) => {
                bytes.push(MODULE_TYPEDEF_IMPORT);
                bytes.extend(encoders::str(name));
                ty.encode(bytes);
            }
        }
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
    /// The type is the unit type.
    Unit,
    /// The type is a compound interface type.
    ///
    /// The value is a type index to a compound type.
    Compound(u32),
}

impl InterfaceType {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::Bool => bytes.push(INTERFACE_TYPE_BOOL),
            Self::S8 => bytes.push(INTERFACE_TYPE_S8),
            Self::U8 => bytes.push(INTERFACE_TYPE_U8),
            Self::S16 => bytes.push(INTERFACE_TYPE_S16),
            Self::U16 => bytes.push(INTERFACE_TYPE_U16),
            Self::S32 => bytes.push(INTERFACE_TYPE_S32),
            Self::U32 => bytes.push(INTERFACE_TYPE_U32),
            Self::S64 => bytes.push(INTERFACE_TYPE_S64),
            Self::U64 => bytes.push(INTERFACE_TYPE_U64),
            Self::F32 => bytes.push(INTERFACE_TYPE_F32),
            Self::F64 => bytes.push(INTERFACE_TYPE_F64),
            Self::Char => bytes.push(INTERFACE_TYPE_CHAR),
            Self::String => bytes.push(INTERFACE_TYPE_STRING),
            Self::Unit => bytes.push(INTERFACE_TYPE_UNIT),
            Self::Compound(index) => bytes.extend(encoders::u32(*index)),
        }
    }
}

/// Represents a compound interface type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompoundInterfaceType<'a> {
    /// A list interface type.
    List(InterfaceType),
    /// A record interface type.
    Record(&'a [(&'a str, InterfaceType)]),
    /// A variant interface type.
    Variant(&'a [(&'a str, Option<InterfaceType>)]),
    /// A tuple interface type.
    Tuple(&'a [InterfaceType]),
    /// A flags interface type.
    Flags(&'a [&'a str]),
    /// An enumeration interface type.
    Enum(&'a [&'a str]),
    /// A union interface type.
    Union(&'a [InterfaceType]),
    /// An optional interface type.
    Optional(InterfaceType),
    /// An expected interface type.
    ///
    /// If `error` is `None`, then the error type is unit.
    Expected {
        /// The type representing success.
        ok: Option<InterfaceType>,
        /// The type representing failure.
        error: Option<InterfaceType>,
    },
    /// A named interface type.
    ///
    /// Named types are either part of a type's declaration (i.e. a record)
    /// or may be a type alias.
    Named(NamedInterfaceType<'a>),
}

impl CompoundInterfaceType<'_> {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::List(ty) => {
                bytes.push(COMPOUND_INTERFACE_TYPE_LIST);
                ty.encode(bytes);
            }
            Self::Record(fields) => {
                bytes.push(COMPOUND_INTERFACE_TYPE_RECORD);
                bytes.extend(encoders::u32(u32::try_from(fields.len()).unwrap()));
                for (name, ty) in *fields {
                    bytes.extend(encoders::str(name));
                    ty.encode(bytes);
                }
            }
            Self::Variant(cases) => {
                bytes.push(COMPOUND_INTERFACE_TYPE_VARIANT);
                bytes.extend(encoders::u32(u32::try_from(cases.len()).unwrap()));
                for (name, ty) in *cases {
                    bytes.extend(encoders::str(name));
                    if let Some(ty) = ty {
                        bytes.push(0x01);
                        ty.encode(bytes);
                    } else {
                        bytes.push(0x00);
                    }
                }
            }
            Self::Tuple(types) => {
                bytes.push(COMPOUND_INTERFACE_TYPE_TUPLE);
                bytes.extend(encoders::u32(u32::try_from(types.len()).unwrap()));
                for ty in *types {
                    ty.encode(bytes);
                }
            }
            Self::Flags(names) => {
                bytes.push(COMPOUND_INTERFACE_TYPE_FLAGS);
                bytes.extend(encoders::u32(u32::try_from(names.len()).unwrap()));
                for name in *names {
                    bytes.extend(encoders::str(name));
                }
            }
            Self::Enum(tags) => {
                bytes.push(COMPOUND_INTERFACE_TYPE_ENUM);
                bytes.extend(encoders::u32(u32::try_from(tags.len()).unwrap()));
                for tag in *tags {
                    bytes.extend(encoders::str(tag));
                }
            }
            Self::Union(types) => {
                bytes.push(COMPOUND_INTERFACE_TYPE_UNION);
                bytes.extend(encoders::u32(u32::try_from(types.len()).unwrap()));
                for ty in *types {
                    ty.encode(bytes);
                }
            }
            Self::Optional(ty) => {
                bytes.push(COMPOUND_INTERFACE_TYPE_OPTIONAL);
                ty.encode(bytes);
            }
            Self::Expected { ok, error } => {
                bytes.push(COMPOUND_INTERFACE_TYPE_EXPECTED);
                if let Some(ok) = ok {
                    bytes.push(0x01);
                    ok.encode(bytes);
                } else {
                    bytes.push(0x00);
                }
                if let Some(error) = error {
                    bytes.push(0x01);
                    error.encode(bytes);
                } else {
                    bytes.push(0x00);
                }
            }
            Self::Named(named) => {
                bytes.push(COMPOUND_INTERFACE_TYPE_NAMED);
                named.encode(bytes);
            }
        }
    }
}

/// An encoder for the component type section.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::component::{Component, TypeSection, Type};
///
/// let mut types = TypeSection::new();
/// types.ty(Type::Function { params: &[], results: &[] });
///
/// let mut component = Component::new();
/// component.section(&types);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct TypeSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl TypeSection {
    /// Create a new component type section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of types in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define a type in the type section.
    pub fn ty(&mut self, ty: Type) -> &mut Self {
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl ComponentSection for TypeSection {
    fn id(&self) -> u8 {
        SectionId::Type.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}
