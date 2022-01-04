use super::{AdapterModuleSection, SectionId, TypeRef};
use crate::{encoders, ValType};

const TYPE_INSTANCE: u8 = 0x7f;
const TYPE_MODULE: u8 = 0x7e;
const TYPE_FUNCTION: u8 = 0x7d;

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
    Instance = 0,
    /// The definition is a module.
    Module = 1,
    /// The definition is a function.
    Function = 2,
    /// The definition is a table.
    Table = 3,
    /// The definition is a memory.
    Memory = 4,
    /// The definition is a global.
    Global = 5,
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

/// Represents an instance type.
#[derive(Debug, Clone, Default)]
pub struct InstanceType {
    bytes: Vec<u8>,
    num_added: u32,
}

impl InstanceType {
    /// Creates a new instance type.
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a type in this instance type.
    #[must_use = "the encoder must be used to encode the type"]
    pub fn ty(&mut self) -> TypeEncoder {
        self.bytes.push(INSTANCE_TYPEDEF_TYPE);
        self.num_added += 1;
        TypeEncoder(&mut self.bytes)
    }

    /// Defines an alias to an instance export in the instance type.
    pub fn alias_instance_export(
        &mut self,
        instance: u32,
        name: &str,
        kind: DefinitionKind,
    ) -> &mut Self {
        self.bytes.push(INSTANCE_TYPEDEF_ALIAS);
        self.bytes.push(ALIAS_TYPE_INSTANCE_EXPORT);
        self.bytes.extend(encoders::u32(instance));
        self.bytes.extend(encoders::str(name));
        self.bytes.push(kind as u8);
        self.num_added += 1;
        self
    }

    /// Defines an alias to an outer module in the instance type.
    pub fn alias_outer_module(&mut self, count: u32, index: OuterAliasIndex) -> &mut Self {
        self.bytes.push(INSTANCE_TYPEDEF_ALIAS);
        self.bytes.push(ALIAS_TYPE_OUTER);
        self.bytes.extend(encoders::u32(count));
        index.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines an export in the instance type.
    pub fn export(&mut self, name: &str, ty: TypeRef) -> &mut Self {
        self.bytes.push(INSTANCE_TYPEDEF_EXPORT);
        self.bytes.extend(encoders::str(name));
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.extend(encoders::u32(self.num_added));
        bytes.extend(self.bytes.iter().copied());
    }
}

/// Represents a module type.
#[derive(Debug, Clone, Default)]
pub struct ModuleType {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ModuleType {
    /// Creates a new module type.
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a type in this module type.
    #[must_use = "the encoder must be used to encode the type"]
    pub fn ty(&mut self) -> TypeEncoder {
        self.bytes.push(MODULE_TYPEDEF_TYPE);
        self.num_added += 1;
        TypeEncoder(&mut self.bytes)
    }

    /// Defines an alias to an instance export in the module type.
    pub fn alias_instance_export(
        &mut self,
        instance: u32,
        name: &str,
        kind: DefinitionKind,
    ) -> &mut Self {
        self.bytes.push(MODULE_TYPEDEF_ALIAS);
        self.bytes.push(ALIAS_TYPE_INSTANCE_EXPORT);
        self.bytes.extend(encoders::u32(instance));
        self.bytes.extend(encoders::str(name));
        self.bytes.push(kind as u8);
        self.num_added += 1;
        self
    }

    /// Defines an alias to an outer module in the module type.
    pub fn alias_outer_module(&mut self, count: u32, index: OuterAliasIndex) -> &mut Self {
        self.bytes.push(MODULE_TYPEDEF_ALIAS);
        self.bytes.push(ALIAS_TYPE_OUTER);
        self.bytes.extend(encoders::u32(count));
        index.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines an export in the module type.
    pub fn export(&mut self, name: &str, ty: TypeRef) -> &mut Self {
        self.bytes.push(MODULE_TYPEDEF_EXPORT);
        self.bytes.extend(encoders::str(name));
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines an import in the module type.
    pub fn import(&mut self, name: &str, ty: TypeRef) -> &mut Self {
        self.bytes.push(MODULE_TYPEDEF_IMPORT);
        self.bytes.extend(encoders::str(name));
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.extend(encoders::u32(self.num_added));
        bytes.extend(self.bytes.iter().copied());
    }
}

/// Used to encode types.
#[derive(Debug)]
pub struct TypeEncoder<'a>(&'a mut Vec<u8>);

impl<'a> TypeEncoder<'a> {
    /// Define an instance type.
    pub fn instance(self, ty: &InstanceType) {
        self.0.push(TYPE_INSTANCE);
        ty.encode(self.0);
    }

    /// Define a module type.
    pub fn module(self, ty: &ModuleType) {
        self.0.push(TYPE_MODULE);
        ty.encode(self.0);
    }

    /// Define a function type.
    pub fn function(self, params: &[ValType], results: &[ValType]) {
        self.0.push(TYPE_FUNCTION);
        self.0
            .extend(encoders::u32(u32::try_from(params.len()).unwrap()));
        self.0.extend(params.iter().map(|p| u8::from(*p)));
        self.0
            .extend(encoders::u32(u32::try_from(results.len()).unwrap()));
        self.0.extend(results.iter().map(|r| u8::from(*r)));
    }
}

/// An encoder for the adapter module type section.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::adapter::{AdapterModule, TypeSection};
///
/// let mut types = TypeSection::new();
/// types.function(&[], &[]);
///
/// let mut module = AdapterModule::new();
/// module.section(&types);
///
/// let bytes = module.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct TypeSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl TypeSection {
    /// Create a new module adapter type section encoder.
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

    /// Define an instance type in this type section.
    pub fn instance(&mut self, ty: &InstanceType) -> &mut Self {
        let encoder = TypeEncoder(&mut self.bytes);
        encoder.instance(ty);
        self.num_added += 1;
        self
    }

    /// Define a module type in this type section.
    pub fn module(&mut self, ty: &ModuleType) -> &mut Self {
        let encoder = TypeEncoder(&mut self.bytes);
        encoder.module(ty);
        self.num_added += 1;
        self
    }

    /// Define a function type in this type section.
    pub fn function(&mut self, params: &[ValType], results: &[ValType]) -> &mut Self {
        let encoder = TypeEncoder(&mut self.bytes);
        encoder.function(params, results);
        self.num_added += 1;
        self
    }
}

impl AdapterModuleSection for TypeSection {
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
