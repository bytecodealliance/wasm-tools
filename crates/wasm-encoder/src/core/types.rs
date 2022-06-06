use crate::{encode_section, Encode, EntityType, Section, SectionId};

/// The type of a core WebAssembly value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
#[repr(u8)]
pub enum ValType {
    /// The `i32` type.
    I32 = 0x7F,
    /// The `i64` type.
    I64 = 0x7E,
    /// The `f32` type.
    F32 = 0x7D,
    /// The `f64` type.
    F64 = 0x7C,
    /// The `v128` type.
    ///
    /// Part of the SIMD proposal.
    V128 = 0x7B,
    /// The `funcref` type.
    ///
    /// Part of the reference types proposal when used anywhere other than a
    /// table's element type.
    FuncRef = 0x70,
    /// The `externref` type.
    ///
    /// Part of the reference types proposal.
    ExternRef = 0x6F,
}

impl From<ValType> for u8 {
    #[inline]
    fn from(t: ValType) -> u8 {
        t as u8
    }
}

impl Encode for ValType {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(*self as u8);
    }
}

/// Represents the type of a core module.
#[derive(Debug, Clone, Default)]
pub struct ModuleType {
    bytes: Vec<u8>,
    num_added: u32,
    types_added: u32,
}

impl ModuleType {
    /// Creates a new core module type.
    pub fn new() -> Self {
        Self::default()
    }

    /// Defines an import in this module type.
    pub fn import(&mut self, module: &str, name: &str, ty: EntityType) -> &mut Self {
        self.bytes.push(0x00);
        module.encode(&mut self.bytes);
        name.encode(&mut self.bytes);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Define a type in this module type.
    ///
    /// The returned encoder must be used before adding another definition.
    #[must_use = "the encoder must be used to encode the type"]
    pub fn ty(&mut self) -> TypeEncoder {
        self.bytes.push(0x01);
        self.num_added += 1;
        self.types_added += 1;
        TypeEncoder(&mut self.bytes)
    }

    /// Defines an export in this module type.
    pub fn export(&mut self, name: &str, ty: EntityType) -> &mut Self {
        self.bytes.push(0x03);
        name.encode(&mut self.bytes);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Gets the number of types that have been added to this module type.
    pub fn type_count(&self) -> u32 {
        self.types_added
    }
}

impl Encode for ModuleType {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(0x50);
        self.num_added.encode(sink);
        sink.extend(&self.bytes);
    }
}

/// Used to encode core types.
#[derive(Debug)]
pub struct TypeEncoder<'a>(pub(crate) &'a mut Vec<u8>);

impl<'a> TypeEncoder<'a> {
    /// Define a function type.
    pub fn function<P, R>(self, params: P, results: R)
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        let params = params.into_iter();
        let results = results.into_iter();

        self.0.push(0x60);
        params.len().encode(self.0);
        self.0.extend(params.map(u8::from));
        results.len().encode(self.0);
        self.0.extend(results.map(u8::from));
    }

    /// Define a module type.
    ///
    /// Currently this is only used for core type sections in components.
    pub fn module(self, ty: &ModuleType) {
        ty.encode(self.0);
    }
}

/// An encoder for the type section of WebAssembly modules.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Module, TypeSection, ValType};
///
/// let mut types = TypeSection::new();
///
/// types.function([ValType::I32, ValType::I32], [ValType::I64]);
///
/// let mut module = Module::new();
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
    /// Create a new module type section encoder.
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

    /// Encode a type into this section.
    ///
    /// The returned encoder must be finished before adding another type.
    #[must_use = "the encoder must be used to encode the type"]
    pub fn ty(&mut self) -> TypeEncoder<'_> {
        self.num_added += 1;
        TypeEncoder(&mut self.bytes)
    }

    /// Define a function type in this type section.
    pub fn function<P, R>(&mut self, params: P, results: R) -> &mut Self
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        self.ty().function(params, results);
        self
    }

    /// Define a module type in this type section.
    ///
    /// Currently this is only used for core type sections in components.
    pub fn module(&mut self, ty: &ModuleType) -> &mut Self {
        self.ty().module(ty);
        self
    }
}

impl Encode for TypeSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl Section for TypeSection {
    fn id(&self) -> u8 {
        SectionId::Type.into()
    }
}
