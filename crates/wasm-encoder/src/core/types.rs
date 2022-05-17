use crate::{encode_section, encoders, Encode, Section, SectionId};

/// The type of a value.
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

pub(crate) fn encode_functype<P, R>(bytes: &mut Vec<u8>, params: P, results: R)
where
    P: IntoIterator<Item = ValType>,
    P::IntoIter: ExactSizeIterator,
    R: IntoIterator<Item = ValType>,
    R::IntoIter: ExactSizeIterator,
{
    let params = params.into_iter();
    let results = results.into_iter();

    bytes.push(0x60);
    bytes.extend(encoders::u32(u32::try_from(params.len()).unwrap()));
    bytes.extend(params.map(u8::from));
    bytes.extend(encoders::u32(u32::try_from(results.len()).unwrap()));
    bytes.extend(results.map(u8::from));
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

    /// Define a function type in this type section.
    pub fn function<P, R>(&mut self, params: P, results: R) -> &mut Self
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        encode_functype(&mut self.bytes, params, results);
        self.num_added += 1;
        self
    }
}

impl Encode for TypeSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, SectionId::Type, self.num_added, &self.bytes);
    }
}

impl Section for TypeSection {}
