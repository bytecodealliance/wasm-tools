use crate::{encode_section, Encode, Section, SectionId};

/// The type of a core WebAssembly value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum ValType {
    /// The `i32` type.
    I32,
    /// The `i64` type.
    I64,
    /// The `f32` type.
    F32,
    /// The `f64` type.
    F64,
    /// The `v128` type.
    ///
    /// Part of the SIMD proposal.
    V128,
    /// The `funcref` type.
    ///
    /// Part of the reference types proposal when used anywhere other than a
    /// table's element type.
    FuncRef,
    /// The `externref` type.
    ///
    /// Part of the reference types proposal.
    ExternRef,
    /// A reference type from typed function references. In the proposal
    /// `funcref` and `externref` are generalized to a reference type with the heap
    /// type `func` or `extern.` This crate draws a distinction between `funcref`
    /// and `(ref null func)` (similarly for extern) because the latter cannot be read by an
    /// implementation that implements reference types but not function
    /// references. When function references are enabled, there is no
    /// difference, but the distinction is maintained.
    Ref(RefType),
}

impl Encode for ValType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            ValType::I32 => sink.push(0x7F),
            ValType::I64 => sink.push(0x7E),
            ValType::F32 => sink.push(0x7D),
            ValType::F64 => sink.push(0x7C),
            ValType::V128 => sink.push(0x7B),
            ValType::FuncRef => sink.push(0x70),
            ValType::ExternRef => sink.push(0x6F),
            ValType::Ref(rt) => rt.encode(sink),
        }
    }
}

/// Part of the function references proposal. These types are only produced
/// when the feature is enabled, despite sometimes being equivalent to FuncRef,
/// because they're encoded differently.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct RefType {
    nullable: bool,
    heap_type: HeapType,
}

impl Encode for RefType {
    fn encode(&self, sink: &mut Vec<u8>) {
        if self.nullable {
            sink.push(0x6C);
        } else {
            sink.push(0x6B);
        }
        self.heap_type.encode(sink);
    }
}

/// Part of the function references proposal.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum HeapType {
    /// A function reference. When nullable, equivalent to `funcref`
    Func,
    /// An extern reference. When nullable, equivalent to `externref`
    Extern,
    /// A reference to a particular index in a table.
    Index(u32),
}

impl Encode for HeapType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            HeapType::Func => sink.push(0x70),
            HeapType::Extern => sink.push(0x6F),
            HeapType::Index(i) if i & 0x80 != 0 => i.encode(sink),
            _ => panic!("invalid heap_type"),
        }
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

    /// Define a function type in this type section.
    pub fn function<P, R>(&mut self, params: P, results: R) -> &mut Self
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        let params = params.into_iter();
        let results = results.into_iter();

        self.bytes.push(0x60);
        params.len().encode(&mut self.bytes);
        params.for_each(|p| p.encode(&mut self.bytes));
        results.len().encode(&mut self.bytes);
        results.for_each(|p| p.encode(&mut self.bytes));
        self.num_added += 1;
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
