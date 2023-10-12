use crate::{encode_section, Encode, Section, SectionId};

/// Represents a subtype of possible other types in a WebAssembly module.
#[derive(Debug, Clone)]
pub struct SubType {
    /// Is the subtype final.
    pub is_final: bool,
    /// The list of supertype indexes. As of GC MVP, there can be at most one supertype.
    pub supertype_idx: Option<u32>,
    /// The structural type of the subtype.
    pub structural_type: StructuralType,
}

#[cfg(feature = "wasmparser")]
impl From<wasmparser::SubType> for SubType {
    fn from(sub_ty: wasmparser::SubType) -> Self {
        SubType {
            is_final: sub_ty.is_final,
            supertype_idx: sub_ty.supertype_idx,
            structural_type: sub_ty.structural_type.into(),
        }
    }
}

/// Represents a structural type in a WebAssembly module.
#[derive(Debug, Clone)]
pub enum StructuralType {
    /// The type is for a function.
    Func(FuncType),
    /// The type is for an array.
    Array(ArrayType),
    /// The type is for a struct.
    Struct(StructType),
}

impl Encode for StructuralType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            StructuralType::Func(ty) => TypeSection::encode_function(
                sink,
                ty.params().iter().copied(),
                ty.results().iter().copied(),
            ),
            StructuralType::Array(ArrayType(ty)) => {
                TypeSection::encode_array(sink, &ty.element_type, ty.mutable)
            }
            StructuralType::Struct(ty) => {
                TypeSection::encode_struct(sink, ty.fields.iter().cloned())
            }
        }
    }
}

#[cfg(feature = "wasmparser")]
impl From<wasmparser::StructuralType> for StructuralType {
    fn from(structural_ty: wasmparser::StructuralType) -> Self {
        match structural_ty {
            wasmparser::StructuralType::Func(f) => StructuralType::Func(f.into()),
            wasmparser::StructuralType::Array(a) => StructuralType::Array(a.into()),
            wasmparser::StructuralType::Struct(s) => StructuralType::Struct(s.into()),
        }
    }
}

/// Represents a type of a function in a WebAssembly module.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FuncType {
    /// The combined parameters and result types.
    params_results: Box<[ValType]>,
    /// The number of parameter types.
    len_params: usize,
}

#[cfg(feature = "wasmparser")]
impl From<wasmparser::FuncType> for FuncType {
    fn from(func_ty: wasmparser::FuncType) -> Self {
        FuncType::new(
            func_ty.params().iter().cloned().map(Into::into),
            func_ty.results().iter().cloned().map(Into::into),
        )
    }
}

/// Represents a type of an array in a WebAssembly module.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ArrayType(pub FieldType);

#[cfg(feature = "wasmparser")]
impl From<wasmparser::ArrayType> for ArrayType {
    fn from(array_ty: wasmparser::ArrayType) -> Self {
        ArrayType(array_ty.0.into())
    }
}

/// Represents a type of a struct in a WebAssembly module.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructType {
    /// Struct fields.
    pub fields: Box<[FieldType]>,
}

#[cfg(feature = "wasmparser")]
impl From<wasmparser::StructType> for StructType {
    fn from(struct_ty: wasmparser::StructType) -> Self {
        StructType {
            fields: struct_ty.fields.iter().cloned().map(Into::into).collect(),
        }
    }
}

/// Field type in structural types (structs, arrays).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct FieldType {
    /// Storage type of the field.
    pub element_type: StorageType,
    /// Is the field mutable.
    pub mutable: bool,
}

#[cfg(feature = "wasmparser")]
impl From<wasmparser::FieldType> for FieldType {
    fn from(field_ty: wasmparser::FieldType) -> Self {
        FieldType {
            element_type: field_ty.element_type.into(),
            mutable: field_ty.mutable,
        }
    }
}

/// Storage type for structural type fields.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum StorageType {
    /// The `i8` type.
    I8,
    /// The `i16` type.
    I16,
    /// A value type.
    Val(ValType),
}

#[cfg(feature = "wasmparser")]
impl From<wasmparser::StorageType> for StorageType {
    fn from(storage_ty: wasmparser::StorageType) -> Self {
        match storage_ty {
            wasmparser::StorageType::I8 => StorageType::I8,
            wasmparser::StorageType::I16 => StorageType::I16,
            wasmparser::StorageType::Val(v) => StorageType::Val(v.into()),
        }
    }
}

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
    /// A reference type.
    ///
    /// The `funcref` and `externref` type fall into this category and the full
    /// generalization here is due to the implementation of the
    /// function-references proposal.
    Ref(RefType),
}

#[cfg(feature = "wasmparser")]
impl From<wasmparser::ValType> for ValType {
    fn from(val_ty: wasmparser::ValType) -> Self {
        match val_ty {
            wasmparser::ValType::I32 => ValType::I32,
            wasmparser::ValType::I64 => ValType::I64,
            wasmparser::ValType::F32 => ValType::F32,
            wasmparser::ValType::F64 => ValType::F64,
            wasmparser::ValType::V128 => ValType::V128,
            wasmparser::ValType::Ref(r) => ValType::Ref(r.into()),
        }
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

impl ValType {
    /// Alias for the `funcref` type in WebAssembly
    pub const FUNCREF: ValType = ValType::Ref(RefType::FUNCREF);
    /// Alias for the `externref` type in WebAssembly
    pub const EXTERNREF: ValType = ValType::Ref(RefType::EXTERNREF);
}

impl Encode for StorageType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            StorageType::I8 => sink.push(0x78),
            StorageType::I16 => sink.push(0x77),
            StorageType::Val(vt) => vt.encode(sink),
        }
    }
}

impl Encode for ValType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            ValType::I32 => sink.push(0x7F),
            ValType::I64 => sink.push(0x7E),
            ValType::F32 => sink.push(0x7D),
            ValType::F64 => sink.push(0x7C),
            ValType::V128 => sink.push(0x7B),
            ValType::Ref(rt) => rt.encode(sink),
        }
    }
}

/// A reference type.
///
/// This is largely part of the function references proposal for WebAssembly but
/// additionally is used by the `funcref` and `externref` types. The full
/// generality of this type is only exercised with function-references.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
#[allow(missing_docs)]
pub struct RefType {
    pub nullable: bool,
    pub heap_type: HeapType,
}

impl RefType {
    /// Alias for the `funcref` type in WebAssembly
    pub const FUNCREF: RefType = RefType {
        nullable: true,
        heap_type: HeapType::Func,
    };

    /// Alias for the `externref` type in WebAssembly
    pub const EXTERNREF: RefType = RefType {
        nullable: true,
        heap_type: HeapType::Extern,
    };
}

impl Encode for RefType {
    fn encode(&self, sink: &mut Vec<u8>) {
        if self.nullable {
            // Favor the original encodings of `funcref` and `externref` where
            // possible
            match self.heap_type {
                HeapType::Func => return sink.push(0x70),
                HeapType::Extern => return sink.push(0x6f),
                _ => {}
            }
        }

        if self.nullable {
            sink.push(0x63);
        } else {
            sink.push(0x64);
        }
        self.heap_type.encode(sink);
    }
}

#[cfg(feature = "wasmparser")]
impl From<wasmparser::RefType> for RefType {
    fn from(ref_type: wasmparser::RefType) -> Self {
        RefType {
            nullable: ref_type.is_nullable(),
            heap_type: ref_type.heap_type().into(),
        }
    }
}

impl From<RefType> for ValType {
    fn from(ty: RefType) -> ValType {
        ValType::Ref(ty)
    }
}

/// Part of the function references proposal.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum HeapType {
    /// Untyped (any) function.
    Func,
    /// External heap type.
    Extern,
    /// The `any` heap type. The common supertype (a.k.a. top) of all internal types.
    Any,
    /// The `none` heap type. The common subtype (a.k.a. bottom) of all internal types.
    None,
    /// The `noextern` heap type. The common subtype (a.k.a. bottom) of all external types.
    NoExtern,
    /// The `nofunc` heap type. The common subtype (a.k.a. bottom) of all function types.
    NoFunc,
    /// The `eq` heap type. The common supertype of all referenceable types on which comparison
    /// (ref.eq) is allowed.
    Eq,
    /// The `struct` heap type. The common supertype of all struct types.
    Struct,
    /// The `array` heap type. The common supertype of all array types.
    Array,
    /// The i31 heap type.
    I31,
    /// User defined type at the given index.
    Indexed(u32),
}

impl Encode for HeapType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            HeapType::Func => sink.push(0x70),
            HeapType::Extern => sink.push(0x6F),
            HeapType::Any => sink.push(0x6E),
            HeapType::None => sink.push(0x71),
            HeapType::NoExtern => sink.push(0x72),
            HeapType::NoFunc => sink.push(0x73),
            HeapType::Eq => sink.push(0x6D),
            HeapType::Struct => sink.push(0x6B),
            HeapType::Array => sink.push(0x6A),
            HeapType::I31 => sink.push(0x6C),
            // Note that this is encoded as a signed type rather than unsigned
            // as it's decoded as an s33
            HeapType::Indexed(i) => i64::from(*i).encode(sink),
        }
    }
}

#[cfg(feature = "wasmparser")]
impl From<wasmparser::HeapType> for HeapType {
    fn from(heap_type: wasmparser::HeapType) -> Self {
        match heap_type {
            wasmparser::HeapType::Indexed(i) => HeapType::Indexed(i),
            wasmparser::HeapType::Func => HeapType::Func,
            wasmparser::HeapType::Extern => HeapType::Extern,
            wasmparser::HeapType::Any => HeapType::Any,
            wasmparser::HeapType::None => HeapType::None,
            wasmparser::HeapType::NoExtern => HeapType::NoExtern,
            wasmparser::HeapType::NoFunc => HeapType::NoFunc,
            wasmparser::HeapType::Eq => HeapType::Eq,
            wasmparser::HeapType::Struct => HeapType::Struct,
            wasmparser::HeapType::Array => HeapType::Array,
            wasmparser::HeapType::I31 => HeapType::I31,
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
        Self::encode_function(&mut self.bytes, params, results);
        self.num_added += 1;
        self
    }

    fn encode_function<P, R>(sink: &mut Vec<u8>, params: P, results: R)
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        let params = params.into_iter();
        let results = results.into_iter();

        sink.push(0x60);
        params.len().encode(sink);
        params.for_each(|p| p.encode(sink));
        results.len().encode(sink);
        results.for_each(|p| p.encode(sink));
    }

    /// Define an array type in this type section.
    pub fn array(&mut self, ty: &StorageType, mutable: bool) -> &mut Self {
        Self::encode_array(&mut self.bytes, ty, mutable);
        self.num_added += 1;
        self
    }

    fn encode_array(sink: &mut Vec<u8>, ty: &StorageType, mutable: bool) {
        sink.push(0x5e);
        Self::encode_field(sink, ty, mutable);
    }

    fn encode_field(sink: &mut Vec<u8>, ty: &StorageType, mutable: bool) {
        ty.encode(sink);
        sink.push(mutable as u8);
    }

    /// Define a struct type in this type section.
    pub fn struct_<F>(&mut self, fields: F) -> &mut Self
    where
        F: IntoIterator<Item = FieldType>,
        F::IntoIter: ExactSizeIterator,
    {
        Self::encode_struct(&mut self.bytes, fields);
        self.num_added += 1;
        self
    }

    fn encode_struct<F>(sink: &mut Vec<u8>, fields: F)
    where
        F: IntoIterator<Item = FieldType>,
        F::IntoIter: ExactSizeIterator,
    {
        let fields = fields.into_iter();
        sink.push(0x5f);
        fields.len().encode(sink);
        for f in fields {
            Self::encode_field(sink, &f.element_type, f.mutable);
        }
    }

    /// Define an explicit subtype in this type section.
    pub fn subtype(&mut self, ty: &SubType) -> &mut Self {
        // We only need to emit a prefix byte before the actual structural type
        // when either the type is not final or it has a declared super type.
        if ty.supertype_idx.is_some() || !ty.is_final {
            self.bytes.push(if ty.is_final { 0x4f } else { 0x50 });
            ty.supertype_idx.encode(&mut self.bytes);
        }

        ty.structural_type.encode(&mut self.bytes);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Module;

    #[test]
    fn func_types_dont_require_wasm_gc() {
        let mut types = TypeSection::new();
        types.subtype(&SubType {
            is_final: true,
            supertype_idx: None,
            structural_type: StructuralType::Func(FuncType::new([], [])),
        });

        let mut module = Module::new();
        module.section(&types);
        let wasm_bytes = module.finish();

        let mut validator = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
            gc: false,
            ..Default::default()
        });

        validator.validate_all(&wasm_bytes).expect(
            "Encoding pre Wasm GC type should not accidentally use Wasm GC specific encoding",
        );
    }
}
