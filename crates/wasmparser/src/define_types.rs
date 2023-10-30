// Define the core Wasm type hierarchy with the given index type.
//
// The index type must satisfy the following constraints:
//
// * It must implement `Display`
//
// * It must implement `Into<u32>` and `From<u32>`
//
// * `$index_type::from(u32::from(index))` must be the identity function.
//
// * `u32::from($index_type::from(x))` must also be the identity function.
//
// * Its `u32` representation must fit within 20 bits, that is
//
//       index.into() <= (1 << 20) - 1
//
//   must hold true for all indices.
macro_rules! define_core_wasm_types {
    ($index_type:ty) => {
        /// Represents a recursive type group in a WebAssembly module.
        #[derive(Debug, Clone)]
        pub struct RecGroup {
            inner: RecGroupInner,
        }

        #[derive(Debug, Clone)]
        enum RecGroupInner {
            Implicit(SubType),
            Explicit(Vec<SubType>),
        }

        impl RecGroup {
            /// Create an explicit `RecGroup` for the given types.
            pub(crate) fn explicit(types: Vec<SubType>) -> Self {
                RecGroup {
                    inner: RecGroupInner::Explicit(types),
                }
            }

            /// Create an implicit `RecGroup` for a type that was not contained
            /// in a `(rec ...)`.
            pub(crate) fn implicit(ty: SubType) -> Self {
                RecGroup {
                    inner: RecGroupInner::Implicit(ty),
                }
            }

            /// Is this an explicit recursion group?
            pub fn is_explicit_rec_group(&self) -> bool {
                matches!(self.inner, RecGroupInner::Explicit(_))
            }

            /// Returns the list of subtypes in the recursive type group.
            pub fn types(&self) -> &[SubType] {
                match &self.inner {
                    RecGroupInner::Implicit(ty) => std::slice::from_ref(ty),
                    RecGroupInner::Explicit(types) => types,
                }
            }

            /// Returns an owning iterator of all subtypes in this recursion
            /// group.
            pub fn into_types(self) -> impl ExactSizeIterator<Item = SubType> {
                return match self.inner {
                    RecGroupInner::Implicit(ty) => Iter::Implicit(Some(ty)),
                    RecGroupInner::Explicit(types) => Iter::Explicit(types.into_iter()),
                };

                enum Iter {
                    Implicit(Option<SubType>),
                    Explicit(std::vec::IntoIter<SubType>),
                }

                impl Iterator for Iter {
                    type Item = SubType;

                    fn next(&mut self) -> Option<SubType> {
                        match self {
                            Self::Implicit(ty) => ty.take(),
                            Self::Explicit(types) => types.next(),
                        }
                    }

                    fn size_hint(&self) -> (usize, Option<usize>) {
                        match self {
                            Self::Implicit(None) => (0, Some(0)),
                            Self::Implicit(Some(_)) => (1, Some(1)),
                            Self::Explicit(types) => types.size_hint(),
                        }
                    }
                }

                impl ExactSizeIterator for Iter {}
            }
        }

        /// Represents a subtype of possible other types in a WebAssembly module.
        #[derive(Debug, Clone)]
        pub struct SubType {
            /// Is the subtype final.
            pub is_final: bool,
            /// The list of supertype indexes. As of GC MVP, there can be at most one supertype.
            pub supertype_idx: Option<$index_type>,
            /// The composite type of the subtype.
            pub composite_type: CompositeType,
        }

        impl SubType {
            /// Unwrap an `ArrayType` or panic.
            ///
            /// Does not check finality or whether there is a supertype.
            pub fn unwrap_array(&self) -> &ArrayType {
                self.composite_type.unwrap_array()
            }

            /// Unwrap an `FuncType` or panic.
            ///
            /// Does not check finality or whether there is a supertype.
            pub fn unwrap_func(&self) -> &FuncType {
                self.composite_type.unwrap_func()
            }

            /// Unwrap an `StructType` or panic.
            ///
            /// Does not check finality or whether there is a supertype.
            pub fn unwrap_struct(&self) -> &StructType {
                self.composite_type.unwrap_struct()
            }
        }

        /// Represents a composite type in a WebAssembly module.
        #[derive(Debug, Clone)]
        pub enum CompositeType {
            /// The type is for a function.
            Func(FuncType),
            /// The type is for an array.
            Array(ArrayType),
            /// The type is for a struct.
            Struct(StructType),
        }

        impl CompositeType {
            /// Unwrap a `FuncType` or panic.
            pub fn unwrap_func(&self) -> &FuncType {
                match self {
                    Self::Func(f) => f,
                    _ => panic!("not a func"),
                }
            }

            /// Unwrap a `ArrayType` or panic.
            pub fn unwrap_array(&self) -> &ArrayType {
                match self {
                    Self::Array(a) => a,
                    _ => panic!("not a array"),
                }
            }

            /// Unwrap a `StructType` or panic.
            pub fn unwrap_struct(&self) -> &StructType {
                match self {
                    Self::Struct(s) => s,
                    _ => panic!("not a struct"),
                }
            }
        }

        /// Represents a type of a function in a WebAssembly module.
        #[derive(Clone, Eq, PartialEq, Hash)]
        pub struct FuncType {
            /// The combined parameters and result types.
            params_results: Box<[ValType]>,
            /// The number of parameter types.
            len_params: usize,
        }

        impl std::fmt::Debug for FuncType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("FuncType")
                    .field("params", &self.params())
                    .field("results", &self.results())
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
            pub(crate) fn from_raw_parts(
                params_results: Box<[ValType]>,
                len_params: usize,
            ) -> Self {
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

            pub(crate) fn desc(&self) -> String {
                let mut s = String::new();
                s.push_str("[");
                for (i, param) in self.params().iter().enumerate() {
                    if i > 0 {
                        s.push_str(" ");
                    }
                    write!(s, "{param}").unwrap();
                }
                s.push_str("] -> [");
                for (i, result) in self.results().iter().enumerate() {
                    if i > 0 {
                        s.push_str(" ");
                    }
                    write!(s, "{result}").unwrap();
                }
                s.push_str("]");
                s
            }
        }

        /// Represents a type of an array in a WebAssembly module.
        #[derive(Debug, Clone, Eq, PartialEq, Hash)]
        pub struct ArrayType(pub FieldType);

        /// Represents a field type of an array or a struct.
        #[derive(Debug, Clone, Eq, PartialEq, Hash)]
        pub struct FieldType {
            /// Array element type.
            pub element_type: StorageType,
            /// Are elements mutable.
            pub mutable: bool,
        }

        /// Represents storage types introduced in the GC spec for array and struct fields.
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        pub enum StorageType {
            /// The storage type is i8.
            I8,
            /// The storage type is i16.
            I16,
            /// The storage type is a value type.
            Val(ValType),
        }

        /// Represents a type of a struct in a WebAssembly module.
        #[derive(Debug, Clone, Eq, PartialEq, Hash)]
        pub struct StructType {
            /// Struct fields.
            pub fields: Box<[FieldType]>,
        }

        /// Represents the types of values in a WebAssembly module.
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        pub enum ValType {
            /// The value type is i32.
            I32,
            /// The value type is i64.
            I64,
            /// The value type is f32.
            F32,
            /// The value type is f64.
            F64,
            /// The value type is v128.
            V128,
            /// The value type is a reference.
            Ref(RefType),
        }

        impl From<RefType> for ValType {
            #[inline]
            fn from(ty: RefType) -> ValType {
                ValType::Ref(ty)
            }
        }

        impl std::fmt::Display for ValType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    ValType::I32 => f.write_str("i32"),
                    ValType::I64 => f.write_str("i64"),
                    ValType::F32 => f.write_str("f32"),
                    ValType::F64 => f.write_str("f64"),
                    ValType::V128 => f.write_str("v128"),
                    ValType::Ref(r) => std::fmt::Display::fmt(r, f),
                }
            }
        }

        impl ValType {
            /// Alias for the wasm `funcref` type.
            pub const FUNCREF: ValType = ValType::Ref(RefType::FUNCREF);

            /// Alias for the wasm `externref` type.
            pub const EXTERNREF: ValType = ValType::Ref(RefType::EXTERNREF);

            /// Returns whether this value type is a "reference type".
            ///
            /// Only reference types are allowed in tables, for example, and with some
            /// instructions. Current reference types include `funcref` and `externref`.
            pub fn is_reference_type(&self) -> bool {
                matches!(self, ValType::Ref(_))
            }

            /// Whether the type is defaultable, i.e. it is not a non-nullable reference
            /// type.
            pub fn is_defaultable(&self) -> bool {
                match *self {
                    Self::I32 | Self::I64 | Self::F32 | Self::F64 | Self::V128 => true,
                    Self::Ref(rt) => rt.is_nullable(),
                }
            }
        }

        /// A reference type.
        ///
        /// The reference types proposal first introduced `externref` and
        /// `funcref`.
        ///
        /// The function references proposal introduced typed function
        /// references.
        ///
        /// The GC proposal introduces heap types: any, eq, i31, struct, array,
        /// nofunc, noextern, none.
        //
        // RefType is a bit-packed enum that fits in a `u24` aka `[u8; 3]`.
        // Note that its content is opaque (and subject to change), but its API
        // is stable.
        //
        // It has the following internal structure:
        //
        // ```
        // [nullable:u1 concrete==1:u1 unused:u2 index:u20]
        // [nullable:u1 concrete==0:u1 abstype:u4 (unused):u18]
        // ```
        //
        // Where
        //
        // - `nullable` determines nullability of the ref,
        //
        // - `concrete` determines if the ref is of a dynamically defined type
        //   with an index (encoded in a following bit-packing section) or of a
        //   known fixed type,
        //
        // - `index` is the type index,
        //
        // - `abstype` is an enumeration of abstract types:
        //
        //   ```
        //   1111 = any
        //
        //   1101 = eq
        //   1000 = i31
        //   1001 = struct
        //   1100 = array
        //
        //   0101 = func
        //   0100 = nofunc
        //
        //   0011 = extern
        //   0010 = noextern
        //
        //   0000 = none
        //   ```
        #[derive(Copy, Clone, PartialEq, Eq, Hash)]
        pub struct RefType([u8; 3]);

        impl std::fmt::Debug for RefType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match (self.is_nullable(), self.heap_type()) {
                    (true, HeapType::Any) => write!(f, "anyref"),
                    (false, HeapType::Any) => write!(f, "(ref any)"),
                    (true, HeapType::None) => write!(f, "nullref"),
                    (false, HeapType::None) => write!(f, "(ref none)"),
                    (true, HeapType::NoExtern) => write!(f, "nullexternref"),
                    (false, HeapType::NoExtern) => write!(f, "(ref noextern)"),
                    (true, HeapType::NoFunc) => write!(f, "nullfuncref"),
                    (false, HeapType::NoFunc) => write!(f, "(ref nofunc)"),
                    (true, HeapType::Eq) => write!(f, "eqref"),
                    (false, HeapType::Eq) => write!(f, "(ref eq)"),
                    (true, HeapType::Struct) => write!(f, "structref"),
                    (false, HeapType::Struct) => write!(f, "(ref struct)"),
                    (true, HeapType::Array) => write!(f, "arrayref"),
                    (false, HeapType::Array) => write!(f, "(ref array)"),
                    (true, HeapType::I31) => write!(f, "i31ref"),
                    (false, HeapType::I31) => write!(f, "(ref i31)"),
                    (true, HeapType::Extern) => write!(f, "externref"),
                    (false, HeapType::Extern) => write!(f, "(ref extern)"),
                    (true, HeapType::Func) => write!(f, "funcref"),
                    (false, HeapType::Func) => write!(f, "(ref func)"),
                    (true, HeapType::Concrete(idx)) => write!(f, "(ref null {idx})"),
                    (false, HeapType::Concrete(idx)) => write!(f, "(ref {idx})"),
                }
            }
        }

        impl std::fmt::Display for RefType {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                std::fmt::Debug::fmt(self, f)
            }
        }

        // Assert that we can fit indices up to `MAX_WASM_TYPES` inside `RefType`.
        #[test]
        fn can_fit_max_wasm_types_in_ref_type() {
            fn can_roundtrip_index(index: u32) -> bool {
                assert!(RefType::can_represent_type_index(index));
                let rt = match RefType::concrete(true, index) {
                    Some(rt) => rt,
                    None => panic!(),
                };
                assert!(rt.is_nullable());
                let actual_index = match rt.type_index() {
                    Some(i) => i,
                    None => panic!(),
                };
                actual_index == index
            }

            assert!(can_roundtrip_index(crate::limits::MAX_WASM_TYPES as u32));
            assert!(can_roundtrip_index(0b00000000_00001111_00000000_00000000));
            assert!(can_roundtrip_index(0b00000000_00000000_11111111_00000000));
            assert!(can_roundtrip_index(0b00000000_00000000_00000000_11111111));
            assert!(can_roundtrip_index(0));
        }

        impl RefType {
            const NULLABLE_BIT: u32 = 1 << 23; // bit #23
            const CONCRETE_BIT: u32 = 1 << 22; // bit #22

            const ABSTYPE_MASK: u32 = 0b1111 << 18; // 4 bits #21-#18 (if `concrete == 0`)
            const ANY_ABSTYPE: u32 = 0b1111 << 18;
            const EQ_ABSTYPE: u32 = 0b1101 << 18;
            const I31_ABSTYPE: u32 = 0b1000 << 18;
            const STRUCT_ABSTYPE: u32 = 0b1001 << 18;
            const ARRAY_ABSTYPE: u32 = 0b1100 << 18;
            const FUNC_ABSTYPE: u32 = 0b0101 << 18;
            const NOFUNC_ABSTYPE: u32 = 0b0100 << 18;
            const EXTERN_ABSTYPE: u32 = 0b0011 << 18;
            const NOEXTERN_ABSTYPE: u32 = 0b0010 << 18;
            const NONE_ABSTYPE: u32 = 0b0000 << 18;

            const INDEX_MASK: u32 = (1 << 20) - 1; // 20 bits #19-#0 (if `concrete == 1`)

            /// A nullable untyped function reference aka `(ref null func)` aka
            /// `funcref` aka `anyfunc`.
            pub const FUNCREF: Self = RefType::FUNC.nullable();

            /// A nullable reference to an extern object aka `(ref null extern)` aka
            /// `externref`.
            pub const EXTERNREF: Self = RefType::EXTERN.nullable();

            /// A non-nullable untyped function reference aka `(ref func)`.
            pub const FUNC: Self = RefType::from_u32(Self::FUNC_ABSTYPE);

            /// A non-nullable reference to an extern object aka `(ref extern)`.
            pub const EXTERN: Self = RefType::from_u32(Self::EXTERN_ABSTYPE);

            /// A non-nullable reference to any object aka `(ref any)`.
            pub const ANY: Self = RefType::from_u32(Self::ANY_ABSTYPE);

            /// A non-nullable reference to no object aka `(ref none)`.
            pub const NONE: Self = RefType::from_u32(Self::NONE_ABSTYPE);

            /// A non-nullable reference to a noextern object aka `(ref noextern)`.
            pub const NOEXTERN: Self = RefType::from_u32(Self::NOEXTERN_ABSTYPE);

            /// A non-nullable reference to a nofunc object aka `(ref nofunc)`.
            pub const NOFUNC: Self = RefType::from_u32(Self::NOFUNC_ABSTYPE);

            /// A non-nullable reference to an eq object aka `(ref eq)`.
            pub const EQ: Self = RefType::from_u32(Self::EQ_ABSTYPE);

            /// A non-nullable reference to a struct aka `(ref struct)`.
            pub const STRUCT: Self = RefType::from_u32(Self::STRUCT_ABSTYPE);

            /// A non-nullable reference to an array aka `(ref array)`.
            pub const ARRAY: Self = RefType::from_u32(Self::ARRAY_ABSTYPE);

            /// A non-nullable reference to an i31 object aka `(ref i31)`.
            pub const I31: Self = RefType::from_u32(Self::I31_ABSTYPE);

            const fn can_represent_type_index(index: u32) -> bool {
                index & Self::INDEX_MASK == index
            }

            const fn u24_to_u32(bytes: [u8; 3]) -> u32 {
                let expanded_bytes = [bytes[0], bytes[1], bytes[2], 0];
                u32::from_le_bytes(expanded_bytes)
            }

            const fn u32_to_u24(x: u32) -> [u8; 3] {
                let bytes = x.to_le_bytes();
                debug_assert!(bytes[3] == 0);
                [bytes[0], bytes[1], bytes[2]]
            }

            #[inline]
            const fn as_u32(&self) -> u32 {
                Self::u24_to_u32(self.0)
            }

            #[inline]
            const fn from_u32(x: u32) -> Self {
                debug_assert!(x & (0b11111111 << 24) == 0);

                // Either concrete or it must be a known abstract type.
                debug_assert!(
                    x & Self::CONCRETE_BIT != 0
                        || matches!(
                            x & Self::ABSTYPE_MASK,
                            Self::ANY_ABSTYPE
                                | Self::EQ_ABSTYPE
                                | Self::I31_ABSTYPE
                                | Self::STRUCT_ABSTYPE
                                | Self::ARRAY_ABSTYPE
                                | Self::FUNC_ABSTYPE
                                | Self::NOFUNC_ABSTYPE
                                | Self::EXTERN_ABSTYPE
                                | Self::NOEXTERN_ABSTYPE
                                | Self::NONE_ABSTYPE
                        )
                );

                RefType(Self::u32_to_u24(x))
            }

            /// Create a reference to a concrete Wasm-defined type at the given
            /// index.
            ///
            /// Returns `None` when the type index is beyond this crate's
            /// implementation limits and therefore is not representable.
            pub fn concrete(nullable: bool, index: $index_type) -> Option<Self> {
                let index: u32 = index.into();
                if Self::can_represent_type_index(index) {
                    let nullable32 = Self::NULLABLE_BIT * nullable as u32;
                    Some(RefType::from_u32(nullable32 | Self::CONCRETE_BIT | index))
                } else {
                    None
                }
            }

            /// Create a new `RefType`.
            ///
            /// Returns `None` when the heap type's type index (if any) is
            /// beyond this crate's implementation limits and therefore is not
            /// representable.
            pub fn new(nullable: bool, heap_type: HeapType) -> Option<Self> {
                let nullable32 = Self::NULLABLE_BIT * (nullable as u32);
                match heap_type {
                    HeapType::Concrete(index) => RefType::concrete(nullable, index),
                    HeapType::Func => Some(Self::from_u32(nullable32 | Self::FUNC_ABSTYPE)),
                    HeapType::Extern => Some(Self::from_u32(nullable32 | Self::EXTERN_ABSTYPE)),
                    HeapType::Any => Some(Self::from_u32(nullable32 | Self::ANY_ABSTYPE)),
                    HeapType::None => Some(Self::from_u32(nullable32 | Self::NONE_ABSTYPE)),
                    HeapType::NoExtern => Some(Self::from_u32(nullable32 | Self::NOEXTERN_ABSTYPE)),
                    HeapType::NoFunc => Some(Self::from_u32(nullable32 | Self::NOFUNC_ABSTYPE)),
                    HeapType::Eq => Some(Self::from_u32(nullable32 | Self::EQ_ABSTYPE)),
                    HeapType::Struct => Some(Self::from_u32(nullable32 | Self::STRUCT_ABSTYPE)),
                    HeapType::Array => Some(Self::from_u32(nullable32 | Self::ARRAY_ABSTYPE)),
                    HeapType::I31 => Some(Self::from_u32(nullable32 | Self::I31_ABSTYPE)),
                }
            }

            /// Is this a reference to an concrete type?
            pub const fn is_concrete_type_ref(&self) -> bool {
                self.as_u32() & Self::CONCRETE_BIT != 0
            }

            /// If this is a reference to a typed function, get its type index.
            pub fn type_index(&self) -> Option<$index_type> {
                if self.is_concrete_type_ref() {
                    let index = self.as_u32() & Self::INDEX_MASK;
                    Some(<$index_type>::from(index))
                } else {
                    None
                }
            }

            const fn abstype(&self) -> u32 {
                self.as_u32() & Self::ABSTYPE_MASK
            }

            /// Is this the abstract untyped function reference type aka `(ref
            /// null func)` aka `funcref` aka `anyfunc`?
            pub const fn is_func_ref(&self) -> bool {
                !self.is_concrete_type_ref() && self.abstype() == Self::FUNC_ABSTYPE
            }

            /// Is this the abstract external reference type aka `(ref null
            /// extern)` aka `externref`?
            pub const fn is_extern_ref(&self) -> bool {
                !self.is_concrete_type_ref() && self.abstype() == Self::EXTERN_ABSTYPE
            }

            /// Is this the abstract untyped array refrence type aka `(ref null
            /// array)` aka `arrayref`?
            pub const fn is_array_ref(&self) -> bool {
                !self.is_concrete_type_ref() && self.abstype() == Self::ARRAY_ABSTYPE
            }

            /// Is this the abstract untyped struct reference type aka `(ref
            /// null struct)` aka `structref`?
            pub const fn is_struct_ref(&self) -> bool {
                !self.is_concrete_type_ref() && self.abstype() == Self::STRUCT_ABSTYPE
            }

            /// Is this ref type nullable?
            pub const fn is_nullable(&self) -> bool {
                self.as_u32() & Self::NULLABLE_BIT != 0
            }

            /// Get the non-nullable version of this ref type.
            pub const fn as_non_null(&self) -> Self {
                Self::from_u32(self.as_u32() & !Self::NULLABLE_BIT)
            }

            /// Get the non-nullable version of this ref type.
            pub const fn nullable(&self) -> Self {
                Self::from_u32(self.as_u32() | Self::NULLABLE_BIT)
            }

            /// Get the heap type that this is a reference to.
            pub fn heap_type(&self) -> HeapType {
                let s = self.as_u32();
                if self.is_concrete_type_ref() {
                    HeapType::Concrete(self.type_index().unwrap())
                } else {
                    match s & Self::ABSTYPE_MASK {
                        Self::FUNC_ABSTYPE => HeapType::Func,
                        Self::EXTERN_ABSTYPE => HeapType::Extern,
                        Self::ANY_ABSTYPE => HeapType::Any,
                        Self::NONE_ABSTYPE => HeapType::None,
                        Self::NOEXTERN_ABSTYPE => HeapType::NoExtern,
                        Self::NOFUNC_ABSTYPE => HeapType::NoFunc,
                        Self::EQ_ABSTYPE => HeapType::Eq,
                        Self::STRUCT_ABSTYPE => HeapType::Struct,
                        Self::ARRAY_ABSTYPE => HeapType::Array,
                        Self::I31_ABSTYPE => HeapType::I31,
                        _ => unreachable!(),
                    }
                }
            }

            // Note that this is similar to `Display for RefType` except that it has
            // the indexes stubbed out.
            pub(crate) fn wat(&self) -> &'static str {
                match (self.is_nullable(), self.heap_type()) {
                    (true, HeapType::Func) => "funcref",
                    (true, HeapType::Extern) => "externref",
                    (true, HeapType::Concrete(_)) => "(ref null $type)",
                    (true, HeapType::Any) => "anyref",
                    (true, HeapType::None) => "nullref",
                    (true, HeapType::NoExtern) => "nullexternref",
                    (true, HeapType::NoFunc) => "nullfuncref",
                    (true, HeapType::Eq) => "eqref",
                    (true, HeapType::Struct) => "structref",
                    (true, HeapType::Array) => "arrayref",
                    (true, HeapType::I31) => "i31ref",
                    (false, HeapType::Func) => "(ref func)",
                    (false, HeapType::Extern) => "(ref extern)",
                    (false, HeapType::Concrete(_)) => "(ref $type)",
                    (false, HeapType::Any) => "(ref any)",
                    (false, HeapType::None) => "(ref none)",
                    (false, HeapType::NoExtern) => "(ref noextern)",
                    (false, HeapType::NoFunc) => "(ref nofunc)",
                    (false, HeapType::Eq) => "(ref eq)",
                    (false, HeapType::Struct) => "(ref struct)",
                    (false, HeapType::Array) => "(ref array)",
                    (false, HeapType::I31) => "(ref i31)",
                }
            }
        }

        /// A heap type.
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        pub enum HeapType {
            /// A concrete, user-defined type.
            ///
            /// Introduced in the function-references proposal.
            Concrete($index_type),

            /// The abstract, untyped (any) function.
            ///
            /// Introduced in the references-types proposal.
            Func,

            /// The abstract, external heap type.
            ///
            /// Introduced in the references-types proposal.
            Extern,

            /// The abstract `any` heap type.
            ///
            /// The common supertype (a.k.a. top) of all internal types.
            ///
            /// Introduced in the GC proposal.
            Any,

            /// The abstract `none` heap type.
            ///
            /// The common subtype (a.k.a. bottom) of all internal types.
            ///
            /// Introduced in the GC proposal.
            None,

            /// The abstract `noextern` heap type.
            ///
            /// The common subtype (a.k.a. bottom) of all external types.
            ///
            /// Introduced in the GC proposal.
            NoExtern,

            /// The abstract `nofunc` heap type.
            ///
            /// The common subtype (a.k.a. bottom) of all function types.
            ///
            /// Introduced in the GC proposal.
            NoFunc,

            /// The abstract `eq` heap type.
            ///
            /// The common supertype of all heap types on which the `ref.eq`
            /// instruction is allowed.
            ///
            /// Introduced in the GC proposal.
            Eq,

            /// The abstract `struct` heap type.
            ///
            /// The common supertype of all struct types.
            ///
            /// Introduced in the GC proposal.
            Struct,

            /// The abstract `array` heap type.
            ///
            /// The common supertype of all array types.
            ///
            /// Introduced in the GC proposal.
            Array,

            /// The abstract `i31` heap type.
            ///
            /// It is not expected that Wasm runtimes actually store these
            /// values on the heap, but unbox them inline into the `i31ref`s
            /// themselves instead.
            ///
            /// Introduced in the GC proposal.
            I31,
        }
    };
}
