use std::{borrow::Cow, fmt::Debug};

/// The kind of a [`WasmType`]. These correspond to the value types defined by the
/// [Component Model design](https://github.com/WebAssembly/component-model/blob/673d5c43c3cc0f4aeb8996a5c0931af623f16808/design/mvp/WIT.md).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
#[non_exhaustive]
pub enum WasmTypeKind {
    Bool,
    S8,
    S16,
    S32,
    S64,
    U8,
    U16,
    U32,
    U64,
    Float32,
    Float64,
    Char,
    String,
    List,
    Record,
    Tuple,
    Variant,
    Enum,
    Option,
    Result,
    Flags,
    #[doc(hidden)]
    Unsupported,
}

impl std::fmt::Display for WasmTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            WasmTypeKind::Bool => "bool",
            WasmTypeKind::S8 => "s8",
            WasmTypeKind::S16 => "s16",
            WasmTypeKind::S32 => "s32",
            WasmTypeKind::S64 => "s64",
            WasmTypeKind::U8 => "u8",
            WasmTypeKind::U16 => "u16",
            WasmTypeKind::U32 => "u32",
            WasmTypeKind::U64 => "u64",
            WasmTypeKind::Float32 => "float32",
            WasmTypeKind::Float64 => "float64",
            WasmTypeKind::Char => "char",
            WasmTypeKind::String => "string",
            WasmTypeKind::List => "list",
            WasmTypeKind::Record => "record",
            WasmTypeKind::Tuple => "tuple",
            WasmTypeKind::Variant => "variant",
            WasmTypeKind::Enum => "enum",
            WasmTypeKind::Option => "option",
            WasmTypeKind::Result => "result",
            WasmTypeKind::Flags => "flags",
            WasmTypeKind::Unsupported => "<<UNSUPPORTED>>",
        })
    }
}

/// The WasmType trait may be implemented to represent types to be
/// (de)serialized with WAVE, notably [`value::Type`](crate::value::Type).
/// The [`wasmtime`] crate provides an impl for [`wasmtime::component::Type`].
///
/// The `Self`-returning methods should be called only for corresponding
/// [`WasmTypeKind`]s.
pub trait WasmType: Clone + Sized {
    /// Returns the [`WasmTypeKind`] of this type.
    fn kind(&self) -> WasmTypeKind;

    /// Returns the list element type or `None` if `self` is not a list type.
    /// # Panics
    /// Panics if the type is not implemented (the trait default).
    fn list_element_type(&self) -> Option<Self> {
        unimplemented!()
    }
    /// Returns an iterator of the record's field names and Types. The
    /// iterator will be empty iff `self` is not a record type.
    /// # Panics
    /// Panics if the type is not implemented (the trait default).
    fn record_fields(&self) -> Box<dyn Iterator<Item = (Cow<str>, Self)> + '_> {
        unimplemented!()
    }
    /// Returns an iterator of the tuple's field Types. The iterator will be
    /// empty iff `self` is not a tuple type.
    /// # Panics
    /// Panics if the type is not implemented (the trait default).
    fn tuple_element_types(&self) -> Box<dyn Iterator<Item = Self> + '_> {
        unimplemented!()
    }
    /// Returns an iterator of the variant's case names and optional payload
    /// Types. The iterator will be empty iff `self` is not a tuple type.
    /// # Panics
    /// Panics if the type is not implemented (the trait default).
    fn variant_cases(&self) -> Box<dyn Iterator<Item = (Cow<str>, Option<Self>)> + '_> {
        unimplemented!()
    }
    /// Returns an iterator of the enum's case names. The iterator will be
    /// empty iff `self` is not an enum type.
    /// # Panics
    /// Panics if the type is not implemented (the trait default).
    fn enum_cases(&self) -> Box<dyn Iterator<Item = Cow<str>> + '_> {
        unimplemented!()
    }
    /// Returns the option's "some" type or `None` if `self` is not an option type.
    /// # Panics
    /// Panics if the type is not implemented (the trait default).
    fn option_some_type(&self) -> Option<Self> {
        unimplemented!()
    }
    /// Returns the result's optional "ok" and "err" Types or `None` if `self`
    /// is not a result type.
    /// # Panics
    /// Panics if the type is not implemented (the trait default).
    fn result_types(&self) -> Option<(Option<Self>, Option<Self>)> {
        unimplemented!()
    }
    /// Returns an iterator of the flags' names. The iterator will be empty iff
    /// `self` is not a flags type.
    /// # Panics
    /// Panics if the type is not implemented (the trait default).
    fn flags_names(&self) -> Box<dyn Iterator<Item = Cow<str>> + '_> {
        unimplemented!()
    }
}

macro_rules! maybe_unwrap_type {
    ($ty:expr, $case:path) => {
        match $ty {
            $case(v) => Some(v),
            _ => None,
        }
    };
}
pub(crate) use maybe_unwrap_type;
