use std::{borrow::Cow, sync::Arc};

use crate::wasm::{maybe_unwrap_type, WasmType, WasmTypeKind};

/// The [`WasmType`] of a [`Value`](super::Value).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type(pub(super) TypeEnum);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum TypeEnum {
    Simple(SimpleType),
    List(Arc<ListType>),
    Record(Arc<RecordType>),
    Tuple(Arc<TupleType>),
    Variant(Arc<VariantType>),
    Enum(Arc<EnumType>),
    Option(Arc<OptionType>),
    Result(Arc<ResultType>),
    Flags(Arc<FlagsType>),
}

#[allow(missing_docs)]
impl Type {
    pub const BOOL: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::Bool)));
    pub const S8: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::S8)));
    pub const S16: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::S16)));
    pub const S32: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::S32)));
    pub const S64: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::S64)));
    pub const U8: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::U8)));
    pub const U16: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::U16)));
    pub const U32: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::U32)));
    pub const U64: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::U64)));
    pub const FLOAT32: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::Float32)));
    pub const FLOAT64: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::Float64)));
    pub const CHAR: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::Char)));
    pub const STRING: Self = Self(TypeEnum::Simple(SimpleType(WasmTypeKind::String)));

    /// Returns the simple type of the given `kind`. Returns None if the kind
    /// represents a parameterized type.
    pub fn simple(kind: WasmTypeKind) -> Option<Self> {
        is_simple(kind).then_some(Self(TypeEnum::Simple(SimpleType(kind))))
    }

    #[doc(hidden)]
    pub const fn must_simple(kind: WasmTypeKind) -> Self {
        if !is_simple(kind) {
            panic!("kind is not simple");
        }
        Self(TypeEnum::Simple(SimpleType(kind)))
    }

    /// Returns a list type with the given element type.
    pub fn list(element_type: impl Into<Self>) -> Self {
        let element = element_type.into();
        Self(TypeEnum::List(Arc::new(ListType { element })))
    }

    /// Returns a record type with the given field types. Returns None if
    /// `fields` is empty.
    pub fn record<T: Into<Box<str>>>(
        field_types: impl IntoIterator<Item = (T, Self)>,
    ) -> Option<Self> {
        let fields = field_types
            .into_iter()
            .map(|(name, ty)| (name.into(), ty))
            .collect::<Box<[_]>>();
        if fields.is_empty() {
            return None;
        }
        Some(Self(TypeEnum::Record(Arc::new(RecordType { fields }))))
    }

    /// Returns a tuple type with the given element types. Returns None if
    /// `element_types` is empty.
    pub fn tuple(element_types: impl Into<Box<[Self]>>) -> Option<Self> {
        let elements = element_types.into();
        if elements.is_empty() {
            return None;
        }
        Some(Self(TypeEnum::Tuple(Arc::new(TupleType { elements }))))
    }

    /// Returns a variant type with the given case names and optional payloads.
    /// Returns None if `cases` is empty.
    pub fn variant<T: Into<Box<str>>>(
        cases: impl IntoIterator<Item = (T, Option<Self>)>,
    ) -> Option<Self> {
        let cases = cases
            .into_iter()
            .map(|(name, ty)| (name.into(), ty))
            .collect::<Box<[_]>>();
        if cases.is_empty() {
            return None;
        }
        Some(Self(TypeEnum::Variant(Arc::new(VariantType { cases }))))
    }

    /// Returns an enum type with the given case names. Returns None if `cases`
    /// is empty.
    pub fn enum_ty<T: Into<Box<str>>>(cases: impl IntoIterator<Item = T>) -> Option<Self> {
        let cases = cases.into_iter().map(Into::into).collect::<Box<[_]>>();
        if cases.is_empty() {
            return None;
        }
        Some(Self(TypeEnum::Enum(Arc::new(EnumType { cases }))))
    }

    /// Returns an option type with the given "some" type.
    pub fn option(some: Self) -> Self {
        Self(TypeEnum::Option(Arc::new(OptionType { some })))
    }

    /// Returns a result type with the given optional "ok" and "err" payloads.
    pub fn result(ok: Option<Self>, err: Option<Self>) -> Self {
        Self(TypeEnum::Result(Arc::new(ResultType { ok, err })))
    }

    /// Returns a flags type with the given flag names. Returns None if `flags`
    /// is empty.
    pub fn flags<T: Into<Box<str>>>(flags: impl IntoIterator<Item = T>) -> Option<Self> {
        let flags = flags.into_iter().map(Into::into).collect::<Box<[_]>>();
        if flags.is_empty() {
            return None;
        }
        Some(Self(TypeEnum::Flags(Arc::new(FlagsType { flags }))))
    }

    /// Returns a [`Type`] matching the given [`WasmType`]. Returns None if the
    /// given type is unsupported or otherwise invalid.
    pub fn from_wasm_type(ty: &impl WasmType) -> Option<Self> {
        super::convert::from_wasm_type(ty)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SimpleType(WasmTypeKind);

const fn is_simple(kind: WasmTypeKind) -> bool {
    use WasmTypeKind::*;
    matches!(
        kind,
        Bool | S8 | S16 | S32 | S64 | U8 | U16 | U32 | U64 | Float32 | Float64 | Char | String
    )
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ListType {
    pub(super) element: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub struct RecordType {
    pub(super) fields: Box<[(Box<str>, Type)]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TupleType {
    pub(super) elements: Box<[Type]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VariantType {
    pub(super) cases: Box<[(Box<str>, Option<Type>)]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumType {
    pub(super) cases: Box<[Box<str>]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OptionType {
    pub(super) some: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ResultType {
    pub(super) ok: Option<Type>,
    pub(super) err: Option<Type>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FlagsType {
    pub(super) flags: Box<[Box<str>]>,
}

impl WasmType for Type {
    fn kind(&self) -> WasmTypeKind {
        match self.0 {
            TypeEnum::Simple(simple) => simple.0,
            TypeEnum::List(_) => WasmTypeKind::List,
            TypeEnum::Record(_) => WasmTypeKind::Record,
            TypeEnum::Tuple(_) => WasmTypeKind::Tuple,
            TypeEnum::Variant(_) => WasmTypeKind::Variant,
            TypeEnum::Enum(_) => WasmTypeKind::Enum,
            TypeEnum::Option(_) => WasmTypeKind::Option,
            TypeEnum::Result(_) => WasmTypeKind::Result,
            TypeEnum::Flags(_) => WasmTypeKind::Flags,
        }
    }

    fn list_element_type(&self) -> Option<Self> {
        let list = maybe_unwrap_type!(&self.0, TypeEnum::List)?;
        Some(list.element.clone())
    }

    fn record_fields(&self) -> Box<dyn Iterator<Item = (Cow<str>, Self)> + '_> {
        let TypeEnum::Record(record) = &self.0 else {
            return Box::new(std::iter::empty());
        };
        Box::new(
            record
                .fields
                .iter()
                .map(|(name, ty)| (name.as_ref().into(), ty.clone())),
        )
    }

    fn tuple_element_types(&self) -> Box<dyn Iterator<Item = Self> + '_> {
        let TypeEnum::Tuple(tuple) = &self.0 else {
            return Box::new(std::iter::empty());
        };
        Box::new(tuple.elements.iter().cloned())
    }

    fn variant_cases(&self) -> Box<dyn Iterator<Item = (Cow<str>, Option<Self>)> + '_> {
        let TypeEnum::Variant(variant) = &self.0 else {
            return Box::new(std::iter::empty());
        };
        Box::new(
            variant
                .cases
                .iter()
                .map(|(name, ty)| (name.as_ref().into(), ty.clone())),
        )
    }

    fn enum_cases(&self) -> Box<dyn Iterator<Item = Cow<str>> + '_> {
        let TypeEnum::Enum(enum_) = &self.0 else {
            return Box::new(std::iter::empty());
        };
        Box::new(enum_.cases.iter().map(|name| name.as_ref().into()))
    }

    fn option_some_type(&self) -> Option<Self> {
        let option = maybe_unwrap_type!(&self.0, TypeEnum::Option)?;
        Some(option.some.clone())
    }

    fn result_types(&self) -> Option<(Option<Self>, Option<Self>)> {
        let result = maybe_unwrap_type!(&self.0, TypeEnum::Result)?;
        Some((result.ok.clone(), result.err.clone()))
    }

    fn flags_names(&self) -> Box<dyn Iterator<Item = Cow<str>> + '_> {
        let TypeEnum::Flags(flags) = &self.0 else {
            return Box::new(std::iter::empty());
        };
        Box::new(flags.flags.iter().map(|name| name.as_ref().into()))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::wasm::DisplayType(self).fmt(f)
    }
}
