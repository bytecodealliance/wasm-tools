use crate::{Docs, Enum, Flags, Handle, Record, Result_, Tuple, Variant};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Bool,
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
    F32,
    F64,
    Char,
    String,
    TypeDef(Box<TypeDef>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Int {
    U8,
    U16,
    U32,
    U64,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Case {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: Option<Type>,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TypeDef {
    pub name: Option<String>,
    pub kind: TypeDefKind,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum TypeDefKind {
    Record(Record),
    Resource,
    Handle(Handle),
    Flags(Flags),
    Tuple(Tuple),
    Variant(Variant),
    Enum(Enum),
    Option(Type),
    Result(Result_),
    List(Type),
    Type(Type),
}

impl TypeDefKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            TypeDefKind::Record(_) => "record",
            TypeDefKind::Resource => "resource",
            TypeDefKind::Handle(handle) => match handle {
                Handle::Own(_) => "own",
                Handle::Borrow(_) => "borrow",
            },
            TypeDefKind::Flags(_) => "flags",
            TypeDefKind::Tuple(_) => "tuple",
            TypeDefKind::Variant(_) => "variant",
            TypeDefKind::Enum(_) => "enum",
            TypeDefKind::Option(_) => "option",
            TypeDefKind::Result(_) => "result",
            TypeDefKind::List(_) => "list",
            TypeDefKind::Type(_) => "type",
        }
    }
}
