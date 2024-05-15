use std::fmt::{self, Display};

use crate::{Docs, Enum, Flags, Record, Result_, Render, RenderOpts, Tuple, Variant};

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
    Borrow(Box<Type>),
    Option(Box<Type>),
    Result(Box<Result_>),
    List(Box<Type>),
    Tuple(Tuple),
    Named(String),
}

impl Type {
    pub fn borrow(type_: Type) -> Self {
        Type::Borrow(Box::new(type_))
    }
    pub fn option(type_: Type) -> Self {
        Type::Option(Box::new(type_))
    }
    pub fn result(result: Result_) -> Self {
        Type::Result(Box::new(result))
    }
    pub fn result_ok(type_: Type) -> Self {
        Type::Result(Box::new(Result_::ok(type_)))
    }
    pub fn result_err(type_: Type) -> Self {
        Type::Result(Box::new(Result_::err(type_)))
    }
    pub fn result_both(ok: Type, err: Type) -> Self {
        Type::Result(Box::new(Result_::both(ok, err)))
    }
    pub fn result_empty() -> Self {
        Type::Result(Box::new(Result_::empty()))
    }
    pub fn list(type_: Type) -> Self {
        Type::List(Box::new(type_))
    }
    pub fn tuple(types: Vec<Type>) -> Self {
        Type::Tuple(Tuple { types })
    }
    pub fn named(name: impl Into<String>) -> Self {
        Type::Named(name.into())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::S8 => write!(f, "s8"),
            Type::S16 => write!(f, "s16"),
            Type::S32 => write!(f, "s32"),
            Type::S64 => write!(f, "s64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Char => write!(f, "char"),
            Type::String => write!(f, "string"),
            Type::Named(name) => write!(f, "{}", name),
            Type::Borrow(type_) => {
                write!(f, "borrow<{type_}>")
            }
            Type::Option(type_) => {
                write!(f, "option<{type_}>")
            }
            Type::Result(result) => result.fmt(f),
            Type::List(type_) => {
                write!(f, "list<{type_}>")
            }
            Type::Tuple(tuple) => tuple.fmt(f),
        }
    }
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
pub struct EnumCase {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,
}

impl From<&str> for EnumCase {
    fn from(value: &str) -> Self {
        Self {
            name: value.to_string(),
            docs: Docs::default(),
        }
    }
}

impl EnumCase {
    pub fn docs(mut self, docs: Docs) -> Self {
        self.docs = docs;
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct VariantCase {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: Option<Type>,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TypeDef {
    name: String,
    kind: TypeDefKind,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    docs: Docs,
}

impl TypeDef {
    pub fn enum_(name: impl Into<String>, cases: Vec<impl Into<EnumCase>>) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::enum_(cases),
            docs: Docs::default(),
        }
    }

    pub fn type_(name: impl Into<String>, type_: Type) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::type_(type_),
            docs: Docs::default(),
        }
    }

    pub fn docs(&mut self, docs: Docs) {
        self.docs = docs;
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum TypeDefKind {
    Record(Record),
    Resource,
    Flags(Flags),
    Variant(Variant),
    Enum(Enum),
    Type(Type),
}

impl TypeDefKind {
    pub fn enum_(cases: Vec<impl Into<EnumCase>>) -> Self {
        Self::Enum(Enum {
            cases: cases.into_iter().map(|c| c.into()).collect(),
        })
    }

    pub fn type_(type_: Type) -> Self {
        Self::Type(type_)
    }
}

impl Render for TypeDef {
    fn render_opts(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: usize,
        opts: RenderOpts,
    ) -> fmt::Result {
        match &self.kind {
            TypeDefKind::Record(_) => todo!(),
            TypeDefKind::Resource => todo!(),
            TypeDefKind::Flags(_) => todo!(),
            TypeDefKind::Variant(_) => todo!(),
            TypeDefKind::Enum(enum_) => {
                write!(f, "{:depth$}enum {} {{\n", "", self.name, depth = opts.indent(depth))?;
                for case in &enum_.cases {
                    write!(f, "{:depth$}{},\n", "", case.name, depth = opts.indent(depth + 1))?;
                }
                write!(f, "{:depth$}}}\n", "")?;
            }
            TypeDefKind::Type(type_) => {
                write!(f, "{:depth$}type {} = {};\n", "", self.name, type_, depth = opts.indent(depth))?;
            }
        }
        Ok(())
    }
}
