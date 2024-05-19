use std::fmt::{self, Display};

use crate::{
    Docs, Enum, Field, Flag, Flags, Record, Render, RenderOpts, Resource, ResourceFunc, Result_,
    Tuple, Variant,
};

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
    pub fn tuple(types: impl IntoIterator<Item = Type>) -> Self {
        Type::Tuple(Tuple {
            types: types.into_iter().collect(),
        })
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

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct EnumCase {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Option<Docs>,
}

impl From<&str> for EnumCase {
    fn from(value: &str) -> Self {
        Self {
            name: value.to_string(),
            docs: None,
        }
    }
}

impl EnumCase {
    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct VariantCase {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: Option<Type>,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Option<Docs>,
}

impl VariantCase {
    pub fn empty(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ty: None,
            docs: None,
        }
    }
    pub fn value(name: impl Into<String>, ty: Type) -> Self {
        Self {
            name: name.into(),
            ty: Some(ty),
            docs: None,
        }
    }
    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

impl<N> Into<VariantCase> for (N,)
where
    N: Into<String>,
{
    fn into(self) -> VariantCase {
        VariantCase::empty(self.0)
    }
}

impl<N> Into<VariantCase> for (N, Type)
where
    N: Into<String>,
{
    fn into(self) -> VariantCase {
        VariantCase::value(self.0, self.1)
    }
}

impl<N, D> Into<VariantCase> for (N, Type, D)
where
    N: Into<String>,
    D: Into<Docs>,
{
    fn into(self) -> VariantCase {
        let mut field = VariantCase::value(self.0, self.1);
        field.docs(Some(self.2.into()));
        field
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TypeDef {
    name: String,
    kind: TypeDefKind,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    docs: Option<Docs>,
}

impl TypeDef {
    pub fn new(name: impl Into<String>, kind: TypeDefKind) -> Self {
        TypeDef {
            name: name.into(),
            kind,
            docs: None,
        }
    }

    pub fn record(
        name: impl Into<String>,
        fields: impl IntoIterator<Item = impl Into<Field>>,
    ) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::record(fields),
            docs: None,
        }
    }

    pub fn resource(
        name: impl Into<String>,
        funcs: impl IntoIterator<Item = impl Into<ResourceFunc>>,
    ) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::resource(funcs),
            docs: None,
        }
    }

    pub fn flags(
        name: impl Into<String>,
        flags: impl IntoIterator<Item = impl Into<Flag>>,
    ) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::flags(flags),
            docs: None,
        }
    }

    pub fn variant(
        name: impl Into<String>,
        cases: impl IntoIterator<Item = impl Into<VariantCase>>,
    ) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::variant(cases),
            docs: None,
        }
    }

    pub fn enum_(
        name: impl Into<String>,
        cases: impl IntoIterator<Item = impl Into<EnumCase>>,
    ) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::enum_(cases),
            docs: None,
        }
    }

    pub fn type_(name: impl Into<String>, type_: Type) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::type_(type_),
            docs: None,
        }
    }

    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum TypeDefKind {
    Record(Record),
    Resource(Resource),
    Flags(Flags),
    Variant(Variant),
    Enum(Enum),
    Type(Type),
}

impl TypeDefKind {
    pub fn record(fields: impl IntoIterator<Item = impl Into<Field>>) -> Self {
        Self::Record(Record {
            fields: fields.into_iter().map(|c| c.into()).collect(),
        })
    }

    pub fn resource(funcs: impl IntoIterator<Item = impl Into<ResourceFunc>>) -> Self {
        Self::Resource(Resource {
            funcs: funcs.into_iter().map(|f| f.into()).collect(),
        })
    }

    pub fn flags(flags: impl IntoIterator<Item = impl Into<Flag>>) -> Self {
        Self::Flags(Flags {
            flags: flags.into_iter().map(|f| f.into()).collect(),
        })
    }

    pub fn variant(cases: impl IntoIterator<Item = impl Into<VariantCase>>) -> Self {
        Self::Variant(Variant {
            cases: cases.into_iter().map(|c| c.into()).collect(),
        })
    }

    pub fn enum_(cases: impl IntoIterator<Item = impl Into<EnumCase>>) -> Self {
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
            TypeDefKind::Record(record) => {
                if let Some(docs) = &self.docs {
                    docs.render_opts(f, depth, opts.clone())?;
                }
                write!(
                    f,
                    "{:depth$}record {} {{\n",
                    "",
                    self.name,
                    depth = opts.indent(depth)
                )?;
                for field in &record.fields {
                    if let Some(docs) = &field.docs {
                        docs.render_opts(f, depth + 1, opts.clone())?;
                    }
                    write!(
                        f,
                        "{:depth$}{}: {},\n",
                        "",
                        field.name,
                        field.ty,
                        depth = opts.indent(depth + 1)
                    )?;
                }
                write!(f, "{:depth$}}}\n", "", depth = opts.indent(depth))?;
            }
            TypeDefKind::Resource(resource) => {
                if let Some(docs) = &self.docs {
                    docs.render_opts(f, depth, opts.clone())?;
                }
                write!(
                    f,
                    "{:depth$}resource {} {{\n",
                    "",
                    self.name,
                    depth = opts.indent(depth)
                )?;
                for func in &resource.funcs {
                    if let Some(docs) = &func.docs {
                        docs.render_opts(f, depth + 1, opts.clone())?;
                    }
                    match &func.kind {
                        crate::ResourceFuncKind::Method(name, results) => {
                            write!(
                                f,
                                "{:depth$}{}: func({})",
                                "",
                                name,
                                func.params,
                                depth = opts.indent(depth + 1)
                            )?;
                            if !results.is_empty() {
                                write!(f, " -> {}", results)?;
                            }
                            write!(f, ";\n",)?;
                        }
                        crate::ResourceFuncKind::Static(name, results) => {
                            write!(
                                f,
                                "{:depth$}{}: static func({})",
                                "",
                                name,
                                func.params,
                                depth = opts.indent(depth + 1)
                            )?;
                            if !results.is_empty() {
                                write!(f, " -> {}", results)?;
                            }
                            write!(f, ";\n",)?;
                        }
                        crate::ResourceFuncKind::Constructor => {
                            write!(
                                f,
                                "{:depth$}constructor({});\n",
                                "",
                                func.params,
                                depth = opts.indent(depth + 1)
                            )?;
                        }
                    }
                }
                write!(f, "{:depth$}}}\n", "", depth = opts.indent(depth))?;
            }
            TypeDefKind::Flags(flags) => {
                if let Some(docs) = &self.docs {
                    docs.render_opts(f, depth, opts.clone())?;
                }
                write!(
                    f,
                    "{:depth$}flags {} {{\n",
                    "",
                    self.name,
                    depth = opts.indent(depth)
                )?;
                for flag in &flags.flags {
                    if let Some(docs) = &flag.docs {
                        docs.render_opts(f, depth + 1, opts.clone())?;
                    }
                    write!(
                        f,
                        "{:depth$}{},\n",
                        "",
                        flag.name,
                        depth = opts.indent(depth + 1)
                    )?;
                }
                write!(f, "{:depth$}}}\n", "", depth = opts.indent(depth))?;
            }
            TypeDefKind::Variant(variant) => {
                if let Some(docs) = &self.docs {
                    docs.render_opts(f, depth, opts.clone())?;
                }
                write!(
                    f,
                    "{:depth$}variant {} {{\n",
                    "",
                    self.name,
                    depth = opts.indent(depth)
                )?;
                for case in &variant.cases {
                    if let Some(docs) = &case.docs {
                        docs.render_opts(f, depth + 1, opts.clone())?;
                    }
                    match &case.ty {
                        Some(type_) => {
                            write!(
                                f,
                                "{:depth$}{}({}),\n",
                                "",
                                case.name,
                                type_,
                                depth = opts.indent(depth + 1)
                            )?;
                        }
                        None => {
                            write!(
                                f,
                                "{:depth$}{},\n",
                                "",
                                case.name,
                                depth = opts.indent(depth + 1)
                            )?;
                        }
                    }
                }
                write!(f, "{:depth$}}}\n", "", depth = opts.indent(depth))?;
            }
            TypeDefKind::Enum(enum_) => {
                if let Some(docs) = &self.docs {
                    docs.render_opts(f, depth, opts.clone())?;
                }
                write!(
                    f,
                    "{:depth$}enum {} {{\n",
                    "",
                    self.name,
                    depth = opts.indent(depth)
                )?;
                for case in &enum_.cases {
                    if let Some(docs) = &case.docs {
                        docs.render_opts(f, depth + 1, opts.clone())?;
                    }
                    write!(
                        f,
                        "{:depth$}{},\n",
                        "",
                        case.name,
                        depth = opts.indent(depth + 1)
                    )?;
                }
                write!(f, "{:depth$}}}\n", "", depth = opts.indent(depth))?;
            }
            TypeDefKind::Type(type_) => {
                if let Some(docs) = &self.docs {
                    docs.render_opts(f, depth, opts.clone())?;
                }
                write!(
                    f,
                    "{:depth$}type {} = {};\n",
                    "",
                    self.name,
                    type_,
                    depth = opts.indent(depth)
                )?;
            }
        }
        Ok(())
    }
}
