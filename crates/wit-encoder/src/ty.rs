use std::fmt::{self, Display};

use crate::{
    ident::Ident, Docs, Enum, EnumCase, Field, Flag, Flags, Record, Render, RenderOpts, Resource,
    ResourceFunc, Result_, Tuple, Variant,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
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
    Borrow(Ident),
    Option(Box<Type>),
    Result(Box<Result_>),
    List(Box<Type>),
    Tuple(Tuple),
    Named(Ident),
}

impl Type {
    pub fn borrow(name: impl Into<Ident>) -> Self {
        Type::Borrow(name.into())
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
    pub fn named(name: impl Into<Ident>) -> Self {
        Type::Named(name.into())
    }
}
impl From<Result_> for Type {
    fn from(value: Result_) -> Self {
        Self::result(value)
    }
}
impl From<Tuple> for Type {
    fn from(value: Tuple) -> Self {
        Type::Tuple(value)
    }
}
impl From<Ident> for Type {
    fn from(value: Ident) -> Self {
        Self::named(value)
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct VariantCase {
    name: Ident,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    type_: Option<Type>,
    docs: Option<Docs>,
}

impl VariantCase {
    pub fn empty(name: impl Into<Ident>) -> Self {
        Self {
            name: name.into(),
            type_: None,
            docs: None,
        }
    }

    pub fn value(name: impl Into<Ident>, ty: Type) -> Self {
        Self {
            name: name.into(),
            type_: Some(ty),
            docs: None,
        }
    }

    pub fn set_name(&mut self, name: impl Into<Ident>) {
        self.name = name.into();
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn name_mut(&mut self) -> &mut Ident {
        &mut self.name
    }

    pub fn type_(&self) -> Option<&Type> {
        self.type_.as_ref()
    }

    pub fn type_mut(&mut self) -> &mut Option<Type> {
        &mut self.type_
    }

    pub fn set_docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }

    pub fn docs(&self) -> &Option<Docs> {
        &self.docs
    }
}

impl<N> Into<VariantCase> for (N,)
where
    N: Into<Ident>,
{
    fn into(self) -> VariantCase {
        VariantCase::empty(self.0)
    }
}

impl<N> Into<VariantCase> for (N, Type)
where
    N: Into<Ident>,
{
    fn into(self) -> VariantCase {
        VariantCase::value(self.0, self.1)
    }
}

impl<N, D> Into<VariantCase> for (N, Type, D)
where
    N: Into<Ident>,
    D: Into<Docs>,
{
    fn into(self) -> VariantCase {
        let mut field = VariantCase::value(self.0, self.1);
        field.set_docs(Some(self.2.into()));
        field
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct TypeDef {
    name: Ident,
    kind: TypeDefKind,
    docs: Option<Docs>,
}

impl TypeDef {
    pub fn new(name: impl Into<Ident>, kind: TypeDefKind) -> Self {
        TypeDef {
            name: name.into(),
            kind,
            docs: None,
        }
    }

    pub fn record(
        name: impl Into<Ident>,
        fields: impl IntoIterator<Item = impl Into<Field>>,
    ) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::record(fields),
            docs: None,
        }
    }

    pub fn resource(
        name: impl Into<Ident>,
        funcs: impl IntoIterator<Item = impl Into<ResourceFunc>>,
    ) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::resource(funcs),
            docs: None,
        }
    }

    pub fn flags(name: impl Into<Ident>, flags: impl IntoIterator<Item = impl Into<Flag>>) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::flags(flags),
            docs: None,
        }
    }

    pub fn variant(
        name: impl Into<Ident>,
        cases: impl IntoIterator<Item = impl Into<VariantCase>>,
    ) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::variant(cases),
            docs: None,
        }
    }

    pub fn enum_(
        name: impl Into<Ident>,
        cases: impl IntoIterator<Item = impl Into<EnumCase>>,
    ) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::enum_(cases),
            docs: None,
        }
    }

    pub fn type_(name: impl Into<Ident>, type_: Type) -> Self {
        TypeDef {
            name: name.into(),
            kind: TypeDefKind::type_(type_),
            docs: None,
        }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn name_mut(&mut self) -> &mut Ident {
        &mut self.name
    }

    pub fn kind(&self) -> &TypeDefKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut TypeDefKind {
        &mut self.kind
    }

    pub fn set_docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
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
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
        match &self.kind {
            TypeDefKind::Record(record) => {
                if let Some(docs) = &self.docs {
                    docs.render(f, opts)?;
                }
                write!(f, "{}record {} {{", opts.spaces(), self.name)?;
                for (index, field) in record.fields.iter().enumerate() {
                    if index == 0 {
                        write!(f, "\n")?;
                    }
                    let opts = opts.indent();
                    if let Some(docs) = &field.docs {
                        docs.render(f, &opts)?;
                    }
                    write!(f, "{}{}: {},\n", opts.spaces(), field.name, field.type_)?;
                }
                write!(f, "{}}}\n", opts.spaces())?;
            }
            TypeDefKind::Resource(resource) => {
                if let Some(docs) = &self.docs {
                    docs.render(f, opts)?;
                }
                write!(f, "{}resource {} {{\n", opts.spaces(), self.name)?;
                for func in &resource.funcs {
                    let opts = opts.indent();
                    if let Some(docs) = &func.docs {
                        docs.render(f, &opts)?;
                    }
                    match &func.kind {
                        crate::ResourceFuncKind::Method(name, results) => {
                            write!(f, "{}{}: func({})", opts.spaces(), name, func.params)?;
                            if !results.is_empty() {
                                write!(f, " -> {}", results)?;
                            }
                            write!(f, ";\n")?;
                        }
                        crate::ResourceFuncKind::Static(name, results) => {
                            write!(f, "{}{}: static func({})", opts.spaces(), name, func.params)?;
                            if !results.is_empty() {
                                write!(f, " -> {}", results)?;
                            }
                            write!(f, ";\n")?;
                        }
                        crate::ResourceFuncKind::Constructor => {
                            write!(f, "{}constructor({});\n", opts.spaces(), func.params)?;
                        }
                    }
                }
                write!(f, "{}}}\n", opts.spaces())?;
            }
            TypeDefKind::Flags(flags) => {
                if let Some(docs) = &self.docs {
                    docs.render(f, opts)?;
                }
                write!(f, "{}flags {} {{\n", opts.spaces(), self.name)?;
                for flag in &flags.flags {
                    let opts = opts.indent();
                    if let Some(docs) = &flag.docs {
                        docs.render(f, &opts)?;
                    }
                    write!(f, "{}{},\n", opts.spaces(), flag.name)?;
                }
                write!(f, "{}}}\n", opts.spaces())?;
            }
            TypeDefKind::Variant(variant) => {
                if let Some(docs) = &self.docs {
                    docs.render(f, opts)?;
                }
                write!(f, "{}variant {} {{\n", opts.spaces(), self.name)?;
                for case in &variant.cases {
                    let opts = opts.indent();
                    if let Some(docs) = &case.docs {
                        docs.render(f, &opts)?;
                    }
                    match &case.type_ {
                        Some(type_) => {
                            write!(f, "{}{}({}),\n", opts.spaces(), case.name, type_)?;
                        }
                        None => {
                            write!(f, "{}{},\n", opts.spaces(), case.name)?;
                        }
                    }
                }
                write!(f, "{}}}\n", opts.spaces())?;
            }
            TypeDefKind::Enum(enum_) => {
                if let Some(docs) = &self.docs {
                    docs.render(f, opts)?;
                }
                write!(f, "{}enum {} {{\n", opts.spaces(), self.name)?;
                for case in &enum_.cases {
                    let opts = opts.indent();
                    if let Some(docs) = &case.docs {
                        docs.render(f, &opts)?;
                    }
                    write!(f, "{}{},\n", opts.spaces(), case.name)?;
                }
                write!(f, "{}}}\n", opts.spaces())?;
            }
            TypeDefKind::Type(type_) => {
                if let Some(docs) = &self.docs {
                    docs.render(f, opts)?;
                }
                write!(f, "{}type {} = {};\n", opts.spaces(), self.name, type_)?;
            }
        }
        Ok(())
    }
}
