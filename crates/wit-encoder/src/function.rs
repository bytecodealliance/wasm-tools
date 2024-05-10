use crate::{Docs, Type};

pub type Params = Vec<(String, Type)>;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
pub enum Results {
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_params"))]
    Named(Params),
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_anon_result"))]
    Anon(Type),
}

pub enum ResultsTypeIter<'a> {
    Named(std::slice::Iter<'a, (String, Type)>),
    Anon(std::iter::Once<&'a Type>),
}

impl<'a> Iterator for ResultsTypeIter<'a> {
    type Item = &'a Type;

    fn next(&mut self) -> Option<&'a Type> {
        match self {
            ResultsTypeIter::Named(ps) => ps.next().map(|p| &p.1),
            ResultsTypeIter::Anon(ty) => ty.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            ResultsTypeIter::Named(ps) => ps.size_hint(),
            ResultsTypeIter::Anon(ty) => ty.size_hint(),
        }
    }
}

impl<'a> ExactSizeIterator for ResultsTypeIter<'a> {}

impl Results {
    // For the common case of an empty results list.
    pub fn empty() -> Results {
        Results::Named(Vec::new())
    }

    pub fn len(&self) -> usize {
        match self {
            Results::Named(params) => params.len(),
            Results::Anon(_) => 1,
        }
    }

    pub fn iter_types(&self) -> ResultsTypeIter {
        match self {
            Results::Named(ps) => ResultsTypeIter::Named(ps.iter()),
            Results::Anon(ty) => ResultsTypeIter::Anon(std::iter::once(ty)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Function {
    pub name: String,
    pub kind: FunctionKind,
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_params"))]
    pub params: Params,
    pub results: Results,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,
}

impl Function {
    pub fn new(name: impl Into<String>, kind: FunctionKind) -> Self {
        Self {
            name: name.into(),
            kind: kind,
            params: vec![],
            results: Results::empty(),
            docs: Docs::default(),
        }
    }

    pub fn new_freestanding(name: impl Into<String>) -> Self {
        Self::new(name, FunctionKind::Freestanding)
    }

    pub fn new_method(name: impl Into<String>) -> Self {
        Self::new(name, FunctionKind::Method)
    }

    pub fn new_static(name: impl Into<String>) -> Self {
        Self::new(name, FunctionKind::Static)
    }

    pub fn new_constructor(name: impl Into<String>) -> Self {
        Self::new(name, FunctionKind::Constructor)
    }

    pub fn params(&mut self, params: Params) {
        self.params = params;
    }

    pub fn results(&mut self, results: Results) {
        self.results = results;
    }

    pub fn docs(&mut self, docs: Docs) {
        self.docs = docs;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum FunctionKind {
    Freestanding,
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id"))]
    Method,
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id"))]
    Static,
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id"))]
    Constructor,
}
