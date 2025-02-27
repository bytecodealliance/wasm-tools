use std::fmt::{self, Display};

use crate::{ident::Ident, Docs, Type};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Params {
    items: Vec<(Ident, Type)>,
}

impl<N> From<(N, Type)> for Params
where
    N: Into<Ident>,
{
    fn from(value: (N, Type)) -> Self {
        Self {
            items: vec![(value.0.into(), value.1)],
        }
    }
}

impl<N> FromIterator<(N, Type)> for Params
where
    N: Into<Ident>,
{
    fn from_iter<T: IntoIterator<Item = (N, Type)>>(iter: T) -> Self {
        Self {
            items: iter.into_iter().map(|(n, t)| (n.into(), t)).collect(),
        }
    }
}

impl Display for Params {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut peekable = self.items.iter().peekable();
        while let Some((name, type_)) = peekable.next() {
            write!(f, "{}: {}", name, type_)?;
            if peekable.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

impl Params {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn push(&mut self, name: impl Into<Ident>, ty: Type) {
        self.items.push((name.into(), ty));
    }

    pub fn item(&mut self, name: impl Into<Ident>, item: impl Into<Type>) {
        self.items.push((name.into(), item.into()));
    }

    pub fn items(&self) -> &Vec<(Ident, Type)> {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut Vec<(Ident, Type)> {
        &mut self.items
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct StandaloneFunc {
    pub(crate) name: Ident,
    pub(crate) params: Params,
    pub(crate) result: Option<Type>,
    pub(crate) docs: Option<Docs>,
    pub(crate) async_: bool,
}

impl StandaloneFunc {
    pub fn new(name: impl Into<Ident>, async_: bool) -> Self {
        Self {
            name: name.into(),
            params: Params::empty(),
            result: None,
            docs: None,
            async_,
        }
    }

    pub fn set_name(&self) -> &Ident {
        &self.name
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn name_mut(&mut self) -> &mut Ident {
        &mut self.name
    }

    pub fn set_params(&mut self, params: impl Into<Params>) {
        self.params = params.into();
    }

    pub fn params(&self) -> &Params {
        &self.params
    }

    pub fn params_mut(&mut self) -> &mut Params {
        &mut self.params
    }

    pub fn result(&self) -> &Option<Type> {
        &self.result
    }

    pub fn set_result(&mut self, result: Option<Type>) {
        self.result = result;
    }

    pub fn result_mut(&mut self) -> &mut Option<Type> {
        &mut self.result
    }

    pub fn set_docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }

    pub fn docs(&self) -> &Option<Docs> {
        &self.docs
    }

    pub fn set_async(&mut self, async_: bool) {
        self.async_ = async_;
    }

    pub fn async_(&self) -> bool {
        self.async_
    }

    pub fn async_mut(&mut self) -> &mut bool {
        &mut self.async_
    }
}
