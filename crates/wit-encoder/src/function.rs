use std::fmt::{self, Display};

use crate::{ident::Ident, Docs, Type};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
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

    pub fn items(&self) -> &Vec<(Ident, Type)> {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut Vec<(Ident, Type)> {
        &mut self.items
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Results {
    Named(Params),
    Anon(Type),
}

impl Display for Results {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Results::Anon(type_) => type_.fmt(f)?,
            Results::Named(vals) => {
                if !vals.items.is_empty() {
                    write!(f, "(")?;
                    let mut peekable = vals.items.iter().peekable();
                    while let Some((name, type_)) = peekable.next() {
                        write!(f, "{}: {}", name, type_)?;
                        if peekable.peek().is_some() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")?;
                }
            }
        };
        Ok(())
    }
}

impl Default for Results {
    fn default() -> Self {
        Results::empty()
    }
}

impl From<Type> for Results {
    fn from(value: Type) -> Self {
        Results::Anon(value)
    }
}

impl<N> FromIterator<(N, Type)> for Results
where
    N: Into<Ident>,
{
    fn from_iter<T: IntoIterator<Item = (N, Type)>>(iter: T) -> Self {
        Results::Named(Params::from_iter(iter))
    }
}

impl Results {
    // For the common case of an empty results list.
    pub fn empty() -> Results {
        Results::Named(Params::empty())
    }

    pub fn anon(type_: Type) -> Results {
        Results::Anon(type_)
    }

    pub fn named(types: impl IntoIterator<Item = (impl Into<Ident>, Type)>) -> Results {
        Results::Named(
            types
                .into_iter()
                .map(|(name, ty)| (name.into(), ty))
                .collect(),
        )
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        match self {
            Results::Named(params) => params.items().len(),
            Results::Anon(_) => 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StandaloneFunc {
    pub(crate) name: Ident,
    pub(crate) params: Params,
    pub(crate) results: Results,
    pub(crate) docs: Option<Docs>,
}

impl StandaloneFunc {
    pub fn new(name: impl Into<Ident>) -> Self {
        Self {
            name: name.into(),
            params: Params::empty(),
            results: Results::empty(),
            docs: None,
        }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn name_mut(&mut self) -> &mut Ident {
        &mut self.name
    }

    pub fn params(&mut self, params: impl Into<Params>) {
        self.params = params.into();
    }

    pub fn params_mut(&mut self) -> &mut Params {
        &mut self.params
    }

    pub fn results(&mut self, results: impl Into<Results>) {
        self.results = results.into();
    }

    pub fn results_mut(&mut self) -> &mut Results {
        &mut self.results
    }

    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

#[cfg(test)]
mod test {
    use crate::Results;

    #[test]
    fn is_empty() {
        let res = Results::empty();
        assert!(res.is_empty());
    }
}
