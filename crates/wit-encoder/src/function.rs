use std::fmt::{self, Display};

use crate::{ident::Ident, Docs, Render, RenderOpts, Type};

#[derive(Debug, Clone, PartialEq, Default)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
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

    pub fn items(&self) -> &Vec<(Ident, Type)> {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut Vec<(Ident, Type)> {
        &mut self.items
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
pub enum Results {
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_params"))]
    Named(Params),
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_anon_result"))]
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
        Results::Named(Default::default())
    }

    pub fn anon(type_: Type) -> Results {
        Results::Anon(type_)
    }

    pub fn named(types: impl IntoIterator<Item = (impl Into<String>, Type)>) -> Results {
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

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct StandaloneFunction {
    name: Ident,
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_params"))]
    params: Params,
    results: Results,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    docs: Option<Docs>,
}

impl StandaloneFunction {
    pub fn new(name: impl Into<Ident>) -> Self {
        Self {
            name: name.into(),
            params: Params::empty(),
            results: Results::empty(),
            docs: None,
        }
    }

    pub fn params(&mut self, params: Params) {
        self.params = params;
    }

    pub fn results(&mut self, results: Results) {
        self.results = results;
    }

    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

impl Render for StandaloneFunction {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
        if let Some(docs) = &self.docs {
            docs.render(f, opts)?;
        }
        write!(f, "{}{}: func({})", opts.spaces(), self.name, self.params,)?;
        if !self.results.is_empty() {
            write!(f, " -> {}", self.results)?;
        }
        write!(f, ";\n")?;
        Ok(())
    }
}
