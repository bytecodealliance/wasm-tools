use crate::{Docs, Ident};

/// A variant without a payload
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Enum {
    pub(crate) cases: Vec<EnumCase>,
}

impl<C> FromIterator<C> for Enum
where
    C: Into<EnumCase>,
{
    fn from_iter<T: IntoIterator<Item = C>>(iter: T) -> Self {
        Self {
            cases: iter.into_iter().map(|c| c.into()).collect(),
        }
    }
}

impl Enum {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn case(&mut self, case: impl Into<EnumCase>) {
        self.cases.push(case.into());
    }

    pub fn cases(&self) -> &[EnumCase] {
        &self.cases
    }

    pub fn cases_mut(&mut self) -> &mut Vec<EnumCase> {
        &mut self.cases
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct EnumCase {
    pub(crate) name: Ident,
    pub(crate) docs: Option<Docs>,
}

impl<N> From<N> for EnumCase
where
    N: Into<Ident>,
{
    fn from(value: N) -> Self {
        Self {
            name: value.into(),
            docs: None,
        }
    }
}

impl EnumCase {
    pub fn new(name: impl Into<Ident>) -> Self {
        Self {
            name: name.into(),
            docs: None,
        }
    }

    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}
