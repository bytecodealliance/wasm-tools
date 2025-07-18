use crate::{Docs, ident::Ident};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Flags {
    pub(crate) flags: Vec<Flag>,
}

impl Flags {
    pub fn new(flags: impl IntoIterator<Item = impl Into<Flag>>) -> Self {
        Self {
            flags: flags.into_iter().map(|f| f.into()).collect(),
        }
    }

    pub fn flag(&mut self, flag: impl Into<Flag>) {
        self.flags.push(flag.into());
    }

    pub fn flags(&self) -> &[Flag] {
        &self.flags
    }

    pub fn flags_mut(&mut self) -> &mut Vec<Flag> {
        &mut self.flags
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Flag {
    pub(crate) name: Ident,
    pub(crate) docs: Option<Docs>,
}

impl Flag {
    pub fn new(name: impl Into<Ident>) -> Self {
        Flag {
            name: name.into(),
            docs: None,
        }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn set_name(&mut self, name: impl Into<Ident>) {
        self.name = name.into();
    }

    pub fn docs(&self) -> &Option<Docs> {
        &self.docs
    }

    pub fn set_docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

impl<T> Into<Flag> for (T,)
where
    T: Into<Ident>,
{
    fn into(self) -> Flag {
        Flag::new(self.0)
    }
}

impl<T, D> Into<Flag> for (T, D)
where
    T: Into<Ident>,
    D: Into<Docs>,
{
    fn into(self) -> Flag {
        let mut flag = Flag::new(self.0);
        flag.set_docs(Some(self.1));
        flag
    }
}
