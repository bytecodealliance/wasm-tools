use crate::{ident::Ident, Docs};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Flags {
    pub(crate) flags: Vec<Flag>,
}

impl Flags {
    pub fn new(flags: impl IntoIterator<Item = impl Into<Flag>>) -> Self {
        Self {
            flags: flags.into_iter().map(|f| f.into()).collect(),
        }
    }

    pub fn flags(&self) -> &[Flag] {
        &self.flags
    }

    pub fn flags_mut(&mut self) -> &mut Vec<Flag> {
        &mut self.flags
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
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
        flag.docs(Some(self.1));
        flag
    }
}
