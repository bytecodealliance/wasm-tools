use crate::Docs;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Flags {
    pub flags: Vec<Flag>,
}

impl Flags {
    pub fn new(flags: impl IntoIterator<Item = impl Into<Flag>>) -> Self {
        Self {
            flags: flags.into_iter().map(|f| f.into()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Flag {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Option<Docs>,
}

impl Flag {
    pub fn new(name: impl Into<String>) -> Self {
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
    T: Into<String>,
{
    fn into(self) -> Flag {
        Flag::new(self.0)
    }
}

impl<T, D> Into<Flag> for (T, D)
where
    T: Into<String>,
    D: Into<Docs>,
{
    fn into(self) -> Flag {
        let mut flag = Flag::new(self.0);
        flag.docs(Some(self.1));
        flag
    }
}
