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
    pub docs: Docs,
}

impl Flag {
    pub fn new(name: impl Into<String>) -> Self {
        Flag {
            name: name.into(),
            docs: Docs::default(),
        }
    }

    pub fn docs(&mut self, docs: Docs) {
        self.docs = docs
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
