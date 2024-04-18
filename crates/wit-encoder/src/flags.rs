use crate::Docs;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Flags {
    pub flags: Vec<Flag>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Flag {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FlagsRepr {
    U8,
    U16,
    U32(usize),
}

impl FlagsRepr {
    pub fn count(&self) -> usize {
        match self {
            FlagsRepr::U8 => 1,
            FlagsRepr::U16 => 1,
            FlagsRepr::U32(n) => *n,
        }
    }
}
