use crate::Type;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Result_ {
    pub ok: Option<Type>,
    pub err: Option<Type>,
}
