use crate::Type;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Tuple {
    pub types: Vec<Type>,
}
