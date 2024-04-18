use crate::Case;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Variant {
    pub cases: Vec<Case>,
}
