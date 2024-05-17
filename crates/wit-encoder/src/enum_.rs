use crate::EnumCase;

/// A variant without a payload
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Enum {
    pub cases: Vec<EnumCase>,
}
