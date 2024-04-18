use crate::Type;

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum Handle {
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id"))]
    Own(Type),
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id"))]
    Borrow(Type),
}
