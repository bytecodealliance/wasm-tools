use crate::{Docs, Type};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Record {
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Field {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: Type,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,
}
