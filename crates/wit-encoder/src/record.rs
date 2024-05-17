use crate::{Docs, Type};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Record {
    pub fields: Vec<Field>,
}

impl Record {
    pub fn new(fields: impl IntoIterator<Item = impl Into<Field>>) -> Self {
        Self {
            fields: fields.into_iter().map(|f| f.into()).collect(),
        }
    }
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

impl Field {
    pub fn new(name: impl Into<String>, ty: Type) -> Self {
        Self {
            name: name.into(),
            ty,
            docs: Docs::default(),
        }
    }

    pub fn docs(&mut self, docs: Docs) {
        self.docs = docs;
    }
}

impl<N> Into<Field> for (N, Type)
where
    N: Into<String>,
{
    fn into(self) -> Field {
        Field::new(self.0, self.1)
    }
}

impl<N> Into<Field> for (N, Type, Docs)
where
    N: Into<String>,
{
    fn into(self) -> Field {
        let mut field = Field::new(self.0, self.1);
        field.docs(self.2);
        field
    }
}
