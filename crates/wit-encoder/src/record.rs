use crate::{ident::Ident, Docs, Type};

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
    pub name: Ident,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: Type,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Option<Docs>,
}

impl Field {
    pub fn new(name: impl Into<Ident>, ty: Type) -> Self {
        Self {
            name: name.into(),
            ty,
            docs: None,
        }
    }

    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}

impl<N> Into<Field> for (N, Type)
where
    N: Into<Ident>,
{
    fn into(self) -> Field {
        Field::new(self.0, self.1)
    }
}

impl<N, D> Into<Field> for (N, Type, D)
where
    N: Into<Ident>,
    D: Into<Docs>,
{
    fn into(self) -> Field {
        let mut field = Field::new(self.0, self.1);
        field.docs(Some(self.2.into()));
        field
    }
}
