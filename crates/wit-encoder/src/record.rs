use crate::{ident::Ident, Docs, Type};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Record {
    pub(crate) fields: Vec<Field>,
}

impl Record {
    pub fn new(fields: impl IntoIterator<Item = impl Into<Field>>) -> Self {
        Self {
            fields: fields.into_iter().map(|f| f.into()).collect(),
        }
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    pub fn fields_mut(&mut self) -> &mut Vec<Field> {
        &mut self.fields
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Field {
    pub(crate) name: Ident,
    pub(crate) ty: Type,
    pub(crate) docs: Option<Docs>,
}

impl Field {
    pub fn new(name: impl Into<Ident>, ty: Type) -> Self {
        Self {
            name: name.into(),
            ty,
            docs: None,
        }
    }

    pub fn name(&mut self) -> &Ident {
        &self.name
    }

    pub fn set_name(&mut self, name: impl Into<Ident>) {
        self.name = name.into();
    }

    pub fn set_docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }

    pub fn docs(&self) -> &Option<Docs> {
        &self.docs
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn ty_mut(&mut self) -> &mut Type {
        &mut self.ty
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
        field.set_docs(Some(self.2.into()));
        field
    }
}
