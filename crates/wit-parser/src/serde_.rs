use serde::ser::{SerializeSeq, SerializeStruct, Serializer};
use serde::Serialize;
use crate::{Docs, Function, FunctionKind, Params, Results, Type};
use crate::id_arena_::Id;

impl<T> Serialize for Id<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u64(self.index() as u64)
    }
}

impl Serialize for Function {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("Function", 5)?;
        s.serialize_field("docs", &self.docs)?;
        s.serialize_field("name", &self.name)?;
        s.serialize_field("kind", &self.kind)?;
        s.serialize_field("params", &self.params)?;
        s.serialize_field("results", &self.results)?;
        s.end()
    }
}

impl Serialize for Docs {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("Docs", 2)?;
        s.serialize_field("contents", &self.contents)?;
        s.end()
    }
}

impl Serialize for FunctionKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            FunctionKind::Freestanding => serializer.serialize_str("Freestanding"),
            FunctionKind::Method(type_id) => {
                serializer.serialize_newtype_variant("FunctionKind", 0, "Method", &(type_id.index() as u64))
            }
            FunctionKind::Static(type_id) => {
                serializer.serialize_newtype_variant("FunctionKind", 1, "Static", &(type_id.index() as u64))
            }
            FunctionKind::Constructor(type_id) => {
                serializer.serialize_newtype_variant("FunctionKind", 2, "Constructor", &(type_id.index() as u64))
            }
        }
    }
}

impl Serialize for Type {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Type::Bool => serializer.serialize_str("Bool"),
            Type::U8 => serializer.serialize_str("U8"),
            Type::U16 => serializer.serialize_str("U16"),
            Type::U32 => serializer.serialize_str("U32"),
            Type::U64 => serializer.serialize_str("U64"),
            Type::S8 => serializer.serialize_str("S8"),
            Type::S16 => serializer.serialize_str("S16"),
            Type::S32 => serializer.serialize_str("S32"),
            Type::S64 => serializer.serialize_str("S64"),
            Type::Float32 => serializer.serialize_str("Float32"),
            Type::Float64 => serializer.serialize_str("Float64"),
            Type::Char => serializer.serialize_str("Char"),
            Type::String => serializer.serialize_str("String"),
            Type::Id(type_id) => serializer.serialize_u64(type_id.index() as u64)
        }
    }
}

impl Serialize for Results {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Results::Named(params) => {
                serializer.serialize_some(params)
            }
            Results::Anon(typ) => {
                let mut seq = serializer.serialize_seq(Some(1))?;
                let param = Param{name: None, ty: *typ};
                seq.serialize_element(&param)?;
                seq.end()
            }
        }
    }
}

impl Serialize for Params {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for (name, typ) in self.iter() {
            let param = Param{name: Some(name.to_string()), ty: *typ};
            seq.serialize_element(&param)?;
        }
        seq.end()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
struct Param {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(rename = "type")]
    pub ty: Type
}
