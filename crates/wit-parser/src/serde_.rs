use serde::ser::{Serialize, SerializeStruct, Serializer};

use crate::{Docs, Function, FunctionKind, Results, Type, WorldItem};

impl Serialize for WorldItem {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            WorldItem::Interface(id) => {
                let mut s = serializer.serialize_struct("WorldItem", 2)?;
                s.serialize_field("Interface", &format!("interface-{}", id.index()))?;
                s.end()
            }
            WorldItem::Function(func) => {
                let mut s = serializer.serialize_struct("WorldItem", 1)?;
                s.serialize_field("Function", func)?;
                s.end()
            }
            WorldItem::Type(type_id) => {
                let mut s = serializer.serialize_struct("WorldItem", 1)?;
                s.serialize_field("Type", &format!("type-{}", type_id.index()))?;
                s.end()
            }
        }
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
                let mut s = serializer.serialize_struct("Method", 1)?;
                s.serialize_field("TypeId", type_id)?;
                s.end()
            }
            FunctionKind::Static(type_id) => {
                let mut s = serializer.serialize_struct("Static", 1)?;
                s.serialize_field("TypeId", type_id)?;
                s.end()
            }
            FunctionKind::Constructor(type_id) => {
                let mut s = serializer.serialize_struct("Constructor", 1)?;
                s.serialize_field("TypeId", type_id)?;
                s.end()
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
            Type::Id(type_id) => {
                let type_str = format!("type-{}", type_id.index());
                serializer.serialize_str(&type_str)
            }
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
                serializer.serialize_newtype_variant("Results", 0, "Named", params)
            }
            Results::Anon(type_) => {
                serializer.serialize_newtype_variant("Results", 1, "Anon", type_)
            }
        }
    }
}
