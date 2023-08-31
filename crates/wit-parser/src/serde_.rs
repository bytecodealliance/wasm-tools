use serde::ser::{Serialize, SerializeStruct, Serializer};

use crate::{Docs, Function, FunctionKind, Handle, Results, Type, TypeOwner, WorldItem};
use crate::id_arena_::Id;

impl<T> Serialize for Id<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u64(self.index() as u64)
    }
}

impl Serialize for WorldItem {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            WorldItem::Interface(interface_id) => {
                serializer.serialize_newtype_variant("WorldItem", 0, "Interface", &(interface_id.index() as u64))
            }
            WorldItem::Function(func) => {
                serializer.serialize_newtype_variant("WorldItem", 1, "Function", func)
            }
            WorldItem::Type(type_id) => {
                serializer.serialize_newtype_variant("WorldItem", 2, "Type", &(type_id.index() as u64))
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

impl Serialize for TypeOwner {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            TypeOwner::World(world_id) => {
                serializer.serialize_newtype_variant("TypeOwner", 0, "World", &(world_id.index() as u64))
            }
            TypeOwner::Interface(interface_id) => {
                serializer.serialize_newtype_variant("TypeOwner", 1, "Interface", &(interface_id.index() as u64))
            }
            TypeOwner::None => {
                serializer.serialize_none()
            }
        }
    }
}

impl Serialize for Handle {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Handle::Own(type_id) => {
                serializer.serialize_newtype_variant("Handle", 0, "Own", &(type_id.index() as u64))
            }
            Handle::Borrow(type_id) => {
                serializer.serialize_newtype_variant("Handle", 1, "Borrow", &(type_id.index() as u64))
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
            Results::Anon(typ) => {
                serializer.serialize_newtype_variant("Results", 1, "Anon", typ)
            }
        }
    }
}
