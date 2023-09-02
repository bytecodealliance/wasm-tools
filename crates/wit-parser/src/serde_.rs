use serde::ser::{SerializeSeq, Serializer};
use serde::Serialize;
use crate::{FunctionKind, Handle, Params, Results, Type, TypeOwner, WorldItem};
use crate::id_arena_::{Arena, Id};

pub fn serialize_arena<T, S>(v: &Arena<T>, serializer: S) -> Result<S::Ok, S::Error>
where
    T: Serialize,
    S: Serializer,
{
    let mut seq = serializer.serialize_seq(Some(v.len()))?;
    for (_, item) in v.iter() {
        seq.serialize_element(&item)?;
    }
    seq.end()
}

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
                serializer.serialize_newtype_variant("WorldItem", 0, "interface", &(interface_id.index() as u64))
            }
            WorldItem::Function(func) => {
                serializer.serialize_newtype_variant("WorldItem", 2, "function", func)
            }
            WorldItem::Type(type_id) => {
                serializer.serialize_newtype_variant("WorldItem", 2, "type", &(type_id.index() as u64))
            }
        }
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
                serializer.serialize_newtype_variant("TypeOwner", 0, "world", &(world_id.index() as u64))
            }
            TypeOwner::Interface(interface_id) => {
                serializer.serialize_newtype_variant("TypeOwner", 1, "interface", &(interface_id.index() as u64))
            }
            TypeOwner::None => serializer.serialize_none(),
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
                serializer.serialize_newtype_variant("Handle", 0, "own", &(type_id.index() as u64))
            }
            Handle::Borrow(type_id) => {
                serializer.serialize_newtype_variant("Handle", 1, "borrow", &(type_id.index() as u64))
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
