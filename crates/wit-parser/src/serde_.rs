use serde::ser::{SerializeSeq, Serializer};
use serde::Serialize;
use crate::{Params, Results, Type};
use crate::id_arena_::{Arena, Id};

pub fn serialize_arena<T, S>(arena: &Arena<T>, serializer: S) -> Result<S::Ok, S::Error>
where
    T: Serialize,
    S: Serializer,
{
    let mut seq = serializer.serialize_seq(Some(arena.len()))?;
    for (_, item) in arena.iter() {
        seq.serialize_element(&item)?;
    }
    seq.end()
}

pub fn serialize_id<T, S>(id: &Id<T>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_u64(id.index() as u64)
}

impl<T> Serialize for Id<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u64(self.index() as u64)
    }
}

impl Serialize for Type {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Type::Bool => serializer.serialize_str("bool"),
            Type::U8 => serializer.serialize_str("u8"),
            Type::U16 => serializer.serialize_str("u16"),
            Type::U32 => serializer.serialize_str("u32"),
            Type::U64 => serializer.serialize_str("u64"),
            Type::S8 => serializer.serialize_str("s8"),
            Type::S16 => serializer.serialize_str("s16"),
            Type::S32 => serializer.serialize_str("s32"),
            Type::S64 => serializer.serialize_str("s64"),
            Type::Float32 => serializer.serialize_str("float32"),
            Type::Float64 => serializer.serialize_str("float64"),
            Type::Char => serializer.serialize_str("char"),
            Type::String => serializer.serialize_str("string"),
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
                serialize_params(params, serializer)
            }
            Results::Anon(typ) => {
                let params: Params = vec![(String::default(), *typ)];
                serialize_params(&params, serializer)
            }
        }
    }
}

pub fn serialize_params<S>(params: &Params, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut seq = serializer.serialize_seq(Some(params.len()))?;
    for (name, typ) in params.iter() {
        let param = Param{name: name.to_string(), typ: *typ};
        seq.serialize_element(&param)?;
    }
    seq.end()
}

#[derive(Debug, Clone, PartialEq, Serialize)]
struct Param {
    #[serde(skip_serializing_if = "String::is_empty")]
    pub name: String,
    #[serde(rename = "type")]
    pub typ: Type
}

// pub fn serialize_param<S>(param: &(String, Type), serializer: S) -> Result<S::Ok, S::Error>
// where
//     S: Serializer,
// {
//     let len = if param.0.is_empty() { 1 } else { 2 };
//     let mut s = serializer.serialize_struct("Param", len)?;
//     if !param.0.is_empty() {
//         s.serialize_field("name", &param.0)
//     }
//     s.serialize_field("type", &param.1);
//     s.end()
// }
