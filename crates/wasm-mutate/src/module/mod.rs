use std::convert::TryFrom;

use wasm_encoder::{BlockType, Instruction, MemArg, ValType};
use wasmparser::{Ieee32, Ieee64, MemoryImmediate, Operator, Type, TypeDef};

use crate::{error::EitherType, Error};

#[derive(Debug, Clone)]
pub enum PrimitiveTypeInfo {
    I32,
    I64,
    F32,
    F64,
    // TODO, add others
}
#[derive(Debug, Clone)]
pub struct FuncInfo {
    pub params: Vec<PrimitiveTypeInfo>,
    pub returns: Vec<PrimitiveTypeInfo>,
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Func(FuncInfo),
    Instance(),
    Module(),
}

impl TryFrom<Type> for PrimitiveTypeInfo {
    type Error = super::Error;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            wasmparser::Type::I32 => Ok(PrimitiveTypeInfo::I32),
            wasmparser::Type::I64 => Ok(PrimitiveTypeInfo::I64),
            wasmparser::Type::F32 => Ok(PrimitiveTypeInfo::F32),
            wasmparser::Type::F64 => Ok(PrimitiveTypeInfo::F64),
            _ => Err(super::Error::UnsupportedType(EitherType::Type(value))),
        }
    }
}

impl TryFrom<TypeDef<'_>> for TypeInfo {
    type Error = super::Error;

    fn try_from(value: TypeDef<'_>) -> Result<Self, Self::Error> {
        match value {
            TypeDef::Func(ft) => Ok(TypeInfo::Func(FuncInfo {
                params: ft
                    .params
                    .iter()
                    .map(|&t| PrimitiveTypeInfo::try_from(t).unwrap())
                    .collect(),
                returns: ft
                    .returns
                    .iter()
                    .map(|&t| PrimitiveTypeInfo::try_from(t).unwrap())
                    .collect(),
            })),
            _ => Err(super::Error::UnsupportedType(EitherType::TypeDef(format!(
                "{:?}",
                value
            )))),
        }
    }
}

pub fn map_type(tpe: Type) -> super::Result<ValType> {
    match tpe {
        Type::I32 => Ok(ValType::I32),
        Type::I64 => Ok(ValType::I64),
        Type::F32 => Ok(ValType::F32),
        Type::F64 => Ok(ValType::F64),
        _ => Err(super::Error::UnsupportedType(EitherType::Type(tpe))),
    }
}
