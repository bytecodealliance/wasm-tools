use std::convert::TryFrom;

use wasm_encoder::{BlockType, ValType};
use wasmparser::{Type, TypeDef, TypeOrFuncType};

use crate::error::EitherType;

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveTypeInfo {
    I32,
    I64,
    F32,
    F64,
    V128,
    FuncRef,
    ExternRef,
    Empty,
    ExnRef,
    Func,
}

#[derive(Debug, Clone)]
pub struct FuncInfo {
    pub params: Vec<PrimitiveTypeInfo>,
    pub returns: Vec<PrimitiveTypeInfo>,
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Func(FuncInfo),
    // TODO: module linking support will require instance and module types.
}

impl TryFrom<Type> for PrimitiveTypeInfo {
    type Error = super::Error;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            Type::I32 => Ok(PrimitiveTypeInfo::I32),
            Type::I64 => Ok(PrimitiveTypeInfo::I64),
            Type::F32 => Ok(PrimitiveTypeInfo::F32),
            Type::F64 => Ok(PrimitiveTypeInfo::F64),
            Type::V128 => Ok(PrimitiveTypeInfo::V128),
            Type::FuncRef => Ok(PrimitiveTypeInfo::FuncRef),
            Type::ExternRef => Ok(PrimitiveTypeInfo::ExternRef),
            Type::EmptyBlockType => Ok(PrimitiveTypeInfo::Empty),
            Type::ExnRef => Ok(PrimitiveTypeInfo::ExnRef),
            Type::Func => Ok(PrimitiveTypeInfo::Func),
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
                    .map(|&t| PrimitiveTypeInfo::try_from(t))
                    .collect::<Result<Vec<_>, _>>()?,
                returns: ft
                    .returns
                    .iter()
                    .map(|&t| PrimitiveTypeInfo::try_from(t))
                    .collect::<Result<Vec<_>, _>>()?,
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
        Type::V128 => Ok(ValType::V128),
        Type::FuncRef => Ok(ValType::FuncRef),
        Type::ExternRef => Ok(ValType::ExternRef),
        _ => Err(super::Error::UnsupportedType(EitherType::Type(tpe))),
    }
}

pub fn map_block_type(ty: TypeOrFuncType) -> super::Result<BlockType> {
    match ty {
        TypeOrFuncType::Type(ty) => match ty {
            Type::I32 => Ok(BlockType::Result(ValType::I32)),
            Type::I64 => Ok(BlockType::Result(ValType::I64)),
            Type::F32 => Ok(BlockType::Result(ValType::F32)),
            Type::F64 => Ok(BlockType::Result(ValType::F64)),
            Type::EmptyBlockType => Ok(BlockType::Empty),
            _ => Err(super::Error::NoMutationsApplicable),
        },
        TypeOrFuncType::FuncType(_) => Err(super::Error::NoMutationsApplicable),
    }
}
