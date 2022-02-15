use crate::{Error, Result};
use std::convert::TryFrom;
use wasm_encoder::{BlockType, ValType};
use wasmparser::{Type, TypeDef};

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

impl From<Type> for PrimitiveTypeInfo {
    fn from(value: Type) -> Self {
        match value {
            Type::I32 => PrimitiveTypeInfo::I32,
            Type::I64 => PrimitiveTypeInfo::I64,
            Type::F32 => PrimitiveTypeInfo::F32,
            Type::F64 => PrimitiveTypeInfo::F64,
            Type::V128 => PrimitiveTypeInfo::V128,
            Type::FuncRef => PrimitiveTypeInfo::FuncRef,
            Type::ExternRef => PrimitiveTypeInfo::ExternRef,
        }
    }
}

impl TryFrom<TypeDef> for TypeInfo {
    type Error = Error;

    fn try_from(value: TypeDef) -> Result<Self> {
        match value {
            TypeDef::Func(ft) => Ok(TypeInfo::Func(FuncInfo {
                params: ft
                    .params
                    .iter()
                    .map(|&t| PrimitiveTypeInfo::from(t))
                    .collect(),
                returns: ft
                    .returns
                    .iter()
                    .map(|&t| PrimitiveTypeInfo::from(t))
                    .collect(),
            })),
        }
    }
}

pub fn map_type(tpe: Type) -> Result<ValType> {
    match tpe {
        Type::I32 => Ok(ValType::I32),
        Type::I64 => Ok(ValType::I64),
        Type::F32 => Ok(ValType::F32),
        Type::F64 => Ok(ValType::F64),
        Type::V128 => Ok(ValType::V128),
        Type::FuncRef => Ok(ValType::FuncRef),
        Type::ExternRef => Ok(ValType::ExternRef),
    }
}

pub fn map_block_type(ty: wasmparser::BlockType) -> Result<BlockType> {
    match ty {
        wasmparser::BlockType::Empty => Ok(BlockType::Empty),
        wasmparser::BlockType::Type(ty) => Ok(BlockType::Result(map_type(ty)?)),
        wasmparser::BlockType::FuncType(f) => Ok(BlockType::FunctionType(f)),
    }
}
