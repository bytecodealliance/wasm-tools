/// TODO
#[derive(Debug)]
pub struct Import {
    /// TODO
    pub module: String,
    /// TODO
    pub name: Option<String>,
    /// TODO
    pub desc: EntityDesc,
}

/// TODO
#[non_exhaustive]
#[derive(Debug)]
pub enum EntityDesc {
    /// TODO
    Func(FuncType),
    /// TODO
    Global(GlobalType),
    /// TODO
    Table(TableType),
    /// TODO
    Memory(MemoryType),
    /// TODO
    Instance(InstanceType),
}

/// A function type.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FuncType {
    /// Types of parameters of the function.
    pub params: Vec<ValType>,
    /// Types of results of the function.
    pub results: Vec<ValType>,
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub struct GlobalType {
    /// TODO
    pub val_type: ValType,
    /// TODO
    pub mutable: bool,
}

/// Primitive WASM type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[non_exhaustive]
pub enum ValType {
    /// Signed 32-bit integer.
    I32,
    /// Signed 64-bit integer.
    I64,
    /// 32-bit floating point number.
    F32,
    /// 64-bit floating point number.
    F64,
    /// TODO:
    V128,
    /// TODO:
    FuncRef,
    /// TODO:
    ExternRef,
}

/// TODO:
#[derive(Clone, Debug)]
pub struct MemoryType {
    /// TODO:
    pub limits: Limits,
}

/// TODO:
#[derive(Clone, Debug)]
pub struct TableType {
    /// TODO:
    pub limits: Limits,
    /// TODO:
    pub elem_ty: ValType,
}

/// TODO:
#[derive(Clone, Debug)]
pub struct Limits {
    /// TODO:
    pub min: u32,
    /// TODO:
    pub max: Option<u32>,
}

#[derive(Debug)]
pub struct InstanceType {
    pub exports: indexmap::IndexMap<String, EntityDesc>,
}
