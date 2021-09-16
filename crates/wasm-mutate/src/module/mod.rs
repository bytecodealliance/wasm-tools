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
