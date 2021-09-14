

#[derive(Debug)]
pub enum PrimitiveTypeInfo {
    I32,
    I64,
    F32,
    F64,
    // TODO, add others
}
#[derive(Debug)]
pub struct FuncInfo {
    pub params: Vec<PrimitiveTypeInfo>,
    pub returns: Vec<PrimitiveTypeInfo>,
}

#[derive(Debug)]
pub enum TypeInfo {
    Func(FuncInfo),
    Instance(),
    Module()
}

