use crate::{FunctionKind, Params};

use crate::Function;

#[derive(Debug, Clone, PartialEq)]
pub enum Resource {
    Item(String),
    ItemList(String, Vec<ResourceMethod>),
    Method(ResourceMethod),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResourceMethod {
    Function(Function),
    Static(String, FunctionKind),
    Constructor(Params),
}
