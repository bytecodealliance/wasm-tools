use crate::limits::{MAX_WASM_FUNCTION_RETURNS, MAX_WASM_START_ARGS};
use crate::prelude::*;
use crate::{BinaryReader, FromReader, Result};
use wasm_types::{ComponentFuncIdx, ComponentValueIdx};

/// Represents the start function in a WebAssembly component.
#[derive(Debug, Clone)]
pub struct ComponentStartFunction {
    /// The index to the start function.
    pub func_index: ComponentFuncIdx,
    /// The start function arguments.
    ///
    /// The arguments are specified by value index.
    pub arguments: Box<[ComponentValueIdx]>,
    /// The number of expected results for the start function.
    pub results: u32,
}

impl<'a> FromReader<'a> for ComponentStartFunction {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let func_index = reader.read_component_func_idx()?;
        let arguments = reader
            .read_iter(MAX_WASM_START_ARGS, "start function arguments")?
            .collect::<Result<_>>()?;
        let results = reader.read_size(MAX_WASM_FUNCTION_RETURNS, "start function results")? as u32;
        Ok(ComponentStartFunction {
            func_index,
            arguments,
            results,
        })
    }
}
