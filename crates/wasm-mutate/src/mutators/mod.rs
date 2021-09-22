use rand::prelude::SmallRng;
use wasm_encoder::Module;

use super::Result;
use crate::module::*;
use crate::{ModuleInfo, WasmMutate};
pub trait Mutator {
    /// Method where the mutation happpens
    ///
    /// * `config` instance of WasmMutate
    /// * `rnd` random number generator
    /// * `info` parsed lane AST of the input Wasm module
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &mut ModuleInfo,
    ) -> Result<Module>;

    /// Returns if this mutator can be applied with the info and the byte range in which it can be applied
    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> Result<bool>;

    /// Provides the name of the mutator, mostly used for debugging purposes
    fn name(&self) -> String {
        return format!("{:?}", std::any::type_name::<Self>());
    }
}

pub mod function2unreachable;
pub mod remove_export;
pub mod rename_export;
pub mod snip_function;
