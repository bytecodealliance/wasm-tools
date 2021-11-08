//! Mutator trait
use rand::prelude::SmallRng;
use wasm_encoder::Module;
use wasmparser::Operator;

use super::Result;
use crate::{ModuleInfo, WasmMutate};

/// This trait is implemented for all mutators
/// TODO extend and add example here
pub trait Mutator {
    /// Method where the mutation happpens
    ///
    /// * `config` instance of WasmMutate
    /// * `rnd` random number generator
    /// * `info` parsed lane AST of the input Wasm module
    fn mutate(&self, config: &WasmMutate, rnd: &mut SmallRng, info: &ModuleInfo) -> Result<Module>;

    /// Returns if this mutator can be applied with the info and the byte range in which it can be applied
    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> bool;

    /// Provides the name of the mutator, mostly used for debugging purposes
    fn name(&self) -> String {
        return std::any::type_name::<Self>().into();
    }
}

/// Type helper to wrap operator and the byte offset in the code section of a Wasm module
pub type OperatorAndByteOffset<'a> = (Operator<'a>, usize);

pub(crate) mod codemotion;
pub(crate) mod function_body_unreachable;
pub(crate) mod peephole;
pub(crate) mod remove_export;
pub(crate) mod rename_export;
pub(crate) mod snip_function;

#[cfg(test)]
pub(crate) fn match_mutation(original: &str, mutator: &dyn Mutator, expected: &str) {
    use rand::SeedableRng;

    let wasmmutate = WasmMutate::default();
    let original = &wat::parse_str(original).unwrap();

    let info = ModuleInfo::new(original).unwrap();
    let can_mutate = mutator.can_mutate(&wasmmutate, &info);

    assert!(can_mutate);

    let mut rnd = SmallRng::seed_from_u64(0);
    let mutation = mutator.mutate(&wasmmutate, &mut rnd, &info);

    let mutation_bytes = mutation.unwrap().finish();

    let mut validator = wasmparser::Validator::new();
    crate::validate(&mut validator, &mutation_bytes);

    // If it fails, it is probably an invalid
    // reformatting expected
    let text = wasmprinter::print_bytes(mutation_bytes).unwrap();
    let expected = &wat::parse_str(expected).unwrap();
    let expected_text = wasmprinter::print_bytes(expected).unwrap();

    assert_eq!(text, expected_text);
}
