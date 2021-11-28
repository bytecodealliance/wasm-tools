//! Mutator trait
//!
//! `wasm-mutate` in build on top of three main group or mutators:
//! **top wasm module struct mutators**, [**code
//! motion mutators**][super::CodemotionMutator] and [**peephole
//! mutators**][super::PeepholeMutator].
//!
//! The former type is meant to change(mutate) the top structure of the input Wasm
//! binary, like for example, [by renaming the name of an exported function][super::RenameExportMutator].
//!
//! The later two types make changes deeper on the code of the input Wasm binary,
//! specifycally at the code section level of the binary. The code motion mutator
//! parses the code and provides an AST which is transformed in a semantically
//! equivalent way. We provide two concrete implementations using this type of
//! mutator: [LoopUnrollMutator][codemotion::mutators::loop_unrolling::LoopUnrollMutator] and [IfComplementMutator][codemotion::mutators::if_complement::IfComplementMutator].
//!
//! The last group of mutators are the [**peephole
//! mutators**][super::PeepholeMutator]. When it comes to the input Wasm binary code section, it
//! iterates through the defined functions, and then each instruction of the
//! functions is processed in order to construct an equivalent piece of code.
//!
use rand::prelude::SmallRng;
use wasm_encoder::Module;
use wasmparser::Operator;

use super::Result;
use crate::{ModuleInfo, WasmMutate};

/// This trait needs to be implemented for all mutators
///
/// Take a look to the implementation of the
/// [RenameExportMutator][super::RenameExportMutator] implementation for a reference
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

pub mod codemotion;
pub mod function_body_unreachable;
pub mod peephole;
pub mod remove_export;
pub mod rename_export;
pub mod snip_function;

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
