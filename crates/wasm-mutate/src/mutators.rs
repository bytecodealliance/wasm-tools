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

pub mod add_function;
pub mod add_type;
pub mod codemotion;
pub mod custom;
pub mod function_body_unreachable;
pub mod modify_const_exprs;
pub mod modify_data;
pub mod peephole;
pub mod remove_export;
pub mod remove_item;
pub mod remove_section;
pub mod rename_export;
pub mod snip_function;
pub mod start;

mod translate;
pub use self::translate::Item;
use self::translate::{DefaultTranslator, Translator};

use std::borrow::Cow;

use super::Result;
use crate::WasmMutate;
use wasm_encoder::Module;
use wasmparser::Operator;

/// A mutation that can be applied to a Wasm module to produce a new, mutated
/// Wasm module.
pub trait Mutator {
    /// Can this `Mutator` *probably* be applied to the the given Wasm and
    /// configuration?
    ///
    /// When checking Wasm applicability, these checks should be implemented as
    /// quick, incomplete checks. For example, a mutator that removes a single
    /// function at a time should *not* build the whole call graph to determine
    /// if there are any functions that are dead code. Instead, it should just
    /// check that the Wasm contains at least one function. (It can always
    /// return an `Err` from the `mutate` method later, but we want to delay
    /// expensive computations until if/when we've committed to applying a given
    /// mutation).
    ///
    /// As an example of configuration checking, if a mutator adds new functions
    /// to the Wasm, increasing the Wasm's size, it should check whether the
    /// `WasmMutate` has been configured to only perform size-reducing
    /// mutations, and if so return `false` here.
    fn can_mutate(&self, config: &WasmMutate) -> bool;

    /// Run this mutation.
    ///
    /// Rather than returning a single, mutated module, we allow for mutators to
    /// return many.
    ///
    /// Mutators that return `Ok(iterator)` should always return an iterator
    /// with at least one item. Mutators should never return an empty iterator,
    /// instead they should return `Err(...)`.
    ///
    /// The iterators returned from this method must be *lazy*. Mutators should
    /// *not* return `Ok(Box::new(vec![...].into_iter()))`. Only one mutation
    /// should ever be computed at a time. Mutator implementations might find
    /// `std::iter::from_fn` helpful.
    ///
    /// When should a mutator return a single item via `std::iter::once` versus
    /// an iterator with many items? When the mutator builds up a bunch of state
    /// that was expensive to build and can be reused, it should return an
    /// iterator with many items that reuse that state. For example, the
    /// peephole mutator's e-graph is expensive to build and should be reused.
    /// If, however, the mutator doesn't build up state or its state is cheap to
    /// recompute, then the mutator should return a single-item iterator with
    /// `std::iter::once`, to give the fuzzer a chance to choose a new kind of
    /// mutation.
    fn mutate<'a>(
        self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>>;

    /// What is this mutator's name?
    ///
    /// This is only used for debugging and logging purposes.
    fn name(&self) -> Cow<'static, str> {
        return std::any::type_name::<Self>().into();
    }
}

/// Type helper to wrap operator and the byte offset in the code section of a Wasm module
pub type OperatorAndByteOffset<'a> = (Operator<'a>, usize);

#[cfg(test)]
fn match_mutation<T>(original: &str, mutator: T, expected: &str)
where
    T: Mutator + Clone,
{
    WasmMutate::default().match_mutation(original, mutator, expected)
}

impl WasmMutate<'_> {
    #[cfg(test)]
    fn match_mutation<T>(&mut self, original: &str, mutator: T, expected: &str)
    where
        T: Mutator + Clone,
    {
        use crate::ErrorKind;

        drop(env_logger::try_init());

        let original = &wat::parse_str(original).unwrap();

        let expected = &wat::parse_str(expected).unwrap();
        let expected_text = wasmprinter::print_bytes(expected).unwrap();

        let mut config = self.clone();
        config.setup(&original).unwrap();

        let can_mutate = mutator.can_mutate(&config);

        assert!(can_mutate);

        let attempts = 100;
        let mut last_mutation = None;

        for _ in 0..attempts {
            let mutation = match mutator
                .clone()
                .mutate(&mut config)
                .map(|mut mutation| mutation.next())
            {
                Ok(Some(mutation)) => mutation.unwrap(),
                Ok(None) => continue,
                Err(e) if matches!(e.kind(), ErrorKind::NoMutationsApplicable) => continue,
                Err(e) => panic!("mutation error: {}", e),
            };

            let mutation_bytes = mutation.finish();

            crate::validate(&mutation_bytes);

            // If it fails, it is probably an invalid
            // reformatting expected
            let text = wasmprinter::print_bytes(mutation_bytes).unwrap();
            if text.trim() == expected_text.trim() {
                return;
            }
            log::debug!("skipping mutation {}", text);
            last_mutation = Some(text);
        }

        match last_mutation {
            Some(mutation) => {
                panic!(
                    "after {} attempts the last mutation:\n{:?}\n\n\
                     did not match the expected mutation\n{:?}",
                    attempts, mutation, expected_text
                );
            }
            None => {
                panic!(
                    "never found any applicable mutations after {} attempts",
                    attempts
                );
            }
        }
    }
}
