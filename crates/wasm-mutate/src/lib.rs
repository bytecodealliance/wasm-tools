//! A WebAssembly test case mutator.
//!
//! `wasm-mutate` takes in an existing Wasm module and then applies a
//! pseudo-random transformation to it, producing a new, mutated Wasm
//! module. This new, mutated Wasm module can be fed as a test input to your
//! Wasm parser, validator, compiler, or any other Wasm-consuming
//! tool. `wasm-mutate` can serve as a custom mutator for mutation-based
//! fuzzing.

#![cfg_attr(not(feature = "clap"), deny(missing_docs))]

mod error;
mod info;
mod module;
mod mutators;

pub use error::*;

use crate::mutators::{
    codemotion::CodemotionMutator,
    custom::RemoveCustomSection,
    function_body_unreachable::FunctionBodyUnreachable,
    peephole::PeepholeMutator,
    remove_export::RemoveExportMutator,
    remove_item::{Item, RemoveItemMutator},
    rename_export::RenameExportMutator,
    snip_function::SnipMutator,
};
use info::ModuleInfo;
use mutators::Mutator;
use rand::{rngs::SmallRng, Rng, SeedableRng};
use std::{cell::Cell, sync::Arc};

#[cfg(feature = "clap")]
use clap::Parser;

macro_rules! define_mutators {
    (@count) => {0};
    (@count $first: expr , $( $tail: expr ,) *) => { 1 + define_mutators!(@count $($tail , )*) };
    (@expand $self:ident, $discriminator: ident, $start: expr, . , $( $rest: expr ,)*) => {};
    // The dot (.) is the mark to avoid infinite recursion , something like and
    // LR parser
    (@expand $self:ident, $discriminator: ident, $start: expr, $first: expr , $( $head: expr ,)*  . , $( $rest: expr ,)*) => {
        if $discriminator == $start {
            // Start by the current node
            let m = $first;

            if m.can_mutate($self) {
                match m.mutate($self) {
                    Ok(iter) => {
                        return Ok(Box::new(iter.into_iter().map(|r| r.map(|m| m.finish()))))
                    }
                    Err(e) => {
                        log::debug!("mutator {} failed: {}; will try again", m.name(), e);
                        return Err(e);
                    }
                }
            }
            // Follow the tail
            $(
                let m = $rest;

                if m.can_mutate($self) {
                    match m.mutate($self) {
                        Ok(iter) => {
                            return Ok(Box::new(iter.into_iter().map(|r| r.map(|m| m.finish()))))
                        }
                        Err(e) => {
                            log::debug!("mutator {} failed: {}; will try again", m.name(), e);
                            return Err(e);
                        }
                    }
                }
            )*
            // Follow the head
            $(
                let m = $head;

                if m.can_mutate($self) {
                    match m.mutate($self) {
                        Ok(iter) => {
                            return Ok(Box::new(iter.into_iter().map(|r| r.map(|m| m.finish()))))
                        }
                        Err(e) => {
                            log::debug!("mutator {} failed: {}; will try again", m.name(), e);
                            return Err(e);
                        }
                    }
                }
            )*
        };

        define_mutators!(@expand $self, $discriminator, ($start + 1), $($head ,)* ., $($rest ,)* $first, )
    };
    ( $self: ident , ($first: expr , $( $tail: expr ,)* ) ) => {
        {
            let count = define_mutators!(@count $first , $($tail ,)*);
            let discriminator:u32 = $self.rng().gen_range(0, count);
            define_mutators!(@expand $self, discriminator , 0 , $first , $($tail ,)*  . , );
        }
    };
}

// NB: only add this doc comment if we are not building the CLI, since otherwise
// it will override the main CLI's about text.
#[cfg_attr(
    not(feature = "clap"),
    doc = r###"
A WebAssembly test case mutator.

This is the main entry point into this crate. It provides various methods for
configuring what kinds of mutations might be applied to the input Wasm. Once
configured, you can apply a transformation to the input Wasm via the
[`run`][crate::WasmMutate::run] method.

# Example

```
# fn _foo() -> anyhow::Result<()> {
use wasm_mutate::WasmMutate;

let input_wasm = wat::parse_str(r#"
           (module
            (func (export "hello") (result i32)
             (i32.const 1234)
            )
           )
           "#)?;

// Create a `WasmMutate` builder and configure it.
let mut mutate = WasmMutate::default();
mutate
    // Set the RNG seed.
    .seed(42)
    // Allow mutations that change the semantics of the Wasm module.
    .preserve_semantics(false)
    // Use at most this much "fuel" when trying to mutate the Wasm module before
    // giving up.
    .fuel(1_000);

// Run the configured `WasmMutate` to get a sequence of mutations of the input
// Wasm!
for mutated_wasm in mutate.run(&input_wasm)? {
    let mutated_wasm = mutated_wasm?;
    // Feed `mutated_wasm` into your tests...
}
# Ok(())
# }
```
"###
)]
#[cfg_attr(feature = "clap", derive(Parser))]
#[derive(Clone)]
pub struct WasmMutate<'wasm> {
    /// The RNG seed used to choose which transformation to apply. Given the
    /// same input Wasm and same seed, `wasm-mutate` will always generate the
    /// same output Wasm.
    #[cfg_attr(feature = "clap", clap(short, long))]
    seed: u64,

    /// Only perform semantics-preserving transformations on the Wasm module.
    #[cfg_attr(feature = "clap", clap(long))]
    preserve_semantics: bool,

    /// Fuel to control the time of the mutation.
    #[cfg_attr(feature = "clap", clap(skip = Cell::new(u64::MAX)))]
    fuel: Cell<u64>,
    /// Only perform size-reducing transformations on the Wasm module. This
    /// allows `wasm-mutate` to be used as a test case reducer.
    #[cfg_attr(feature = "clap", clap(long))]
    reduce: bool,

    // Note: this is only exposed via the programmatic interface, not via the
    // CLI.
    #[cfg_attr(feature = "clap", clap(skip = None))]
    raw_mutate_func: Option<Arc<dyn Fn(&mut Vec<u8>) -> Result<()>>>,

    #[cfg_attr(feature = "clap", clap(skip = None))]
    rng: Option<SmallRng>,

    #[cfg_attr(feature = "clap", clap(skip = None))]
    info: Option<ModuleInfo<'wasm>>,
}

impl Default for WasmMutate<'_> {
    fn default() -> Self {
        let seed = 3;
        WasmMutate {
            seed,
            preserve_semantics: false,
            reduce: false,
            raw_mutate_func: None,
            fuel: Cell::new(u64::MAX),
            rng: None,
            info: None,
        }
    }
}

impl<'wasm> WasmMutate<'wasm> {
    /// Set the RNG seed used to choose which transformation to apply.
    ///
    /// Given the same input Wasm and same seed, `wasm-mutate` will always
    /// generate the same output Wasm.
    pub fn seed(&mut self, seed: u64) -> &mut Self {
        self.seed = seed;
        self
    }

    /// Configure whether we will only perform semantics-preserving
    /// transformations on the Wasm module.
    pub fn preserve_semantics(&mut self, preserve_semantics: bool) -> &mut Self {
        self.preserve_semantics = preserve_semantics;
        self
    }

    /// Configure the fuel used during the mutation
    pub fn fuel(&mut self, fuel: u64) -> &mut Self {
        self.fuel = Cell::new(fuel);
        self
    }

    /// Configure whether we will only perform size-reducing transformations on
    /// the Wasm module.
    ///
    /// Setting this to `true` allows `wasm-mutate` to be used as a test case
    /// reducer.
    pub fn reduce(&mut self, reduce: bool) -> &mut Self {
        self.reduce = reduce;
        self
    }

    /// Set a custom raw mutation function.
    ///
    /// This is used when we need some underlying raw bytes, for example when
    /// mutating the contents of a data segment.
    ///
    /// You can override this to use `libFuzzer`'s `LLVMFuzzerMutate` function
    /// to get raw bytes from `libFuzzer`, for example.
    pub fn raw_mutate_func(
        &mut self,
        raw_mutate_func: Option<Arc<dyn Fn(&mut Vec<u8>) -> Result<()>>>,
    ) -> &mut Self {
        self.raw_mutate_func = raw_mutate_func;
        self
    }

    pub(crate) fn consume_fuel(&self, qt: u64) -> Result<()> {
        if qt > self.fuel.get() {
            log::info!("Out of fuel");
            return Err(Error::out_of_fuel());
        }
        self.fuel.set(self.fuel.get() - qt);
        Ok(())
    }

    /// Run this configured `WasmMutate` on the given input Wasm.
    pub fn run<'a>(
        &'a mut self,
        input_wasm: &'wasm [u8],
    ) -> Result<Box<dyn Iterator<Item = Result<Vec<u8>>> + 'a>> {
        self.info = Some(ModuleInfo::new(input_wasm)?);
        self.rng = Some(SmallRng::seed_from_u64(self.seed));

        // This macro just expands the logic to return an iterator form the
        // mutators
        // It simulates a circular checking of the mutators starting by a random
        // one, returning the first one that can provides a mutation.
        // All possible start indexes are calculated at compilation time, if N
        // is the number of mutataros, N possible starting indexes are injected
        // and compiled to the final code
        define_mutators!(
            self,
            (
                PeepholeMutator::new(2),
                RemoveExportMutator,
                RenameExportMutator { max_name_size: 100 },
                SnipMutator,
                CodemotionMutator,
                FunctionBodyUnreachable,
                RemoveCustomSection,
                RemoveItemMutator(Item::Function),
                RemoveItemMutator(Item::Global),
                RemoveItemMutator(Item::Memory),
                RemoveItemMutator(Item::Table),
                RemoveItemMutator(Item::Type),
                RemoveItemMutator(Item::Data),
                RemoveItemMutator(Item::Element),
                RemoveItemMutator(Item::Tag),
            )
        );

        Err(Error::no_mutations_applicable())
    }

    pub(crate) fn rng(&mut self) -> &mut SmallRng {
        self.rng.as_mut().unwrap()
    }

    pub(crate) fn info(&self) -> &ModuleInfo<'wasm> {
        self.info.as_ref().unwrap()
    }
}

#[cfg(test)]
pub(crate) fn validate(validator: &mut wasmparser::Validator, bytes: &[u8]) {
    let err = match validator.validate_all(bytes) {
        Ok(()) => return,
        Err(e) => e,
    };
    drop(std::fs::write("test.wasm", &bytes));
    if let Ok(text) = wasmprinter::print_bytes(bytes) {
        drop(std::fs::write("test.wat", &text));
    }

    panic!("wasm failed to validate {:?}", err);
}
