//! A WebAssembly test case mutator.
//!
//! `wasm-mutate` takes in an existing Wasm module and then applies a
//! pseudo-random transformation to it, producing a new, mutated Wasm
//! module. This new, mutated Wasm module can be fed as a test input to your
//! Wasm parser, validator, compiler, or any other Wasm-consuming
//! tool. `wasm-mutate` can serve as a custom mutator for mutation-based
//! fuzzing.

#![cfg_attr(not(feature = "structopt"), deny(missing_docs))]

mod error;
pub(crate) mod info;
pub(crate) mod module;
pub mod mutators;

use crate::mutators::{
    function_body_unreachable::FunctionBodyUnreachable, peephole::PeepholeMutator,
    remove_export::RemoveExportMutator, rename_export::RenameExportMutator,
    snip_function::SnipMutator,
};
use info::ModuleInfo;
use mutators::Mutator;
use rand::{rngs::SmallRng, Rng, SeedableRng};
use std::sync::Arc;

#[cfg(feature = "structopt")]
use structopt::StructOpt;

pub use error::{Error, Result};

// NB: only add this doc comment if we are not building the CLI, since otherwise
// it will override the main CLI's about text.
#[cfg_attr(
    not(feature = "structopt"),
    doc = r###"
A WebAssembly test case mutator.

This is the main entry point into this crate. It provides various methods
for configuring what kinds of mutations might be applied to the input
Wasm. Once configured, you can apply a transformation to the input Wasm via
the [`run`][wasm_mutate::WasmMutate::run] method.

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

// Create a `WasmMutate` builder, configure its seed, and then run it on the
// input Wasm!
let mutated_wasm = WasmMutate::default()
    .seed(42)
    .run(&input_wasm)?;

// Feed `mutated_wasm` into your tests...
# Ok(())
# }
```
"###
)]
#[derive(Clone)]
#[cfg_attr(feature = "structopt", derive(StructOpt))]
pub struct WasmMutate {
    /// The RNG seed used to choose which transformation to apply. Given the
    /// same input Wasm and same seed, `wasm-mutate` will always generate the
    /// same output Wasm.
    #[cfg_attr(feature = "structopt", structopt(short, long))]
    seed: u64,

    /// Only perform semantics-preserving transformations on the Wasm module.
    #[cfg_attr(feature = "structopt", structopt(long))]
    preserve_semantics: bool,

    /// Fuel to control the time of the mutation.
    #[cfg_attr(feature = "structopt", structopt(long))]
    fuel: u64,

    /// Only perform size-reducing transformations on the Wasm module. This
    /// allows `wasm-mutate` to be used as a test case reducer.
    #[cfg_attr(feature = "structopt", structopt(long))]
    reduce: bool,

    // Note: this is only exposed via the programmatic interface, not via the
    // CLI.
    #[cfg_attr(feature = "structopt", structopt(skip = None))]
    raw_mutate_func: Option<Arc<dyn Fn(&mut Vec<u8>) -> Result<()>>>,
}

impl Default for WasmMutate {
    fn default() -> Self {
        let seed = 3;
        WasmMutate {
            seed,
            preserve_semantics: false,
            reduce: false,
            raw_mutate_func: None,
            fuel: u64::MAX
        }
    }
}

#[derive(Default)]
/// Manages resources for the mutation and handles how the resources are consumed
pub struct Resources {
    fuel: u64
}

impl Resources {
    pub(crate) fn consume(&mut self, qt: u64) -> crate::Result<()> {
        if qt > self.fuel{
            self.fuel = 0;
            log::debug!("Resource limits reached!");
            return Err(crate::Error::NoMutationsApplicable) // Replace by a TimeoutError type
        }
        self.fuel -= qt;
        Ok(())
        
    }

    /// Creates a new allocation of resources for the mutation
    /// 
    pub fn new(fuel: u64) -> Self {
        Resources{
            fuel
        }
    }
}

impl WasmMutate {
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
        self.fuel = fuel;
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

    pub(crate) fn consume_fuel(&mut self, qt: u64) -> bool {
        if qt > self.fuel{
            self.fuel = 0;
            return false
        } else {
            self.fuel -= qt;
            return true;
        }
    }

    /// Run this configured `WasmMutate` on the given input Wasm.
    pub fn run<'a>(&self, input_wasm: &'a [u8]) -> Result<Vec<u8>> {
        let mut rng = SmallRng::seed_from_u64(self.seed);
        let info = ModuleInfo::new(input_wasm)?;

        let mutators: Vec<Box<dyn Mutator>> = vec![
            Box::new(RenameExportMutator { max_name_size: 100 }),
            Box::new(RemoveExportMutator),
            Box::new(SnipMutator),
            Box::new(FunctionBodyUnreachable),
            Box::new(PeepholeMutator),
        ];
        let mut mutators: Vec<_> = mutators
            .into_iter()
            .filter(|m| m.can_mutate(self, &info))
            .collect();

        while !mutators.is_empty() {
            let i = rng.gen_range(0, mutators.len());
            let mutator = mutators.swap_remove(i);
            let mut resources = Resources::new(self.fuel);
            if let Ok(module) = mutator.mutate(&self, &mut rng, &info, &mut resources) {
                return Ok(module.finish());
            }
        }

        Err(Error::NoMutationsApplicable)
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
