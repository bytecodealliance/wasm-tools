//! Shrink a Wasm file while maintaining a property of interest (such as
//! triggering a compiler bug).
//!
//! See the [`WasmShrink`][WasmShrink] type for details.

use std::collections::HashSet;

use anyhow::{Context, Result};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use wasm_mutate::WasmMutate;

#[rustfmt::skip]
static EMPTY_WASM: &'static [u8] = &[
    // Magic.
    0x00, b'a', b's', b'm',
    // Version.
    0x01, 0x00, 0x00, 0x00,
];

#[cfg_attr(
    not(feature = "clap"),
    doc = r###"
Shrink a Wasm file while maintaining a property of interest (such as
triggering a compiler bug).

# Example

```
use wasm_shrink::WasmShrink;

# fn foo() -> anyhow::Result<()> {
// Get the Wasm you want to shrink from somewhere.
let my_input_wasm: Vec<u8> = todo!();

// Configure the shrinking task.
let shrink = WasmShrink::default()
    // Give up on shrinking after 999 failed attempts to shrink a given
    // Wasm test case any further.
    .attempts(999)
    // Optionally do something with each new smallest Wasm as it is discovered.
    .on_new_smallest(Some(Box::new(|new_smallest| {
        // Do stuff...
        Ok(())
    })));

// Run the configured shrinking task.
let info = shrink.run(
    my_input_wasm,
    // Predicate.
    &mut |wasm| {
        let is_interesting: bool = todo!(
            "check for whether the given Wasm is interesting"
        );
        Ok(is_interesting)
    }
)?;

// Get the shrunken Wasm and other information about the completed shrink
// task from the returned `ShrinkInfo`.
let shrunken_wasm = info.output;
# Ok(()) }
```
"###
)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
pub struct WasmShrink {
    /// The number of shrink attempts to try before considering a Wasm module as
    /// small as it will ever get.
    #[cfg_attr(feature = "clap", clap(short, long, default_value = "1000"))]
    attempts: u32,

    /// Allow shrinking the input down to an empty Wasm module.
    ///
    /// This is usually not desired and typically reflects a bug in the
    /// predicate script.
    #[cfg_attr(feature = "clap", clap(long))]
    allow_empty: bool,

    /// The RNG seed for choosing which size-reducing mutation to attempt next.
    #[cfg_attr(feature = "clap", clap(short, long, default_value = "42"))]
    seed: u64,

    #[cfg_attr(feature = "clap", clap(skip))]
    on_new_smallest: Option<Box<dyn FnMut(&[u8]) -> Result<()>>>,
}

impl Default for WasmShrink {
    fn default() -> Self {
        WasmShrink {
            attempts: 1000,
            allow_empty: false,
            seed: 42,
            on_new_smallest: None,
        }
    }
}

impl WasmShrink {
    /// Set the number of shrink attempts to try before considering a Wasm
    /// module as small as it will ever get.
    pub fn attempts(mut self, attempts: u32) -> WasmShrink {
        self.attempts = attempts;
        self
    }

    /// Is it okay to shrink the input down to an empty Wasm module?
    ///
    /// This is usually not desired and typically reflects a bug in the
    /// predicate.
    pub fn allow_empty(mut self, allow_empty: bool) -> WasmShrink {
        self.allow_empty = allow_empty;
        self
    }

    /// Set the RNG seed for choosing which size-reducing mutation to attempt
    /// next.
    pub fn seed(mut self, seed: u64) -> WasmShrink {
        self.seed = seed;
        self
    }

    /// Set the callback that is called each time we discover a new smallest
    /// test case that is interesting.
    pub fn on_new_smallest(
        mut self,
        on_new_smallest: Option<Box<dyn FnMut(&[u8]) -> Result<()>>>,
    ) -> WasmShrink {
        self.on_new_smallest = on_new_smallest;
        self
    }

    /// Run this configured Wasm shrinking task.
    ///
    /// The `predicate` function is called on each candidate Wasm to determine
    /// whether the Wasm is interesting or not. Returning `true` means that it
    /// is interesting, `false` means that it is not.
    ///
    /// Returns the shrunken Wasm and information and metrics about the shrink
    /// task, such as the size of the input Wasm, the size of the output Wasm,
    /// etc.
    pub fn run<P, I>(self, input: Vec<u8>, predicate: P) -> Result<ShrinkInfo>
    where
        P: FnMut(&[u8]) -> Result<I>,
        I: IsInteresting,
    {
        ShrinkRun::new(self, input).run(predicate)
    }
}

struct ShrinkRun {
    shrink: WasmShrink,
    rng: SmallRng,

    // The size of the original input Wasm.
    input_size: u64,

    // The smallest Wasm that passes the predicate.
    best: Vec<u8>,

    // Mutated Wasm test cases that we've already tested and either found to be
    // uninteresting or ultimately not lead to test cases smaller than our
    // current best.
    already_tested: HashSet<blake3::Hash>,

    // The count of how many times we've attempted to shrink our current test
    // case smaller than `best`.
    attempt: u32,
}

impl ShrinkRun {
    pub fn new(shrink: WasmShrink, input: Vec<u8>) -> ShrinkRun {
        let rng = SmallRng::seed_from_u64(shrink.seed);
        let input_size = input.len() as u64;
        let best = input;
        ShrinkRun {
            shrink,
            rng,
            input_size,
            best,
            already_tested: HashSet::new(),
            attempt: 0,
        }
    }

    fn on_new_best(&mut self, new_best: Vec<u8>) -> Result<()> {
        debug_assert!(
            new_best.len() < self.best.len() || (new_best == EMPTY_WASM && self.best == EMPTY_WASM)
        );
        log::info!("New smallest Wasm found: {} bytes", new_best.len());
        if let Some(f) = self.shrink.on_new_smallest.as_mut() {
            f(&new_best)?;
        }
        self.best = new_best;
        self.attempt = 0;
        Ok(())
    }

    fn should_accept(&mut self, current: &[u8], new_interesting: &[u8]) -> bool {
        new_interesting.len() < current.len() ||
            // With low probability, accept larger interesting Wasm test cases
            // to avoid getting stuck in local minima.
            self.rng.gen_ratio(1, 100)
    }

    fn on_new_interesting(
        &mut self,
        current: &mut Vec<u8>,
        new_interesting: Vec<u8>,
    ) -> Result<()> {
        debug_assert!(self.best.len() <= current.len());
        *current = new_interesting;
        if current.len() < self.best.len() {
            self.on_new_best(current.clone())?;
        }
        Ok(())
    }

    fn validate_wasm(&self, wasm: &[u8]) -> Result<()> {
        let mut validator = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
            // TODO: we should have CLI flags for each Wasm proposal.
            reference_types: true,
            multi_value: true,
            bulk_memory: true,
            simd: true,
            threads: true,
            tail_call: true,
            multi_memory: true,
            exceptions: true,
            memory64: true,
            relaxed_simd: true,
            extended_const: true,
            mutable_global: true,
            saturating_float_to_int: true,
            sign_extension: true,
            component_model: false,
            function_references: false,
            gc: false,
            component_model_values: false,

            floats: true,
            memory_control: true,
        });

        validator.validate_all(wasm)?;
        Ok(())
    }

    fn finish(self) -> ShrinkInfo {
        ShrinkInfo {
            input_size: self.input_size,
            output_size: self.best.len() as u64,
            output: self.best,
        }
    }

    pub fn run<P, I>(mut self, mut predicate: P) -> Result<ShrinkInfo>
    where
        P: FnMut(&[u8]) -> Result<I>,
        I: IsInteresting,
    {
        // The Wasm that we are currently mutating.
        //
        // This can differ from `best` in that, with a very small probability,
        // we will sometimes accept mutations that don't shrink Wasm size. This
        // behavior is borrowed from MCMC[0] and helps us avoid getting stuck in
        // local minima. For example, we might replace a `ref.func $f` with a
        // `ref.null`, which doesn't actually shrink code size itself, but which
        // might make `$f` dead code such that we can remove `$f` altogether in
        // a follow up mutation.
        //
        // [0]: https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo
        let mut current = self.best.clone();

        // Check prerequisites for the input Wasm.
        self.validate_wasm(&current)
            .context("The input is not valid Wasm.")?;

        // First double check that the input Wasm passes the predicate.
        //
        // It is fairly common to accidentally write a predicate script that
        // doesn't consider the input Wasm interesting. Better to surface this
        // user error as quick as possible than to make them wait until we've
        // exhausted all the ways we could shrink it further.
        let result = predicate(&current)?;
        anyhow::ensure!(
            result.is_interesting(),
            "The predicate does not consider the input Wasm interesting: {}",
            result
        );

        // Next try running the predicate on an empty Wasm module.
        //
        // It is fairly common to accidentally write a predicate script that
        // considers the empty module interesting, and we might as well check
        // for it eagerly, rather than make the user wait forever until we
        // finally to reduce the whole Wasm module to nothing.
        let result = predicate(EMPTY_WASM)?;
        if result.is_interesting() {
            if self.shrink.allow_empty {
                self.on_new_best(EMPTY_WASM.to_vec())?;
                return Ok(self.finish());
            } else {
                anyhow::bail!(
                    "The predicate considers the empty Wasm module \
                     interesting, which is usually not desired and \
                     is a symptom of a bug in the predicate:\n\
                     \n\
                     {}",
                    result
                );
            }
        }

        // Now we perform the main search. Keep trying to find smaller and
        // interesting variants of the current smallest interesting Wasm file
        // until we run out of attempts and get stuck.
        while self.attempt < self.shrink.attempts {
            self.attempt += 1;

            let mut mutate = WasmMutate::default();
            let seed = self.rng.gen();
            mutate.reduce(true).seed(seed);
            log::trace!("Attempt #{}: seed: {}", self.attempt, seed);

            let mutations = match mutate.run(&current) {
                Ok(m) => m,
                Err(e) => {
                    // This mutation failed, but another randomly chosen
                    // mutation might succeed, so keep attempting.
                    log::trace!("Attempt #{}: mutation failed ({:?})", self.attempt, e);
                    continue;
                }
            };

            let mut mutations = mutations
                // NB: The only mutator that takes advantage of returning an
                // iterator with more than one item is the peephole mutator, which
                // doesn't help shrinking too much. Therefore, we only take at most
                // 10 elements.
                .take(std::cmp::min(self.shrink.attempts - self.attempt, 10) as usize)
                .peekable();

            if mutations.peek().is_none() {
                log::trace!(
                    "Attempt #{}: `wasm-mutate` failed to generate any mutations",
                    self.attempt
                );
                continue;
            }

            let mut new_current_wasm = None;
            for (i, mutated_wasm) in mutations.enumerate() {
                if i > 0 {
                    self.attempt += 1;
                }

                let mutated_wasm = match mutated_wasm {
                    Ok(w) => w,
                    Err(e) => {
                        // This mutation failed, but another randomly chosen
                        // mutation might succeed, so keep attempting.
                        log::trace!("Attempt #{}: mutation failed ({:?})", self.attempt, e);
                        continue;
                    }
                };

                let hash = blake3::hash(&mutated_wasm);
                if !self.already_tested.insert(hash) {
                    log::trace!("Attempt #{}: already tested this candidate", self.attempt,);
                    continue;
                }

                log::trace!(
                    "Attempt #{}: testing candidate ({} bytes)",
                    self.attempt,
                    mutated_wasm.len()
                );

                let result = predicate(&mutated_wasm)?;
                if result.is_interesting() {
                    log::trace!("Attempt #{}: candidate is interesting", self.attempt);
                    if self.should_accept(&current, &mutated_wasm) {
                        log::trace!("Attempt #{}: accepting candidate", self.attempt);
                        new_current_wasm = Some(mutated_wasm);
                        break;
                    }
                } else {
                    log::trace!("Attempt #{}: candidate is not interesting", self.attempt);
                }
            }

            if let Some(new_current_wasm) = new_current_wasm {
                self.on_new_interesting(&mut current, new_current_wasm)?;
            }
        }

        Ok(self.finish())
    }
}

/// A type that describes whether a Wasm is interesting or not.
pub trait IsInteresting: std::fmt::Display {
    /// Was the Wasm interesting?
    fn is_interesting(&self) -> bool;
}

impl IsInteresting for bool {
    fn is_interesting(&self) -> bool {
        *self
    }
}

/// Information about a completed shrinking run.
pub struct ShrinkInfo {
    /// The original size of the input Wasm.
    pub input_size: u64,

    /// The final size of the output, shrunken Wasm.
    pub output_size: u64,

    /// The final, shrunken Wasm.
    pub output: Vec<u8>,
}
