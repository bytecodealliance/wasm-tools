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
    .attempts(999);

// Run the configured shrinking task.
let info = shrink.run(
    my_input_wasm,
    // Predicate.
    &mut |wasm| {
        let is_interesting: bool = todo!(
            "check for whether the given Wasm is interesting"
        );
        Ok(is_interesting)
    },
    // Callback called each time we find a new smallest interesting
    // Wasm.
    &mut |new_smallest| {
        // Optionally do something with the new smallest Wasm.
        Ok(())
    },
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
}

impl Default for WasmShrink {
    fn default() -> Self {
        WasmShrink {
            attempts: 1000,
            allow_empty: false,
            seed: 42,
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

    /// Run this configured Wasm shrinking task.
    ///
    /// The `predicate` function is called on each candidate Wasm to determine
    /// whether the Wasm is interesting or not. Returning `true` means that it
    /// is interesting, `false` means that it is not.
    ///
    /// The `on_new_smallest` function is called whenever we find a new smallest
    /// interesting Wasm.
    ///
    /// Returns information and metrics about the shrink task, such as the
    /// shrunken Wasm's output file path, the size of the input Wasm, the size
    /// of the output Wasm, etc.
    pub fn run<P, I, S>(
        self,
        input: Vec<u8>,
        mut predicate: P,
        mut on_new_smallest: S,
    ) -> Result<ShrinkInfo>
    where
        P: FnMut(&[u8]) -> Result<I>,
        I: IsInteresting,
        S: FnMut(&[u8]) -> Result<()>,
    {
        let input_size = input.len() as u64;

        // Prerequisites for the input Wasm.
        self.validate_wasm(&input)
            .context("The input is not valid Wasm.")?;

        // First double check that the input Wasm passes the predicate.
        //
        // It is fairly common to accidentally write a predicate script that
        // doesn't consider the input Wasm interesting. Better to surface this
        // user error as quick as possible than to make them wait until we've
        // exhausted all the ways we could shrink it further.
        let result = predicate(&input)?;
        anyhow::ensure!(
            result.is_interesting(),
            "The predicate does not consider the input Wasm interesting: {}",
            result
        );

        // The smallest Wasm that passes the predicate.
        let mut best = input;

        let mut new_best = |old_best: &mut Vec<u8>, new: Vec<u8>| -> Result<()> {
            debug_assert!(
                new.len() < old_best.len() || (new == EMPTY_WASM && *old_best == EMPTY_WASM)
            );
            log::info!("New smallest Wasm found: {} bytes", new.len());
            on_new_smallest(&new)?;
            *old_best = new;
            Ok(())
        };

        // Next try running the predicate on an empty Wasm module.
        //
        // It is fairly common to accidentally write a predicate script that
        // considers the empty module interesting, and we might as well check
        // for it eagerly, rather than make the user wait forever until we
        // finally to reduce the whole Wasm module to nothing.
        let result = predicate(EMPTY_WASM)?;
        if result.is_interesting() {
            if self.allow_empty {
                new_best(&mut best, EMPTY_WASM.to_vec())?;
                return Ok(ShrinkInfo {
                    input_size,
                    output_size: best.len() as u64,
                    output: best,
                });
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

        let mut rng = SmallRng::seed_from_u64(self.seed);

        // Mutated Wasm test cases that we've already tested and found to be
        // uninteresting.
        let mut uninteresting = HashSet::new();

        loop {
            let mut new_smallest = None;
            let mut attempt = 0;

            'attempts: while attempt < self.attempts {
                let mut mutate = WasmMutate::default();
                let seed = rng.gen();
                mutate.reduce(true).seed(seed);
                log::trace!("Attempt #{}: seed: {}", attempt, seed);

                let mutations = match mutate.run(&best) {
                    Ok(m) => m,
                    Err(e) => {
                        // This mutation failed, but another randomly chosen
                        // mutation might succeed, so keep attempting.
                        log::trace!("Attempt #{}: mutation failed ({:?})", attempt, e);
                        attempt += 1;
                        continue;
                    }
                };

                for mutated_wasm in mutations
                    // NB: The only mutator that takes advantage of returning an
                    // iterator with more than one item is the peephole mutator,
                    // which doesn't help shrinking too much. Therefore, we only
                    // take at most 10 elements.
                    .take(std::cmp::min(self.attempts - attempt, 10) as usize)
                {
                    let mutated_wasm = match mutated_wasm {
                        Ok(w) => w,
                        Err(e) => {
                            // This mutation failed, but another randomly chosen
                            // mutation might succeed, so keep attempting.
                            log::trace!("Attempt #{}: mutation failed ({:?})", attempt, e);
                            attempt += 1;
                            continue;
                        }
                    };

                    // TODO: This "shouldn't" happen when we set `.reduce(true)`
                    // on `WasmMutate`, but it does. This should turn into a
                    // `debug_assert!` once we fix `wasm-mutate`.
                    if mutated_wasm.len() >= best.len() {
                        log::trace!(
                            "Attempt #{}: mutated Wasm ({} bytes) is not smaller than \
                             best ({} bytes)",
                            attempt,
                            mutated_wasm.len(),
                            best.len(),
                        );
                        attempt += 1;
                        continue;
                    }

                    let hash = blake3::hash(&mutated_wasm);
                    if uninteresting.contains(&hash) {
                        log::trace!(
                            "Attempt #{}: already tested this candidate and found it uninteresting",
                            attempt
                        );
                        attempt += 1;
                        continue;
                    }

                    log::trace!(
                        "Attempt #{}: testing candidate ({} bytes)",
                        attempt,
                        mutated_wasm.len()
                    );
                    attempt += 1;

                    if predicate(&mutated_wasm)?.is_interesting() {
                        new_smallest = Some(mutated_wasm);
                        break 'attempts;
                    }

                    // Uninteresting. Don't try it again.
                    uninteresting.insert(hash);
                }
            }

            match new_smallest {
                // We've exhausted our attempt budget for shrinking this Wasm
                // any further.
                None => break,

                // We found a new smallest Wasm module, try reducing it further.
                Some(w) => new_best(&mut best, w)?,
            }
        }

        Ok(ShrinkInfo {
            input_size,
            output_size: best.len() as u64,
            output: best,
        })
    }

    fn validate_wasm(&self, wasm: &[u8]) -> Result<()> {
        let mut validator = wasmparser::Validator::new();
        validator.wasm_features(wasmparser::WasmFeatures {
            // TODO: we should have CLI flags for each Wasm proposal.
            reference_types: true,
            multi_value: true,
            bulk_memory: true,
            module_linking: false,
            simd: true,
            threads: true,
            tail_call: true,
            multi_memory: true,
            exceptions: true,
            memory64: true,
            relaxed_simd: true,
            extended_const: true,

            // We'll never enable this here.
            deterministic_only: false,
        });

        validator.validate_all(wasm)?;
        Ok(())
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
