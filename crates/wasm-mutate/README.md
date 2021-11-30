<div align="center">
  <h1><code>wasm-mutate</code></h1>

<strong>A <a href="https://bytecodealliance.org/">Bytecode Alliance</a> project</strong>

  <p>
    <strong>wasm-mutate is a new tool for fuzzing Wasm compilers, runtimes, validators, and other Wasm-consuming programs.</strong>
  </p>

  <!-- TODO add proper links --> 
  <p>
    <a href="https://crates.io/crates/wasm-mutate"><img src="https://img.shields.io/crates/v/wasm-mutate.svg?style=flat-square" alt="Crates.io version" /></a>
    <a href="https://crates.io/crates/wasm-mutate"><img src="https://img.shields.io/crates/d/wasm-mutate.svg?style=flat-square" alt="Download" /></a>
    <a href="https://docs.rs/wasm-mutate/"><img src="https://img.shields.io/static/v1?label=docs&message=wasm-mutate&color=blue&style=flat-square" alt="docs.rs docs" /></a>
  </p>
</div>

<!-- .  -->


## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
wasm-mutate = "0.1.0"
```

You can mutate a WebAssembly binary by using the cli tool:

```bash
./wasm-mutate original.wasm --seed 0 -o out.wasm --preserve-semantics
```

## Features

* **semantically equivalent transformations:** `wasm-mutate` has the ability to
  only apply semantics-preserving changes to the input Wasm module. When it is
  used in this mode, the mutated Wasm computes identical results when
  given the same inputs as the original Wasm module.
* **determinism:** `wasm-mutate` is deterministic, i.e., given the same input
  Wasm module and the same seed, it always produces the same mutated
  output Wasm module.
* **libfuzzer integration**: `wasm-mutate` integrates well with mutation-based fuzzers like libFuzzer. It
  reuses the fuzzer's raw input strings. `wasm-mutate` works with the
  `LLVMFuzzerCustomMutator` hook and the
  `libfuzzer_sys::fuzz_mutator!` macro.
  
  ### Example
  
  ```rust
  #![no_main]

  use libfuzzer_sys::{fuzz_mutator, fuzz_target};
  use std::io::{BufRead, Read, Write};
  use wasmparser::WasmFeatures;

  fuzz_target!(|bytes: &[u8]| {
      // Initialize the Wasm for example
  });

  fuzz_mutator!(|data: &mut [u8], size: usize, max_size: usize, seed: u32| {
      // Generate a random Wasm module with `wasm-smith` as well as a RNG seed for
      let wasm = &data[..size];
      let features = WasmFeatures::default();
      let mut validator = wasmparser::Validator::new();
      validator.wasm_features(features);
      let validation_result = validator.validate_all(&wasm);

      // Mutate the data if its a valid Wasm file, otherwise, create a random one
      let wasm = if validation_result.is_ok() {
          wasm.to_vec()
      } else {
          let (w, _) = match wasm_tools_fuzz::generate_valid_module_from_seed(seed, |config, u| {
              config.module_linking_enabled = false;
              config.exceptions_enabled = false;
              config.simd_enabled = false;
              config.reference_types_enabled = false;
              config.memory64_enabled = false;
              config.max_memories = 1;
              Ok(())
          }) {
              Ok(m) => m,
              Err(_) => {
                  return size;
              }
          };
          w
      };

      let mutated_wasm = wasm_mutate::WasmMutate::default()
          .seed(seed.into())
          .fuel(1000)
          .preserve_semantics(true)
          .run(&wasm);

      let mutated_wasm = match mutated_wasm {
          Ok(w) => w,
          Err(_) => wasm,
      };

      // The mutated Wasm should still be valid, since the input Wasm was valid.
      let newsize = mutated_wasm.len();
      data[..newsize].copy_from_slice(&mutated_wasm[..newsize]);
      newsize
  });


  ```

* **test case reduction (WIP):** `wasm-mutate` can have the ability to restrict
  mutations to only those that shrink the size of the Wasm module. If it is used
  in this mode, `wasm-mutate` essentially becomes a Wasm test-case reducer. We
  are currently working to provide a prototype of this feature as a separate
  binary. The following pseudo-Rust provides the general picture of it as an
  standard hill-climbing algorithm.

  ```rust
    let wasmmutate = wasm_mutate::WasmMutate::default()
            .seed(seed)
            .fuel(1000)
            .preserve_semantics(true)
            .reduce(true);

    while MAX_ITERATIONS > 0 {
        let new_wasm = wasmmutate.run(&wasm); 
        wasm = if check_equivalence(new_wasm, wasm) {
          wasm
        } else{
          panic!("No valid transformations")
        }
        MAX_ITERATIONS -= 1;
    }

    return wasm

  ```

# License

This project is licensed under the Apache 2.0 license with the LLVM exception.
See [LICENSE](LICENSE) for more details.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be licensed as above, without any additional terms or conditions.

### Special contribution

* Javier Cabrera Arteaga (Phd. student at KTH)

