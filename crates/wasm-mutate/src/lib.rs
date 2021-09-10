//! A WebAssembly test case mutator.
//!
//! `wasm-mutate` takes in an existing Wasm module and then applies a
//! pseudo-random transformation to it, producing a new, mutated Wasm
//! module. This new, mutated Wasm module can be fed as a test input to your
//! Wasm parser, validator, compiler, or any other Wasm-consuming
//! tool. `wasm-mutate` can serve as a custom mutator for mutation-based
//! fuzzing.
#![cfg_attr(not(feature = "structopt"), deny(missing_docs))]

use std::{io::Write, sync::Arc};

use mutators::{Mutator, ReturnI32SnipMutator, SetFunction2Unreachable, RemoveExportMutator, NoMutator, RenameExportMutator};
use rand::{rngs::StdRng, Rng, SeedableRng};
#[cfg(feature = "structopt")]
use structopt::StructOpt;
use std::convert::TryFrom;

mod error;

pub mod mutators;

pub use error::{Error, Result};
use wasm_encoder::{CodeSection, Module, RawSection, Section, SectionId, encoders};
use wasmparser::{Chunk, Parser, Payload};


macro_rules! mutators {
    (
        $(
            ($tpe: pat$(, $mutation:expr)*),
        )*
    ) => {
        fn mutate<'a, A>(p:&'a mut Payload<'a>, context: &'a WasmMutate, chunk:&Vec<u8>, sink: &'a mut A) -> Result<()>
            where A: Extend<u8>
        {
            match  p {
                $(
                    $tpe => {
                        let options = [
                            #[cfg(feature="no-mutator")]
                                move | config, chunk, out, payload  | (Some(String::from("NoMutator")), NoMutator{}.mutate(config, chunk, out, payload)),
                            $(  
                                move | config, chunk, out, payload  | (Some($mutation.name()), $mutation.mutate(config, chunk, out, payload)), 
                            )*
                        ];// Instantiate after select

                        let mut rnd = context.get_rnd();
                        
                        let mutation = options.get(rnd.gen_range(0, options.len())).unwrap();

                        let (name, _ ) = mutation(context, chunk.clone(), sink, p);
                        
                        #[cfg(debug_assertions)] {
                            eprintln!("Selected mutator {} on {:?}", name.unwrap(), p);
                        }
                        return Ok(());
                    } ,
                )
                *
                _ => {
                    // default behavior, bypass payload
                    sink.extend(chunk.clone());
                    Ok(())
                }
            }
        }
    };
}

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
        let seed = 1;
        WasmMutate {
            seed: seed,
            preserve_semantics: false,
            reduce: false,
            raw_mutate_func: None
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

    /// This type of construction allows to map payloads with specific mutators
    mutators! {
        (Payload::CodeSectionEntry(_) 
            ,ReturnI32SnipMutator{}
            ,SetFunction2Unreachable{}
        ),
        (
            Payload::ExportSection(_)
               ,RemoveExportMutator{}
               ,RenameExportMutator{max_name_size: 100}
        ),
    }

    /// Returns Random generator from seed
    pub fn get_rnd(&self) -> StdRng {
        StdRng::seed_from_u64(self.seed as u64)
    }

    /// Run this configured `WasmMutate` on the given input Wasm.
    pub fn run(&self, input_wasm: &[u8]) -> Result<Vec<u8>> {
        // Declare available mutators here

        let _ = input_wasm;
        
        // no mutator as the default?

        let mut parser = Parser::new(0);
        let mut result: Vec<u8> = Vec::new();
        let mut consumed = 0;
        
        let mut function_count = 0;


        let mut code_parsing_started= false;
        let mut first_half= Vec::new();
        let mut second_half = Vec::new();
        let mut code: Vec<u8> = Vec::new(); 

        loop {
            let (mut payload, chunksize) = match parser.parse(&input_wasm[consumed..], true)? {
                Chunk::NeedMoreData(__) => {
                    // In theory the passed buffer is a complete Wasm module, it should not be need for more data
                    continue;
                },
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };
            
            if let Payload::End = payload {
                // Break the loop and return
                break;
            }

            // Pass the payload and bytes chunk to the real mutator
            let byteschunk = &input_wasm[consumed..(consumed + chunksize)].to_vec();

            // This code is a patch, the code section need to be updated if some changes are made to code entries
            // If the code section start, the previous buffer is saved for later writting
            // This pattern match returns the Write in which the data will be written
            match payload {
                Payload::CodeSectionStart { count: _, range: _, size:_ } => {
                    code_parsing_started = true;
                    // The code section will be encoded after

                },
                Payload::CodeSectionEntry(_) => {
                    // In theory the code section entries come inmediatly after the code section start, if another payload is parsed after, all code sections have been parsed
                    function_count += 1;
                    let mut func = Vec::new();

                    WasmMutate::mutate(
                        &mut payload, self, byteschunk, &mut func);

                    code.extend(&func);
                },
                _ => {
                    WasmMutate::mutate(
                        &mut payload, &self, byteschunk,     
                        if !code_parsing_started {
                            &mut first_half
                        } else {
                            &mut second_half
                        });
                }
            }
            consumed += chunksize
        }

        // Write all to result buffer before the code section started
        result.extend(first_half);
        // Recreate the code section
        if code_parsing_started  {
            // Add function count and section size to raw data
            let mut sink = Vec::new();
            let num_added = encoders::u32(function_count);
            sink.extend(num_added);
            sink.extend(code.iter().copied());
            code.get(0);

            let raw_code = RawSection{
                id: SectionId::Code.into(),
                data: &sink
            };

            // To check, add section if in the encoding of the section ?
            result.extend(std::iter::once(SectionId::Code as u8));
            raw_code.encode(&mut result);
            //result.encode(&module.finish()).expect("Code section could not be written");
        }
        // Then write the second half, payloads processed after code entries finised
        result.extend(second_half);
        
        Ok(result)
    }

}
