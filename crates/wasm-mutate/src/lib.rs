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

use examples::no_mutator::NoMutator;
use mutators::{Mutable, Mutator};
#[cfg(feature = "structopt")]
use structopt::StructOpt;

mod error;

mod examples;
pub mod mutators;

pub use error::{Error, Result};
use wasmparser::{Chunk, Parser, Payload};

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

#[derive(Clone, Copy)]
pub struct MutationContext {
    // TODO add here information needed for mutators, e.g. function types information

}



impl Default for WasmMutate {
    fn default() -> Self {
        WasmMutate {
            seed: 0,
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

    fn get_mutator(&self) -> NoMutator
    {
        // Init the mutator
        examples::no_mutator::NoMutator{}
    }

    /// Run this configured `WasmMutate` on the given input Wasm.
    pub fn run(&self, input_wasm: &[u8]) -> Result<Vec<u8>> {
        let _ = input_wasm;
        
        // no mutator as the default?

        let mut mutation_context: &mut MutationContext = &mut MutationContext{
        };

        self.mutate_wasm(input_wasm, &mut self.get_mutator(), mutation_context)
    }

    
    /// Parsing the Wasm module and iterate over it
    fn mutate_wasm<'a, T>(&self, input_wasm: &'a [u8], mutator: &'a mut T, mutation_context: &'a mut MutationContext) -> Result<Vec<u8>>
        where T: Mutator<Payload<'a>>
    {
        let mut parser = Parser::new(0);
        let mut result: Vec<u8> = Vec::new();
        let mut consumed = 0;
        
        let mut function_count = 0;


        let mut code_parsing_started= false;
        let mut first_half = &mut Vec::new();
        let mut second_half =  &mut Vec::new();
        let mut code_entries = &mut Vec::new();

        // From the parsing example
        loop {
            let (mut payload, chunksize) = match parser.parse(&input_wasm[consumed..], true).unwrap() {
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
            let byteschunk = &input_wasm[consumed..(consumed + chunksize)];
            match payload {
                Payload::CodeSectionStart { count, range, size } => {
                    code_parsing_started = true;
                },
                Payload::CodeSectionEntry(_) => {
                    function_count += 1;
                    payload.run_mutator(self, byteschunk, 
                        &mut code_entries, mutator, mutation_context);
                },
                _ => {
                    payload.run_mutator(self, byteschunk, 
                        if !code_parsing_started {
                            &mut first_half
                        } else {
                            &mut second_half
                        } /* first or second half*/, mutator, mutation_context);
                }
            }

            consumed += chunksize
        }
        // Write all to result buffer
        result.write(first_half);

        // TODO this could be replaced with wasm-encoder
        // Recreate the code section
        if function_count > 0 {

            println!("{:?}", &code_entries);
            let mut code_section = Vec::new();
            leb128::write::unsigned(&mut code_section, function_count as u64).expect(
                "Error writing code entries count");
            code_section.write(&code_entries).expect(
                "Error writing code entries"
            );
            result.write(&[10u8]).expect("Error writing code section tag");
            leb128::write::unsigned(&mut result, code_section.len() as u64).expect(
                "Code section size could not be written");    
            result.write(&code_section);
        }
        
        // Then write the second half
        result.write(second_half);
        
        Ok(result)
    }
}


#[cfg(test)]
mod tests{
    use std::io::Write;

    use wasmparser::{Chunk, Operator, Parser, Payload, Type};

    use crate::{MutationContext, WasmMutate, examples::{self}, mutators::Mutator};

    #[test]
    fn idempotent_header() {
        let original = b"\0asm\x01\0\0\0";
        let mutator = WasmMutate::default();
        let mut operator = examples::no_mutator::NoMutator{};
        let mut context = MutationContext{

        };
        let mutated = mutator.mutate_wasm(original, &mut operator, &mut context).unwrap();

        assert_eq!(original.to_vec(), mutated)
    }
    

    pub struct FunctionSnipMutator {
    }
    
    impl Mutator<Payload<'_>> for FunctionSnipMutator{
        fn mutate<'a>(&mut self, _:&'a crate::WasmMutate, chunk: &'a [u8], out_buffer:&'a mut dyn Write, payload: &mut Payload<'_>, mutation_context: &mut crate::MutationContext) -> () {
            match payload {
                
                Payload::CodeSectionEntry(reader) => {
                    let mut local_reader = reader.get_locals_reader().unwrap();
                    
                    let mut tmpbuff: Vec<u8> = Vec::new();
                    // number of locals
                    leb128::write::unsigned(&mut tmpbuff, local_reader.get_count() as u64);
                    // write the locals
                    (0..local_reader.get_count()).for_each(|j|{
                        let (i, localtpe) = local_reader.read().unwrap();
                        leb128::write::unsigned(&mut tmpbuff, i as u64);
                        match localtpe {
                            // Only one returning for now
                            Type::I32 => {tmpbuff.write(&[0x7fu8]);},
                            Type::I64 => {tmpbuff.write(&[0x7eu8]);},
                            Type::F32 => {tmpbuff.write(&[0x7du8]);},
                            Type::F64 => {tmpbuff.write(&[0x7cu8]);},
                            _ => panic!("Unrecognized paramt tytpe")
                        }
    
                    });
                    // Write the default return zero, i32.const 0

                    
                    tmpbuff.write(&[0x41u8]); // i32.const op code
                    tmpbuff.write(&[0]);

                    // Write the size of tmpbuff
                    let l = tmpbuff.len() as u64;
                    leb128::write::unsigned(out_buffer, l);
                    // Write the function payload
                    out_buffer.write(&tmpbuff);
                },
                _ => panic!("Only code entries are allowed"),
            }
        }
    }
    
    
    #[test]
    fn snip_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
            (module
                (func (export "exported_func")
                    i32.const 42
                )
            )
        "#;
        let original = &wat::parse_str(wat).unwrap();
        let mutator = WasmMutate::default();
        let mut context = MutationContext{

        };
        let mut operator = FunctionSnipMutator{};
        
        let mutated = mutator.mutate_wasm(original, &mut operator, &mut context).unwrap();
        
        println!("{:?}", mutated);

        let mut parser = Parser::new(0);
        let mut consumed = 0;

        loop {
            let (mut payload, chunksize) = match parser.parse(&mutated[consumed..], true).unwrap() {
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
            match payload {
                Payload::CodeSectionEntry(reader) => {
                   let ops_reader = reader.get_operators_reader()
                   .unwrap();
                   
                   // Check now that it is the default value, 0
                   for i in ops_reader.into_iter() {
                       match i.unwrap() {
                           Operator::I32Const{value} => assert_eq!(value, 0),
                           _ => {

                                panic!("Only one default instruction should be")

                            }
                       }
                   }
                },
                _ => {
                   // pass
                }
            }

            consumed += chunksize
        }
        // Parse mutated and check that in fact the version is changed

    }
}