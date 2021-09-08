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

use mutators::{Mutable, Mutator, ReturnI32SnipMutator, SetFunction2Unreachable};
#[cfg(feature = "structopt")]
use structopt::StructOpt;

mod error;

pub mod mutators;

pub use error::{Error, Result};
use wasmparser::{Chunk, Parser, Payload};


macro_rules! mutators {
    (
        $(
            ($tpe: pat$(, $mutation:expr)*),
        )*
    ) => {
        // TODO validate
        // Only one payload type in the list
        fn mutate<'a>(p:&'a mut Payload<'a>, seed: u64, context: &'a WasmMutate, chunk:&'a [u8], out_buffer: &'a mut dyn Write) -> ()
        {
            match  p {
                $(
                    $tpe => {
                        let options = [
                            $(  
                                move | a,b, c,d  | $mutation.mutate(a, b, c, d), // The instantiation of the mutator can be expensive, instantiate after select
                            )*
                        ];
                        let mutation = options.get(seed as usize % options.len()).unwrap();
                        mutation(context, chunk, out_buffer, p);
                    } ,
                )
                *
                _ => {
                    // default behavior, bypass payload
                    out_buffer.write(&chunk).expect("Chunk cannot be written !");
                }
            }
        }
    };
}
// Declare available mutators here
mutators! {
    (Payload::CodeSectionEntry(_), 
        ReturnI32SnipMutator{},
        SetFunction2Unreachable{}
    ),
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


    /// Run this configured `WasmMutate` on the given input Wasm.
    pub fn run(&self, input_wasm: &[u8]) -> Result<Vec<u8>> {
        let _ = input_wasm;
        
        // no mutator as the default?

        let mut parser = Parser::new(0);
        let mut result: Vec<u8> = Vec::new();
        let mut consumed = 0;
        
        let mut function_count = 0;


        let mut code_parsing_started= false;
        let mut first_half = &mut Vec::new();
        let mut second_half =  &mut Vec::new();
        let mut code_entries = &mut Vec::new();

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

            // This code is a patch, the code section need to be updated if some changes are made to code entries
            // If the code section start, the previous buffer is saved for later writting
            // This pattern match returns the Write in which the data will be written
            match payload {
                Payload::CodeSectionStart { count, range, size } => {
                    code_parsing_started = true;
                    // The code section will be parsed after

                },
                Payload::CodeSectionEntry(_) => {
                    // In theory the code section entries come inmediatly after the code section start, if another payload is parsed after, all code sections have been parsed
                    function_count += 1;
                    mutate(
                        &mut payload, self.seed, &self, byteschunk, code_entries);
        
                },
                _ => {
                    mutate(
                        &mut payload, self.seed, &self, byteschunk,     
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
        result.write(first_half);

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
        
        // Then write the second half, payloads processed after code section is finised
        result.write(second_half);
        
        Ok(result)
    }

}


#[cfg(test)]
mod tests{

    use wasm_encoder::Instruction;
    use wasmparser::{Chunk, Operator, Parser, Payload};

    use crate::{WasmMutate};

    /// Since there is no code section, any mutator registered under that pattern wont affect the module
    #[test]
    fn idempotent_header() {
        let original = b"\0asm\x01\0\0\0";
        let mutator = WasmMutate::default();
        
        let mutated = mutator.run(original).unwrap();

        assert_eq!(original.to_vec(), mutated)
    }


    /// Since there is no code section, any mutator registered under that pattern wont affect the module
    #[test]
    fn test_snip_mutator() {
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
            // seed is zero, which means first mutator
            
            let mutated = mutator.run(original).unwrap();
            
            println!("{:?}", mutated);
                
            // Down here is the validation for the correct mutation

            let mut parser = Parser::new(0);
            let mut consumed = 0;
            
            loop {
                let (payload, chunksize) = match parser.parse(&mutated[consumed..], true).unwrap() {
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

    #[test]
    fn test_unreachable_mutator() {
                // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
                let wat = r#"
                (module
                    (func (export "exported_func")
                        i32.const 42
                    )
                )
            "#;
            let original = &wat::parse_str(wat).unwrap();
            let mut mutator = WasmMutate::default();
            mutator.seed = 1;
            // seed is zero, which means first mutator
            
            let mutated = mutator.run(original).unwrap();
            
            println!("{:?}", mutated);
                
            // Down here is the validation for the correct mutation

            let mut parser = Parser::new(0);
            let mut consumed = 0;
            
            loop {
                let (payload, chunksize) = match parser.parse(&mutated[consumed..], true).unwrap() {
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
                               Operator::Unreachable => assert_eq!(0, 0),
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