//! A WebAssembly test case mutator.
//!
//! `wasm-mutate` takes in an existing Wasm module and then applies a
//! pseudo-random transformation to it, producing a new, mutated Wasm
//! module. This new, mutated Wasm module can be fed as a test input to your
//! Wasm parser, validator, compiler, or any other Wasm-consuming
//! tool. `wasm-mutate` can serve as a custom mutator for mutation-based
//! fuzzing.
#![cfg_attr(not(feature = "structopt"), deny(missing_docs))]

use std::{any::TypeId, io::Write, sync::Arc};

use mutators::{Mutator, ReturnI32SnipMutator, SetFunction2Unreachable, RemoveExportMutator, NoMutator, RenameExportMutator, Mutators};
use rand::{rngs::SmallRng, Rng, SeedableRng};
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
            ($tpe: pat$(, ($mutation:expr $(, $preserve_semantic: literal)?))*),
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
                                    move | config, chunk, out, payload  | (Some(String::from("NoMutator")), NoMutator.mutate(config, chunk, out, payload)),
                                $(  
                                    move | config, chunk, out, payload  | (Some($mutation.name()), $mutation.mutate(config, chunk, out, payload)), 
                                )*
                            ];// Instantiate after select

                            println!("{:?}", options);
                            panic!();

                            // filter by semantic

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


macro_rules! get_mutators {
    (
        $(
            ($(
                ($mutation:expr $(, $preserve_semantic: literal)?),
            )*),
        )*
    ) => {
            

            fn get_applicable_mutations<'a, A>(&self, info: &ModuleInfo) -> Result<Vec<Vec<Box<dyn Fn(&WasmMutate, Vec<u8>, &mut Vec<u8>, &mut Payload) -> usize>>>>
                where A: Extend<u8>
            {
                let mut mutations = Vec::new();
                //fn mutate<'a, A>(&mut self, _:&'a WasmMutate, chunk: Vec<u8>, sink: &'a mut A, _: &mut T)
                type C =  Vec<u8>;
                type R = Box<dyn Fn(&WasmMutate, Vec<u8>, &mut C, &mut Payload) -> usize>;

                $(
                    let mut dependant_mutations = Vec::new();
                    $(

                        if $mutation.can_mutate(self, info) {
                            
                            if cfg!(debug_assertions) {
                                println!("{} is applicable", $mutation.name());
                            }

                            dependant_mutations.push(Box::new(
                                ( move |a: &WasmMutate, b, c: &mut C, d: &mut Payload| (  $mutation.mutate(a, b, c , d)))
                            ) as R);
                        };
                    )*

                    mutations.push(dependant_mutations);
                )*

                Ok(mutations)
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

/// Provides module information for future usage during mutation
/// an instance of ModuleInfo could be user to determine which mutation could be applied
#[derive(Default)]
pub struct ModuleInfo {

    has_exports: bool,    
    has_types: bool,   
    has_imports: bool,   
    has_tables: bool,  

    has_mem: bool,
    has_global: bool,
    is_start_defined: bool,

    elem_section: bool,
    has_data: bool,
    has_code: bool
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

    // This type of construction allows to determine which mutation can be applied
    // The vertical dimension represents mutations that can be applied indistincly from each other
    // Horizontal dimension allows to define mutations in which only one can be applied 
    get_mutators! {
        ((ReturnI32SnipMutator, false), (SetFunction2Unreachable, false),), // For code
        ((RemoveExportMutator, true), (RenameExportMutator{max_name_size: 100}, true), ), // For exports
    }
    /// Returns Random generator from seed
    pub fn get_rnd(&self) -> SmallRng {
        SmallRng::seed_from_u64(self.seed as u64)
    }

    /// Parse and fill lane AST with metadata information about the module
    fn get_module_info(&self, input_wasm: &[u8]) -> Result<ModuleInfo>{

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        let mut info = ModuleInfo::default();

        loop {
            let (payload, chunksize) = match parser.parse(&input_wasm[consumed..], true)? {
                Chunk::NeedMoreData(_) => {
                    panic!("Invalid Wasm module");
                },
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };

            match payload {
                Payload::CodeSectionStart { count, range, size } => {
                    info.has_code = count >= 1;
                },
                Payload::CodeSectionEntry(_) => {
                    
                },
                Payload::TypeSection(reader ) => { info.has_types = reader.get_count() >= 1; },
                Payload::ImportSection(reader) => { info.has_tables = reader.get_count() >= 1; },
                Payload::FunctionSection(reader) => { 

                },
                Payload::TableSection(reader) => { info.has_tables = reader.get_count() >= 1;},
                Payload::MemorySection(reader) => { info.has_mem = reader.get_count() >= 1; },
                Payload::GlobalSection(reader) => {info.has_global = reader.get_count() >= 1;},
                Payload::ExportSection(reader) => { info.has_exports = reader.get_count() >= 1; },
                Payload::StartSection { func, range } => { info.is_start_defined = true; },
                Payload::ElementSection(reader) => { info.elem_section = true; },
                Payload::DataSection(reader) => { info.has_data = reader.get_count() >= 1; },
                Payload::End => {break;},
                _ => {

                }
            }
            consumed += chunksize
        };

        Ok(info)
    }

    /// Run this configured `WasmMutate` on the given input Wasm.
    pub fn run<'a>(&self, input_wasm: &[u8]) -> Result<Vec<u8>> {
        // Declare available mutators here

        let info = self.get_module_info(input_wasm)?;
        let mutators = self.get_applicable_mutations::<Vec<u8>>(&info)?;

        // Select random
        let mut rnd = self.get_rnd();
        let mut selected = Vec::new();

        mutators.iter().for_each(|mutations|{
            // In this dimension only one mutation can be selected
            if mutations.len() >= 1 { // at least one mutation
                let selected_mutation = rnd.gen_range(0, mutations.len());
                selected.push(mutations.get(selected_mutation));
            }
        });

        if selected.len() == 0 { // return idem if no mutators
            return Ok(input_wasm.to_vec())
        }

        

        let mut parser = Parser::new(0);
        let mut result: Vec<u8> = Vec::new();
        let mut consumed = 0;
        
        let mut function_count = 0;

        let mut code_parsing_started= false;
        let mut first_half= Vec::new();
        let mut second_half = Vec::new();
        let mut dev_null = Vec::new();
        let mut code: Vec<u8> = Vec::new(); 

        loop {
            let (mut payload, chunksize) = match parser.parse(&input_wasm[consumed..], true)? {
                Chunk::NeedMoreData(_) => {
                    // In theory the passed buffer is a complete Wasm module, it should not be need for more data, panic then
                    panic!("Invalid Wasm module");
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
            let mut buffer = match payload {
                Payload::CodeSectionStart { count: _, range: _, size:_ } => {
                    code_parsing_started = true;
                    &mut dev_null
                },
                Payload::CodeSectionEntry(_) => {
                    // In theory the code section entries come inmediatly after the code section start, if another payload is parsed after, all code sections have been parsed
                    function_count += 1;
                    &mut code
                },
                _ => {
                    if !code_parsing_started {
                        &mut first_half
                    } else {
                        &mut second_half
                    }
                }
            };
            

            // Apply the mutations
            // Each mutator should be applied to specific payloads
            // If the mutator is not implemented to mutate the specific payload, then the payload is bypassed (zero bytes written)
            let mut written = 0;

            println!("{:?}", payload);

            for mutator in selected.iter() {
                if let Some(mutator_func) = mutator {
                    
                    written = mutator_func(self, byteschunk.to_vec(), &mut buffer, &mut payload);
                    println!("{:?}", written );

                    if written >= 0 {
                        // This will prevent two mutators rewrite the same payload
                        break
                    }
                    // If something was written break
                }
            }

            if written == 0 {
                buffer.extend(byteschunk)
            }

            // if non mutator was able to mutate, then write the initial

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
        }
        // Then write the second half, payloads processed after code entries finised
        result.extend(second_half);
        
        Ok(result)
    }

}
