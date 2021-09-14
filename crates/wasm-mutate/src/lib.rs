//! A WebAssembly test case mutator.
//!
//! `wasm-mutate` takes in an existing Wasm module and then applies a
//! pseudo-random transformation to it, producing a new, mutated Wasm
//! module. This new, mutated Wasm module can be fed as a test input to your
//! Wasm parser, validator, compiler, or any other Wasm-consuming
//! tool. `wasm-mutate` can serve as a custom mutator for mutation-based
//! fuzzing.
#![cfg_attr(not(feature = "structopt"), deny(missing_docs))]

use std::{any::TypeId, collections::HashMap, io::Write, sync::Arc};

use mutators::{Mutator, ReturnI32SnipMutator};
use rand::{rngs::SmallRng, Rng, SeedableRng};
#[cfg(feature = "structopt")]
use structopt::StructOpt;
use std::convert::TryFrom;

mod error;

pub mod mutators;

pub use error::{Error, Result};
use wasm_encoder::{CodeSection, Module, RawSection, Section, SectionId, encoders};
use wasmparser::{Chunk, Parser, Payload, Range};

use crate::mutators::{SetFunction2Unreachable, RemoveExportMutator, RenameExportMutator};


macro_rules! get_mutators {
    (
        $(
            ($(
                ($mutation:expr),
            )*),
        )*
    ) => {
            

            fn get_applicable_mutations<'a, A>(&self, info: &ModuleInfo) -> Result<(Vec<Vec<Box<dyn Fn(&WasmMutate, &[u8], &ModuleInfo) -> Vec<u8>>>>, Vec<Vec<(Range, String)>>)>
                where A: Extend<u8>
            {
                let mut mutations = Vec::new();
                let mut ranges = Vec::new();
                type R = Box<dyn Fn(&WasmMutate, &[u8], &ModuleInfo) -> Vec<u8>>;

                $(
                    let mut dependant_mutations = Vec::new();
                    let mut applicable_on = Vec::new();
                    $(
                        let (can_mutate, on) = $mutation.can_mutate(self, &info);
                        if can_mutate {
                            dependant_mutations.push(Box::new(
                                ( move |a: &WasmMutate, b: &[u8], c: &ModuleInfo| (  $mutation.mutate(a, b, c )))
                            ) as R);
                            // `on` can be unwrapped since, otherwise the can_mutate is not doing what it supposes to
                            applicable_on.push((on.unwrap(), $mutation.name()));
                        };
                    )*

                    mutations.push(dependant_mutations);
                    ranges.push(applicable_on);
                )*

                Ok((mutations, ranges))
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
        let seed = 0;
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
/// We have the ranges where the sections are for sake of memory safe, instead of having raw sections
#[derive(Default)]
pub struct ModuleInfo {
    exports: Option<Range>,    
    types: Option<Range>,   
    imports: Option<Range>,   
    tables: Option<Range>, 
    memories: Option<Range>,
    globals: Option<Range>,
    is_start_defined: bool,
    elements: Option<Range>,
    functions: Option<Range>,
    data: Option<Range>,
    code: Option<Range>
}

impl ModuleInfo {
    fn has_code(&self) -> bool {
        self.code != None
    }

    fn has_exports(&self) -> bool {
        self.exports != None
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

    // This type of construction allows to determine which mutation can be applied
    // The vertical dimension represents mutations that can be applied indistincly from each other
    // Horizontal dimension allows to define mutations in which only one can be applied 
    get_mutators! {
        ((ReturnI32SnipMutator), (SetFunction2Unreachable), ), // For code
        // In the case of these two mutators, they can be applied independtly to the same section
        // For exports, they can be applied simultaneusly
        ((RenameExportMutator{max_name_size: 100}),), 
        ((RemoveExportMutator),), 
       
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
                    info.code = Some(Range {start:consumed, end: consumed+chunksize + size as usize});
                },
                Payload::TypeSection(_ ) => { info.types = Some(Range {start:consumed, end: consumed+chunksize}) },
                Payload::ImportSection(_) => { info.imports = Some(Range {start:consumed, end: consumed+chunksize}) },
                Payload::FunctionSection(_) => { info.functions =  Some(Range {start:consumed, end: consumed + chunksize})},
                Payload::TableSection(_) => { info.tables =  Some(Range {start:consumed, end: consumed + chunksize})},
                Payload::MemorySection(_) => { info.memories = Some(Range {start:consumed, end: consumed + chunksize} ) },
                Payload::GlobalSection(_) => { info.globals =  Some(Range {start:consumed, end: consumed + chunksize} )},
                Payload::ExportSection(_) => { info.exports =  Some(Range {start:consumed, end: consumed + chunksize} )},
                Payload::StartSection { func: _, range: _ } => { info.is_start_defined = true; },
                Payload::ElementSection(_) => { info.elements = Some(Range {start:consumed, end: consumed+chunksize} )},
                Payload::DataSection(_) => { info.data = Some(Range {start:consumed, end: consumed+chunksize})},
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

        let mut result: Vec<u8> = Vec::new();
        let info = self.get_module_info(input_wasm)?;
        let (mutators, ranges) = self.get_applicable_mutations::<Vec<u8>>(&info)?;

        if cfg!(debug_assertions){
            eprintln!("Applicable mutators:");

            ranges.iter().for_each(|v|{
                eprint!("(");
                v.iter().for_each(|(r, name)|{
                    eprint!("{} ^ ", name)
                });
                eprint!("\u{8}");
                eprint!("\u{8}");
                eprint!(") || ")
            });

            eprint!("\u{8}");
            eprint!("\u{8}");
            eprint!("\u{8}");
            eprintln!("");
        }
        // Select random
        let mut rnd = self.get_rnd();
        let mut selected = Vec::new();

        mutators.iter().zip(ranges.iter()).for_each(|(mutations, ranges)|{
            // In this dimension only one mutation can be selected
            if mutations.len() >= 1 { // at least one mutation
                // Simulate the last possible mutation is in fact a no-mutator
                let number_of_mutations = if cfg!(feature="no-mutator") {
                    mutations.len() + 1
                } else {
                    mutations.len()
                };

                let selected_mutation = rnd.gen_range(0, number_of_mutations);
                if selected_mutation < mutations.len() {
                    selected.push((mutations.get(selected_mutation).unwrap(), ranges.get(selected_mutation).unwrap()));
                }// skip otherwise 
                else {

                    if cfg!(debug_assertions){
                        eprintln!("Selecting to not mutate");
                    }
                }

                if cfg!(debug_assertions){
                    eprintln!("Selecting mutation {:?} ({:?})/{:?}", selected_mutation, mutations.len(), number_of_mutations)
                }
            }
        });

        if selected.len() == 0 { // return idem if no mutators
            return Ok(input_wasm.to_vec())
        }


        if cfg!(debug_assertions){
            selected.iter().for_each(|(_, (range, name))| {
                eprintln!("Applying {} on piece {:?}", name, range);
            })
        }

        // Group selected mutations by range
        // Grouping by range will allow to mutate over the same section after one mutation has already passed
        type R = Box<dyn Fn(&WasmMutate, &[u8], &ModuleInfo) -> Vec<u8>>;
        let mut ranges_to_mutate: HashMap<Range, Vec<&R>> = HashMap::new();
        
        selected.iter().for_each(|(muta, (range, name))|{
            ranges_to_mutate.entry(*range).or_insert_with(|| Vec::new()).push(muta);
        });

        // Get the keys and sort them
        let mut ranges = ranges_to_mutate.keys().collect::<Vec<&Range>>();

        ranges.sort_by_key(|&&range| {
            range.start
        });



        let mut offset = 0;
        // Mutators will be applied in specific ranges otherwise the chunk is copied to the resultant byte stream
        // [.....][mutator1()][.....][mutator2()][mutator3()][.....]
        //        [mutator6()]
        for range in ranges{
            // Write previous chunk of data, e.g. not mutated section
            result.extend(&input_wasm[offset..range.start]);
            
            // Mutate the section several times
            let mutation = ranges_to_mutate.get(&range).unwrap().iter().fold(input_wasm[range.start..range.end].to_vec(),|mutation, &muta|{
                muta(self, &mutation[..], &info)
            });
            result.extend(mutation);
            offset = range.end;
        }
        result.extend(&input_wasm[offset..]);
        Ok(result)
    }

}
