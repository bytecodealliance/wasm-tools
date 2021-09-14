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
                ($mutation:expr $(, $preserve_semantic: literal)?),
            )*),
        )*
    ) => {
            

            fn get_applicable_mutations<'a, A>(&self, info: &ModuleInfo) -> Result<(Vec<Vec<Box<dyn Fn(&WasmMutate, &[u8], &ModuleInfo) -> Vec<u8>>>>, Vec<Vec<Range>>)>
                where A: Extend<u8>
            {
                let mut mutations = Vec::new();
                let mut ranges = Vec::new();
                //fn mutate<'a, A>(&mut self, _:&'a WasmMutate, chunk: Vec<u8>, sink: &'a mut A, _: &mut T)
                type R = Box<dyn Fn(&WasmMutate, &[u8], &ModuleInfo) -> Vec<u8>>;

                $(
                    let mut dependant_mutations = Vec::new();
                    let mut applicable_on = Vec::new();
                    $(
                        let (can_mutate, on) = $mutation.can_mutate(self, &info);
                        if can_mutate {
                            
                            if cfg!(debug_assertions) {
                                println!("{} is applicable", $mutation.name());
                            }

                            dependant_mutations.push(Box::new(
                                ( move |a: &WasmMutate, b: &[u8], c: &ModuleInfo| (  $mutation.mutate(a, b, c )))
                            ) as R);
                            applicable_on.push(on);
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
        let seed = 10;
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
pub struct ModuleInfo {

    exports: Range,    
    types: Range,   
    imports: Range,   
    tables: Range,  

    memories: Range,
    globals: Range,
    
    is_start_defined: bool,

    elements: Range,
    functions: Range,
    data: Range,
    code: Range
}

impl Default for ModuleInfo {
    fn default() -> Self {
        ModuleInfo {
            exports: Range{start: 0, end: 0},
            types: Range{start: 0, end: 0},
            imports: Range{start: 0, end: 0},
            tables: Range{start: 0, end: 0},
            memories: Range{start: 0, end: 0},
            globals: Range{start: 0, end: 0},
            elements: Range{start: 0, end: 0},
            code: Range{start: 0, end: 0},
            data: Range{start: 0, end: 0},
            functions: Range{start: 0, end: 0},
            is_start_defined: false
        }
    }
}

impl ModuleInfo {
    fn has_code(&self) -> bool {
        self.code.end - self.code.start >= 1
    }

    fn has_exports(&self) -> bool {
        self.exports.end - self.exports.start >= 1
    }

    // TODO finish others
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
        ((ReturnI32SnipMutator, false), (SetFunction2Unreachable, false), ), // For code
        ((RenameExportMutator{max_name_size: 100}, false), (RemoveExportMutator, false), ), // For exports
       
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
                    info.code = Range {start:consumed, end: consumed+chunksize + size as usize};
                },
                Payload::CodeSectionEntry(_) => {
                    
                },
                Payload::TypeSection(reader ) => { info.types = Range {start:consumed, end: consumed+chunksize} },
                Payload::ImportSection(reader) => { info.imports = Range {start:consumed, end: consumed+chunksize} },
                Payload::FunctionSection(reader) => { 

                },
                Payload::TableSection(reader) => { info.functions = Range {start:consumed, end: consumed + chunksize}},
                Payload::MemorySection(reader) => { info.memories = Range {start:consumed, end: consumed + chunksize} },
                Payload::GlobalSection(reader) => { info.globals = Range {start:consumed, end: consumed + chunksize}},
                Payload::ExportSection(reader) => { info.exports = Range {start:consumed, end: consumed + chunksize} },
                Payload::StartSection { func, range } => { info.is_start_defined = true; },
                Payload::ElementSection(reader) => { info.elements = Range {start:consumed, end: consumed+chunksize} },
                Payload::DataSection(reader) => { info.data = Range {start:consumed, end: consumed+chunksize} },
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

        // Select random
        let mut rnd = self.get_rnd();
        let mut selected = Vec::new();

        mutators.iter().zip(ranges.iter()).for_each(|(mutations, ranges)|{
            // In this dimension only one mutation can be selected
            if mutations.len() >= 1 { // at least one mutation
                let selected_mutation = rnd.gen_range(0, mutations.len());
                selected.push((mutations.get(selected_mutation).unwrap(), ranges.get(selected_mutation).unwrap()));
            }
        });

        if selected.len() == 0 { // return idem if no mutators
            return Ok(input_wasm.to_vec())
        }

        // Sort selected mutations by range
        // TODO validate, Ranges should not overlap ?
        selected.sort_by_key(|(_, range)| {
            range.start
        });

        let mut offset = 0;

        // Mutators will be applied in specific ranges otherwise the chunk is copied to the resultant byte stream
        // [.....][mutator1()][.....][mutator2()][mutator3()][.....]
        // TODO, prepare mutation to be possible to 'reduce', this will allow to mutate over the same section after one mutation has already passed
        for (muta, on) in selected {
            // Copy from last offset to on.start
            if on.start - offset >= 1 {
                result.extend(&input_wasm[offset..on.start]);

                let mutation = muta(self, input_wasm, &info);
                result.extend(mutation);
                offset = on.end;
            }
        }

        result.extend(&input_wasm[offset..]);
        Ok(result)
    }

}
