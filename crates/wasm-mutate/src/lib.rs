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

use module::TypeInfo;
use mutators::{Mutator, RenameExportMutator};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use std::convert::TryFrom;
#[cfg(feature = "structopt")]
use structopt::StructOpt;

mod error;

mod module;
pub mod mutators;

pub use error::{Error, Result};
use wasm_encoder::{CodeSection, ExportSection, Module, RawSection, Section, SectionId, encoders};
use wasmparser::{Chunk, ExportSectionReader, Parser, Payload, Range, SectionReader, TypeDef};

use crate::{
    module::{FuncInfo, PrimitiveTypeInfo},
    mutators::{RemoveExportMutator }//, RenameExportMutator, SetFunction2Unreachable, ReturnI32SnipMutator},
};

macro_rules! initialize_and_filter {
    (
        $info: ident, $config: ident, $result: ident,$(
            $mutation:expr,            
        )*
    ) => {
            $(
                if $mutation.can_mutate($config, &$info) {
                    $result.push(Box::new($mutation))
                }
            )*
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
        let seed = 3;
        WasmMutate {
            seed: seed,
            preserve_semantics: false,
            reduce: false,
            raw_mutate_func: None,
        }
    }
}


/// Provides module information for future usage during mutation
/// an instance of ModuleInfo could be user to determine which mutation could be applied
/// We have the ranges where the sections are for sake of memory safe, instead of having raw sections
/// TODO, make this structure serializable in order to save time if the same module is mutated several times
#[derive(Default)]
pub struct ModuleInfo<'a> {

    exports: Option<usize>,

    types: Option<usize>,
    imports: Option<usize>,
    tables: Option<usize>,
    memories: Option<usize>,
    globals: Option<usize>,
    elements: Option<usize>,
    functions: Option<usize>,
    data: Option<usize>,
    code: Option<usize>,
    
    is_start_defined: bool,

    // types for inner functions
    types_map: Vec<TypeInfo>,
    function_map: Vec<u32>,

    raw_sections: Vec<RawSection<'a>>,

}

impl<'a> ModuleInfo<'a> {


    fn has_code(&self) -> bool {
        self.code != None
    }


    pub fn section(&mut self, 
                        id: SectionId,
                        range: wasmparser::Range,
                        full_wasm: &'a [u8]){
        self.raw_sections.push(RawSection{
            id: id.into(),
            data: &full_wasm[range.start..range.end]
        });
    }

    fn get_code_section(&mut self) -> RawSection{
        return self.raw_sections[self.code.unwrap()]
    }

    fn get_exports_section(&self) -> &RawSection{
        return &self.raw_sections[self.exports.unwrap()]
    }


    fn has_exports(&self) -> bool {
        self.exports != None
    }

    pub fn get_type_idx(&self, idx: usize) -> &TypeInfo {
        self.types_map.get(idx).unwrap()
    }

    pub fn get_functype_idx(&self, idx: usize) -> &TypeInfo {
        self.types_map
            .get(*self.function_map.get(idx).unwrap() as usize)
            .unwrap()
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

    /// Returns Random generator from seed
    pub fn get_rnd(&self) -> SmallRng {
        SmallRng::seed_from_u64(self.seed as u64)
    }

    /// Parse and fill lane AST with metadata information about the module
    fn get_module_info<'a>(&'a self, input_wasm: &'a [u8], info: &mut ModuleInfo<'a>) {
        
        let mut parser = Parser::new(0);
        let mut consumed = 0;
      
        let mut index = 0;
        println!("{:?}", input_wasm);

        loop {
            let (payload, chunksize) = match parser.parse(&input_wasm[consumed..], true).unwrap() {
                Chunk::NeedMoreData(_) => {
                    panic!("Invalid Wasm module");
                }
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };
            
            match payload {
                Payload::CodeSectionStart { count, range, size } => {
                    info.code = Some(info.raw_sections.len());
                    let allcoderange = Range{
                        start: range.start,
                        end: range.end
                    };
                    info.section(SectionId::Code, allcoderange, input_wasm);
                },
                Payload::TypeSection(mut reader) => {
                    info.types = Some(info.raw_sections.len());
                    info.section(SectionId::Type, reader.range(), input_wasm);
                    
                    // Save function types
                    (0..reader.get_count()).for_each(|i| {
                        let ty = reader.read().unwrap();

                        // Replace by try_into ?
                        info.types_map.push(match ty {
                            TypeDef::Func(FT) => TypeInfo::Func(FuncInfo {
                                params: FT
                                    .params
                                    .iter()
                                    .map(|&t| match t {
                                        wasmparser::Type::I32 => PrimitiveTypeInfo::I32,
                                        wasmparser::Type::I64 => PrimitiveTypeInfo::I64,
                                        wasmparser::Type::F32 => PrimitiveTypeInfo::F32,
                                        wasmparser::Type::F64 => PrimitiveTypeInfo::F64,
                                        _ => panic!("Unsupported type {:?}", t)
                                    })
                                    .collect(),
                                returns: FT
                                    .returns
                                    .iter()
                                    .map(|&t| match t {
                                        wasmparser::Type::I32 => PrimitiveTypeInfo::I32,
                                        wasmparser::Type::I64 => PrimitiveTypeInfo::I64,
                                        wasmparser::Type::F32 => PrimitiveTypeInfo::F32,
                                        wasmparser::Type::F64 => PrimitiveTypeInfo::F64,
                                        _ => panic!("Unsupported type {:?}", t)
                                    })
                                    .collect(),
                            }),
                            TypeDef::Instance(_) => todo!(),
                            TypeDef::Module(_) => todo!(),
                        });
                    })
                }
                Payload::ImportSection(reader) => {
                    info.imports = Some(info.raw_sections.len());
                    info.section(SectionId::Import, reader.range(), input_wasm);
                }
                Payload::FunctionSection(mut reader) => {
                    info.functions = Some(info.raw_sections.len());
                    info.section(SectionId::Function, reader.range(), input_wasm);

                    (0..reader.get_count()).for_each(|_| {
                        let ty = reader.read().unwrap();
                        info.function_map.push(ty);
                    });
                }
                Payload::TableSection(reader) => {
                    info.tables = Some(info.raw_sections.len());
                    info.section(SectionId::Table, reader.range(), input_wasm);
                }
                Payload::MemorySection(reader) => {
                    info.memories = Some(info.raw_sections.len());
                    info.section(SectionId::Memory, reader.range(), input_wasm);
                }
                Payload::GlobalSection(reader) => {
                    info.globals = Some(info.raw_sections.len());
                    info.section(SectionId::Global, reader.range(), input_wasm);
                }
                Payload::ExportSection(mut reader) => {
                    info.exports = Some(info.raw_sections.len());
                    info.section(SectionId::Export, reader.range(), input_wasm);
                }
                Payload::StartSection { func: _, range: _ } => {
                    info.is_start_defined = true;
                }
                Payload::ElementSection(reader) => {
                    info.elements = Some(info.raw_sections.len());
                    info.section(SectionId::Element, reader.range(), input_wasm);
                }
                Payload::DataSection(reader) => {
                    info.data = Some(info.raw_sections.len());
                    info.section(SectionId::Data, reader.range(), input_wasm);
                },
                Payload::End => {
                    break;
                }
                // TODO, Add the others, make the mutator recursive on nested modules ?
                _ => {}
            }
            consumed += chunksize
        }
    }

    /// Run this configured `WasmMutate` on the given input Wasm.
    pub fn run<'a>(&self, input_wasm: &'a [u8]) -> Result<Vec<u8>> {
        let mut info = ModuleInfo{
            exports: None,
            types: None,
            imports: None,
            tables: None,
            memories: None,
            globals: None,
            elements: None,
            functions: None,
            data: None,
            code: None,
            is_start_defined: false,
            types_map: Vec::new(),
            function_map: Vec::new(),
            raw_sections: Vec::new()
        };

        
        self.get_module_info(&input_wasm, &mut info);

        let mut mutators: Vec<Box<dyn Mutator>> = Vec::new();
        initialize_and_filter!{
            info,
            self,
            mutators,

            // RenameExportMutator{max_name_size: 100},
            RemoveExportMutator,
        };

        if mutators.len() == 0 {
            return Ok(input_wasm.to_vec())
        }
        // Select random
        let mut rnd = self.get_rnd();

        let selected_mutation = rnd.gen_range(0, mutators.len());
        let mutator = mutators.get(selected_mutation).unwrap();
        
        let module = mutator.mutate(&self, &mut info);
        
        println!("{:?}", module);
        Ok(module.finish())
    }
}
