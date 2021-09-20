//! A WebAssembly test case mutator.
//!
//! `wasm-mutate` takes in an existing Wasm module and then applies a
//! pseudo-random transformation to it, producing a new, mutated Wasm
//! module. This new, mutated Wasm module can be fed as a test input to your
//! Wasm parser, validator, compiler, or any other Wasm-consuming
//! tool. `wasm-mutate` can serve as a custom mutator for mutation-based
//! fuzzing.
#![cfg_attr(not(feature = "structopt"), deny(missing_docs))]
use std::sync::Arc;

use module::TypeInfo;
use mutators::{Mutator, RenameExportMutator};
use rand::{prelude::SliceRandom, rngs::SmallRng, SeedableRng};
use std::convert::TryFrom;
#[cfg(feature = "structopt")]
use structopt::StructOpt;

mod error;

mod module;
pub mod mutators;

pub use error::{Error, Result};
use wasm_encoder::{RawSection, SectionId};
use wasmparser::{Chunk, Parser, Payload, SectionReader};

use crate::mutators::{RemoveExportMutator, ReturnI32SnipMutator, SetFunction2Unreachable, swap_commutative::SwapCommutativeOperator};

macro_rules! initialize_and_filter {
    (
        $info: ident, $config: ident, $result: ident,$(
            $mutation:expr,
        )*
    ) => {
            $(
                // Generate this recursivelly
                if $mutation.can_mutate($config, &$info)? {
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
#[derive(Default)]
pub struct ModuleInfo<'a> {
    // The following fields are offsets inside the `raw_sections` field.
    // The main idea is to maintain the order of the sections in the input Wasm.
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

    // raw_sections
    raw_sections: Vec<RawSection<'a>>,
    input_wasm: &'a [u8],
}

impl<'a> ModuleInfo<'a> {
    fn has_code(&self) -> bool {
        self.code != None
    }

    pub fn section(&mut self, id: u8, range: wasmparser::Range, full_wasm: &'a [u8]) {
        self.raw_sections.push(RawSection {
            id,
            data: &full_wasm[range.start..range.end],
        });
    }

    fn get_code_section(&self) -> RawSection {
        return self.raw_sections[self.code.unwrap()];
    }

    fn get_exports_section(&self) -> &RawSection {
        return &self.raw_sections[self.exports.unwrap()];
    }

    fn has_exports(&self) -> bool {
        self.exports != None
    }

    pub fn get_type_idx(&self, idx: usize) -> &TypeInfo {
        &self.types_map[idx]
    }

    pub fn get_functype_idx(&self, idx: usize) -> &TypeInfo {
        let functpeindex = self.function_map[idx] as usize;
        &self.types_map[functpeindex]
    }

    fn replace_section(
        &self,
        i: usize,
        new_section: &impl wasm_encoder::Section,
    ) -> wasm_encoder::Module {
        let mut module = wasm_encoder::Module::new();
        self.raw_sections.iter().enumerate().for_each(|(j, s)| {
            if i == j {
                module.section(new_section);
            } else {
                module.section(s);
            }
        });
        module
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

    /// Parse and fill lane AST with metadata information about the module
    pub fn get_module_info<'a>(&'a self, input_wasm: &'a [u8]) -> Result<ModuleInfo> {
        let mut parser = Parser::new(0);
        let mut info = ModuleInfo::default();
        let mut wasm = input_wasm;
        info.input_wasm = wasm;

        loop {
            let (payload, consumed) = match parser.parse(&wasm, true)? {
                Chunk::NeedMoreData(hint) => {
                    panic!("Invalid Wasm module {:?}", hint);
                }
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };
            match payload {
                Payload::CodeSectionStart {
                    count: _,
                    range,
                    size: _,
                } => {
                    info.code = Some(info.raw_sections.len());
                    info.section(SectionId::Code.into(), range, input_wasm);
                    parser.skip_section();
                    println!("{:?} {:?}", wasm, consumed);
                    // update slice
                    wasm = &wasm[range.end - range.start + consumed - 1..];

                    continue;
                }
                Payload::TypeSection(mut reader) => {
                    info.types = Some(info.raw_sections.len());
                    info.section(SectionId::Type.into(), reader.range(), input_wasm);

                    // Save function types
                    for _ in 0..reader.get_count() {
                        reader.read().and_then(|ty| {
                            let typeinfo = TypeInfo::try_from(ty).unwrap();
                            info.types_map.push(typeinfo);
                            Ok(())
                        })?;
                    }
                }
                Payload::ImportSection(reader) => {
                    info.imports = Some(info.raw_sections.len());
                    info.section(SectionId::Import.into(), reader.range(), input_wasm);
                }
                Payload::FunctionSection(mut reader) => {
                    info.functions = Some(info.raw_sections.len());
                    info.section(SectionId::Function.into(), reader.range(), input_wasm);

                    for _ in 0..reader.get_count() {
                        reader.read().and_then(|ty| {
                            info.function_map.push(ty);
                            Ok(())
                        })?;
                    }
                }
                Payload::TableSection(reader) => {
                    info.tables = Some(info.raw_sections.len());
                    info.section(SectionId::Table.into(), reader.range(), input_wasm);
                }
                Payload::MemorySection(reader) => {
                    info.memories = Some(info.raw_sections.len());
                    info.section(SectionId::Memory.into(), reader.range(), input_wasm);
                }
                Payload::GlobalSection(reader) => {
                    info.globals = Some(info.raw_sections.len());
                    info.section(SectionId::Global.into(), reader.range(), input_wasm);
                }
                Payload::ExportSection(reader) => {
                    info.exports = Some(info.raw_sections.len());
                    info.section(SectionId::Export.into(), reader.range(), input_wasm);
                }
                Payload::StartSection { func: _, range: _ } => {
                    info.is_start_defined = true;
                }
                Payload::ElementSection(reader) => {
                    info.elements = Some(info.raw_sections.len());
                    info.section(SectionId::Element.into(), reader.range(), input_wasm);
                }
                Payload::DataSection(reader) => {
                    info.data = Some(info.raw_sections.len());
                    info.section(SectionId::Data.into(), reader.range(), input_wasm);
                }
                Payload::CustomSection {
                    name: _,
                    data_offset: _,
                    data: _,
                    range,
                } => {
                    info.section(SectionId::Custom.into(), range, input_wasm);
                }
                Payload::AliasSection(reader) => {
                    info.section(SectionId::Alias.into(), reader.range(), input_wasm);
                }
                Payload::UnknownSection {
                    id,
                    contents: _,
                    range,
                } => {
                    info.section(id, range, input_wasm);
                }
                Payload::DataCountSection { count: _, range } => {
                    info.section(SectionId::DataCount.into(), range, input_wasm);
                }
                Payload::Version { .. } => {}
                Payload::End => {
                    break;
                }
                _ => todo!("{:?} not implemented", payload),
            }
            wasm = &wasm[consumed..];
        }

        Ok(info)
    }

    /// Run this configured `WasmMutate` on the given input Wasm.
    pub fn run<'a>(&self, input_wasm: &'a [u8]) -> Result<Vec<u8>> {
        let mut info = self.get_module_info(&input_wasm)?;

        let mut mutators: Vec<Box<dyn Mutator>> = Vec::new();
        initialize_and_filter! {
            info,
            self,
            mutators,

            RenameExportMutator{max_name_size: 100},
            RemoveExportMutator,
            ReturnI32SnipMutator,
            SetFunction2Unreachable,
            SwapCommutativeOperator,
        };

        let mut rnd = SmallRng::seed_from_u64(self.seed);
        let mut mutator = mutators
            .choose(&mut rnd)
            .ok_or(Error::NoMutationsAplicable)?;

        let module = mutator.mutate(&self, &mut rnd, &mut info);
        Ok(module?.finish())
    }
}
