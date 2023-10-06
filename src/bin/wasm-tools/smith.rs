use anyhow::{Context, Result};
use arbitrary::Arbitrary;
use clap::Parser;
use std::borrow::Cow;
use std::io::{stdin, Read};
use std::path::PathBuf;
use std::process;
use wasm_smith::{InstructionKind, InstructionKinds, MaybeInvalidModule, Module};

/// A WebAssembly test case generator.
///
/// Given an arbitrary input seed, `wasm-smith` generates a valid WebAssembly
/// module. The input seed is interpreted as a series of predetermined choices
/// through a decision tree. Given the same input seed, `wasm-smith` will always
/// generate the same output WebAssembly module; it is deterministic. Larger
/// input seeds tend to generate larger WebAssembly modules. Small changes to
/// the input seed tends to produce a small change to the output WebAssembly
/// module. These properties, taken together, make `wasm-smith` suitable for use
/// not just with purely random input seeds, but also with coverage-guided,
/// mutation-based fuzzing engines like libFuzzer and AFL.
///
/// ## Example
///
/// Generate a WebAssembly module from 100 bytes of random data:
///
/// $ head -c 100 /dev/urandom | wasm-smith -o test.wasm
///
/// ## Exit Codes
///
/// * 0: Success.
///
/// * 1: An unexpected failure occurred.
///
/// * 2: Failed to generate a Webassembly module from the input seed. (Happens
///      rarely; try again with a new input.)
#[derive(Parser)]
pub struct Opts {
    /// The arbitrary input seed.
    ///
    /// `stdin` is used if this argument is not supplied.
    input: Option<PathBuf>,

    #[clap(flatten)]
    output: wasm_tools::OutputArg,

    /// Output the text format of WebAssembly instead of the binary format.
    #[clap(short = 't', long)]
    wat: bool,

    /// Ensure that execution of generated Wasm modules will always terminate.
    ///
    /// This inserts a global "fuel" counter that is decremented at loop headers
    /// and in function prologues. When the fuel reaches 0, a trap is raised to
    /// terminate execution. Control the default amount of fuel with the
    /// `--fuel` flag.
    #[clap(long = "ensure-termination")]
    ensure_termination: bool,

    /// Indicates that the generated module may contain invalid wasm functions,
    /// taken raw from the input DNA.
    #[clap(long = "maybe-invalid")]
    maybe_invalid: bool,

    /// The default amount of fuel used with `--ensure-termination`.
    ///
    /// This is roughly the number of loop iterations and function calls that
    /// will be executed before a trap is raised to prevent infinite loops.
    #[clap(short = 'f', long = "fuel")]
    fuel: Option<u32>,

    /// JSON configuration file with settings to control the wasm output.
    #[clap(short = 'c', long = "config")]
    config: Option<PathBuf>,

    #[clap(flatten)]
    module_config: Config,

    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,
}

#[derive(Default, Debug, Parser, Clone, serde_derive::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Config {
    #[clap(long = "min-types")]
    min_types: Option<usize>,
    #[clap(long = "max-types")]
    max_types: Option<usize>,
    #[clap(long = "min-imports")]
    min_imports: Option<usize>,
    #[clap(long = "max-imports")]
    max_imports: Option<usize>,
    #[clap(long = "min-tags")]
    min_tags: Option<usize>,
    #[clap(long = "max-tags")]
    max_tags: Option<usize>,
    #[clap(long = "min-funcs")]
    min_funcs: Option<usize>,
    #[clap(long = "max-funcs")]
    max_funcs: Option<usize>,
    #[clap(long = "min-globals")]
    min_globals: Option<usize>,
    #[clap(long = "max-globals")]
    max_globals: Option<usize>,
    #[clap(long = "min-exports")]
    min_exports: Option<usize>,
    #[clap(long = "max-exports")]
    max_exports: Option<usize>,
    #[clap(long = "export-everything")]
    export_everything: Option<bool>,
    #[clap(long = "min-element-segments")]
    min_element_segments: Option<usize>,
    #[clap(long = "max-element-segments")]
    max_element_segments: Option<usize>,
    #[clap(long = "min-data-segments")]
    min_data_segments: Option<usize>,
    #[clap(long = "max-data-segments")]
    max_data_segments: Option<usize>,
    #[clap(long = "max-instructions")]
    max_instructions: Option<usize>,
    #[clap(long = "min-memories")]
    min_memories: Option<u32>,
    #[clap(long = "max-memories")]
    max_memories: Option<usize>,
    #[clap(long = "min-tables")]
    min_tables: Option<u32>,
    #[clap(long = "max-tables")]
    max_tables: Option<usize>,
    #[clap(long = "max-memory-pages")]
    max_memory_pages: Option<u64>,
    #[clap(long = "memory-max-size-required")]
    memory_max_size_required: Option<bool>,
    #[clap(long = "max-table-elements")]
    max_table_elements: Option<u32>,
    #[clap(long = "table-max-size-required")]
    table_max_size_required: Option<bool>,
    #[clap(long = "max-instances")]
    max_instances: Option<usize>,
    #[clap(long = "max-modules")]
    max_modules: Option<usize>,
    #[clap(long = "min-uleb-size")]
    min_uleb_size: Option<u8>,
    #[clap(long = "bulk-memory")]
    #[serde(rename = "bulk-memory")]
    bulk_memory_enabled: Option<bool>,
    #[clap(long = "reference-types")]
    #[serde(rename = "reference-types")]
    reference_types_enabled: Option<bool>,
    #[clap(long = "tail-call")]
    #[serde(rename = "tail-call")]
    tail_call_enabled: Option<bool>,
    #[clap(long = "simd")]
    #[serde(rename = "simd")]
    simd_enabled: Option<bool>,
    #[clap(long = "relaxed-simd")]
    #[serde(rename = "relaxed-simd")]
    relaxed_simd_enabled: Option<bool>,
    #[clap(long = "exception-handling")]
    #[serde(rename = "exception-handling")]
    exceptions_enabled: Option<bool>,
    #[clap(long = "allow-start")]
    #[serde(rename = "allow-start")]
    allow_start_export: Option<bool>,
    #[clap(long = "max-aliases")]
    max_aliases: Option<usize>,
    #[clap(long = "max-nesting-depth")]
    max_nesting_depth: Option<usize>,
    #[clap(long = "max-type-size")]
    max_type_size: Option<u32>,
    #[clap(long = "memory64")]
    memory64_enabled: Option<bool>,
    #[clap(long = "canonicalize-nans")]
    canonicalize_nans: Option<bool>,
    #[clap(long = "multi-value")]
    multi_value_enabled: Option<bool>,
    #[clap(long = "sign-extension-ops")]
    sign_extension_ops_enabled: Option<bool>,
    #[clap(long = "saturating-float-to-int")]
    saturating_float_to_int_enabled: Option<bool>,
    #[clap(long = "generate-custom-sections")]
    generate_custom_sections: Option<bool>,
    #[clap(long = "available-imports")]
    available_imports: Option<PathBuf>,
    /// Limit what kinds of instructions are allowed.
    ///
    /// By default, all kinds are allowed; available kinds: numeric, vector,
    /// reference, parametric, variable, table, memory, control. Specify
    /// multiple kinds with a comma-separated list: e.g.,
    /// `--allowed-instructions numeric,control,parametric`
    #[clap(long = "allowed-instructions", use_value_delimiter = true)]
    allowed_instructions: Option<Vec<InstructionKind>>,
    #[clap(long = "threads")]
    #[serde(rename = "threads")]
    threads_enabled: Option<bool>,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    pub fn run(&self) -> Result<()> {
        let seed = match &self.input {
            Some(f) => {
                std::fs::read(f).with_context(|| format!("failed to read '{}'", f.display()))?
            }
            None => {
                let mut seed = Vec::new();
                stdin()
                    .read_to_end(&mut seed)
                    .context("failed to read <stdin>")?;
                seed
            }
        };

        let mut u = arbitrary::Unstructured::new(&seed);
        let wasm_bytes = if self.maybe_invalid {
            MaybeInvalidModule::arbitrary(&mut u)
                .unwrap_or_else(|e| {
                    eprintln!("error: failed to generate module: {}", e);
                    process::exit(2);
                })
                .to_bytes()
        } else {
            let json = match &self.config {
                Some(path) => {
                    let json = std::fs::read_to_string(&path).with_context(|| {
                        format!("failed to read json config: {}", path.display())
                    })?;
                    serde_json::from_str(&json).with_context(|| {
                        format!("failed to decode json config: {}", path.display())
                    })?
                }
                None => Config::default(),
            };
            let config = CliAndJsonConfig {
                json,
                cli: self.module_config.clone(),
            };
            let mut module = Module::new(config, &mut u).unwrap_or_else(|e| {
                eprintln!("error: failed to generate module: {}", e);
                process::exit(2);
            });
            if self.ensure_termination {
                module.ensure_termination(self.fuel.unwrap_or(100));
            }
            module.to_bytes()
        };

        self.output.output(wasm_tools::Output::Wasm {
            bytes: &wasm_bytes,
            wat: self.wat,
        })?;
        Ok(())
    }
}

macro_rules! fields {
    ($(
        ($field:ident, $ty:ty, $default:expr),
    )*) => ($(
        fn $field(&self) -> $ty {
            self.cli.$field.or(self.json.$field).unwrap_or($default)
        }
    )*)
}

#[derive(Clone, Debug)]
struct CliAndJsonConfig {
    json: Config,
    cli: Config,
}

impl wasm_smith::Config for CliAndJsonConfig {
    fields! {
        (min_types, usize, 0),
        (max_types, usize, 100),
        (min_imports, usize, 0),
        (max_imports, usize, 100),
        (min_tags, usize, 0),
        (max_tags, usize, 100),
        (min_funcs, usize, 0),
        (max_funcs, usize, 100),
        (min_globals, usize, 0),
        (max_globals, usize, 100),
        (min_exports, usize, 0),
        (max_exports, usize, 100),
        (export_everything, bool, false),
        (min_element_segments, usize, 0),
        (max_element_segments, usize, 100),
        (min_data_segments, usize, 0),
        (max_data_segments, usize, 100),
        (max_instructions, usize, 100),
        (min_memories, u32, 0),
        (max_memories, usize, 1),
        (min_tables, u32, 0),
        (max_tables, usize, 1),
        (memory_max_size_required, bool, false),
        (max_table_elements, u32, 1_000_000),
        (table_max_size_required, bool, false),
        (max_instances, usize, 10),
        (max_modules, usize, 10),
        (min_uleb_size, u8, 1),
        (bulk_memory_enabled, bool, true),
        (reference_types_enabled, bool, true),
        (tail_call_enabled, bool, true),
        (simd_enabled, bool, true),
        (relaxed_simd_enabled, bool, false),
        (exceptions_enabled, bool, false),
        (multi_value_enabled, bool, true),
        (saturating_float_to_int_enabled, bool, true),
        (sign_extension_ops_enabled, bool, true),
        (memory64_enabled, bool, false),
        (allow_start_export, bool, true),
        (max_aliases, usize, 1000),
        (max_nesting_depth, usize, 1000),
        (max_type_size, u32, 1000),
        (canonicalize_nans, bool, false),
        (generate_custom_sections, bool, false),
        (threads_enabled, bool, false),
    }

    fn max_memory_pages(&self, _is_64: bool) -> u64 {
        self.cli
            .max_memory_pages
            .or(self.json.max_memory_pages)
            .unwrap_or(65536)
    }

    fn allowed_instructions(&self) -> InstructionKinds {
        match self
            .cli
            .allowed_instructions
            .as_ref()
            .or(self.json.allowed_instructions.as_ref())
        {
            Some(ks) => InstructionKinds::new(ks),
            None => InstructionKinds::all(),
        }
    }

    fn available_imports(&self) -> Option<Cow<'static, [u8]>> {
        let file = self
            .cli
            .available_imports
            .as_ref()
            .or(self.json.available_imports.as_ref())?;
        Some(wat::parse_file(file).unwrap().into())
    }
}
