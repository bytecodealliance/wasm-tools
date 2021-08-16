use anyhow::Context;
use arbitrary::Arbitrary;
use std::fs;
use std::io::{stdin, stdout, Read, Write};
use std::path::PathBuf;
use std::process;
use structopt::StructOpt;
use wasm_smith::{MaybeInvalidModule, Module};

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
#[derive(StructOpt)]
struct Options {
    /// The arbitrary input seed.
    ///
    /// `stdin` is used if this argument is not supplied.
    #[structopt(parse(from_os_str))]
    input: Option<PathBuf>,

    /// The output file path, where the generated WebAssembly module is
    /// placed.
    ///
    /// `stdout` is used if this argument is not supplied.
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    output: Option<PathBuf>,

    /// Ensure that execution of generated Wasm modules will always terminate.
    ///
    /// This inserts a global "fuel" counter that is decremented at loop headers
    /// and in function prologues. When the fuel reaches 0, a trap is raised to
    /// terminate execution. Control the default amount of fuel with the
    /// `--fuel` flag.
    #[structopt(short = "t", long = "ensure-termination")]
    ensure_termination: bool,

    /// Indicates that the generated module may contain invalid wasm functions,
    /// taken raw from the input DNA.
    #[structopt(long = "maybe-invalid")]
    maybe_invalid: bool,

    /// The default amount of fuel used with `--ensure-termination`.
    ///
    /// This is roughly the number of loop iterations and function calls that
    /// will be executed before a trap is raised to prevent infinite loops.
    #[structopt(short = "f", long = "fuel")]
    fuel: Option<u32>,

    /// JSON configuration file with settings to control the wasm output.
    #[structopt(short = "c", long = "config", parse(from_os_str))]
    config: Option<PathBuf>,

    #[structopt(flatten)]
    module_config: Config,
}

#[derive(Default, Debug, StructOpt, Clone, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Config {
    #[structopt(long = "min-types")]
    min_types: Option<usize>,
    #[structopt(long = "max-types")]
    max_types: Option<usize>,
    #[structopt(long = "min-imports")]
    min_imports: Option<usize>,
    #[structopt(long = "max-imports")]
    max_imports: Option<usize>,
    #[structopt(long = "min-funcs")]
    min_funcs: Option<usize>,
    #[structopt(long = "max-funcs")]
    max_funcs: Option<usize>,
    #[structopt(long = "min-globals")]
    min_globals: Option<usize>,
    #[structopt(long = "max-globals")]
    max_globals: Option<usize>,
    #[structopt(long = "min-exports")]
    min_exports: Option<usize>,
    #[structopt(long = "max-exports")]
    max_exports: Option<usize>,
    #[structopt(long = "min-element-segments")]
    min_element_segments: Option<usize>,
    #[structopt(long = "max-element-segments")]
    max_element_segments: Option<usize>,
    #[structopt(long = "min-data-segments")]
    min_data_segments: Option<usize>,
    #[structopt(long = "max-data-segments")]
    max_data_segments: Option<usize>,
    #[structopt(long = "max-instructions")]
    max_instructions: Option<usize>,
    #[structopt(long = "min-memories")]
    min_memories: Option<u32>,
    #[structopt(long = "max-memories")]
    max_memories: Option<usize>,
    #[structopt(long = "min-tables")]
    min_tables: Option<u32>,
    #[structopt(long = "max-tables")]
    max_tables: Option<usize>,
    #[structopt(long = "max-memory-pages")]
    max_memory_pages: Option<u64>,
    #[structopt(long = "memory-max-size-required")]
    memory_max_size_required: Option<bool>,
    #[structopt(long = "max-instances")]
    max_instances: Option<usize>,
    #[structopt(long = "max-modules")]
    max_modules: Option<usize>,
    #[structopt(long = "min-uleb-size")]
    min_uleb_size: Option<u8>,
    #[structopt(long = "bulk-memory")]
    #[serde(rename = "bulk-memory")]
    bulk_memory_enabled: Option<bool>,
    #[structopt(long = "reference-types")]
    #[serde(rename = "reference-types")]
    reference_types_enabled: Option<bool>,
    #[structopt(long = "simd")]
    #[serde(rename = "simd")]
    simd_enabled: Option<bool>,
    #[structopt(long = "module-linking")]
    #[serde(rename = "module-linking")]
    module_linking_enabled: Option<bool>,
    #[structopt(long = "allow-start")]
    #[serde(rename = "allow-start")]
    allow_start_export: Option<bool>,
    #[structopt(long = "max-aliases")]
    max_aliases: Option<usize>,
    #[structopt(long = "max-nesting-depth")]
    max_nesting_depth: Option<usize>,
    #[structopt(long = "max-type-size")]
    max_type_size: Option<u32>,
    #[structopt(long = "memory64")]
    memory64_enabled: Option<bool>,
    #[structopt(long = "canonicalize-nans")]
    canonicalize_nans: Option<bool>,
}

fn main() -> anyhow::Result<()> {
    let opts = Options::from_args();

    let stdin = stdin();
    let (mut input, input_name): (Box<dyn Read>, _) = match &opts.input {
        Some(f) => {
            let input = Box::new(
                fs::File::open(f).with_context(|| format!("failed to open '{}'", f.display()))?,
            );
            (input, f.display().to_string())
        }
        None => {
            let input = Box::new(stdin.lock());
            (input, "<stdin>".to_string())
        }
    };

    let stdout = stdout();
    let (mut output, output_name): (Box<dyn Write>, _) = match &opts.output {
        Some(f) => {
            let output = Box::new(
                fs::File::create(f)
                    .with_context(|| format!("failed to create '{}'", f.display()))?,
            );
            (output, f.display().to_string())
        }
        None => {
            let output = Box::new(stdout.lock());
            (output, "<stdout>".to_string())
        }
    };

    let mut seed = vec![];
    input
        .read_to_end(&mut seed)
        .with_context(|| format!("failed to read '{}'", input_name))?;

    let mut u = arbitrary::Unstructured::new(&seed);
    let wasm_bytes = if opts.maybe_invalid {
        MaybeInvalidModule::arbitrary(&mut u)
            .unwrap_or_else(|e| {
                eprintln!("error: failed to generate module: {}", e);
                process::exit(2);
            })
            .to_bytes()
    } else {
        let json = match &opts.config {
            Some(path) => {
                let json = std::fs::read_to_string(&path)
                    .with_context(|| format!("failed to read json config: {}", path.display()))?;
                serde_json::from_str(&json)
                    .with_context(|| format!("failed to decode json config: {}", path.display()))?
            }
            None => Config::default(),
        };
        let config = CliAndJsonConfig {
            json,
            cli: opts.module_config.clone(),
        };
        let mut module = Module::new(config, &mut u).unwrap_or_else(|e| {
            eprintln!("error: failed to generate module: {}", e);
            process::exit(2);
        });
        if opts.ensure_termination {
            module.ensure_termination(opts.fuel.unwrap_or(100));
        }
        module.to_bytes()
    };

    output
        .write_all(&wasm_bytes)
        .with_context(|| format!("failed to write to '{}'", output_name))?;

    drop(output);
    Ok(())
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
        (min_funcs, usize, 0),
        (max_funcs, usize, 100),
        (min_globals, usize, 0),
        (max_globals, usize, 100),
        (min_exports, usize, 0),
        (max_exports, usize, 100),
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
        (max_instances, usize, 10),
        (max_modules, usize, 10),
        (min_uleb_size, u8, 1),
        (bulk_memory_enabled, bool, false),
        (reference_types_enabled, bool, false),
        (memory64_enabled, bool, false),
        (simd_enabled, bool, false),
        (module_linking_enabled, bool, false),
        (allow_start_export, bool, true),
        (max_aliases, usize, 1000),
        (max_nesting_depth, usize, 1000),
        (max_type_size, u32, 1000),
        (canonicalize_nans, bool, false),
    }

    fn max_memory_pages(&self, _is_64: bool) -> u64 {
        self.cli
            .max_memory_pages
            .or(self.json.max_memory_pages)
            .unwrap_or(65536)
    }
}
