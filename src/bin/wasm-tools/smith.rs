use anyhow::{Context, Result};
use clap::Parser;
use std::io::{stdin, Read};
use std::path::PathBuf;
use std::process;
use wasm_smith::Module;

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
    ///
    /// Note that when combined with `--allow-invalid-funcs` this subcommand may
    /// return an error because arbitrary function bodies taken from input bytes
    /// cannot be validated to ensure they always terminate.
    #[clap(long)]
    ensure_termination: bool,

    /// The default amount of fuel used with `--ensure-termination`.
    ///
    /// This is roughly the number of loop iterations and function calls that
    /// will be executed before a trap is raised to prevent infinite loops.
    #[clap(short, long)]
    fuel: Option<u32>,

    /// JSON configuration file with settings to control the wasm output.
    #[clap(short, long)]
    config: Option<PathBuf>,

    #[clap(flatten)]
    module_config: wasm_smith::InternalOptionalConfig,

    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,
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
        let json = match &self.config {
            Some(path) => {
                let json = std::fs::read_to_string(&path)
                    .with_context(|| format!("failed to read json config: {}", path.display()))?;
                serde_json::from_str(&json)
                    .with_context(|| format!("failed to decode json config: {}", path.display()))?
            }
            None => wasm_smith::InternalOptionalConfig::default(),
        };
        let config = self.module_config.clone().or(json);
        let config = wasm_smith::Config::try_from(config)?;
        let mut module = Module::new(config, &mut u).unwrap_or_else(|e| {
            eprintln!("error: failed to generate module: {}", e);
            process::exit(2);
        });
        if self.ensure_termination {
            module
                .ensure_termination(self.fuel.unwrap_or(100))
                .context(
                    "failed to ensure module always terminates, \
                    `--ensure-termination` is incompatible with \
                    `--allow-invalid-funcs`",
                )?;
        }
        let wasm_bytes = module.to_bytes();

        self.output
            .output_wasm(&self.general, &wasm_bytes, self.wat)?;
        Ok(())
    }
}
