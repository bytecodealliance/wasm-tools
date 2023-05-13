use anyhow::{Context, Result};
use clap::Parser;
use std::io::{stdin, Read};
use std::path::PathBuf;
use wit_smith::Config;

/// A WIT test case generator.
///
/// Given an arbitrary input seed, `wit-smith` generates a valid WIT package set
/// encoded as a WebAssembly component. The input seed is interpreted as a
/// series of predetermined choices through a decision tree. Given the same
/// input seed, `wit-smith` will always generate the same output WebAssembly
/// component; it is deterministic. This tool is suitable for taking
/// fuzz-generated input to generate a wide array of WIT documents.
///
/// ## Example
///
/// Generate a WIT document from 100 bytes of random data:
///
/// $ head -c 100 /dev/urandom | wasm-tools wit-smith -t
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

    #[clap(flatten)]
    config: Config,

    /// Indicates that "arbitrary configuration" should be used meaning that the
    /// input seed is first used to generate the configuration and then
    /// afterwards the rest of the seed is used to generate the document.
    #[clap(long)]
    arbitrary_config: bool,

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
        let config = if self.arbitrary_config {
            u.arbitrary()?
        } else {
            self.config.clone()
        };
        let wasm_bytes = wit_smith::smith(&config, &mut u)?;

        self.output.output(wasm_tools::Output::Wasm {
            bytes: &wasm_bytes,
            wat: self.wat,
        })?;
        Ok(())
    }
}
