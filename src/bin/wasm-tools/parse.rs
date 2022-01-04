use anyhow::{Context, Result};
use clap::Parser;
use std::path::PathBuf;

/// Parse the WebAssembly text format.
///
/// This subcommand will parse the provided input as the WebAssembly text format
/// and optionally write the binary form to a provided file.
#[derive(Parser)]
pub struct Opts {
    /// Input WebAssembly text file to parse.
    input: PathBuf,

    /// An optional output file to place the binary WebAssembly output into.
    ///
    /// If this is not provided then the input is simply parsed for validity and
    /// the output is not placed anywhere.
    #[clap(short = 'o', long)]
    output: Option<PathBuf>,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let binary = wat::parse_file(&self.input)?;
        if let Some(output) = &self.output {
            std::fs::write(&output, binary).context(format!("failed to write: {:?}", output))?;
        }
        Ok(())
    }
}
