use anyhow::{Context, Result};
use clap::Parser;
use std::path::PathBuf;

/// Print the textual form of a WebAssembly binary.
#[derive(Parser)]
pub struct Opts {
    /// Input WebAssembly file to print.
    input: PathBuf,

    /// An optional output file to place the output into.
    ///
    /// If not specified then the wasm file is printed to standard output.
    #[clap(short = 'o', long)]
    output: Option<PathBuf>,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let wat = wasmprinter::print_file(&self.input)?;
        if let Some(output) = &self.output {
            std::fs::write(&output, wat).context(format!("failed to write {:?}", output))?;
        } else {
            println!("{}", wat);
        }

        Ok(())
    }
}
