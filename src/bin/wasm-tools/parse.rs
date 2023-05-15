use anyhow::Result;
use clap::Parser;

/// Parse the WebAssembly text format.
///
/// This subcommand will parse the provided input as the WebAssembly text format
/// and optionally write the binary form to a provided file.
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Output the text format of WebAssembly instead of the binary format.
    #[clap(short = 't', long)]
    wat: bool,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let binary = self.io.parse_input_wasm()?;
        self.io.output(wasm_tools::Output::Wasm {
            bytes: &binary,
            wat: self.wat,
        })?;
        Ok(())
    }
}
