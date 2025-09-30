use anyhow::Result;
use clap::Parser;

/// Parse the WebAssembly text format.
///
/// This subcommand will parse the provided input as the WebAssembly text format
/// and optionally write the binary or text form to a provided file.
#[derive(Parser)]
#[clap(after_help = "
Examples:

   To parse the file input.wat as WebAssembly text format and write the
   normalized text format to stdout:

          wasm-tools parse -t input.wat

   To parse the file input.wat as WebAssembly text format and write the binary
   form to the file output.wasm, including debugging information:

          wasm-tools parse input.wat -g -o output.wasm

   To parse the file input.wat as WebAssembly text format and write the
   normalized text format to the file output.wat:

          wasm-tools parse -t input.wat -o output.wat

Exit status:
    0 if OK,
    nonzero to indicate a parse error.")]
pub struct Opts {
    #[clap(flatten)]
    generate_dwarf: wasm_tools::GenerateDwarfArg,

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
        let binary = self.io.parse_input_wasm(Some(&self.generate_dwarf))?;
        self.io.output_wasm(&binary, self.wat)?;
        Ok(())
    }
}
