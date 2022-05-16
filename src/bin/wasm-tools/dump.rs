use anyhow::Result;

/// Debugging utility to dump information about a wasm binary.
///
/// This can be useful to figure out precisely how each byte of a wasm binary is
/// classified or where particular constructs are at particular offsets.
#[derive(clap::Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let output = self.io.output_writer()?;
        wasmparser_dump::dump_wasm_into(&input, output)?;
        Ok(())
    }
}
