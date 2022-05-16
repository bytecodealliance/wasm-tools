use anyhow::Result;
use clap::Parser;

/// Print the textual form of a WebAssembly binary.
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let wasm = self.io.parse_input_wasm()?;
        let wat = wasmprinter::print_bytes(&wasm)?;
        self.io.output(wasm_tools::Output::Wat(&wat))?;
        Ok(())
    }
}
