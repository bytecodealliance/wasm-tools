use anyhow::Result;
use clap::Parser;

/// Print the textual form of a WebAssembly binary.
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Whether or not to print binary offsets intermingled in the text format
    /// as comments for debugging.
    #[clap(short, long)]
    print_offsets: bool,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let wasm = self.io.parse_input_wasm()?;
        let mut printer = wasmprinter::Printer::new();
        printer.print_offsets(self.print_offsets);
        let wat = printer.print(&wasm)?;
        self.io.output(wasm_tools::Output::Wat(&wat))?;
        Ok(())
    }
}
