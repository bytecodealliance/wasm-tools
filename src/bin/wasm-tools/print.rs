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

    /// Indicates that the "skeleton" of a module should be printed where items
    /// such as function bodies, data segments, and element segments are
    /// replaced with "..." instead of printing their actual contents.
    #[clap(long)]
    skeleton: bool,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let wasm = self.io.parse_input_wasm()?;
        let mut printer = wasmprinter::Printer::new();
        printer.print_offsets(self.print_offsets);
        printer.print_skeleton(self.skeleton);
        let wat = printer.print(&wasm)?;
        self.io.output(wasm_tools::Output::Wat(&wat))?;
        Ok(())
    }
}
