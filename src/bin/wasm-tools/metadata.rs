use anyhow::Result;
use std::io::Write;

/// Manipulate metadata (module name, producers) to a WebAssembly file.
#[derive(clap::Parser)]
pub enum Opts {
    Show(ShowOpts),
    Add(AddOpts),
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        match self {
            Opts::Show(opts) => opts.run(),
            Opts::Add(opts) => opts.run(),
        }
    }
}

/// Read metadata (module name, producers) from a WebAssembly file.
#[derive(clap::Parser)]
pub struct ShowOpts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl ShowOpts {
    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let mut output = self.io.output_writer()?;

        let metadata = wasm_metadata::Metadata::from_binary(&input)?;
        write!(output, "{metadata:?}")?;
        Ok(())
    }
}

/// Add metadata (module name, producers) to a WebAssembly file
#[derive(clap::Parser)]
pub struct AddOpts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    #[clap(flatten)]
    add_metadata: wasm_metadata::AddMetadata,

    /// Output the text format of WebAssembly instead of the binary format
    #[clap(short = 't', long)]
    wat: bool,
}

impl AddOpts {
    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;

        let output = self.add_metadata.run(&input)?;

        self.io.output(wasm_tools::Output::Wasm {
            bytes: output.as_slice(),
            wat: self.wat,
        })?;
        Ok(())
    }
}
