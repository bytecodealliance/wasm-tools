use anyhow::Result;
use std::path::PathBuf;

/// Debugging utility to dump information about a wasm binary.
///
/// This can be useful to figure out precisely how each byte of a wasm binary is
/// classified or where particular constructs are at particular offsets.
#[derive(clap::Parser)]
pub struct Opts {
    /// Input WebAssembly file to dump information about.
    input: PathBuf,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let input = wat::parse_file(&self.input)?;
        println!("{}", wasmparser_dump::dump_wasm(&input)?);

        Ok(())
    }
}
