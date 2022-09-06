use anyhow::Result;
use clap::Parser;
use wasm_componentize::cli::WasmComponentizeCommand;

#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    cmd: WasmComponentizeCommand,
}

impl Opts {
    pub fn run(self) -> Result<()> {
        self.cmd.execute()
    }
}
