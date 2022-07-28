use anyhow::Result;
use clap::Parser;
use wasm_compose::cli::WasmComposeCommand;

#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    cmd: WasmComposeCommand,
}

impl Opts {
    pub fn run(self) -> Result<()> {
        self.cmd.execute()
    }
}
