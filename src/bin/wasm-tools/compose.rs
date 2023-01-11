use anyhow::Result;
use clap::Parser;
use wasm_compose::cli::WasmComposeCommand;

#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    cmd: WasmComposeCommand,
    #[clap(flatten)]
    verbosity: wasm_tools::Verbosity,
}

impl Opts {
    pub fn run(self) -> Result<()> {
        self.verbosity.init_logger();
        self.cmd.execute()
    }
}
