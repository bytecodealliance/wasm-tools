use anyhow::Result;
use clap::Parser;
use wasm_compose::cli::WasmComposeCommand;

#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    cmd: WasmComposeCommand,
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    pub fn run(self) -> Result<()> {
        self.cmd.execute()
    }
}
