use crate::WasmTools;
use anyhow::Result;
use clap::{CommandFactory, Parser};
use clap_complete::{generate, Shell};
use std::io::stdout;

/// Generate shell completion scripts for this CLI.
///
/// Shells have different paths for their completion scripts. Please refer to their documentation.
/// For example, to generate completions for the fish shell, run the following command below:
///
/// wasm-tools completion fish > ~/.config/fish/completions/wasm-tools.fish
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    /// The shell to generate completions for.
    #[clap(index = 1)]
    shell: Shell,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    pub fn run(&self) -> Result<()> {
        let mut cmd = WasmTools::command();
        let cli_name = cmd.get_name().to_owned();

        generate(self.shell, &mut cmd, cli_name, &mut stdout());
        Ok(())
    }
}
