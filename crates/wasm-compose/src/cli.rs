//! Module for CLI parsing.

use crate::{composer::ComponentComposer, config::Config};
use anyhow::{bail, Context, Result};
use clap::Parser;
use std::path::PathBuf;
use wasmparser::{Validator, WasmFeatures};

/// WebAssembly component composer.
///
/// A tool for composing WebAssembly components together.
#[derive(Debug, Parser)]
#[clap(name = "component-encoder", version = env!("CARGO_PKG_VERSION"))]
pub struct WasmComposeCommand {
    /// The path of the output composed WebAssembly component.
    #[clap(long, short = 'o', value_name = "OUTPUT")]
    pub output: Option<PathBuf>,

    /// The path to the configuration file to use.
    #[clap(
        long,
        short = 'c',
        value_name = "CONFIG",
        default_value = "wasm-compose.yml"
    )]
    pub config: PathBuf,

    /// Skip validation of the composed output component.
    #[clap(long)]
    pub skip_validation: bool,
}

impl WasmComposeCommand {
    /// Executes the application.
    pub fn execute(self) -> Result<()> {
        let config = Config::from_file(&self.config)?;

        log::debug!("configuration:\n{:#?}", config);

        let output = match (self.output, &config.output) {
            (Some(output), _) => output,
            (None, Some(output)) => output.clone(),
            (None, None) => {
                bail!("output option is required");
            }
        };

        let bytes = ComponentComposer::new(&config).compose()?;

        std::fs::write(&output, &bytes).with_context(|| {
            format!("failed to write composed component `{}`", output.display())
        })?;

        if self.skip_validation || config.skip_validation {
            log::debug!("output validation was skipped");
        } else {
            Validator::new_with_features(WasmFeatures {
                component_model: true,
                ..Default::default()
            })
            .validate_all(&bytes)
            .with_context(|| {
                format!("failed to validate output component `{}`", output.display())
            })?;

            log::debug!("output component validated successfully");
        }

        println!("composed component `{output}`", output = output.display());

        Ok(())
    }
}
