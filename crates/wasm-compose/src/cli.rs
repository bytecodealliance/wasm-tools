//! Module for CLI parsing.

use crate::{composer::ComponentComposer, config::Config};
use anyhow::{Context, Result};
use clap::Parser;
use std::path::{Path, PathBuf};
use wasmparser::{Validator, WasmFeatures};

/// WebAssembly component composer.
///
/// A tool for composing WebAssembly components together.
#[derive(Debug, Parser)]
#[clap(name = "component-encoder", version = env!("CARGO_PKG_VERSION"))]
pub struct WasmComposeCommand {
    /// The path of the output composed WebAssembly component.
    #[clap(long, short = 'o', value_name = "OUTPUT")]
    pub output: PathBuf,

    /// The path to the configuration file to use.
    #[clap(long, short = 'c', value_name = "CONFIG")]
    pub config: Option<PathBuf>,

    /// Definition components whose exports define import dependencies to fulfill from.
    #[clap(long = "definitions", short = 'd', value_name = "DEFS")]
    pub defs: Vec<PathBuf>,

    /// A path to search for imports.
    #[clap(long = "search-path", short = 'p', value_name = "PATH")]
    pub paths: Vec<PathBuf>,

    /// Skip validation of the composed output component.
    #[clap(long)]
    pub skip_validation: bool,

    /// Do not allow instance imports in the composed output component.
    #[clap(long = "no-imports")]
    pub disallow_imports: bool,

    /// The path to the root component to compose.
    #[clap(value_name = "COMPONENT")]
    pub component: PathBuf,
}

impl WasmComposeCommand {
    /// Executes the application.
    pub fn execute(self) -> Result<()> {
        let config = self.create_config()?;
        log::debug!("configuration:\n{:#?}", config);

        let bytes = ComponentComposer::new(&self.component, &config).compose()?;

        std::fs::write(&self.output, &bytes).with_context(|| {
            format!(
                "failed to write composed component `{output}`",
                output = self.output.display()
            )
        })?;

        if config.skip_validation {
            log::debug!("output validation was skipped");
        } else {
            Validator::new_with_features(WasmFeatures {
                component_model: true,
                ..Default::default()
            })
            .validate_all(&bytes)
            .with_context(|| {
                format!(
                    "failed to validate output component `{output}`",
                    output = self.output.display()
                )
            })?;

            log::debug!("output component validated successfully");
        }

        println!(
            "composed component `{output}`",
            output = self.output.display()
        );

        Ok(())
    }

    fn create_config(&self) -> Result<Config> {
        let mut config = if let Some(config) = &self.config {
            Config::from_file(config)?
        } else {
            // Pretend a default configuration file is sitting next to the component
            Config {
                dir: self
                    .component
                    .parent()
                    .map(Path::to_path_buf)
                    .unwrap_or_default(),
                ..Default::default()
            }
        };

        // Use paths relative to the current directory; otherwise, the paths are interpreted as
        // relative to the configuration file.
        let cur_dir = std::env::current_dir().context("failed to get current directory")?;
        config
            .definitions
            .extend(self.defs.iter().map(|p| cur_dir.join(p)));
        config
            .search_paths
            .extend(self.paths.iter().map(|p| cur_dir.join(p)));
        config.skip_validation |= self.skip_validation;
        config.disallow_imports |= self.disallow_imports;
        Ok(config)
    }
}
