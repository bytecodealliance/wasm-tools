//! Module for CLI parsing.

use anyhow::{Context, Result};
use clap::Parser;
use std::path::{Path, PathBuf};
use wasm_compose::{composer::ComponentComposer, config::Config};
use wasmparser::{Validator, WasmFeatures};

/// WebAssembly component composer.
///
/// A tool for composing WebAssembly components together.
#[derive(Parser)]
#[clap(name = "component-encoder", version = env!("CARGO_PKG_VERSION"))]
pub struct Opts {
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    #[clap(flatten)]
    output: wasm_tools::OutputArg,

    /// The path to the configuration file to use.
    #[clap(long, short = 'c', value_name = "CONFIG")]
    config: Option<PathBuf>,

    /// Definition components whose exports define import dependencies to fulfill from.
    #[clap(long = "definitions", short = 'd', value_name = "DEFS")]
    defs: Vec<PathBuf>,

    /// A path to search for imports.
    #[clap(long = "search-path", short = 'p', value_name = "PATH")]
    paths: Vec<PathBuf>,

    /// Skip validation of the composed output component.
    #[clap(long)]
    skip_validation: bool,

    /// Do not allow instance imports in the composed output component.
    #[clap(long = "no-imports")]
    disallow_imports: bool,

    /// The path to the root component to compose.
    #[clap(value_name = "COMPONENT")]
    component: PathBuf,

    /// Output the text format of WebAssembly instead of the binary format.
    #[clap(short = 't', long)]
    wat: bool,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    pub fn run(self) -> Result<()> {
        let config = self.create_config()?;
        log::debug!("configuration:\n{:#?}", config);

        let bytes = ComponentComposer::new(&self.component, &config).compose()?;

        self.output.output(wasm_tools::Output::Wasm {
            bytes: &bytes,
            wat: self.wat,
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
                let output = match self.output.output_path() {
                    Some(s) => format!(" `{}`", s.display()),
                    None => String::new(),
                };
                format!("failed to validate output component{output}")
            })?;

            log::debug!("output component validated successfully");
        }

        if let Some(path) = self.output.output_path() {
            println!("composed component `{}`", path.display());
        }

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
