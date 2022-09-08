//! Module for CLI parsing.

use anyhow::{Context, Result};
use clap::Parser;
use std::path::PathBuf;
use wasmparser::{Validator, WasmFeatures};

/// WebAssembly componentizer.
///
/// A tool for translating core WebAssembly modules (containing mangled
/// component type descriptions in the import & export names) into WebAssembly
/// Components.
#[derive(Debug, Parser)]
#[clap(name = "componentize", version = env!("CARGO_PKG_VERSION"))]
pub struct WasmComponentizeCommand {
    /// The path of the output WebAssembly component.
    #[clap(long, short = 'o', value_name = "OUTPUT")]
    pub output: PathBuf,

    /// Skip validation of the composed output component.
    #[clap(long)]
    pub skip_validation: bool,

    /// The path to the core WebAssembly input.
    #[clap(value_name = "INPUT")]
    pub input: PathBuf,
}

impl WasmComponentizeCommand {
    /// Executes the application.
    pub fn execute(self) -> Result<()> {
        let bytes = std::fs::read(&self.input).with_context(|| {
            format!(
                "failed to read input `{input}`",
                input = self.input.display()
            )
        })?;

        let bytes = crate::lift(&bytes)?;

        std::fs::write(&self.output, &bytes).with_context(|| {
            format!(
                "failed to write composed component `{output}`",
                output = self.output.display()
            )
        })?;

        if self.skip_validation {
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

        Ok(())
    }
}
