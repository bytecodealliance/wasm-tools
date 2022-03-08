//! The WebAssembly component tool command line interface.

#![deny(missing_docs)]

use crate::{decode_interface_component, encode_interface_component, InterfacePrinter};
use anyhow::{bail, Context, Result};
use clap::Parser;
use std::path::PathBuf;

/// WebAssembly interface encoder.
///
/// Encodes a WebAssembly interface as a WebAssembly component.
#[derive(Debug, Parser)]
#[clap(name = "wit2wasm", version = env!("CARGO_PKG_VERSION"))]
pub struct WitToWasmApp {
    /// The path of the output WebAssembly component.
    #[clap(long, short = 'o', value_name = "OUTPUT", parse(from_os_str))]
    pub output: Option<PathBuf>,

    /// The name of the interface to encode (defaults to interface file name).
    #[clap(long, short = 'n', value_name = "NAME")]
    pub name: Option<String>,

    /// The path to the WebAssembly interface file to encode.
    #[clap(index = 1, value_name = "INTERFACE", parse(from_os_str))]
    pub interface: PathBuf,
}

impl WitToWasmApp {
    /// Executes the application.
    pub fn execute(self) -> Result<()> {
        let name = self
            .name
            .as_deref()
            .unwrap_or_else(|| self.interface.file_stem().unwrap().to_str().unwrap());

        let output = self.output.unwrap_or_else(|| {
            let mut stem: PathBuf = self.interface.file_stem().unwrap().into();
            stem.set_extension("wasm");
            stem
        });

        std::fs::write(&output, encode_interface_component(name, &self.interface)?)
            .with_context(|| format!("failed to write output file `{}`", output.display()))?;

        println!("encoded interface as component `{}`", output.display());

        Ok(())
    }
}

/// WebAssembly interface decoder.
///
/// Decodes a WebAssembly interface from a WebAssembly component.
#[derive(Debug, Parser)]
#[clap(name = "wit2wasm", version = env!("CARGO_PKG_VERSION"))]
pub struct WasmToWitApp {
    /// The path of the output WebAssembly interface file.
    #[clap(long, short = 'o', value_name = "OUTPUT", parse(from_os_str))]
    pub output: Option<PathBuf>,

    /// The path to the WebAssembly component to decode.
    #[clap(index = 1, value_name = "COMPONENT", parse(from_os_str))]
    pub component: PathBuf,
}

impl WasmToWitApp {
    /// Executes the application.
    pub fn execute(self) -> Result<()> {
        let output = self.output.unwrap_or_else(|| {
            let mut stem: PathBuf = self.component.file_stem().unwrap().into();
            stem.set_extension("wit");
            stem
        });

        if !self.component.is_file() {
            bail!(
                "component `{}` does not exist as a file",
                self.component.display()
            );
        }

        let bytes = wat::parse_file(&self.component)
            .with_context(|| format!("failed to parse component `{}`", self.component.display()))?;

        let interface = decode_interface_component(&bytes).with_context(|| {
            format!("failed to decode component `{}`", self.component.display())
        })?;

        let mut printer = InterfacePrinter::default();

        std::fs::write(&output, printer.print(&interface)?)
            .with_context(|| format!("failed to write output file `{}`", output.display()))?;

        println!("decoded interface to `{}`", output.display());

        Ok(())
    }
}
