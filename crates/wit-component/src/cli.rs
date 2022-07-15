//! The WebAssembly component tool command line interface.

#![deny(missing_docs)]

use crate::{
    decode_interface_component, ComponentEncoder, InterfaceEncoder, InterfacePrinter,
    StringEncoding,
};
use anyhow::{bail, Context, Result};
use clap::Parser;
use std::path::{Path, PathBuf};
use wit_parser::Interface;

fn parse_named_interface(s: &str) -> Result<Interface> {
    let (name, path) = s
        .split_once('=')
        .ok_or_else(|| anyhow::anyhow!("expected a value with format `NAME=INTERFACE`"))?;

    parse_interface(Some(name.to_string()), Path::new(path))
}

fn parse_unnamed_interface(s: &str) -> Result<Interface> {
    parse_interface(None, Path::new(s))
}

fn parse_interface(name: Option<String>, path: &Path) -> Result<Interface> {
    if !path.is_file() {
        bail!("interface file `{}` does not exist", path.display(),);
    }

    let mut interface = Interface::parse_file(&path)
        .with_context(|| format!("failed to parse interface file `{}`", path.display()))?;

    interface.name = name.unwrap_or_else(|| "".to_string());

    Ok(interface)
}

/// WebAssembly component encoder.
///
/// Encodes a WebAssembly component from a core WebAssembly module.
#[derive(Debug, Parser)]
#[clap(name = "component-encoder", version = env!("CARGO_PKG_VERSION"))]
pub struct WitComponentApp {
    /// The path to an interface definition file the component imports.
    #[clap(long = "import", value_name = "NAME=INTERFACE", parse(try_from_str = parse_named_interface))]
    pub imports: Vec<Interface>,

    /// The path to an interface definition file the component exports.
    #[clap(long = "export", value_name = "NAME=INTERFACE", parse(try_from_str = parse_named_interface))]
    pub exports: Vec<Interface>,

    /// The path of the output WebAssembly component.
    #[clap(long, short = 'o', value_name = "OUTPUT")]
    pub output: Option<PathBuf>,

    /// The default interface the component exports.
    #[clap(long, short = 'i', value_name = "INTERFACE", parse(try_from_str = parse_unnamed_interface))]
    pub interface: Option<Interface>,

    /// Skip validation of the output component.
    #[clap(long)]
    pub skip_validation: bool,

    /// The expected string encoding format for the component.
    /// Supported values are: `utf8` (default), `utf16`, and `compact-utf16`.
    #[clap(long, value_name = "ENCODING")]
    pub encoding: Option<StringEncoding>,

    /// Path to the WebAssembly module to encode.
    #[clap(index = 1, value_name = "MODULE")]
    pub module: PathBuf,
}

impl WitComponentApp {
    /// Executes the application.
    pub fn execute(self) -> Result<()> {
        if !self.module.is_file() {
            bail!(
                "module `{}` does not exist as a file",
                self.module.display()
            );
        }

        let output = self.output.unwrap_or_else(|| {
            let mut stem: PathBuf = self.module.file_stem().unwrap().into();
            stem.set_extension("wasm");
            stem
        });

        let module = wat::parse_file(&self.module)
            .with_context(|| format!("failed to parse module `{}`", self.module.display()))?;

        let mut encoder = ComponentEncoder::default()
            .module(&module)
            .imports(&self.imports)
            .exports(&self.exports)
            .validate(!self.skip_validation);

        if let Some(interface) = &self.interface {
            encoder = encoder.interface(interface);
        }

        if let Some(encoding) = &self.encoding {
            encoder = encoder.encoding(*encoding);
        }

        let bytes = encoder.encode().with_context(|| {
            format!(
                "failed to encode a component from module `{}`",
                self.module.display()
            )
        })?;

        std::fs::write(&output, bytes)
            .with_context(|| format!("failed to write output file `{}`", output.display()))?;

        println!("encoded component `{}`", output.display());

        Ok(())
    }
}

/// WebAssembly interface encoder.
///
/// Encodes a WebAssembly interface as a WebAssembly component.
#[derive(Debug, Parser)]
#[clap(name = "wit2wasm", version = env!("CARGO_PKG_VERSION"))]
pub struct WitToWasmApp {
    /// The path of the output WebAssembly component.
    #[clap(long, short = 'o', value_name = "OUTPUT")]
    pub output: Option<PathBuf>,

    /// The path to the WebAssembly interface file to encode.
    #[clap(index = 1, value_name = "INTERFACE")]
    pub interface: PathBuf,
}

impl WitToWasmApp {
    /// Executes the application.
    pub fn execute(self) -> Result<()> {
        let output = self.output.unwrap_or_else(|| {
            let mut stem: PathBuf = self.interface.file_stem().unwrap().into();
            stem.set_extension("wasm");
            stem
        });

        let interface = parse_interface(None, &self.interface)?;

        let encoder = InterfaceEncoder::new(&interface).validate(true);

        let bytes = encoder.encode().with_context(|| {
            format!(
                "failed to encode a component from interface `{}`",
                self.interface.display()
            )
        })?;

        std::fs::write(&output, bytes)
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
    #[clap(long, short = 'o', value_name = "OUTPUT")]
    pub output: Option<PathBuf>,

    /// The path to the WebAssembly component to decode.
    #[clap(index = 1, value_name = "COMPONENT")]
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
