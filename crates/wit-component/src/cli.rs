//! The WebAssembly component tool command line interface.

#![deny(missing_docs)]

use crate::extract::{extract_module_interfaces, ModuleInterfaces};
use crate::{
    decode_interface_component, ComponentEncoder, ComponentInterfaces, InterfacePrinter,
    StringEncoding,
};
use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;
use std::path::{Path, PathBuf};
use wit_parser::Interface;

fn parse_optionally_name_file(s: &str) -> (&str, &str) {
    let mut parts = s.splitn(2, '=');
    let name_or_path = parts.next().unwrap();
    match parts.next() {
        Some(path) => (name_or_path, path),
        None => {
            let name = Path::new(name_or_path)
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap();
            (name, name_or_path)
        }
    }
}

fn parse_named_interface(s: &str) -> Result<Interface> {
    let (name, path) = parse_optionally_name_file(s);
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

fn parse_adapter(s: &str) -> Result<(String, Vec<u8>, Interface)> {
    let mut parts = s.splitn(2, ':');
    let maybe_named_module = parts.next().unwrap();
    let (name, path) = parse_optionally_name_file(maybe_named_module);
    let wasm = wat::parse_file(path)?;

    match parts.next() {
        Some(maybe_named_interface) => {
            let interface = parse_named_interface(maybe_named_interface)?;
            Ok((name.to_string(), wasm, interface))
        }
        None => {
            let ModuleInterfaces {
                wasm,
                interfaces:
                    ComponentInterfaces {
                        imports,
                        exports,
                        default,
                    },
            } = extract_module_interfaces(&wasm)?;
            if exports.len() > 0 || default.is_some() {
                bail!("adapter modules cannot have an exported interface");
            }
            let import = match imports.len() {
                0 => Interface::default(),
                1 => imports.into_iter().next().unwrap().1,
                _ => bail!("adapter modules can only import one interface at this time"),
            };
            Ok((name.to_string(), wasm, import))
        }
    }
}

/// WebAssembly component encoder.
///
/// Encodes a WebAssembly component from a core WebAssembly module.
#[derive(Debug, Parser)]
#[clap(name = "component-encoder", version = env!("CARGO_PKG_VERSION"))]
pub struct WitComponentApp {
    /// The path to an interface definition file the component imports.
    #[clap(long = "import", value_name = "[NAME=]INTERFACE", value_parser = parse_named_interface)]
    pub imports: Vec<Interface>,

    /// The path to an interface definition file the component exports.
    #[clap(long = "export", value_name = "[NAME=]INTERFACE", value_parser = parse_named_interface)]
    pub exports: Vec<Interface>,

    /// The path to an adapter module to satisfy imports.
    ///
    /// An adapter module can be used to translate the `wasi_snapshot_preview1`
    /// ABI, for example, to one that uses the component model. The first
    /// `[NAME=]` specified in the argument is inferred from the name of file
    /// specified by `MODULE` if not present and is the name of the import
    /// module that's being implemented (e.g. `wasi_snapshot_preview1.wasm`.
    ///
    /// The second part of this argument, optionally specified, is the interface
    /// that this adapter module imports. If not specified then the interface
    /// imported is inferred from the adapter module itself.
    #[clap(long = "adapt", value_name = "[NAME=]MODULE[:[NAME=]INTERFACE]", value_parser = parse_adapter)]
    pub adapters: Vec<(String, Vec<u8>, Interface)>,

    /// The path of the output WebAssembly component.
    #[clap(long, short = 'o', value_name = "OUTPUT")]
    pub output: Option<PathBuf>,

    /// The default interface the component exports.
    #[clap(long, short = 'i', value_name = "INTERFACE", value_parser = parse_unnamed_interface)]
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
            .module(&module)?
            .imports(self.imports)?
            .exports(self.exports)?
            .validate(!self.skip_validation);

        for (name, wasm, interface) in self.adapters.iter() {
            encoder = encoder.adapter(name, wasm, interface);
        }

        if let Some(interface) = self.interface {
            encoder = encoder.interface(interface)?;
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

/// WebAssembly interface decoder.
///
/// Decodes a WebAssembly interface from a WebAssembly component.
#[derive(Debug, Parser)]
#[clap(name = "wit2wasm", version = env!("CARGO_PKG_VERSION"))]
pub struct WasmToWitApp {
    /// The path of the output WebAssembly interface file.
    #[clap(long, short = 'o', value_name = "OUTPUT")]
    pub output: Option<PathBuf>,

    /// Print the "default" interface for a component.
    #[clap(long, short)]
    pub interface: bool,

    /// Print the interface of the specified import.
    #[clap(long)]
    pub import: Option<String>,

    /// Print the interface of the specified export.
    #[clap(long)]
    pub export: Option<String>,

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

        let interfaces = decode_interface_component(&bytes).with_context(|| {
            format!("failed to decode component `{}`", self.component.display())
        })?;
        let which = match &self.import {
            Some(s) => interfaces
                .imports
                .get(s.as_str())
                .ok_or_else(|| anyhow!("no import interface named `{s}`"))?,
            None => match &self.export {
                Some(s) => interfaces
                    .exports
                    .get(s.as_str())
                    .ok_or_else(|| anyhow!("no export interface named `{s}`"))?,
                None => {
                    if self.interface {
                        interfaces
                            .default
                            .as_ref()
                            .ok_or_else(|| anyhow!("no default interface"))?
                    } else {
                        bail!("must specify `-i`, `--import`, or `--export`")
                    }
                }
            },
        };

        let mut printer = InterfacePrinter::default();

        std::fs::write(&output, printer.print(which)?)
            .with_context(|| format!("failed to write output file `{}`", output.display()))?;

        println!("decoded interface to `{}`", output.display());

        Ok(())
    }
}
