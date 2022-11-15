//! The WebAssembly component tool command line interface.

use crate::{decode_component_interfaces, ComponentEncoder, StringEncoding, WorldPrinter};
use anyhow::{bail, Context, Result};
use clap::Parser;
use std::io::Write;
use std::path::{Path, PathBuf};
use wit_parser::World;

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

fn parse_adapter(s: &str) -> Result<(String, Vec<u8>)> {
    let (name, path) = parse_optionally_name_file(s);
    let wasm = wat::parse_file(path)?;
    Ok((name.to_string(), wasm))
}

/// WebAssembly component encoder.
///
/// Encodes a WebAssembly component from a core WebAssembly module.
#[derive(Debug, Parser)]
#[clap(name = "component-encoder", version = env!("CARGO_PKG_VERSION"))]
pub struct WitComponentApp {
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
    #[clap(long = "adapt", value_name = "[NAME=]MODULE", value_parser = parse_adapter)]
    pub adapters: Vec<(String, Vec<u8>)>,

    /// The path of the output WebAssembly component.
    #[clap(long, short, value_name = "OUTPUT")]
    pub output: Option<PathBuf>,

    /// The default interface the component exports.
    #[clap(long, value_name = "PATH")]
    pub world: Option<PathBuf>,

    /// Skip validation of the output component.
    #[clap(long)]
    pub skip_validation: bool,

    /// The expected string encoding format for the component.
    /// Supported values are: `utf8` (default), `utf16`, and `compact-utf16`.
    #[clap(long, value_name = "ENCODING")]
    pub encoding: Option<StringEncoding>,

    /// Path to the WebAssembly module to encode.
    #[clap(index = 1, value_name = "MODULE")]
    pub module: Option<PathBuf>,

    /// Print the output in the WebAssembly text format instead of binary.
    #[clap(long, short)]
    pub text: bool,
}

impl WitComponentApp {
    /// Executes the application.
    pub fn execute(self) -> Result<()> {
        let mut encoder = ComponentEncoder::default().validate(!self.skip_validation);

        match &self.module {
            Some(module) => {
                let module = wat::parse_file(module)
                    .with_context(|| format!("failed to parse module `{}`", module.display()))?;
                encoder = encoder.module(&module)?;
            }
            None => {
                encoder = encoder.types_only(true);
            }
        }

        if let Some(world) = &self.world {
            let encoding = self.encoding.unwrap_or(StringEncoding::UTF8);
            let world = World::parse_file(world)?;
            encoder = encoder.interfaces(world.into(), encoding)?;
        }

        for (name, wasm) in self.adapters.iter() {
            encoder = encoder.adapter(name, wasm)?;
        }

        let bytes = encoder
            .encode()
            .with_context(|| format!("failed to encode a component from module ",))?;

        let bytes = if self.text {
            wasmprinter::print_bytes(&bytes)?.into_bytes()
        } else {
            bytes
        };

        match &self.output {
            Some(path) => {
                std::fs::write(path, bytes)
                    .with_context(|| format!("failed to write output file `{}`", path.display()))?;

                println!("encoded component `{}`", path.display());
            }

            None => {
                if !self.text && atty::is(atty::Stream::Stdout) {
                    bail!("cannot print binary wasm output to a terminal, pass the `-t` flag to print the text format");
                }
                std::io::stdout()
                    .write_all(&bytes)
                    .context("failed to write to stdout")?;
            }
        }

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

    /// The name of the world to generate.
    #[clap(long)]
    pub name: Option<String>,

    /// The path to the WebAssembly component to decode.
    #[clap(index = 1, value_name = "COMPONENT")]
    pub component: PathBuf,
}

impl WasmToWitApp {
    /// Executes the application.
    pub fn execute(self) -> Result<()> {
        let stem = self.component.file_stem().unwrap().to_str().unwrap();
        let name = match &self.name {
            Some(name) => name.as_str(),
            None => stem,
        };
        let output = self.output.unwrap_or_else(|| format!("{name}.wit").into());

        if !self.component.is_file() {
            bail!(
                "component `{}` does not exist as a file",
                self.component.display()
            );
        }

        let bytes = wat::parse_file(&self.component)
            .with_context(|| format!("failed to parse component `{}`", self.component.display()))?;

        let interfaces = decode_component_interfaces(&bytes).with_context(|| {
            format!("failed to decode component `{}`", self.component.display())
        })?;
        let world = interfaces.into_world(name);

        let mut printer = WorldPrinter::default();

        std::fs::write(&output, printer.print(&world)?)
            .with_context(|| format!("failed to write output file `{}`", output.display()))?;

        println!("decoded interface to `{}`", output.display());

        Ok(())
    }
}
