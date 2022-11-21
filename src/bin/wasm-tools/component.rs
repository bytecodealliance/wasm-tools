//! The WebAssembly component tool command line interface.

use anyhow::{Context, Result};
use clap::Parser;
use std::path::{Path, PathBuf};
use wasm_tools::Output;
use wit_component::{decode_world, ComponentEncoder, StringEncoding, WorldPrinter};
use wit_parser::World;

/// WebAssembly wit-based component tooling.
#[derive(Parser)]
pub enum Opts {
    New(NewOpts),
    Wit(WitOpts),
}

impl Opts {
    pub fn run(self) -> Result<()> {
        match self {
            Opts::New(new) => new.run(),
            Opts::Wit(wit) => wit.run(),
        }
    }
}

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

/// WebAssembly component encoder from an input core wasm binary.
///
/// This subcommand will create a new component `*.wasm` file from an input core
/// wasm binary. The input core wasm binary is expected to be compiled with
/// `wit-component` or derivative projects which encodes component-based type
/// information into the input core wasm binary's custom sections. The `--wit`
/// option can also be used to specify the interface manually too.
#[derive(Parser)]
pub struct NewOpts {
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
    adapters: Vec<(String, Vec<u8>)>,

    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// The "world" that the input binary implements.
    ///
    /// This argument is a `*.wit` file which describes the imports and exports
    /// of the core wasm module. Users of `wit-bindgen` don't need this as those
    /// generators already embed this information into the input core wasm
    /// binary.
    #[clap(long, value_name = "PATH")]
    wit: Option<PathBuf>,

    /// Skip validation of the output component.
    #[clap(long)]
    skip_validation: bool,

    /// The expected string encoding format for the component.
    ///
    /// Supported values are: `utf8` (default), `utf16`, and `compact-utf16`.
    /// This is only applicable to the `--wit` argument to describe the string
    /// encoding of the functions in that world.
    #[clap(long, value_name = "ENCODING")]
    encoding: Option<StringEncoding>,

    /// Print the output in the WebAssembly text format instead of binary.
    #[clap(long, short = 't')]
    wat: bool,

    /// Generate a "types only" component which is a binary encoding of the
    /// input wit file or the wit already encoded into the module.
    #[clap(long)]
    types_only: bool,
}

impl NewOpts {
    /// Executes the application.
    fn run(self) -> Result<()> {
        let wasm = if self.types_only {
            self.io.init_logger();
            None
        } else {
            Some(self.io.parse_input_wasm()?)
        };
        let mut encoder = ComponentEncoder::default()
            .validate(!self.skip_validation)
            .types_only(self.types_only);

        if let Some(wasm) = wasm {
            encoder = encoder.module(&wasm)?;
        }

        if let Some(wit) = &self.wit {
            let encoding = self.encoding.unwrap_or(StringEncoding::UTF8);
            let world = World::parse_file(wit)?;
            encoder = encoder.world(world, encoding)?;
        }

        for (name, wasm) in self.adapters.iter() {
            encoder = encoder.adapter(name, wasm)?;
        }

        let bytes = encoder
            .encode()
            .with_context(|| format!("failed to encode a component from module ",))?;

        self.io.output(Output::Wasm {
            bytes: &bytes,
            wat: self.wat,
        })?;

        Ok(())
    }
}

/// WebAssembly interface printer.
///
/// Decodes a `*.wit` file from a binary WebAssembly component.
#[derive(Parser)]
pub struct WitOpts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// The name of the world to generate, inferred by default from the input
    /// filename.
    #[clap(long)]
    name: Option<String>,
}

impl WitOpts {
    /// Executes the application.
    fn run(self) -> Result<()> {
        let bytes = self.io.parse_input_wasm()?;
        let name = match &self.name {
            Some(name) => name.as_str(),
            None => match self.io.input_path() {
                Some(path) => path.file_stem().unwrap().to_str().unwrap(),
                None => "component",
            },
        };

        let world = decode_world(name, &bytes).context("failed to decode world")?;
        let mut printer = WorldPrinter::default();
        let output = printer.print(&world)?;
        self.io.output(Output::Wat(&output))?;

        Ok(())
    }
}
