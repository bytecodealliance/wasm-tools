//! The WebAssembly component tool command line interface.

use std::collections::HashMap;
use std::io::Read;
use std::path::{Path, PathBuf};

use anyhow::{bail, Context, Result};
use clap::Parser;

use wasm_tools::Output;
use wit_component::{
    embed_component_metadata, is_wasm_binary_or_wat, parse_wit_from_path, ComponentEncoder,
    DecodedWasm, Linker, StringEncoding, WitPrinter,
};
use wit_parser::{Resolve, UnresolvedPackage};

/// WebAssembly wit-based component tooling.
#[derive(Parser)]
pub enum Opts {
    New(NewOpts),
    Wit(WitOpts),
    Embed(EmbedOpts),
    Targets(TargetsOpts),
    Link(LinkOpts),
}

impl Opts {
    pub fn run(self) -> Result<()> {
        match self {
            Opts::New(new) => new.run(),
            Opts::Wit(wit) => wit.run(),
            Opts::Embed(embed) => embed.run(),
            Opts::Targets(targets) => targets.run(),
            Opts::Link(link) => link.run(),
        }
    }

    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        match self {
            Opts::New(new) => new.general_opts(),
            Opts::Wit(wit) => wit.general_opts(),
            Opts::Embed(embed) => embed.general_opts(),
            Opts::Targets(targets) => targets.general_opts(),
            Opts::Link(link) => link.general_opts(),
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
                .file_name()
                .unwrap()
                .to_str()
                .unwrap();
            let name = match name.find('.') {
                Some(i) => &name[..i],
                None => name,
            };
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
/// wasm binary. The input core wasm binary must have metadata embedded within
/// it about the component-types used during its compilation. This is done
/// automatically for `wit-bindgen`-based projects, for example, and can be
/// manually done through the `wasm-tools component embed` subcommand.
///
/// This command will perform translation by collecting all type information
/// used during compilation of the core wasm module and will produce a component
/// with all of this type information resolved.
#[derive(Parser)]
pub struct NewOpts {
    /// The path to an adapter module to satisfy imports not otherwise bound to
    /// WIT interfaces.
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

    /// Skip validation of the output component.
    #[clap(long)]
    skip_validation: bool,

    /// Print the output in the WebAssembly text format instead of binary.
    #[clap(long, short = 't')]
    wat: bool,

    /// Use memory.grow to realloc memory and stack allocation.
    #[clap(long)]
    realloc_via_memory_grow: bool,
}

impl NewOpts {
    fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    /// Executes the application.
    fn run(self) -> Result<()> {
        let wasm = self.io.parse_input_wasm()?;
        let mut encoder = ComponentEncoder::default()
            .validate(!self.skip_validation)
            .module(&wasm)?;

        for (name, wasm) in self.adapters.iter() {
            encoder = encoder.adapter(name, wasm)?;
        }

        encoder = encoder.realloc_via_memory_grow(self.realloc_via_memory_grow);

        let bytes = encoder
            .encode()
            .context("failed to encode a component from module")?;

        self.io.output(Output::Wasm {
            bytes: &bytes,
            wat: self.wat,
        })?;

        Ok(())
    }
}

/// Embeds metadata for a component inside of a core wasm module.
///
/// This subcommand is a convenience tool provided for producing core wasm
/// binaries which will get consumed by `wasm-tools component new`. This will
/// embed metadata for a component within a core wasm binary as a custom
/// section.
///
/// This metadata describe the imports and exports of a core wasm module with a
/// WIT package's `world`. The metadata will be used when creating a full
/// component.
///
/// Note that this subcommand may not be required most of the time since most
/// language tooling will already embed this metadata in the final wasm binary
/// for you. This is primarily intended for one-off testing or for developers
/// working with text format wasm.
#[derive(Parser)]
pub struct EmbedOpts {
    /// The WIT package where the `world` that the core wasm module implements
    /// lives.
    ///
    /// This can either be a directory or a path to a single `*.wit` file.
    wit: PathBuf,

    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// The expected string encoding format for the component.
    ///
    /// Supported values are: `utf8` (default), `utf16`, and `compact-utf16`.
    /// This is only applicable to the `--wit` argument to describe the string
    /// encoding of the functions in that world.
    #[clap(long, value_name = "ENCODING")]
    encoding: Option<StringEncoding>,

    /// The world that the component uses.
    ///
    /// This is the path, within the `WIT` package provided as a positional
    /// argument, to the `world` that the core wasm module works with. This can
    /// either be a bare string which a document name that has a `default
    /// world`, or it can be a `foo/bar` name where `foo` names a document and
    /// `bar` names a world within that document.
    #[clap(short, long)]
    world: Option<String>,

    /// Don't read a core wasm module as input, instead generating a "dummy"
    /// module as a placeholder.
    ///
    /// This flag will generate a dummy core wasm module on the fly to match the
    /// `WIT` argument provided. This dummy module will have the correct
    /// imports/exports and the right signatures for the component model. This
    /// can be useful to, perhaps, inspect a template module and what it looks
    /// like to work with an interface in the component model.
    #[clap(long)]
    dummy: bool,

    /// Print the output in the WebAssembly text format instead of binary.
    #[clap(long, short = 't')]
    wat: bool,
}

impl EmbedOpts {
    fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    /// Executes the application.
    fn run(self) -> Result<()> {
        let wasm = if self.dummy {
            None
        } else {
            Some(self.io.parse_input_wasm()?)
        };
        let (resolve, id) = parse_wit_from_path(self.wit)?;
        let world = resolve.select_world(id, self.world.as_deref())?;

        let mut wasm = wasm.unwrap_or_else(|| wit_component::dummy_module(&resolve, world));

        embed_component_metadata(
            &mut wasm,
            &resolve,
            world,
            self.encoding.unwrap_or(StringEncoding::UTF8),
        )?;

        self.io.output(Output::Wasm {
            bytes: &wasm,
            wat: self.wat,
        })?;

        Ok(())
    }
}

fn parse_optionally_name_library(s: &str) -> (&str, &str) {
    let mut parts = s.splitn(2, '=');
    let name_or_path = parts.next().unwrap();
    match parts.next() {
        Some(path) => (name_or_path, path),
        None => {
            let name = Path::new(name_or_path)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap();
            (name, name_or_path)
        }
    }
}

fn parse_library(s: &str) -> Result<(String, Vec<u8>)> {
    let (name, path) = parse_optionally_name_library(s);
    let wasm = wat::parse_file(path)?;
    Ok((name.to_string(), wasm))
}

/// Link one or more dynamic library modules, producing a component
///
/// This is similar to the `new` subcommand, except that it accepts an arbitrary number of input modules rather
/// than a single "main" module.  Those modules are expected to conform to the [dynamic linking
/// convention](https://github.com/WebAssembly/tool-conventions/blob/main/DynamicLinking.md).
///
/// The resulting component's type will be the union of the types found in any `component-type*` custom sections in
/// the input modules.
///
/// See
/// https://github.com/WebAssembly/component-model/blob/main/design/mvp/examples/SharedEverythingDynamicLinking.md
/// for further details.
#[derive(Parser)]
pub struct LinkOpts {
    /// Input libraries to link
    #[clap(value_name = "[NAME=]MODULE", value_parser = parse_library)]
    inputs: Vec<(String, Vec<u8>)>,

    /// Input library to link and make available for dynamic resolution via `dlopen` (may be repeated)
    #[clap(long, value_name = "[NAME=]MODULE", value_parser = parse_library)]
    dl_openable: Vec<(String, Vec<u8>)>,

    /// The path to an adapter module to satisfy imports not otherwise bound to
    /// WIT interfaces.
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

    /// Size of stack (in bytes) to allocate in the synthesized main module
    #[clap(long)]
    stack_size: Option<u32>,

    #[clap(flatten)]
    output: wasm_tools::OutputArg,

    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    /// Skip validation of the output component.
    #[clap(long)]
    skip_validation: bool,

    /// Print the output in the WebAssembly text format instead of binary.
    #[clap(long, short = 't')]
    wat: bool,

    /// Generate trapping stubs for any missing functions
    #[clap(long)]
    stub_missing_functions: bool,
}

impl LinkOpts {
    fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    /// Executes the application.
    fn run(self) -> Result<()> {
        let mut linker = Linker::default()
            .validate(!self.skip_validation)
            .stub_missing_functions(self.stub_missing_functions);

        if let Some(stack_size) = self.stack_size {
            linker = linker.stack_size(stack_size);
        }

        for (name, wasm) in &self.inputs {
            linker = linker.library(name, wasm, false)?;
        }

        for (name, wasm) in &self.dl_openable {
            linker = linker.library(name, wasm, true)?;
        }

        for (name, wasm) in &self.adapters {
            linker = linker.adapter(name, wasm)?;
        }

        let bytes = linker
            .encode()
            .context("failed to encode a component from modules")?;

        self.output.output(Output::Wasm {
            bytes: &bytes,
            wat: self.wat,
        })?;

        Ok(())
    }
}

/// Tool for working with the WIT text format for components.
///
/// This subcommand can be used to inspect and debug the WIT text or binary
/// format with either WIT packages or binary components. Using this subcommand
/// a WIT package can be translated to binary, a WIT binary can be translated
/// back to text, and a WIT document can be extracted from a component binary to
/// inspect its interfaces.
#[derive(Parser)]
pub struct WitOpts {
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    /// Input file or directory to process.
    ///
    /// The file specified can be a `*.wit` file parsed as a single-document
    /// package. It can be a directory to be parsed as a WIT package. It can be
    /// a `*.wat` or `*.wasm` file for either the binary representation of a WIT
    /// package or a component itself to extract the interface from. The type of
    /// input is inferred from the contents of the path specified.
    ///
    /// If not provided or if this is `-` then stdin is read entirely and
    /// processed.
    input: Option<PathBuf>,

    #[clap(flatten)]
    output: wasm_tools::OutputArg,

    /// Emit a WebAssembly binary representation instead of the WIT text format.
    #[clap(short, long, conflicts_with = "wat", conflicts_with = "out_dir")]
    wasm: bool,

    /// Emit a WebAssembly textual representation instead of the WIT text
    /// format.
    #[clap(short = 't', long, conflicts_with = "wasm", conflicts_with = "out_dir")]
    wat: bool,

    /// Do not include doc comments when emitting WIT text.
    #[clap(long)]
    no_docs: bool,

    /// Emit the entire WIT resolution graph instead of just the "top level"
    /// package to the output directory specified.
    ///
    /// The output directory will contain textual WIT files which represent all
    /// packages known from the input.
    #[clap(
        long,
        conflicts_with = "wasm",
        conflicts_with = "wat",
        conflicts_with = "output"
    )]
    out_dir: Option<PathBuf>,

    /// Skips the validation performed when using the `--wasm` and `--wat`
    /// options.
    #[clap(long)]
    skip_validation: bool,

    /// Emit the WIT document as JSON instead of text.
    #[clap(
        short,
        long,
        conflicts_with = "wasm",
        conflicts_with = "out_dir",
        conflicts_with = "wat"
    )]
    json: bool,
}

impl WitOpts {
    fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    /// Executes the application.
    fn run(self) -> Result<()> {
        // First up determine the actual `DecodedWasm` as the input. This could
        // come from a number of sources:
        //
        // * If a `*.wat` or `*.wasm` is specified, use `wit_component::decode`
        // * If a directory is specified, parse it as a `Resolve`-oriented
        //   package with a `deps` directory optionally available.
        // * If a file is specified then it's just a normal wit package where
        //   deps can't be resolved.
        // * If no file is specified then parse the input as either `*.wat`,
        //   `*.wasm`, or `*.wit` and do as above.
        //
        // Eventually there will want to be more flags for things like
        // specifying a directory but specifying the WIT dependencies are
        // located elsewhere. This should be sufficient for now though.
        let decoded = match &self.input {
            Some(input) => match input.extension().and_then(|s| s.to_str()) {
                Some("wat") | Some("wasm") => {
                    let bytes = wat::parse_file(&input)?;
                    decode_wasm(&bytes).context("failed to decode WIT document")?
                }
                _ => {
                    let (resolve, id) = parse_wit_from_path(input)?;
                    DecodedWasm::WitPackage(resolve, id)
                }
            },
            None => {
                let mut stdin = Vec::new();
                std::io::stdin()
                    .read_to_end(&mut stdin)
                    .context("failed to read <stdin>")?;

                if is_wasm_binary_or_wat(&stdin) {
                    let bytes = wat::parse_bytes(&stdin).map_err(|mut e| {
                        e.set_path("<stdin>");
                        e
                    })?;

                    decode_wasm(&bytes).context("failed to decode WIT document")?
                } else {
                    let stdin = match std::str::from_utf8(&stdin) {
                        Ok(s) => s,
                        Err(_) => bail!("stdin was not valid utf-8"),
                    };
                    let mut resolve = Resolve::default();
                    let pkg = UnresolvedPackage::parse("<stdin>".as_ref(), stdin)?;
                    let id = resolve.push(pkg)?;
                    DecodedWasm::WitPackage(resolve, id)
                }
            }
        };

        // Now that the WIT document has been decoded, it's time to emit it.
        // This interprets all of the output options and performs such a task.
        if self.json {
            self.emit_json(&decoded)?;
            return Ok(());
        }
        if self.wasm || self.wat {
            self.emit_wasm(&decoded)?;
        } else {
            self.emit_wit(&decoded)?;
        }
        Ok(())
    }

    fn emit_wasm(&self, decoded: &DecodedWasm) -> Result<()> {
        assert!(self.wasm || self.wat);
        assert!(self.out_dir.is_none());

        let bytes = wit_component::encode(decoded.resolve(), decoded.package())?;
        if !self.skip_validation {
            wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
                component_model: true,
                ..Default::default()
            })
            .validate_all(&bytes)?;
        }
        self.output.output(Output::Wasm {
            bytes: &bytes,
            wat: self.wat,
        })?;
        Ok(())
    }

    fn emit_wit(&self, decoded: &DecodedWasm) -> Result<()> {
        assert!(!self.wasm && !self.wat);

        let resolve = decoded.resolve();
        let main = decoded.package();

        let mut printer = WitPrinter::default();
        printer.emit_docs(!self.no_docs);

        match &self.out_dir {
            Some(dir) => {
                assert!(self.output.output_path().is_none());
                std::fs::create_dir_all(dir)
                    .with_context(|| format!("failed to create directory: {dir:?}"))?;

                // Classify all packages by name to determine how to name their
                // output directories.
                let mut names = HashMap::new();
                for (_id, pkg) in resolve.packages.iter() {
                    let cnt = names
                        .entry(&pkg.name.name)
                        .or_insert(HashMap::new())
                        .entry(&pkg.name.namespace)
                        .or_insert(0);
                    *cnt += 1;
                }

                for (id, pkg) in resolve.packages.iter() {
                    let output = printer.print(resolve, id)?;
                    let out_dir = if id == main {
                        dir.clone()
                    } else {
                        let dir = dir.join("deps");
                        let packages_with_same_name = &names[&pkg.name.name];
                        if packages_with_same_name.len() == 1 {
                            dir.join(&pkg.name.name)
                        } else {
                            let packages_with_same_namespace =
                                packages_with_same_name[&pkg.name.namespace];
                            if packages_with_same_namespace == 1 {
                                dir.join(format!("{}:{}", pkg.name.namespace, pkg.name.name))
                            } else {
                                dir.join(pkg.name.to_string())
                            }
                        }
                    };
                    std::fs::create_dir_all(&out_dir)
                        .with_context(|| format!("failed to create directory: {out_dir:?}"))?;
                    let path = out_dir.join("main.wit");
                    std::fs::write(&path, &output)
                        .with_context(|| format!("failed to write file: {path:?}"))?;
                    println!("Writing: {}", path.display());
                }
            }
            None => {
                let output = printer.print(resolve, main)?;
                self.output.output(Output::Wat(&output))?;
            }
        }

        Ok(())
    }

    fn emit_json(&self, decoded: &DecodedWasm) -> Result<()> {
        assert!(!self.wasm && !self.wat);

        let resolve = decoded.resolve();
        let output = serde_json::to_string_pretty(&resolve)?;
        self.output.output(Output::Json(&output))?;

        Ok(())
    }
}

/// Tool for verifying whether a component conforms to a world.
#[derive(Parser)]
pub struct TargetsOpts {
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    /// The WIT package containing the `world` used to test a component for conformance.
    ///
    /// This can either be a directory or a path to a single `*.wit` file.
    wit: PathBuf,

    /// The world used to test whether a component conforms to its signature.
    #[clap(short, long)]
    world: Option<String>,

    #[clap(flatten)]
    input: wasm_tools::InputArg,
}

impl TargetsOpts {
    fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    /// Executes the application.
    fn run(self) -> Result<()> {
        let (resolve, package_id) = parse_wit_from_path(&self.wit)?;
        let world = resolve.select_world(package_id, self.world.as_deref())?;
        let component_to_test = self.input.parse_wasm()?;

        wit_component::targets(&resolve, world, &component_to_test)?;

        Ok(())
    }
}

fn decode_wasm(bytes: &[u8]) -> Result<DecodedWasm> {
    if wasmparser::Parser::is_component(bytes) {
        wit_component::decode(bytes)
    } else {
        let (_wasm, bindgen) = wit_component::metadata::decode(bytes)?;
        Ok(DecodedWasm::Component(bindgen.resolve, bindgen.world))
    }
}
