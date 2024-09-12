//! The WebAssembly component tool command line interface.

use anyhow::{bail, Context, Result};
use clap::Parser;
use std::collections::HashMap;
use std::io::Read;
use std::mem;
use std::path::{Path, PathBuf};
use wasm_encoder::reencode::{Error, Reencode, ReencodeComponent, RoundtripReencoder};
use wasm_encoder::ModuleType;
use wasm_tools::Output;
use wasmparser::types::{CoreTypeId, EntityType, Types};
use wasmparser::{Payload, ValidPayload};
use wat::Detect;
use wit_component::{
    embed_component_metadata, ComponentEncoder, DecodedWasm, Linker, StringEncoding, WitPrinter,
};
use wit_parser::{PackageId, Resolve};

/// WebAssembly wit-based component tooling.
#[derive(Parser)]
pub enum Opts {
    New(NewOpts),
    Wit(WitOpts),
    Embed(EmbedOpts),
    Targets(TargetsOpts),
    Link(LinkOpts),
    SemverCheck(SemverCheckOpts),
    Unbundle(UnbundleOpts),
}

impl Opts {
    pub fn run(self) -> Result<()> {
        match self {
            Opts::New(new) => new.run(),
            Opts::Wit(wit) => wit.run(),
            Opts::Embed(embed) => embed.run(),
            Opts::Targets(targets) => targets.run(),
            Opts::Link(link) => link.run(),
            Opts::SemverCheck(s) => s.run(),
            Opts::Unbundle(s) => s.run(),
        }
    }

    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        match self {
            Opts::New(new) => new.general_opts(),
            Opts::Wit(wit) => wit.general_opts(),
            Opts::Embed(embed) => embed.general_opts(),
            Opts::Targets(targets) => targets.general_opts(),
            Opts::Link(link) => link.general_opts(),
            Opts::SemverCheck(s) => s.general_opts(),
            Opts::Unbundle(s) => s.general_opts(),
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

fn parse_import_name(s: &str) -> Result<(String, String)> {
    s.split_once('=')
        .map(|(old, new)| (old.to_string(), new.to_string()))
        .context("expected `--import-name` option to be of the form `OLD=NEW`")
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
    /// module that's being implemented (e.g. `wasi_snapshot_preview1.wasm`).
    ///
    /// The second part of this argument, optionally specified, is the interface
    /// that this adapter module imports. If not specified then the interface
    /// imported is inferred from the adapter module itself.
    #[clap(long = "adapt", value_name = "[NAME=]MODULE", value_parser = parse_adapter)]
    adapters: Vec<(String, Vec<u8>)>,

    /// Rename an instance import in the output component.
    ///
    /// This may be used to rename instance imports in the final component.
    ///
    /// For example, `--import-name "a:b/c=unlocked-dep=<d:e/f>"` will
    /// rename the instance import `a:b/c` such that it becomes an `unlocked-dep`
    /// name.
    ///
    /// If the old import name is not found, it is ignored.
    #[clap(long = "import-name", value_name = "[OLD]=NEW", value_parser = parse_import_name)]
    import_names: Vec<(String, String)>,

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
            .import_name_map(self.import_names.into_iter().collect())
            .encode()
            .context("failed to encode a component from module")?;

        self.io.output_wasm(&bytes, self.wat)?;

        Ok(())
    }
}

#[derive(Parser)]
struct WitResolve {
    /// Path to WIT files to load.
    ///
    /// This can be a directory containing `*.wit` files, a `*.wit` file itself,
    /// or a `*.wasm` file which is a WIT package encoded as WebAssembly.
    wit: PathBuf,

    /// Features to enable when parsing the `wit` option.
    ///
    /// This flag enables the `@unstable` feature in WIT documents where the
    /// items are otherwise hidden by default.
    #[clap(long)]
    features: Vec<String>,

    /// Enable all features when parsing the `wit` option.
    ///
    /// This flag enables all `@unstable` features in WIT documents where the
    /// items are otherwise hidden by default.
    #[clap(long)]
    all_features: bool,
}

impl WitResolve {
    fn resolve_with_features(features: &[String], all_features: bool) -> Resolve {
        let mut resolve = Resolve::default();
        resolve.all_features = all_features;
        for feature in features {
            for f in feature.split_whitespace() {
                for f in f.split(',').filter(|s| !s.is_empty()) {
                    resolve.features.insert(f.to_string());
                }
            }
        }
        return resolve;
    }

    fn load(&self) -> Result<(Resolve, PackageId)> {
        let mut resolve = Self::resolve_with_features(&self.features, self.all_features);
        let (pkg_id, _) = resolve.push_path(&self.wit)?;
        Ok((resolve, pkg_id))
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
    #[clap(flatten)]
    resolve: WitResolve,

    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// The expected string encoding format for the component.
    ///
    /// Supported values are: `utf8` (default), `utf16`, and `compact-utf16`.
    /// This is only applicable to the `wit` argument to describe the string
    /// encoding of the functions in that world.
    #[clap(long, value_name = "ENCODING")]
    encoding: Option<StringEncoding>,

    /// The world that the component uses.
    ///
    /// This is the path, within the `WIT` source provided as a positional
    /// argument, to the `world` that the core wasm module works with. If this
    /// option is omitted then the "main package" pointed to by `WIT` must have
    /// a single world and that's what is used to embed. Otherwise this could be
    /// a bare string `foo` to point to the `world foo` within the main
    /// package of WIT. Finally this can be a fully qualified name too such as
    /// `wasi:http/proxy` which can select a world from a WIT dependency as
    /// well.
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
        let (resolve, pkg_id) = self.resolve.load()?;
        let world = resolve.select_world(pkg_id, self.world.as_deref())?;
        let mut wasm = wasm.unwrap_or_else(|| wit_component::dummy_module(&resolve, world));

        embed_component_metadata(
            &mut wasm,
            &resolve,
            world,
            self.encoding.unwrap_or(StringEncoding::UTF8),
        )?;

        self.io.output_wasm(&wasm, self.wat)?;

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
/// Note that the order in which input libraries are listed determines priority in cases where more than one
/// library exports the same symbol.
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
    /// module that's being implemented (e.g. `wasi_snapshot_preview1.wasm`).
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

    /// Use built-in implementations of `dlopen`/`dlsym`
    #[clap(long)]
    use_built_in_libdl: bool,
}

impl LinkOpts {
    fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    /// Executes the application.
    fn run(self) -> Result<()> {
        let mut linker = Linker::default()
            .validate(!self.skip_validation)
            .stub_missing_functions(self.stub_missing_functions)
            .use_built_in_libdl(self.use_built_in_libdl);

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

        self.output.output_wasm(&self.general, &bytes, self.wat)?;

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

    /// Generates WIT to import the component specified to this command.
    ///
    /// This flags requires that the input is a binary component, not a
    /// wasm-encoded WIT package. This will then generate a WIT world and output
    /// that. The returned world will have imports corresponding to the exports
    /// of the component which is input.
    ///
    /// This is similar to `--importize-world`, but is used with components.
    #[clap(long, conflicts_with = "importize_world")]
    importize: bool,

    /// Generates a WIT world to import a component which corresponds to the
    /// selected world.
    ///
    /// This flag is used to indicate that the input is a WIT package and the
    /// world passed here is the name of a WIT `world` within the package. The
    /// output of the command will be the same WIT world but one that's
    /// importing the selected world. This effectively moves the world's exports
    /// to imports.
    ///
    /// This is similar to `--importize`, but is used with WIT packages.
    #[clap(long, conflicts_with = "importize", value_name = "WORLD")]
    importize_world: Option<String>,

    /// Features to enable when parsing the `wit` option.
    ///
    /// This flag enables the `@unstable` feature in WIT documents where the
    /// items are otherwise hidden by default.
    #[clap(long)]
    features: Vec<String>,

    /// Enable all features when parsing the `wit` option.
    ///
    /// This flag enables all `@unstable` features in WIT documents where the
    /// items are otherwise hidden by default.
    #[clap(long)]
    all_features: bool,
}

impl WitOpts {
    fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    /// Executes the application.
    fn run(self) -> Result<()> {
        let mut decoded = self.decode_input()?;

        if self.importize {
            self.importize(&mut decoded, None)?;
        } else if self.importize_world.is_some() {
            self.importize(&mut decoded, self.importize_world.as_deref())?;
        }

        // Now that the WIT document has been decoded, it's time to emit it.
        // This interprets all of the output options and performs such a task.
        if self.json {
            self.emit_json(&decoded)?;
        } else if self.wasm || self.wat {
            self.emit_wasm(&decoded)?;
        } else {
            self.emit_wit(&decoded)?;
        }
        Ok(())
    }

    fn decode_input(&self) -> Result<DecodedWasm> {
        // If the input is a directory then it's probably raw WIT files, so use
        // `parse_wit_from_path`.
        if let Some(input) = &self.input {
            if input.is_dir() {
                let mut resolve =
                    WitResolve::resolve_with_features(&self.features, self.all_features);
                let (pkg_id, _) = resolve.push_dir(&input)?;
                return Ok(DecodedWasm::WitPackage(resolve, pkg_id));
            }
        }

        // ... otherwise if the input is not a directory then it's read into
        // memory here and decoded below. Note that this specifically does not
        // use `parse_wit_from_path` because this wants to additionally handle
        // the input case that the input is a core wasm binary with a
        // `component-type` section inside of it.
        let (input, path) = match &self.input {
            Some(input) => (
                std::fs::read(input).with_context(|| format!("failed to read {input:?}"))?,
                input.as_path(),
            ),
            None => {
                let mut stdin = Vec::new();
                std::io::stdin()
                    .read_to_end(&mut stdin)
                    .context("failed to read <stdin>")?;
                (stdin, Path::new("<stdin>"))
            }
        };

        match Detect::from_bytes(&input) {
            Detect::WasmBinary | Detect::WasmText => {
                // Use `wat` to possible translate the text format, and then
                // afterwards use either `decode` or `metadata::decode` depending on
                // if the input is a component or a core wasm module.
                let input = wat::parse_bytes(&input).map_err(|mut e| {
                    e.set_path(path);
                    e
                })?;
                if wasmparser::Parser::is_component(&input) {
                    wit_component::decode(&input)
                } else {
                    let (wasm, bindgen) = wit_component::metadata::decode(&input)?;
                    if wasm.is_none() {
                        bail!(
                            "input is a core wasm module with no `component-type*` \
                             custom sections meaning that there is not WIT information; \
                             is the information not embedded or is this supposed \
                             to be a component?"
                        )
                    }
                    Ok(DecodedWasm::Component(bindgen.resolve, bindgen.world))
                }
            }
            Detect::Unknown => {
                // This is a single WIT file, so create the single-file package and
                // return it.
                let input = match std::str::from_utf8(&input) {
                    Ok(s) => s,
                    Err(_) => bail!("input was not valid utf-8"),
                };
                let mut resolve =
                    WitResolve::resolve_with_features(&self.features, self.all_features);
                let id = resolve.push_str(path, input)?;
                Ok(DecodedWasm::WitPackage(resolve, id))
            }
        }
    }

    fn importize(&self, decoded: &mut DecodedWasm, world: Option<&str>) -> Result<()> {
        let (resolve, world_id) = match (&mut *decoded, world) {
            (DecodedWasm::Component(resolve, world), None) => (resolve, *world),
            (DecodedWasm::Component(..), Some(_)) => {
                bail!(
                    "the `--importize-world` flag is not compatible with a \
                     component input, use `--importize` instead"
                );
            }
            (DecodedWasm::WitPackage(resolve, id), world) => {
                let world = resolve.select_world(*id, world)?;
                (resolve, world)
            }
        };
        // let pkg = decoded.package();
        // let world_id = decoded.resolve().select_world(main, None)?;
        resolve
            .importize(world_id)
            .context("failed to move world exports to imports")?;
        let resolve = mem::take(resolve);
        *decoded = DecodedWasm::Component(resolve, world_id);
        Ok(())
    }

    fn emit_wasm(&self, decoded: &DecodedWasm) -> Result<()> {
        assert!(self.wasm || self.wat);
        assert!(self.out_dir.is_none());

        let decoded_package = decoded.package();
        let bytes = wit_component::encode(decoded.resolve(), decoded_package)?;
        if !self.skip_validation {
            wasmparser::Validator::new().validate_all(&bytes)?;
        }
        self.output.output_wasm(&self.general, &bytes, self.wat)?;
        Ok(())
    }

    fn emit_wit(&self, decoded: &DecodedWasm) -> Result<()> {
        assert!(!self.wasm && !self.wat);

        let resolve = decoded.resolve();

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

                let main = decoded.package();
                for (id, pkg) in resolve.packages.iter() {
                    let is_main = id == main;
                    let output = printer.print(resolve, id, &[])?;
                    let out_dir = if is_main {
                        dir.clone()
                    } else {
                        dir.join("deps")
                    };
                    let packages_with_same_name = &names[&pkg.name.name];
                    let packages_with_same_namespace = packages_with_same_name[&pkg.name.namespace];
                    let stem = if packages_with_same_name.len() == 1 {
                        if packages_with_same_namespace == 1 {
                            pkg.name.name.clone()
                        } else {
                            pkg.name
                                .version
                                .as_ref()
                                .map(|ver| format!("{}@{}", pkg.name.name, ver))
                                .unwrap_or_else(|| pkg.name.name.clone())
                        }
                    } else {
                        if packages_with_same_namespace == 1 {
                            format!("{}:{}", pkg.name.namespace, pkg.name.name)
                        } else {
                            pkg.name.to_string()
                        }
                    };
                    std::fs::create_dir_all(&out_dir)
                        .with_context(|| format!("failed to create directory: {out_dir:?}"))?;
                    let filename = format!("{stem}.wit");
                    let path = out_dir.join(&filename);
                    std::fs::write(&path, &output)
                        .with_context(|| format!("failed to write file: {path:?}"))?;
                    println!("Writing: {}", path.display());
                }
            }
            None => {
                self.output.output(
                    &self.general,
                    Output::Wit {
                        wit: &decoded,
                        printer,
                    },
                )?;
            }
        }

        Ok(())
    }

    fn emit_json(&self, decoded: &DecodedWasm) -> Result<()> {
        assert!(!self.wasm && !self.wat);

        let resolve = decoded.resolve();
        let output = serde_json::to_string_pretty(&resolve)?;
        self.output.output(&self.general, Output::Json(&output))?;

        Ok(())
    }
}

/// Tool for verifying whether a component conforms to a world.
#[derive(Parser)]
pub struct TargetsOpts {
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    #[clap(flatten)]
    resolve: WitResolve,

    /// The world used to test whether a component conforms to its signature. If the `WIT` source
    /// provided contains multiple packages, this option must be set, and must be of the
    /// fully-qualified form (ex: "wasi:http/proxy")
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
        let (resolve, pkg_id) = self.resolve.load()?;
        let world = resolve.select_world(pkg_id, self.world.as_deref())?;
        let component_to_test = self.input.parse_wasm()?;

        wit_component::targets(&resolve, world, &component_to_test)?;

        Ok(())
    }
}

/// Tool for verifying whether one world is a semver compatible evolution of
/// another.
#[derive(Parser)]
pub struct SemverCheckOpts {
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    #[clap(flatten)]
    resolve: WitResolve,

    /// The "previous" world, or older version, of what's being tested.
    ///
    /// This is considered the baseline for the semver compatibility check.
    #[clap(long)]
    prev: String,

    /// The "new" world which is the "prev" world but modified.
    ///
    /// This is what's being tested to see whether it is a backwards-compatible
    /// evolution of the "prev" world specified.
    #[clap(long)]
    new: String,
}

impl SemverCheckOpts {
    fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    fn run(self) -> Result<()> {
        let (resolve, pkg_id) = self.resolve.load()?;
        let prev = resolve.select_world(pkg_id, Some(self.prev.as_str()))?;
        let new = resolve.select_world(pkg_id, Some(self.new.as_str()))?;
        wit_component::semver_check(resolve, prev, new)?;
        Ok(())
    }
}

/// Unbundled core wasm modules from a component, switching them from being
/// embedded to being imported.
///
/// This subcommand will remove core wasm modules from a component and place
/// them in the directory specified by `--module-dir`. Modules are extracted
/// based on their size according to `--threshold`. The output of this command
/// is a new component which imports the modules that are extracted.
#[derive(Parser)]
pub struct UnbundleOpts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Where to place unbundled core wasm modules.
    ///
    /// Modules will be placed in this directory with the name
    /// `unbundled-moduleN.wasm` where `N` is a number starting from 0. The
    /// output component will import `unbundled-moduleN` and each `*.wasm` is
    /// expected to be compiled and provided.
    ///
    /// Note that this option is required to be passed.
    #[clap(long, value_name = "PATH")]
    module_dir: PathBuf,

    /// The size threshold for core wasm modules to unbundled.
    ///
    /// Modules that are larger than this value will be removed from the
    /// component and unbundled. Modules smaller than this value will be left in
    /// place and bundled within the componente.
    #[clap(long, value_name = "SIZE", default_value_t = 10 << 10)]
    threshold: usize,

    /// Output the text format of WebAssembly instead of the binary format.
    #[clap(short = 't', long)]
    wat: bool,
}

impl UnbundleOpts {
    fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    fn run(self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        if !wasmparser::Parser::is_component(&input) {
            return self.io.output_wasm(&input, self.wat);
        }

        // Generate a list of all modules in the component in the order they
        // were found in the component itself. Record for each one the bytes of
        // the module itself or `None` indicating it's not being extracted.
        let mut modules_to_extract = Vec::new();
        for payload in wasmparser::Parser::new(0).parse_all(&input) {
            let range = match payload? {
                Payload::ModuleSection {
                    unchecked_range, ..
                } => unchecked_range,
                _ => continue,
            };
            modules_to_extract.push(if range.len() > self.threshold {
                Some(&input[range])
            } else {
                None
            });
        }

        let mut core_types = CoreTypeInterner::default();
        let mut imports = wasm_encoder::ComponentImportSection::new();
        let mut validator = wasmparser::Validator::new();

        for module in modules_to_extract.iter().filter_map(|m| *m) {
            // Validate the `module` to get its type information, but don't
            // validate function bodies since that's not necessary for this.
            let mut types = None;
            for payload in wasmparser::Parser::new(0).parse_all(module) {
                match validator
                    .payload(&payload?)
                    .context("failed to validate core wasm module found in component")?
                {
                    ValidPayload::Ok | ValidPayload::Parser(_) | ValidPayload::Func(..) => {}
                    ValidPayload::End(t) => types = Some(t),
                }
            }
            let types = types.unwrap();
            validator.reset();

            // Create a `ModuleType` with the imports/exports of `types`. Then
            // create an import corresponding to that module in the import
            // section.
            let mut module_ty = ModuleTypeCreator::new(&mut core_types, &types);

            for (module, name, ty) in types.core_imports().unwrap() {
                let ty = module_ty.convert_entity_type(ty)?;
                module_ty.module.import(module, name, ty);
            }
            for (name, ty) in types.core_exports().unwrap() {
                let ty = module_ty.convert_entity_type(ty)?;
                module_ty.module.export(name, ty);
            }

            let module_type_idx = module_ty.component.section.len();
            module_ty.component.section.ty().module(&module_ty.module);
            let name = format!("unbundled-module{}", imports.len());
            imports.import(
                &name,
                wasm_encoder::ComponentTypeRef::Module(module_type_idx),
            );

            let dst = self.module_dir.join(&name).with_extension("wasm");
            let parent = dst.parent().unwrap();
            std::fs::create_dir_all(parent)
                .with_context(|| format!("failed to create directory {parent:?}"))?;
            std::fs::write(&dst, module)
                .with_context(|| format!("failed to write file {dst:?}"))?;
        }

        let mut component = wasm_encoder::Component::new();
        component.section(&core_types.section);
        component.section(&imports);

        // Next rewrite the original component into the destination component
        // but fix up references at the top level to core types and core
        // modules. Additionally replace all modules optionally with an alias to
        // the appropriate import or leave the module in place.
        ModuleExtractor {
            extra_core_types: core_types.section.len(),
            extra_core_modules: imports.len(),
            depth: 0,
            modules: modules_to_extract.iter(),
            next_module: 0,
        }
        .parse_component(&mut component, wasmparser::Parser::new(0), &input)?;

        let bytes = component.finish();
        self.io.output_wasm(&bytes, self.wat)
    }
}

/// Top-level conversion of core wasm function types to a component core types
/// section.
///
/// This is a builder for a core type section in a component from a list of
/// function types. Functions are dedup'd and translated only once.
#[derive(Default)]
struct CoreTypeInterner {
    section: wasm_encoder::CoreTypeSection,
    intern: HashMap<CoreTypeId, u32>,
}

impl CoreTypeInterner {
    fn convert_func(&mut self, id: CoreTypeId, types: &Types) -> Result<u32> {
        if let Some(ret) = self.intern.get(&id) {
            return Ok(*ret);
        }
        let ty = &types[id];
        if !ty.is_final || ty.supertype_idx.is_some() || ty.composite_type.shared {
            bail!("unsupported core type to translate")
        }
        let f = match &ty.composite_type.inner {
            wasmparser::CompositeInnerType::Func(f) => f,
            _ => bail!("unsupported core type to translate"),
        };

        let ret = self.section.len();
        self.section.ty().core().function(
            f.params()
                .iter()
                .map(|p| RoundtripReencoder.val_type(*p))
                .collect::<Result<Vec<_>, _>>()?,
            f.results()
                .iter()
                .map(|p| RoundtripReencoder.val_type(*p))
                .collect::<Result<Vec<_>, _>>()?,
        );
        self.intern.insert(id, ret);
        Ok(ret)
    }
}

/// Second-level conversion of a module into a component module type.
///
/// This uses a [`CoreTypeInterner`] to create function types and aliases them
/// into the module type being created to cut down on space used to encode
/// function types.
///
/// This also itself has an `intern` cache to avoid creating multiple aliases
/// for the same type.
struct ModuleTypeCreator<'a> {
    component: &'a mut CoreTypeInterner,
    types: &'a Types,
    intern: HashMap<CoreTypeId, u32>,
    module: ModuleType,
}

impl<'a> ModuleTypeCreator<'a> {
    fn new(component: &'a mut CoreTypeInterner, types: &'a Types) -> Self {
        ModuleTypeCreator {
            component,
            types,
            intern: HashMap::new(),
            module: ModuleType::new(),
        }
    }

    fn convert_entity_type(
        &mut self,
        ty: wasmparser::types::EntityType,
    ) -> Result<wasm_encoder::EntityType> {
        Ok(match ty {
            EntityType::Func(id) => wasm_encoder::EntityType::Function(self.convert_func(id)?),
            EntityType::Table(t) => wasm_encoder::EntityType::Table(wasm_encoder::TableType {
                shared: t.shared,
                maximum: t.maximum,
                minimum: t.initial,
                table64: t.table64,
                element_type: RoundtripReencoder.ref_type(t.element_type)?,
            }),
            EntityType::Memory(m) => wasm_encoder::EntityType::Memory(wasm_encoder::MemoryType {
                shared: m.shared,
                maximum: m.maximum,
                minimum: m.initial,
                memory64: m.memory64,
                page_size_log2: m.page_size_log2,
            }),
            EntityType::Global(g) => wasm_encoder::EntityType::Global(wasm_encoder::GlobalType {
                mutable: g.mutable,
                shared: g.shared,
                val_type: RoundtripReencoder.val_type(g.content_type)?,
            }),
            EntityType::Tag(id) => wasm_encoder::EntityType::Tag(wasm_encoder::TagType {
                kind: wasm_encoder::TagKind::Exception,
                func_type_idx: self.convert_func(id)?,
            }),
        })
    }

    fn convert_func(&mut self, id: CoreTypeId) -> Result<u32> {
        if let Some(ret) = self.intern.get(&id) {
            return Ok(*ret);
        }
        let outer_index = self.component.convert_func(id, self.types)?;
        let ret = self.module.type_count();
        self.module.alias_outer_core_type(1, outer_index);
        self.intern.insert(id, ret);
        Ok(ret)
    }
}

struct ModuleExtractor<'a> {
    extra_core_types: u32,
    extra_core_modules: u32,
    depth: u32,
    modules: std::slice::Iter<'a, Option<&'a [u8]>>,
    next_module: u32,
}

impl Reencode for ModuleExtractor<'_> {
    type Error = std::convert::Infallible;

    fn type_index(&mut self, index: u32) -> u32 {
        if self.depth == 0 {
            index + self.extra_core_types
        } else {
            index
        }
    }
}

impl ReencodeComponent for ModuleExtractor<'_> {
    fn module_index(&mut self, index: u32) -> u32 {
        if self.depth == 0 {
            index + self.extra_core_modules
        } else {
            index
        }
    }

    fn outer_type_index(&mut self, count: u32, index: u32) -> u32 {
        if self.depth == count {
            index + self.extra_core_types
        } else {
            index
        }
    }

    fn outer_module_index(&mut self, count: u32, index: u32) -> u32 {
        if self.depth == count {
            index + self.extra_core_modules
        } else {
            index
        }
    }

    fn push_depth(&mut self) {
        self.depth += 1;
    }

    fn pop_depth(&mut self) {
        self.depth -= 1;
    }

    fn parse_component_submodule(
        &mut self,
        component: &mut wasm_encoder::Component,
        _parser: wasmparser::Parser,
        module: &[u8],
    ) -> Result<(), Error<Self::Error>> {
        let extracted = self.modules.next().unwrap().is_some();
        if extracted {
            let mut aliases = wasm_encoder::ComponentAliasSection::new();
            aliases.alias(wasm_encoder::Alias::Outer {
                kind: wasm_encoder::ComponentOuterAliasKind::CoreModule,
                count: self.depth,
                index: self.next_module,
            });
            self.next_module += 1;
            component.section(&aliases);
        } else {
            component.section(&wasm_encoder::RawSection {
                id: wasm_encoder::ComponentSectionId::CoreModule as u8,
                data: module,
            });
        }
        Ok(())
    }
}
