//! The WebAssembly component tool command line interface.

use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;
use std::io::Read;
use std::path::{Path, PathBuf};
use wasm_encoder::{Encode, Section};
use wasm_tools::Output;
use wit_component::{ComponentEncoder, DecodedWasm, DocumentPrinter, StringEncoding};
use wit_parser::{PackageId, Resolve, UnresolvedPackage};

/// WebAssembly wit-based component tooling.
#[derive(Parser)]
pub enum Opts {
    New(NewOpts),
    Wit(WitOpts),
    Embed(EmbedOpts),
}

impl Opts {
    pub fn run(self) -> Result<()> {
        match self {
            Opts::New(new) => new.run(),
            Opts::Wit(wit) => wit.run(),
            Opts::Embed(embed) => embed.run(),
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

    /// Skip validation of the output component.
    #[clap(long)]
    skip_validation: bool,

    /// Print the output in the WebAssembly text format instead of binary.
    #[clap(long, short = 't')]
    wat: bool,
}

impl NewOpts {
    /// Executes the application.
    fn run(self) -> Result<()> {
        let wasm = self.io.parse_input_wasm()?;
        let mut encoder = ComponentEncoder::default()
            .validate(!self.skip_validation)
            .module(&wasm)?;

        for (name, wasm) in self.adapters.iter() {
            encoder = encoder.adapter(name, wasm)?;
        }

        let bytes = encoder
            .encode()
            .with_context(|| format!("failed to encode a component from module "))?;

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
    world: String,
}

impl EmbedOpts {
    /// Executes the application.
    fn run(self) -> Result<()> {
        let mut wasm = self.io.parse_input_wasm()?;
        let (resolve, id) = parse_wit(&self.wit)?;

        let mut parts = self.world.split('/');
        let doc = match parts.next() {
            Some(name) => match resolve.packages[id].documents.get(name) {
                Some(doc) => *doc,
                None => bail!("no document named `{name}` in package"),
            },
            None => bail!("invalid `--world` argument"),
        };
        let world = match parts.next() {
            Some(name) => match resolve.documents[doc].worlds.get(name) {
                Some(world) => *world,
                None => bail!("no world named `{name}` in document"),
            },
            None => match resolve.documents[doc].default_world {
                Some(world) => world,
                None => bail!("no default world found in document"),
            },
        };

        let encoded = wit_component::metadata::encode(
            &resolve,
            world,
            self.encoding.unwrap_or(StringEncoding::UTF8),
        )?;

        let section = wasm_encoder::CustomSection {
            name: "component-type",
            data: &encoded,
        };
        wasm.push(section.id());
        section.encode(&mut wasm);

        self.io.output(Output::Wasm {
            bytes: &wasm,
            wat: false,
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
    verbosity: wasm_tools::Verbosity,

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

    /// If a WIT package is being parsed, then this is the optionally specified
    /// name of the WIT package. If not specified this is automatically inferred
    /// from the filename.
    #[clap(long)]
    name: Option<String>,

    /// When printing a WIT package, the default mode, this option is used to
    /// indicate which document is printed within the package if more than one
    /// document is present.
    #[clap(short, long)]
    document: Option<String>,

    /// Emit a full WIT package into the specified directory when printing the
    /// text form.
    ///
    /// This is incompatible with `-o`.
    #[clap(long)]
    out_dir: Option<PathBuf>,

    /// Emit the WIT binary format, a WebAssembly component, instead of the WIT
    /// text format.
    #[clap(short, long)]
    wasm: bool,

    /// When combined with `--wasm` this will print the WebAssembly text format
    /// instead of the WebAssembly binary format.
    #[clap(short = 't', long)]
    wat: bool,

    /// When specifying `--wasm` the output wasm binary is validated by default,
    /// and this can be used to skip that step.
    #[clap(long)]
    skip_validation: bool,
}

impl WitOpts {
    /// Executes the application.
    fn run(self) -> Result<()> {
        let name = match &self.name {
            Some(name) => name.as_str(),
            None => match &self.input {
                Some(path) => path.file_stem().unwrap().to_str().unwrap(),
                None => "component",
            },
        };

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
                    wit_component::decode(name, &bytes).context("failed to decode WIT document")?
                }
                _ => {
                    let (resolve, id) = parse_wit(input)?;
                    DecodedWasm::WitPackage(resolve, id)
                }
            },
            None => {
                let mut stdin = Vec::new();
                std::io::stdin()
                    .read_to_end(&mut stdin)
                    .context("failed to read <stdin>")?;

                if is_wasm(&stdin) {
                    let bytes = wat::parse_bytes(&stdin).map_err(|mut e| {
                        e.set_path("<stdin>");
                        e
                    })?;

                    wit_component::decode(name, &bytes).context("failed to decode WIT document")?
                } else {
                    let stdin = match std::str::from_utf8(&stdin) {
                        Ok(s) => s,
                        Err(_) => bail!("stdin was not valid utf-8"),
                    };
                    let mut resolve = Resolve::default();
                    let pkg = UnresolvedPackage::parse("<stdin>".as_ref(), stdin)?;
                    let id = resolve.push(pkg, &Default::default())?;
                    DecodedWasm::WitPackage(resolve, id)
                }
            }
        };

        // Now that the WIT document has been decoded, it's time to emit it.
        // This interprets all of the output options and performs such a task.
        match &self.out_dir {
            Some(dir) => {
                if self.output.output_path().is_some() {
                    bail!("cannot specify both `--out-dir` and `--output`");
                }
                if self.wasm {
                    bail!("cannot specify both `--out-dir` and `--wasm`");
                }
                if self.wat {
                    bail!("cannot specify both `--out-dir` and `--wat`");
                }
                if self.document.is_some() {
                    bail!("cannot specify both `--out-dir` and `--document`");
                }
                let package = match &decoded {
                    DecodedWasm::WitPackage(_, package) => *package,

                    DecodedWasm::Component(resolve, world) => {
                        let doc = resolve.worlds[*world].document;
                        resolve.documents[doc].package.unwrap()
                    }
                };
                let resolve = decoded.resolve();
                std::fs::create_dir_all(&dir).context(format!("failed to create {dir:?}"))?;
                for (name, doc) in resolve.packages[package].documents.iter() {
                    let output = DocumentPrinter::default().print(&resolve, *doc)?;
                    let path = dir.join(format!("{name}.wit"));
                    std::fs::write(&path, output).context(format!("failed to write {path:?}"))?;
                }
            }
            None => {
                if self.wasm {
                    self.emit_wasm(&decoded)?;
                } else {
                    self.emit_wit(&decoded)?;
                }
            }
        }
        Ok(())
    }

    fn emit_wasm(&self, decoded: &DecodedWasm) -> Result<()> {
        assert!(self.wasm);
        assert!(self.out_dir.is_none());
        if self.document.is_some() {
            bail!("cannot specify `--document` with `--wasm`");
        }

        let pkg = match decoded {
            DecodedWasm::WitPackage(_resolve, pkg) => *pkg,
            DecodedWasm::Component(resolve, world) => {
                let doc = resolve.worlds[*world].document;
                resolve.documents[doc].package.unwrap()
            }
        };

        let resolve = decoded.resolve();
        let bytes = wit_component::encode(&resolve, pkg)?;
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
        assert!(!self.wasm);
        assert!(self.out_dir.is_none());
        if self.wat {
            bail!("the `--wat` option can only be combined with `--wasm`");
        }

        let doc = match decoded {
            DecodedWasm::WitPackage(resolve, pkg) => {
                let pkg = &resolve.packages[*pkg];
                match &self.document {
                    Some(name) => *pkg
                        .documents
                        .get(name)
                        .ok_or_else(|| anyhow!("no document named `{name}` found in package"))?,
                    None => match pkg.documents.len() {
                        1 => *pkg.documents.iter().next().unwrap().1,
                        _ => bail!(
                            "more than document found in package, \
                             specify which to print with `-d name`"
                        ),
                    },
                }
            }
            DecodedWasm::Component(resolve, world) => resolve.worlds[*world].document,
        };

        let output = DocumentPrinter::default().print(decoded.resolve(), doc)?;
        self.output.output(Output::Wat(&output))?;
        Ok(())
    }
}

fn parse_wit(path: &Path) -> Result<(Resolve, PackageId)> {
    let mut resolve = Resolve::default();
    let id = if path.is_dir() {
        resolve.push_dir(&path)?
    } else {
        let pkg = UnresolvedPackage::parse_file(&path)?;
        resolve.push(pkg, &Default::default())?
    };
    Ok((resolve, id))
}

/// Test to see if a string is probably a `*.wat` text syntax.
///
/// This briefly lexes past whitespace and comments as a `*.wat` file to see if
/// we can find a left-paren. If that fails then it's probably `*.wit` instead.
fn is_wasm(bytes: &[u8]) -> bool {
    use wast::lexer::{Lexer, Token};

    let text = match std::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => return true,
    };

    let mut lexer = Lexer::new(text);

    while let Some(next) = lexer.next() {
        match next {
            Ok(Token::Whitespace(_)) | Ok(Token::BlockComment(_)) | Ok(Token::LineComment(_)) => {}
            Ok(Token::LParen(_)) => return true,
            _ => break,
        }
    }

    false
}
