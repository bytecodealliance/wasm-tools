//! Shared input/output routines amongst most `wasm-tools` subcommands

use anyhow::{Context, Result, bail};
use std::fs::File;
use std::io::IsTerminal;
use std::io::{BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use termcolor::{Ansi, ColorChoice, NoColor, StandardStream, WriteColor};

#[cfg(any(feature = "addr2line", feature = "validate"))]
pub mod addr2line;
#[cfg(any(feature = "component", feature = "wit-dylib"))]
pub mod wit;

#[derive(clap::Parser)]
pub struct GeneralOpts {
    /// Use verbose output (-v info, -vv debug, -vvv trace).
    #[clap(long = "verbose", short = 'v', action = clap::ArgAction::Count)]
    verbose: u8,

    /// Configuration over whether terminal colors are used in output.
    ///
    /// Supports one of `auto|never|always|always-ansi`. The default is to
    /// detect what to do based on the terminal environment, for example by
    /// using `isatty`.
    #[clap(long = "color", default_value = "auto")]
    pub color: ColorChoice,
}

impl GeneralOpts {
    /// Initializes the logger based on the verbosity level.
    pub fn init_logger(&self) {
        let default = match self.verbose {
            0 => "warn",
            1 => "info",
            2 => "debug",
            _ => "trace",
        };

        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or(default))
            .format_target(false)
            .init();
    }
}

// This is intended to be included in a struct as:
//
//      #[clap(flatten)]
//      io: wasm_tools::InputOutput,
//
// and then the methods are used to read the arguments,
#[derive(clap::Parser)]
pub struct InputOutput {
    #[clap(flatten)]
    input: InputArg,

    #[clap(flatten)]
    output: OutputArg,

    #[clap(flatten)]
    general: GeneralOpts,
}

#[derive(clap::Parser)]
pub struct GenerateDwarfArg {
    /// Optionally generate DWARF debugging information from WebAssembly text
    /// files.
    ///
    /// When the input to this command is a WebAssembly text file, such as
    /// `*.wat`, then this option will instruct the text parser to insert DWARF
    /// debugging information to map binary locations back to the original
    /// source locations in the input `*.wat` file. This option has no effect if
    /// the `INPUT` argument is already a WebAssembly binary or if the text
    /// format uses `(module binary ...)`.
    #[clap(
        long,
        value_name = "lines|full",
        conflicts_with = "generate_full_dwarf"
    )]
    generate_dwarf: Option<GenerateDwarf>,

    /// Shorthand for `--generate-dwarf full`
    #[clap(short, conflicts_with = "generate_dwarf")]
    generate_full_dwarf: bool,
}

#[derive(clap::Parser)]
pub struct InputArg {
    /// Input file to process.
    ///
    /// If not provided or if this is `-` then stdin is read entirely and
    /// processed. Note that for most subcommands this input can either be a
    /// binary `*.wasm` file or a textual format `*.wat` file.
    input: Option<PathBuf>,
}

#[derive(Copy, Clone)]
enum GenerateDwarf {
    Lines,
    Full,
}

impl FromStr for GenerateDwarf {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<GenerateDwarf> {
        match s {
            "lines" => Ok(GenerateDwarf::Lines),
            "full" => Ok(GenerateDwarf::Full),
            other => bail!("unknown `--generate-dwarf` setting: {other}"),
        }
    }
}

impl InputArg {
    pub fn get_binary_wasm(
        &self,
        generate_dwarf_optional: Option<&GenerateDwarfArg>,
    ) -> Result<Vec<u8>> {
        let mut parser = wat::Parser::new();
        match generate_dwarf_optional {
            None => {}
            Some(generate_dwarf) => match (
                generate_dwarf.generate_full_dwarf,
                generate_dwarf.generate_dwarf,
            ) {
                (false, Some(GenerateDwarf::Lines)) => {
                    parser.generate_dwarf(wat::GenerateDwarf::Lines);
                }
                (true, _) | (false, Some(GenerateDwarf::Full)) => {
                    parser.generate_dwarf(wat::GenerateDwarf::Full);
                }
                (false, None) => {}
            },
        }
        if let Some(path) = &self.input {
            if path != Path::new("-") {
                let bytes = parser.parse_file(path)?;
                return Ok(bytes);
            }
        }
        let mut stdin = Vec::new();
        std::io::stdin()
            .read_to_end(&mut stdin)
            .context("failed to read <stdin>")?;
        let bytes = parser.parse_bytes(Some("<stdin>".as_ref()), &stdin)?;
        Ok(bytes.into_owned())
    }
}

#[derive(clap::Parser)]
pub struct OutputArg {
    /// Where to place output.
    ///
    /// If not provided then stdout is used.
    #[clap(short, long)]
    output: Option<PathBuf>,
}

pub enum Output<'a> {
    #[cfg(feature = "component")]
    Wit {
        wit: &'a wit_component::DecodedWasm,
        printer: wit_component::WitPrinter,
    },
    Wasm(&'a [u8]),
    Wat {
        wasm: &'a [u8],
        config: wasmprinter::Config,
    },
    Json(&'a str),
}

impl InputOutput {
    pub fn parse_input_wasm(&self, generate_dwarf: Option<&GenerateDwarfArg>) -> Result<Vec<u8>> {
        let ret = self.get_input_wasm(generate_dwarf)?;
        parse_binary_wasm(wasmparser::Parser::new(0), &ret)?;
        Ok(ret)
    }

    pub fn get_input_wasm(&self, generate_dwarf: Option<&GenerateDwarfArg>) -> Result<Vec<u8>> {
        self.input.get_binary_wasm(generate_dwarf)
    }

    pub fn output_wasm(&self, wasm: &[u8], wat: bool) -> Result<()> {
        if wat {
            self.output(Output::Wat {
                wasm,
                config: Default::default(),
            })
        } else {
            self.output(Output::Wasm(wasm))
        }
    }

    pub fn output(&self, bytes: Output<'_>) -> Result<()> {
        self.output.output(&self.general, bytes)
    }

    pub fn output_writer(&self) -> Result<Box<dyn WriteColor>> {
        self.output.output_writer(self.general.color)
    }

    pub fn output_path(&self) -> Option<&Path> {
        self.output.output.as_deref()
    }

    pub fn input_path(&self) -> Option<&Path> {
        self.input.input.as_deref()
    }

    pub fn general_opts(&self) -> &GeneralOpts {
        &self.general
    }
}

impl OutputArg {
    pub fn output_wasm(&self, general: &GeneralOpts, wasm: &[u8], wat: bool) -> Result<()> {
        if wat {
            self.output(
                general,
                Output::Wat {
                    wasm,
                    config: Default::default(),
                },
            )
        } else {
            self.output(general, Output::Wasm(wasm))
        }
    }

    pub fn output(&self, general: &GeneralOpts, output: Output<'_>) -> Result<()> {
        match output {
            Output::Wat { wasm, config } => {
                let mut writer = self.output_writer(general.color)?;
                config.print(wasm, &mut wasmprinter::PrintTermcolor(&mut writer))
            }
            Output::Wasm(bytes) => {
                match &self.output {
                    Some(path) => {
                        std::fs::write(path, bytes)
                            .context(format!("failed to write `{}`", path.display()))?;
                    }
                    None => {
                        let mut stdout = std::io::stdout();
                        if stdout.is_terminal() {
                            bail!(
                                "cannot print binary wasm output to a terminal, pass the `-t` flag to print the text format"
                            );
                        }
                        stdout
                            .write_all(bytes)
                            .context("failed to write to stdout")?;
                    }
                }
                Ok(())
            }
            Output::Json(s) => self.output_str(s),
            #[cfg(feature = "component")]
            Output::Wit { wit, mut printer } => {
                let resolve = wit.resolve();
                let ids = resolve
                    .packages
                    .iter()
                    .map(|(id, _)| id)
                    .filter(|id| *id != wit.package())
                    .collect::<Vec<_>>();
                printer.print(resolve, wit.package(), &ids)?;
                let output = printer.output.to_string();
                self.output_str(&output)
            }
        }
    }

    fn output_str(&self, output: &str) -> Result<()> {
        match &self.output {
            Some(path) => {
                std::fs::write(path, output)
                    .context(format!("failed to write `{}`", path.display()))?;
            }
            None => std::io::stdout()
                .write_all(output.as_bytes())
                .context("failed to write to stdout")?,
        }
        Ok(())
    }

    pub fn output_path(&self) -> Option<&Path> {
        self.output.as_deref()
    }

    pub fn output_writer(&self, color: ColorChoice) -> Result<Box<dyn WriteColor>> {
        match &self.output {
            Some(output) => {
                let writer = BufWriter::new(File::create(&output)?);
                if color == ColorChoice::AlwaysAnsi {
                    Ok(Box::new(Ansi::new(writer)))
                } else {
                    Ok(Box::new(NoColor::new(writer)))
                }
            }
            None => {
                let stdout = std::io::stdout();
                if color == ColorChoice::Auto && !stdout.is_terminal() {
                    Ok(Box::new(StandardStream::stdout(ColorChoice::Never)))
                } else {
                    Ok(Box::new(StandardStream::stdout(color)))
                }
            }
        }
    }
}

pub fn parse_binary_wasm(parser: wasmparser::Parser, bytes: &[u8]) -> Result<()> {
    for payload in parser.parse_all(&bytes) {
        match payload? {
            wasmparser::Payload::TypeSection(s) => parse_section(s)?,
            wasmparser::Payload::ImportSection(s) => parse_section(s)?,
            wasmparser::Payload::FunctionSection(s) => parse_section(s)?,
            wasmparser::Payload::TableSection(s) => parse_section(s)?,
            wasmparser::Payload::MemorySection(s) => parse_section(s)?,
            wasmparser::Payload::TagSection(s) => parse_section(s)?,
            wasmparser::Payload::GlobalSection(s) => parse_section(s)?,
            wasmparser::Payload::ExportSection(s) => parse_section(s)?,
            wasmparser::Payload::ElementSection(s) => parse_section(s)?,
            wasmparser::Payload::DataSection(s) => parse_section(s)?,
            wasmparser::Payload::CodeSectionEntry(body) => {
                let mut locals = body.get_locals_reader()?.into_iter();
                for item in locals.by_ref() {
                    let _ = item?;
                }
                let mut ops = locals.into_operators_reader();
                while !ops.eof() {
                    ops.read()?;
                }
                ops.finish()?;
            }

            wasmparser::Payload::InstanceSection(s) => parse_section(s)?,
            wasmparser::Payload::CoreTypeSection(s) => parse_section(s)?,
            wasmparser::Payload::ComponentInstanceSection(s) => parse_section(s)?,
            wasmparser::Payload::ComponentAliasSection(s) => parse_section(s)?,
            wasmparser::Payload::ComponentTypeSection(s) => parse_section(s)?,
            wasmparser::Payload::ComponentCanonicalSection(s) => parse_section(s)?,
            wasmparser::Payload::ComponentImportSection(s) => parse_section(s)?,
            wasmparser::Payload::ComponentExportSection(s) => parse_section(s)?,

            wasmparser::Payload::UnknownSection { id, .. } => {
                bail!("malformed section id: {}", id)
            }

            _ => (),
        }
    }
    return Ok(());

    fn parse_section<'a, T>(s: wasmparser::SectionLimited<'a, T>) -> Result<()>
    where
        T: wasmparser::FromReader<'a>,
    {
        for item in s {
            let _ = item?;
        }
        Ok(())
    }
}
