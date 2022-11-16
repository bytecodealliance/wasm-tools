//! Shared input/output routines amongst most `wasm-tools` subcommands

use anyhow::{bail, Context, Result};
use std::fs::File;
use std::io::{BufWriter, Read, Write};
use std::path::{Path, PathBuf};

// Implements the verbosity flag for the CLI commands.
#[derive(clap::Parser)]
pub struct Verbosity {
    /// Use verbose output (-vv very verbose output).
    #[clap(long = "verbose", short = 'v', action = clap::ArgAction::Count)]
    verbose: u8,
}

impl Verbosity {
    /// Initializes the logger based on the verbosity level.
    pub fn init_logger(&self) {
        let default = match self.verbose {
            0 => "warn",
            1 => "info",
            _ => "debug",
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
    /// Input file to process.
    ///
    /// If not provided or if this is `-` then stdin is read entirely and
    /// processed. Note that for most subcommands this input can either be a
    /// binary `*.wasm` file or a textual format `*.wat` file.
    input: Option<PathBuf>,

    #[clap(flatten)]
    output: OutputArg,

    #[clap(flatten)]
    verbosity: Verbosity,
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
    Wat(&'a str),
    Wasm { bytes: &'a [u8], wat: bool },
}

impl InputOutput {
    pub fn parse_input_wasm(&self) -> Result<Vec<u8>> {
        self.verbosity.init_logger();

        if let Some(path) = &self.input {
            if path != Path::new("-") {
                let bytes = wat::parse_file(path)?;
                return Ok(bytes);
            }
        }
        let mut stdin = Vec::new();
        std::io::stdin()
            .read_to_end(&mut stdin)
            .context("failed to read <stdin>")?;
        let bytes = wat::parse_bytes(&stdin).map_err(|mut e| {
            e.set_path("<stdin>");
            e
        })?;
        Ok(bytes.into_owned())
    }

    pub fn output(&self, bytes: Output<'_>) -> Result<()> {
        self.output.output(bytes)
    }

    pub fn output_writer(&self) -> Result<Box<dyn Write>> {
        self.output.output_writer()
    }

    pub fn output_path(&self) -> Option<&Path> {
        self.output.output.as_deref()
    }

    pub fn input_path(&self) -> Option<&Path> {
        self.input.as_deref()
    }

    pub fn init_logger(&self) {
        self.verbosity.init_logger();
    }
}

impl OutputArg {
    pub fn output(&self, output: Output<'_>) -> Result<()> {
        match output {
            Output::Wat(s) => self.output_str(s),
            Output::Wasm { bytes, wat: true } => {
                self.output_str(&wasmprinter::print_bytes(&bytes)?)
            }
            Output::Wasm { bytes, wat: false } => {
                match &self.output {
                    Some(path) => {
                        std::fs::write(path, bytes)
                            .context(format!("failed to write `{}`", path.display()))?;
                    }
                    None => {
                        if atty::is(atty::Stream::Stdout) {
                            bail!("cannot print binary wasm output to a terminal, pass the `-t` flag to print the text format");
                        }
                        std::io::stdout()
                            .write_all(bytes)
                            .context("failed to write to stdout")?;
                    }
                }
                Ok(())
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

    pub fn output_writer(&self) -> Result<Box<dyn Write>> {
        match &self.output {
            Some(output) => Ok(Box::new(BufWriter::new(File::create(&output)?))),
            None => Ok(Box::new(std::io::stdout())),
        }
    }
}
