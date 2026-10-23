use addr2line::LookupResult;
use anyhow::Result;
use std::fmt::Write;
use std::time::Instant;
use wasm_tools::addr2line::Addr2lineModules;
use wasm_tools::CliFeatures;

/// Validate a WebAssembly binary
///
/// This subcommand will validate a WebAssembly binary to determine if it's
/// valid or not. This implements the validation algorithm of the WebAssembly
/// specification <https://webassembly.github.io/spec/core/valid/index.html>.
/// No output is printed if the binary is valid; an error message is printed to
/// stderr if the binary is not valid. A WebAssembly text file can also be used.
///
/// Extra information will be included in the error message if the input WebAssembly
/// binary contains DWARF debugging information.
///
#[derive(clap::Parser)]
#[clap(after_help = "\
Examples:

    # Validate `foo.wasm` with the default Wasm feature proposals.
    $ wasm-tools validate foo.wasm

    # Validate `foo.wasm` with more verbose output
    $ wasm-tools validate -vv foo.wasm

    # Validate `fancy.wasm` with all Wasm feature proposals enabled.
    $ wasm-tools validate --features all fancy.wasm

    # Validate `mvp.wasm` with the original wasm feature set enabled.
    $ wasm-tools validate --features=wasm1 mvp.wasm
    $ wasm-tools validate --features=mvp mvp.wasm

Exit status:
    0 if the binary is valid,
    nonzero if the binary is not valid.
")]
pub struct Opts {
    #[clap(flatten)]
    generate_dwarf: wasm_tools::GenerateDwarfArg,

    #[clap(flatten)]
    features: CliFeatures,

    #[clap(flatten)]
    input: wasm_tools::InputArg,

    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    /// Opt-in to validating the contents of custom sections.
    #[clap(long)]
    validate_custom: bool,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    pub fn run(&self) -> Result<()> {
        let start = Instant::now();
        let wasm = self.input.get_binary_wasm(Some(&self.generate_dwarf))?; // no need to parse as the validator will do this
        log::info!("read module in {:?}", start.elapsed());

        // If validation fails then try to attach extra information to the
        // error based on DWARF information in the input wasm binary. If
        // DWARF information isn't present or if the DWARF failed to get parsed
        // then ignore the error and carry on.
        let error = match self.validate(&wasm) {
            Ok(()) => return Ok(()),
            Err(e) => e,
        };
        let offset = match error.downcast_ref::<wasmparser::Error>() {
            Some(err) => err.offset(),
            None => return Err(error.into()),
        };
        match self.annotate_error_with_file_and_line(&wasm, offset) {
            Ok(Some(msg)) => Err(error.context(msg)),
            Ok(None) => Err(error.into()),
            Err(e) => {
                log::warn!("failed to parse DWARF information: {e:?}");
                Err(error.into())
            }
        }
    }

    fn validate(&self, wasm: &[u8]) -> Result<()> {
        wasm_tools::validate(self.features.features(), self.validate_custom, wasm)
    }

    fn annotate_error_with_file_and_line(
        &self,
        wasm: &[u8],
        offset: u64,
    ) -> Result<Option<String>> {
        let mut modules = Addr2lineModules::parse(wasm)?;
        let code_section_relative = false;
        let (context, text_rel) = match modules.context(offset, code_section_relative)? {
            Some(pair) => pair,
            None => return Ok(None),
        };

        let mut frames = match context.find_frames(text_rel) {
            LookupResult::Output(result) => result?,
            LookupResult::Load { .. } => return Ok(None),
        };
        let frame = match frames.next()? {
            Some(frame) => frame,
            None => return Ok(None),
        };

        let mut out = String::new();
        if let Some(loc) = &frame.location {
            if let Some(file) = loc.file {
                write!(out, "{file}")?;
            }
            if let Some(line) = loc.line {
                write!(out, ":{line}")?;
            }
            if let Some(column) = loc.column {
                write!(out, ":{column}")?;
            }
            write!(out, " ")?;
        }
        if let Some(func) = &frame.function {
            write!(out, "function `{}` failed to validate", func.demangle()?)?;
        }

        if out.is_empty() {
            Ok(None)
        } else {
            Ok(Some(out))
        }
    }
}

