use addr2line::LookupResult;
use anyhow::{anyhow, bail, Context, Result};
use bitflags::Flags;
use rayon::prelude::*;
use std::fmt::Write;
use std::mem;
use std::time::Instant;
use wasm_tools::addr2line::Addr2lineModules;
use wasmparser::{
    BinaryReaderError, FuncValidatorAllocations, Parser, ValidPayload, Validator, WasmFeatures,
};

/// Validate a WebAssembly binary
///
/// This subcommand will validate a WebAssembly binary to determine if it's
/// valid or not. This implements the validation algorithm of the WebAssembly
/// specification. The process will exit with 0 and no output if the binary is
/// valid, or nonzero and an error message on stderr if the binary is not valid.
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
")]
pub struct Opts {
    /// Comma-separated list of WebAssembly features to enable during
    /// validation.
    ///
    /// If a "-" character is present in front of a feature it will disable that
    /// feature. For example "-simd" will disable the simd proposal.
    ///
    /// The placeholder "all" can be used to enable all wasm features and the
    /// term "-all" can be used to disable all features.
    ///
    /// The default set of features enabled are all WebAssembly proposals that
    /// are at phase 4 or after. This means that the default set of features
    /// accepted are relatively bleeding edge. Versions of the WebAssembly
    /// specification can also be selected. The "wasm1" or "mvp" feature can
    /// select the original WebAssembly specification and "wasm2" can be used to
    /// select the 2.0 version.
    ///
    /// Available feature options can be found in the wasmparser crate:
    /// <https://github.com/bytecodealliance/wasm-tools/blob/main/crates/wasmparser/src/features.rs>
    #[clap(long, short = 'f', value_parser = parse_features)]
    features: Option<WasmFeatures>,

    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let wasm = self.io.parse_input_wasm()?;

        // If validation fails then try to attach extra information to the
        // error based on DWARF information in the input wasm binary. If
        // DWARF information isn't present or if the DWARF failed to get parsed
        // then ignore the error and carry on.
        let error = match self.validate(&wasm) {
            Ok(()) => return Ok(()),
            Err(e) => e,
        };
        let offset = match error.downcast_ref::<BinaryReaderError>() {
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
        // Note that here we're copying the contents of
        // `Validator::validate_all`, but the end is followed up with a parallel
        // iteration over the functions to validate instead of a synchronous
        // validation.
        //
        // The general idea here is that we're going to use `Parser::parse_all`
        // to divvy up the input bytes into chunks. We'll maintain which
        // `Validator` we're using as we navigate nested modules (the module
        // linking proposal) and any functions found are deferred to get
        // validated later.
        let mut validator = Validator::new_with_features(self.features.unwrap_or_default());
        let mut functions_to_validate = Vec::new();

        let start = Instant::now();
        for payload in Parser::new(0).parse_all(&wasm) {
            match validator.payload(&payload?)? {
                ValidPayload::Ok | ValidPayload::Parser(_) | ValidPayload::End(_) => {}
                ValidPayload::Func(validator, body) => {
                    functions_to_validate.push((validator, body))
                }
            }
        }
        log::info!("module structure validated in {:?}", start.elapsed());

        // After we've validate the entire wasm module we'll use `rayon` to iterate
        // over all functions in parallel and perform parallel validation of the
        // input wasm module.
        let start = Instant::now();
        functions_to_validate.into_par_iter().try_for_each_init(
            FuncValidatorAllocations::default,
            |allocs, (to_validate, body)| -> Result<_> {
                let mut validator = to_validate.into_validator(mem::take(allocs));
                validator
                    .validate(&body)
                    .with_context(|| format!("func {} failed to validate", validator.index()))?;
                *allocs = validator.into_allocations();
                Ok(())
            },
        )?;
        log::info!("functions validated in {:?}", start.elapsed());
        Ok(())
    }

    fn annotate_error_with_file_and_line(
        &self,
        wasm: &[u8],
        offset: usize,
    ) -> Result<Option<String>> {
        let mut modules = Addr2lineModules::parse(wasm)?;
        let code_section_relative = false;
        let (context, text_rel) = match modules.context(offset as u64, code_section_relative)? {
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

fn parse_features(arg: &str) -> Result<WasmFeatures> {
    let mut ret = WasmFeatures::default();

    fn flag_name(flag: &bitflags::Flag<WasmFeatures>) -> String {
        flag.name().to_lowercase().replace('_', "-")
    }

    for part in arg.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
        let (enable, part) = if let Some(part) = part.strip_prefix("-") {
            (false, part)
        } else {
            (true, part)
        };
        match part {
            "all" => {
                for flag in WasmFeatures::FLAGS.iter() {
                    ret.set(*flag.value(), enable);
                }
            }
            "wasm1" | "mvp" => {
                if !enable {
                    bail!("cannot disable `{part}`, it can only be enabled");
                }
                ret = WasmFeatures::wasm1();
            }
            "wasm2" => {
                if !enable {
                    bail!("cannot disable `{part}`, it can only be enabled");
                }
                ret = WasmFeatures::wasm2();
            }

            name => {
                let flag = WasmFeatures::FLAGS
                    .iter()
                    .find(|f| flag_name(f) == name)
                    .ok_or_else(|| {
                        anyhow!(
                            "unknown feature `{}`\nValid features: {}",
                            name,
                            WasmFeatures::FLAGS
                                .iter()
                                .map(flag_name)
                                .collect::<Vec<_>>()
                                .join(", "),
                        )
                    })?;
                ret.set(*flag.value(), enable);
            }
        }
    }

    Ok(ret)
}
