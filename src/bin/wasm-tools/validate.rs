use anyhow::{anyhow, Context, Result};
use rayon::prelude::*;
use std::mem;
use std::time::Instant;
use wasmparser::{FuncValidatorAllocations, Parser, ValidPayload, Validator, WasmFeatures};

/// Validate a WebAssembly binary
///
/// This subcommand will validate a WebAssembly binary to determine if it's
/// valid or not. This implements the validation algorithm of the WebAssembly
/// specification. The process will exit with 0 and no output if the binary is
/// valid, or nonzero and an error message on stderr if the binary is not valid.
///
/// Examples:
///
/// ```sh
/// # Validate `foo.wasm` with the default Wasm feature proposals.
/// $ wasm-tools validate foo.wasm
///
/// # Validate `foo.wasm` with more verbose output
/// $ wasm-tools validate -vv foo.wasm
///
/// # Validate `fancy.wasm` with all Wasm feature proposals enabled.
/// $ wasm-tools validate --features all fancy.wasm
///
/// # Validate `mvp.wasm` without any Wasm feature proposals enabled.
/// $ wasm-tools validate --features=-all mvp.wasm
/// ```
#[derive(clap::Parser)]
pub struct Opts {
    /// Comma-separated list of WebAssembly features to enable during validation.
    ///
    /// The placeholder "all" can be used to enable all wasm features. If a "-"
    /// character is present in front of a feature it will disable that feature.
    /// For example "all,-simd" would enable everything but simd.
    ///
    /// Available feature options can be found in the wasmparser crate:
    /// https://github.com/bytecodealliance/wasm-tools/blob/main/crates/wasmparser/src/validator.rs
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
        let wasm = self.io.parse_input_wasm()?;

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
}

fn parse_features(arg: &str) -> Result<WasmFeatures> {
    let mut ret = WasmFeatures::default();

    const FEATURES: &[(&str, fn(&mut WasmFeatures) -> &mut bool)] = &[
        ("reference-types", |f| &mut f.reference_types),
        ("function-references", |f| &mut f.function_references),
        ("simd", |f| &mut f.simd),
        ("threads", |f| &mut f.threads),
        ("bulk-memory", |f| &mut f.bulk_memory),
        ("multi-value", |f| &mut f.multi_value),
        ("tail-call", |f| &mut f.tail_call),
        ("component-model", |f| &mut f.component_model),
        ("component-model-values", |f| &mut f.component_model_values),
        ("multi-memory", |f| &mut f.multi_memory),
        ("exception-handling", |f| &mut f.exceptions),
        ("memory64", |f| &mut f.memory64),
        ("extended-const", |f| &mut f.extended_const),
        ("floats", |f| &mut f.floats),
        ("saturating-float-to-int", |f| {
            &mut f.saturating_float_to_int
        }),
        ("sign-extension", |f| &mut f.sign_extension),
        ("mutable-global", |f| &mut f.mutable_global),
        ("relaxed-simd", |f| &mut f.relaxed_simd),
        ("gc", |f| &mut f.gc),
    ];

    for part in arg.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
        let (enable, part) = if let Some(part) = part.strip_prefix("-") {
            (false, part)
        } else {
            (true, part)
        };
        match part {
            "all" => {
                for (name, accessor) in FEATURES {
                    // don't count this under "all" for now.
                    if *name == "deterministic" {
                        continue;
                    }

                    *accessor(&mut ret) = enable;
                }
            }

            name => {
                let (_, accessor) = FEATURES.iter().find(|(n, _)| *n == name).ok_or_else(|| {
                    anyhow!(
                        "unknown feature `{}`\nValid features: {}",
                        name,
                        FEATURES
                            .iter()
                            .map(|(name, _)| *name)
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                })?;
                *accessor(&mut ret) = enable;
            }
        }
    }

    Ok(ret)
}
