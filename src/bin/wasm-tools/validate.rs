use anyhow::{anyhow, Result};
use rayon::prelude::*;
use std::path::PathBuf;
use std::time::Instant;
use wasmparser::{Parser, ValidPayload, Validator, WasmFeatures};

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
    #[clap(long, short = 'f', parse(try_from_str = parse_features))]
    features: Option<WasmFeatures>,

    /// Input WebAssembly file to validate.
    ///
    /// This can either be a WebAssembly binary (*.wasm) or a WebAssembly text
    /// file (*.wat) which will be translated to binary before validation.
    input: PathBuf,
}

impl Opts {
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
        let mut validator = Validator::new();
        if let Some(features) = self.features {
            validator.wasm_features(features);
        }
        let mut functions_to_validate = Vec::new();
        let wasm = wat::parse_file(&self.input)?;

        let start = Instant::now();
        for payload in Parser::new(0).parse_all(&wasm) {
            match validator.payload(&payload?)? {
                ValidPayload::Ok | ValidPayload::Submodule(_) => {}
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
        functions_to_validate
            .into_par_iter()
            .try_for_each(|(mut validator, body)| validator.validate(&body))?;
        log::info!("functions validated in {:?}", start.elapsed());
        Ok(())
    }
}

fn parse_features(arg: &str) -> Result<WasmFeatures> {
    let mut ret = WasmFeatures::default();

    const FEATURES: &[(&str, fn(&mut WasmFeatures) -> &mut bool)] = &[
        ("reference-types", |f| &mut f.reference_types),
        ("simd", |f| &mut f.simd),
        ("threads", |f| &mut f.threads),
        ("bulk-memory", |f| &mut f.bulk_memory),
        ("multi-value", |f| &mut f.multi_value),
        ("tail-call", |f| &mut f.tail_call),
        ("module-linking", |f| &mut f.module_linking),
        ("multi-memory", |f| &mut f.multi_memory),
        ("exception-handling", |f| &mut f.exceptions),
        ("memory64", |f| &mut f.memory64),
        ("extended-const", |f| &mut f.extended_const),
        ("deterministic", |f| &mut f.deterministic_only),
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
                let (_, accessor) = FEATURES
                    .iter()
                    .find(|(n, _)| *n == name)
                    .ok_or_else(|| anyhow!("unknown feature `{}`", name))?;
                *accessor(&mut ret) = enable;
            }
        }
    }

    Ok(ret)
}
