//! An example program showing how to validate a WebAssembly binary.
//!
//! Note that this does not use the [`Validator::validate_all`] convenience
//! function, but instead it copies it and tweaks its contents to do parallel
//! validation of all functions after parsing.

use anyhow::{Context, Result};
use rayon::prelude::*;
use std::env;
use std::time::Instant;
use wasmparser::{Parser, ValidPayload, Validator, WasmFeatures};

fn main() -> Result<()> {
    env_logger::init();

    // Use the `getopts` crate to parse the `-o` option as well as `-h`
    let program = env::args().nth(0).unwrap();
    let mut opts = getopts::Options::new();
    opts.optflag(
        "",
        "enable-reference-types",
        "Enable wasm reference types feature",
    );
    opts.optflag("", "enable-threads", "Enable wasm threads feature");
    opts.optflag("", "enable-simd", "Enable wasm simd feature");
    opts.optflag(
        "",
        "enable-bulk-memory",
        "Enable wasm bulk memory operations feature",
    );
    opts.optflag("", "enable-multi-value", "Enable wasm multi-value feature");
    opts.optflag("", "enable-tail-call", "Enable wasm tail-call feature");
    opts.optflag(
        "",
        "enable-module-linking",
        "Enable wasm module-linking feature",
    );
    opts.optflag(
        "",
        "deterministic-only",
        "Require only deterministic instructions",
    );
    opts.optflag("h", "help", "print this help menu");
    let matches = opts.parse(env::args_os().skip(1))?;
    if matches.opt_present("h") {
        return Ok(print_usage(&program, opts));
    }
    let input = match matches.free.len() {
        0 => {
            print_usage(&program, opts);
            std::process::exit(1);
        }
        1 => &matches.free[0],
        _ => anyhow::bail!("more than one input file specified on command line"),
    };

    // Create a `Validator` configured with all of the wasm features according
    // to our CLI flags.
    let mut validator = Validator::new();
    validator.wasm_features(WasmFeatures {
        threads: matches.opt_present("enable-threads"),
        reference_types: matches.opt_present("enable-reference-types"),
        simd: matches.opt_present("enable-simd"),
        bulk_memory: matches.opt_present("enable-bulk-memory"),
        multi_value: matches.opt_present("enable-multi-value"),
        tail_call: matches.opt_present("enable-tail-call"),
        module_linking: matches.opt_present("enable-module-linking"),
        deterministic_only: matches.opt_present("deterministic-only"),
    });

    // Note that here we're copying the contents of `Validator::validate_all`,
    // but the end is followed up with a parallel iteration over the functions
    // to validate instead of a synchronous validation.
    //
    // The general idea here is that we're going to use `Parser::parse_all` to
    // divvy up the input bytes into chunks. We'll maintain which `Validator`
    // we're using as we navigate nested modules (the module linking proposal)
    // and any functions found are deferred to get validated later.
    let mut functions_to_validate = Vec::new();
    let mut stack = Vec::new();
    let wasm = std::fs::read(input).context(format!("failed to read input: {}", input))?;
    let start = Instant::now();
    for payload in Parser::new(0).parse_all(&wasm) {
        match validator.payload(&payload?)? {
            ValidPayload::Ok => {}
            ValidPayload::Pop => validator = stack.pop().unwrap(),
            ValidPayload::Push(_parser, next) => {
                stack.push(validator);
                validator = next;
            }
            ValidPayload::Func(validator, body) => functions_to_validate.push((validator, body)),
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

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}
