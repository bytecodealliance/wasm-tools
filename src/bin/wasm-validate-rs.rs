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

const FEATURES: &[(&str, &str, fn(&mut WasmFeatures) -> &mut bool)] = &[
    ("reference-types", "wasm reference types feature", |f| {
        &mut f.reference_types
    }),
    ("simd", "wasm simd feature", |f| &mut f.simd),
    ("threads", "wasm threads feature", |f| &mut f.threads),
    ("bulk-memory", "wasm bulk memory operations feature", |f| {
        &mut f.bulk_memory
    }),
    ("multi-value", "wasm multi-value feature", |f| {
        &mut f.multi_value
    }),
    ("tail-call", "wasm tail-call feature", |f| &mut f.tail_call),
    ("module-linking", "wasm module-linking feature", |f| {
        &mut f.module_linking
    }),
    ("multi-memory", "wasm multi-memory feature", |f| {
        &mut f.multi_memory
    }),
    ("memory64", "wasm memory64 feature", |f| &mut f.memory64),
];

fn main() -> Result<()> {
    env_logger::init();

    let program = env::args().nth(0).unwrap();
    let mut opts = getopts::Options::new();

    for (name, desc, _) in FEATURES {
        opts.optflag("", &format!("enable-{}", name), &format!("Enable {}", desc));
        opts.optflag(
            "",
            &format!("disable-{}", name),
            &format!("Disable {}", desc),
        );
    }
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

    // Configure enabled wasm features according to the CLI arguments. Note that
    // this isn't required for ussage of `Validator` if you're just using
    // wasmparser's defaults.
    let mut features = WasmFeatures::default();
    for (name, _, get) in FEATURES {
        if matches.opt_present(&format!("enable-{}", name)) {
            *get(&mut features) = true;
        }
        if matches.opt_present(&format!("disable-{}", name)) {
            *get(&mut features) = false;
        }
    }

    // Note that here we're copying the contents of `Validator::validate_all`,
    // but the end is followed up with a parallel iteration over the functions
    // to validate instead of a synchronous validation.
    //
    // The general idea here is that we're going to use `Parser::parse_all` to
    // divvy up the input bytes into chunks. We'll maintain which `Validator`
    // we're using as we navigate nested modules (the module linking proposal)
    // and any functions found are deferred to get validated later.
    let mut validator = Validator::new();
    validator.wasm_features(features);
    let mut functions_to_validate = Vec::new();
    let wasm = std::fs::read(input).context(format!("failed to read input: {}", input))?;
    let start = Instant::now();
    for payload in Parser::new(0).parse_all(&wasm) {
        match validator.payload(&payload?)? {
            ValidPayload::Ok | ValidPayload::Submodule(_) => {}
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
