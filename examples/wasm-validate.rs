use anyhow::{Context, Result};
use std::env;

fn main() -> Result<()> {
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
    #[cfg(feature = "deterministic")]
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

    let config = wasmparser::ValidatingParserConfig {
        operator_config: wasmparser::OperatorValidatorConfig {
            enable_threads: matches.opt_present("enable-threads"),
            enable_reference_types: matches.opt_present("enable-reference-types"),
            enable_simd: matches.opt_present("enable-simd"),
            enable_bulk_memory: matches.opt_present("enable-bulk-memory"),
            enable_multi_value: matches.opt_present("enable-multi-value"),
            #[cfg(feature = "deterministic")]
            deterministic_only: matches.opt_present("deterministic-only"),
        },
    };
    let wasm = std::fs::read(input).context(format!("failed to read input: {}", input))?;
    wasmparser::validate(&wasm, Some(config)).unwrap();

    Ok(())
}

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}
