use anyhow::Result;
use std::env;

fn main() -> Result<()> {
    env_logger::init();

    // Use the `getopts` crate to parse the `-o` option as well as `-h`
    let program = env::args().nth(0).unwrap();
    let mut opts = getopts::Options::new();
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

    let input = std::fs::read(&input)?;
    println!("{}", wasmparser_dump::dump_wasm(&input)?);

    Ok(())
}

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}
