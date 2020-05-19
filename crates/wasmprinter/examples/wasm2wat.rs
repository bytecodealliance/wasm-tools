use anyhow::Context;
use getopts::Options;
use std::env;

fn main() -> anyhow::Result<()> {
    let mut opts = Options::new();
    opts.optopt("o", "", "set output file name", "NAME");
    opts.optflag("h", "help", "print this help menu");
    let matches = opts.parse(env::args_os().skip(1))?;
    if matches.opt_present("h") {
        print_usage(opts);
        return Ok(());
    }
    let input = if matches.free.len() == 1 {
        matches.free[0].clone()
    } else {
        print_usage(opts);
        std::process::exit(1);
    };

    let wit = wasmprinter::print_file(&input)?;
    if let Some(output) = matches.opt_str("o") {
        std::fs::write(&output, wit).context(format!("failed to write `{}`", output))?;
    } else {
        println!("{}", wit);
    }

    Ok(())
}

fn print_usage(opts: Options) {
    let program = env::args().next().unwrap();
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}
