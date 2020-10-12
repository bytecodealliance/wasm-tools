use arbitrary::Arbitrary;
use std::fs;
use std::io::{stdin, stdout, Read, Write};
use std::path::PathBuf;
use std::process;
use structopt::StructOpt;
use wasm_smith::Module;

/// A WebAssembly test case generator.
///
/// Given an arbitrary input seed, `wasm-smith` generates a valid WebAssembly
/// module. The input seed is interpreted as a series of predetermined choices
/// through a decision tree. Given the same input seed, `wasm-smith` will always
/// generate the same output WebAssembly module; it is deterministic. Larger
/// input seeds tend to generate larger WebAssembly modules. Small changes to
/// the input seed tends to produce a small change to the output WebAssembly
/// module. These properties, taken together, make `wasm-smith` suitable for use
/// not just with purely random input seeds, but also with coverage-guided,
/// mutation-based fuzzing engines like libFuzzer and AFL.
///
/// ## Example
///
/// Generate a WebAssembly module from 100 bytes of random data:
///
/// $ head -c 100 /dev/urandom | wasm-smith -o test.wasm
///
/// ## Exit Codes
///
/// * 0: Success.
///
/// * 1: An unexpected failure occurred.
///
/// * 2: Failed to generate a Webassembly module from the input seed. (Happens
///      rarely; try again with a new input.)
#[derive(StructOpt)]
struct Options {
    /// The arbitrary input seed.
    ///
    /// `stdin` is used if this argument is not supplied.
    #[structopt(parse(from_os_str))]
    input: Option<PathBuf>,

    /// The output file path, where the generated WebAssembly module is
    /// placed.
    ///
    /// `stdout` is used if this argument is not supplied.
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    output: Option<PathBuf>,

    /// Ensure that execution of generated Wasm modules will always terminate.
    ///
    /// This inserts a global "fuel" counter that is decremented at loop headers
    /// and in function prologues. When the fuel reaches 0, a trap is raised to
    /// terminate execution. Control the default amount of fuel with the
    /// `--fuel` flag.
    #[structopt(short = "t", long = "ensure-termination")]
    ensure_termination: bool,

    /// The default amount of fuel used with `--ensure-termination`.
    ///
    /// This is roughly the number of loop iterations and function calls that
    /// will be executed before a trap is raised to prevent infinite loops.
    #[structopt(short = "f", long = "fuel", default_value = "100")]
    fuel: u32,
}

fn main() {
    let opts = Options::from_args();

    let stdin = stdin();
    let (mut input, input_name): (Box<dyn Read>, _) = match &opts.input {
        Some(f) => {
            let input = Box::new(fs::File::open(f).unwrap_or_else(|e| {
                eprintln!("error: failed to open '{}': {}", f.display(), e);
                process::exit(1);
            }));
            (input, f.display().to_string())
        }
        None => {
            let input = Box::new(stdin.lock());
            (input, "<stdin>".to_string())
        }
    };

    let stdout = stdout();
    let (mut output, output_name): (Box<dyn Write>, _) = match &opts.output {
        Some(f) => {
            let output = Box::new(fs::File::create(f).unwrap_or_else(|e| {
                eprintln!("error: failed to create '{}': {}", f.display(), e);
                process::exit(1);
            }));
            (output, f.display().to_string())
        }
        None => {
            let output = Box::new(stdout.lock());
            (output, "<stdout>".to_string())
        }
    };

    let mut seed = vec![];
    input.read_to_end(&mut seed).unwrap_or_else(|e| {
        eprintln!("error: failed to '{}': {}", input_name, e);
        process::exit(1);
    });

    let mut u = arbitrary::Unstructured::new(&seed);
    let mut module = Module::arbitrary(&mut u).unwrap_or_else(|e| {
        eprintln!("error: failed to generate module: {}", e);
        process::exit(2);
    });

    if opts.ensure_termination {
        module.ensure_termination(opts.fuel);
    }

    let wasm_bytes = module.to_bytes();
    output.write_all(&wasm_bytes).unwrap_or_else(|e| {
        eprintln!("error: failed to write to '{}': {}", output_name, e);
        process::exit(1);
    });

    drop(output);
    process::exit(0);
}
