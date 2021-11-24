use anyhow::Context;
use std::fs;
use std::io::{stdin, stdout, Read, Write};
use std::path::PathBuf;
use structopt::StructOpt;

/// A WebAssembly test case mutator.
///
/// `wasm-mutate` takes in an existing Wasm module and then applies a
/// pseudo-random transformation to it, producing a new, mutated Wasm
/// module. This new, mutated Wasm module can be fed as a test input to your
/// Wasm parser, validator, compiler, or any other Wasm-consuming
/// tool. `wasm-mutate` can serve as a custom mutator for mutation-based
/// fuzzing.
///
/// ## Example
///
/// Perform a random mutation on an existing Wasm module:
///
/// $ wasm-mutate ./input.wasm --seed 1234 -o output.wasm
///
/// ## Exit Codes
///
/// * 0: Success
///
/// * 1: An unexpected failure occurred.
///
/// * 2: Failed to mutate the Wasm module with the given seed and mutation
///      constraints. (Happens rarely with reasonable constraints; try again
///      with a new seed or loosen your constraints.)
///
/// * 3: The input Wasm module uses a Wasm feature or proposal that is not yet
///      supported by `wasm-mutate`.
///
/// * 4: The input is not a valid Wasm module.
///
/// * 5: Constructing the Ast for code motion mutations fails
#[derive(StructOpt)]
struct Options {
    /// The input WebAssembly binary that will be mutated.
    ///
    /// `stdin` is used if this argument is not supplied.
    #[structopt(parse(from_os_str))]
    input: Option<PathBuf>,

    /// The output file path, where the new, mutated WebAssembly module is
    /// placed.
    ///
    /// `stdout` is used if this argument is not supplied.
    #[structopt(short, long, parse(from_os_str))]
    output: Option<PathBuf>,

    #[structopt(flatten)]
    wasm_mutate: wasm_mutate::WasmMutate,
}

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let opts = Options::from_args();

    let stdin = stdin();
    let (mut input, input_name): (Box<dyn Read>, _) = match &opts.input {
        Some(f) => {
            let input = Box::new(
                fs::File::open(f).with_context(|| format!("failed to open '{}'", f.display()))?,
            );
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
            let output = Box::new(
                fs::File::create(f)
                    .with_context(|| format!("failed to create '{}'", f.display()))?,
            );
            (output, f.display().to_string())
        }
        None => {
            let output = Box::new(stdout.lock());
            (output, "<stdout>".to_string())
        }
    };

    let mut input_wasm = vec![];
    input
        .read_to_end(&mut input_wasm)
        .with_context(|| format!("failed to read '{}'", input_name))?;
    let it = opts.wasm_mutate.run(&input_wasm);
    let mut output_wasm = match it {
        Ok(w) => w,
        Err(e) => {
            let code = match &e {
                wasm_mutate::Error::NoMutationsApplicable => 2,
                wasm_mutate::Error::UnsupportedType(_) => 3,
                wasm_mutate::Error::Parse(_) => 4,
                wasm_mutate::Error::InvalidAstOperation(_) => 5,
            };
            eprintln!("{}", e);
            std::process::exit(code);
        }
    };

    let mut first_good = Err(wasm_mutate::Error::NoMutationsApplicable);

    while let Some(w) = output_wasm.next() {
        match w {
            Ok(w) => {
                first_good = Ok(w);
                break;
            }
            Err(e) => {
                let code = match &e {
                    wasm_mutate::Error::NoMutationsApplicable => {
                        // Continue to the next one if its is type no mutation
                        // otherwrise, raise an error
                        continue;
                    }
                    wasm_mutate::Error::UnsupportedType(_) => 3,
                    wasm_mutate::Error::Parse(_) => 4,
                    wasm_mutate::Error::InvalidAstOperation(_) => 5,
                };
                eprintln!("{}", e);
                std::process::exit(code);
            }
        }
    }

    match first_good {
        Ok(w) => {
            output
                .write_all(&w)
                .with_context(|| format!("failed to write to '{}'", output_name))?;
        }
        Err(e) => {
            let code = match &e {
                wasm_mutate::Error::NoMutationsApplicable => 2,
                wasm_mutate::Error::UnsupportedType(_) => 3,
                wasm_mutate::Error::Parse(_) => 4,
                wasm_mutate::Error::InvalidAstOperation(_) => 5,
            };
            eprintln!("{}", e);
            std::process::exit(code);
        }
    }

    Ok(())
}
