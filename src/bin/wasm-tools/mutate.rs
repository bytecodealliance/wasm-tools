use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::io::{stdin, stdout, Read, Write};
use std::path::PathBuf;
use wasm_mutate::ErrorKind;

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
/// * 2: Failed to parse or validate the input Wasm module.
///
/// * 3: Failed to mutate the Wasm module with the given seed and mutation
///      constraints. (Happens rarely with reasonable constraints; try again
///      with a new seed or loosen your constraints.)
///
/// * 4: Ran out of fuel before successfully applying a mutation.
///
/// * 5: The input Wasm module uses a Wasm feature or proposal that is not yet
///      supported by `wasm-mutate`.
///
/// * 6: Other error.
#[derive(Parser)]
pub struct Opts {
    /// The input WebAssembly binary that will be mutated.
    ///
    /// `stdin` is used if this argument is not supplied.
    #[clap(parse(from_os_str))]
    input: Option<PathBuf>,

    /// The output file path, where the new, mutated WebAssembly module is
    /// placed.
    ///
    /// `stdout` is used if this argument is not supplied.
    #[clap(short, long, parse(from_os_str))]
    output: Option<PathBuf>,

    #[clap(flatten)]
    wasm_mutate: wasm_mutate::WasmMutate<'static>,
}

impl Opts {
    pub fn run(mut self) -> Result<()> {
        let stdin = stdin();
        let (mut input, input_name): (Box<dyn Read>, _) = match &self.input {
            Some(f) => {
                let input = Box::new(
                    fs::File::open(f)
                        .with_context(|| format!("failed to open '{}'", f.display()))?,
                );
                (input, f.display().to_string())
            }
            None => {
                let input = Box::new(stdin.lock());
                (input, "<stdin>".to_string())
            }
        };

        let stdout = stdout();
        let (mut output, output_name): (Box<dyn Write>, _) = match &self.output {
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
        let input_wasm = wat::parse_bytes(&input_wasm)
            .with_context(|| format!("failed to parse '{}'", input_name))?;

        // Currently `self.wasm_mutate` is typed as `'static` for the input wasm
        // due to how this subcommand is defined. To get the input wasm to live
        // for that long we simply leak it, and this shouldn't matter too much
        // in the grand scheme of things since this is a short-lived process
        // anyway.
        let input_wasm = Box::leak(input_wasm.into_owned().into_boxed_slice());

        let mut output_wasms =
            unwrap_wasm_mutate_result(self.wasm_mutate.run(input_wasm)).take(100);
        let wasm = loop {
            if let Some(res) = output_wasms.next() {
                match res {
                    Err(e) if matches!(e.kind(), ErrorKind::NoMutationsApplicable) => {
                        // Try the next mutation.
                        continue;
                    }
                    _ => break unwrap_wasm_mutate_result(res),
                }
            }
        };

        output
            .write_all(&wasm)
            .with_context(|| format!("failed to write to '{}'", output_name))?;

        Ok(())
    }
}

fn unwrap_wasm_mutate_result<T>(result: wasm_mutate::Result<T>) -> T {
    match result {
        Ok(x) => x,
        Err(e) => {
            let code = match e.kind() {
                ErrorKind::Parse(_) => 2,
                ErrorKind::NoMutationsApplicable => 3,
                ErrorKind::OutOfFuel => 4,
                ErrorKind::Unsupported(_) => 5,
                ErrorKind::Other(_) => 6,
            };
            eprintln!("{}", e);
            std::process::exit(code);
        }
    }
}
