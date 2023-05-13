use anyhow::Result;
use clap::Parser;
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
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Output the text format of WebAssembly instead of the binary format.
    #[clap(short = 't', long)]
    wat: bool,

    #[clap(flatten)]
    wasm_mutate: wasm_mutate::WasmMutate<'static>,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(mut self) -> Result<()> {
        let input_wasm = self.io.parse_input_wasm()?;

        // Currently `self.wasm_mutate` is typed as `'static` for the input wasm
        // due to how this subcommand is defined. To get the input wasm to live
        // for that long we simply leak it, and this shouldn't matter too much
        // in the grand scheme of things since this is a short-lived process
        // anyway.
        let input_wasm = Box::leak(input_wasm.into_boxed_slice());

        let mut output_wasms =
            unwrap_wasm_mutate_result(self.wasm_mutate.run(input_wasm)).take(100);
        let wasm = loop {
            let res = match output_wasms.next() {
                Some(res) => res,
                None => {
                    eprintln!("no mutations found");
                    std::process::exit(3);
                }
            };
            match res {
                Err(e) if matches!(e.kind(), ErrorKind::NoMutationsApplicable) => {
                    // Try the next mutation.
                    continue;
                }
                _ => break unwrap_wasm_mutate_result(res),
            }
        };

        self.io.output(wasm_tools::Output::Wasm {
            bytes: &wasm,
            wat: self.wat,
        })?;

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
