use anyhow::Result;
use clap::Parser;
use std::io;
use std::process::ExitCode;

macro_rules! subcommands {
    ($(($name:ident, $string:tt))*) => {
        $(
            #[cfg(feature = $string)]
            mod $name;
        )*

        #[derive(Parser)]
        #[clap(version)]
        #[allow(non_camel_case_types)]
        enum WasmTools {
            $(
                #[cfg(feature = $string)]
                $name {
                    #[clap(flatten)]
                    opts: $name::Opts,
                },
            )*
        }

        impl WasmTools {
            fn run(self) -> Result<()> {
                match self {
                    $(
                        #[cfg(feature = $string)]
                        WasmTools::$name { opts } => opts.run(),
                    )*
                }
            }
        }
    }
}

subcommands! {
    (parse, "parse")
    (validate, "validate")
    (print, "print")
    (smith, "smith")
    (shrink, "shrink")
    (mutate, "mutate")
    (dump, "dump")
    (objdump, "objdump")
    (strip, "strip")
    (compose, "compose")
}

fn main() -> ExitCode {
    env_logger::init();
    let err = match <WasmTools as Parser>::parse().run() {
        Ok(()) => return ExitCode::SUCCESS,
        Err(e) => e,
    };
    // If an error happened and it's connected to something like `EPIPE` then
    // don't print out an error and instead just silently exit with a failure.
    // This prevents stray panic messages when the stdout pipe is closed, for
    // example.
    if let Some(io) = err.downcast_ref::<io::Error>() {
        match io.kind() {
            io::ErrorKind::BrokenPipe => return ExitCode::FAILURE,
            _ => {}
        }
    }
    eprintln!("Error: {:?}", err);
    ExitCode::FAILURE
}
