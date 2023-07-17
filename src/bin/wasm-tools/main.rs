use anyhow::Result;
use clap::Parser;
use std::io::{self, IsTerminal, Write};
use std::process::ExitCode;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

macro_rules! subcommands {
    ($(
        $(#[$attr:meta])*
        ($name:ident, $string:tt $($cfg:tt)*)
    )*) => {
        $(
            #[cfg(feature = $string)]
            $($cfg)*
            mod $name;
        )*

        #[derive(Parser)]
        #[clap(version = version())]
        #[allow(non_camel_case_types)]
        enum WasmTools {
            $(
                #[cfg(feature = $string)]
                $($cfg)*
                $(#[$attr])*
                $name($name::Opts),
            )*
        }

        impl WasmTools {
            fn run(self) -> Result<()> {
                match self {
                    $(
                        #[cfg(feature = $string)]
                        $($cfg)*
                        Self::$name(opts) => opts.run(),
                    )*
                }
            }

            fn general_opts(&self) -> &wasm_tools::GeneralOpts {
                match *self {
                    $(
                        #[cfg(feature = $string)]
                        $($cfg)*
                        Self::$name(ref opts) => opts.general_opts(),
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
    // The shrink subcommand relies on executing new processes to test a
    // predicate which isn't supported on wasm, so always omit this command on
    // wasm.
    (shrink, "shrink" #[cfg(not(target_family = "wasm"))])
    (mutate, "mutate")
    (dump, "dump")
    (objdump, "objdump")
    (strip, "strip")
    (compose, "compose")
    (demangle, "demangle")
    #[command(subcommand)]
    (component, "component")
    #[command(subcommand)]
    (metadata, "metadata")
    (wit_smith, "wit-smith")
    (addr2line, "addr2line")
}

fn main() -> ExitCode {
    let args = <WasmTools as Parser>::parse();
    args.general_opts().init_logger();
    let color = args.general_opts().color;
    let err = match args.run() {
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

    // ignore errors here since if we fail to print an error it's not like we
    // can print it again.
    let _ = print_error(color, err);
    ExitCode::FAILURE
}

fn print_error(color: ColorChoice, err: anyhow::Error) -> Result<()> {
    let color = if color == ColorChoice::Auto && !io::stderr().is_terminal() {
        ColorChoice::Never
    } else {
        color
    };
    let mut stderr = StandardStream::stderr(color);
    stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
    write!(stderr, "error")?;
    stderr.set_color(ColorSpec::new().set_fg(None).set_bold(true))?;
    write!(stderr, ": ")?;

    let msg = err.to_string();
    for (i, line) in msg.lines().enumerate() {
        writeln!(stderr, "{line}")?;
        if i == 0 {
            stderr.set_color(ColorSpec::new().set_reset(true))?;
        }
    }

    if err.chain().len() == 1 {
        return Ok(());
    }
    writeln!(stderr, "\nCaused by:")?;
    for (i, err) in err.chain().skip(1).enumerate() {
        writeln!(
            stderr,
            "{i:>5}: {}",
            err.to_string().replace("\n", "\n       ")
        )?;
    }
    return Ok(());
}

/// If CARGO_VERSION_INFO is set, use it, otherwise use CARGO_PKG_VERSION.
fn version() -> &'static str {
    option_env!("CARGO_VERSION_INFO").unwrap_or(env!("CARGO_PKG_VERSION"))
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    WasmTools::command().debug_assert()
}
