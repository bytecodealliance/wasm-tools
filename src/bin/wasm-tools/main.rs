use anyhow::Result;
use clap::Parser;

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
}

fn main() -> Result<()> {
    env_logger::init();
    <WasmTools as Parser>::parse().run()
}
