use anyhow::Result;
use structopt::StructOpt;

macro_rules! subcommands {
    ($(($name:ident, $string:tt))*) => {
        $(
            #[cfg(feature = $string)]
            mod $name;
        )*

        #[derive(StructOpt)]
            #[allow(non_camel_case_types)]
        enum WasmTools {
            $(
                #[cfg(feature = $string)]
                $name {
                    #[structopt(flatten)]
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
}

fn main() -> Result<()> {
    env_logger::init();
    WasmTools::from_args().run()
}
