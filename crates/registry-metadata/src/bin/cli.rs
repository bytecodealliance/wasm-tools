use anyhow::Result;
use clap::Parser;
use registry_metadata::RegistryMetadata;

#[derive(Debug, PartialEq, Parser)]
struct Opt {
    /// Path to WASM file
    #[clap(short, long)]
    path: String,

    #[clap(subcommand)]
    cmd: Command,
}

#[derive(Parser, PartialEq, Debug)]
enum Command {
    /// Print the registry metadata contained in a WASM file
    Print,

    /// Add registry metadata to WASM file
    Add {
        #[clap(flatten)]
        metadata: RegistryMetadata,

        /// Output path
        #[clap(short, long)]
        out: String,
    },
}

pub fn main() -> Result<()> {
    let opt = Opt::parse();

    let input = std::fs::read(opt.path)?;

    match opt.cmd {
        Command::Print => {
            let metadata = RegistryMetadata::from_wasm(&input)?;

            match &metadata {
                Some(metadata) => {
                    let json = serde_json::to_string_pretty(metadata)?;

                    println!("Registry Metadata: \n{json}");
                }
                None => println!("WASM File doesn't contain any registry metadata"),
            }
        }
        Command::Add { metadata, out } => {
            let output = metadata.add_to_wasm(&input)?;

            std::fs::write(out, output)?;
        }
    }

    Ok(())
}
