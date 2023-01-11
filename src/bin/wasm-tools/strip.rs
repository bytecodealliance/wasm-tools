use anyhow::Result;
use wasm_encoder::RawSection;
use wasmparser::{Parser, Payload::*};

/// Removes custom sections from an input WebAssembly file.
///
/// This command will by default strip all custom sections such as DWARF
/// debugging information from a wasm file. It will not strip the `name` section
/// by default unless the `--all` flag is passed.
#[derive(clap::Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Remove all custom sections, regardless of name.
    #[clap(long, short)]
    all: bool,

    /// Remove custom sections matching the specified regex.
    #[clap(long, short, value_name = "REGEX")]
    delete: Vec<String>,

    /// Output the text format of WebAssembly instead of the binary format.
    #[clap(short = 't', long)]
    wat: bool,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let to_delete = regex::RegexSet::new(self.delete.iter())?;

        let strip_custom_section = |name: &str| {
            // If explicitly specified, strip everything.
            if self.all {
                return true;
            }

            // If any section was called out by name only delete those sections.
            if !to_delete.is_empty() {
                return to_delete.is_match(name);
            }

            // Finally default strip everything but the `name` section.
            name != "name"
        };

        let mut module = wasm_encoder::Module::new();

        for payload in Parser::new(0).parse_all(&input) {
            let payload = payload?;
            match &payload {
                CustomSection(c) => {
                    if strip_custom_section(c.name()) {
                        continue;
                    }
                }

                _ => {}
            }
            if let Some((id, range)) = payload.as_section() {
                module.section(&RawSection {
                    id,
                    data: &input[range],
                });
            }
        }

        self.io.output(wasm_tools::Output::Wasm {
            bytes: module.as_slice(),
            wat: self.wat,
        })?;
        Ok(())
    }
}
