use anyhow::Result;
use std::mem;
use wasm_encoder::{ComponentSectionId, Encode, RawSection, Section};
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
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

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

        let mut output = Vec::new();
        let mut stack = Vec::new();

        for payload in Parser::new(0).parse_all(&input) {
            let payload = payload?;

            // Track nesting depth, so that we don't mess with inner producer sections:
            match payload {
                Version { encoding, .. } => {
                    output.extend_from_slice(match encoding {
                        wasmparser::Encoding::Component => &wasm_encoder::Component::HEADER,
                        wasmparser::Encoding::Module => &wasm_encoder::Module::HEADER,
                    });
                }
                ModuleSection { .. } | ComponentSection { .. } => {
                    stack.push(mem::take(&mut output));
                    continue;
                }
                End { .. } => {
                    let mut parent = match stack.pop() {
                        Some(c) => c,
                        None => break,
                    };
                    if output.starts_with(&wasm_encoder::Component::HEADER) {
                        parent.push(ComponentSectionId::Component as u8);
                        output.encode(&mut parent);
                    } else {
                        parent.push(ComponentSectionId::CoreModule as u8);
                        output.encode(&mut parent);
                    }
                    output = parent;
                }
                _ => {}
            }

            match &payload {
                CustomSection(c) => {
                    if strip_custom_section(c.name()) {
                        continue;
                    }
                }

                _ => {}
            }
            if let Some((id, range)) = payload.as_section() {
                RawSection {
                    id,
                    data: &input[range],
                }
                .append_to(&mut output);
            }
        }

        self.io.output(wasm_tools::Output::Wasm {
            bytes: &output,
            wat: self.wat,
        })?;
        Ok(())
    }
}
