use anyhow::Result;
use std::io::Write;
use wasmparser::{Parser, Payload::*, ProducersSectionReader};

/// Dumps the contents of the "producers" custom sections in a WebAssembly file.
///
/// Spec: https://github.com/WebAssembly/tool-conventions/blob/main/ProducersSection.md
#[derive(clap::Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let mut output = self.io.output_writer()?;

        for payload in Parser::new(0).parse_all(&input) {
            match payload? {
                CustomSection(c) if c.name() == "producers" => {
                    let section = ProducersSectionReader::new(c.data(), c.data_offset())?;
                    for field in section.into_iter() {
                        let field = field?;
                        writeln!(output, "{}:", field.name)?;
                        for value in field.values.into_iter() {
                            let value = value?;
                            if value.version.is_empty() {
                                writeln!(output, "    {}", value.name)?;
                            } else {
                                writeln!(output, "    {}: {}", value.name, value.version)?;
                            }
                        }
                    }
                }

                _ => {}
            }
        }

        Ok(())
    }
}
