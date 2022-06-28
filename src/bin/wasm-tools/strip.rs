use anyhow::{bail, Result};
use std::ops::Range;
use wasm_encoder::{RawSection, SectionId};
use wasmparser::{Encoding, Parser, Payload::*, SectionReader};

/// Removes custom sections from an input WebAssembly file.
///
/// This command will by default strip all custom sections such as DWARF
/// debugging information from a wasm file. It will not strip the `name` section
/// by default unless the `--all` flag is passed.
#[derive(clap::Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Strip all custom sections, including the `name` section
    #[clap(long, short)]
    all: bool,

    /// Output the text format of WebAssembly instead of the binary format.
    #[clap(short = 't', long)]
    wat: bool,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;

        let mut module = wasm_encoder::Module::new();

        for payload in Parser::new(0).parse_all(&input) {
            let payload = payload?;
            let mut section = |id: SectionId, range: Range<usize>| {
                module.section(&RawSection {
                    id: id as u8,
                    data: &input[range],
                });
            };
            match payload {
                Version {
                    encoding: Encoding::Module,
                    ..
                } => {}
                Version {
                    encoding: Encoding::Component,
                    ..
                } => {
                    bail!("components are not supported yet with the `strip` command");
                }

                TypeSection(s) => section(SectionId::Type, s.range()),
                ImportSection(s) => section(SectionId::Import, s.range()),
                FunctionSection(s) => section(SectionId::Function, s.range()),
                TableSection(s) => section(SectionId::Table, s.range()),
                MemorySection(s) => section(SectionId::Memory, s.range()),
                TagSection(s) => section(SectionId::Tag, s.range()),
                GlobalSection(s) => section(SectionId::Global, s.range()),
                ExportSection(s) => section(SectionId::Export, s.range()),
                ElementSection(s) => section(SectionId::Element, s.range()),
                DataSection(s) => section(SectionId::Data, s.range()),
                StartSection { range, .. } => section(SectionId::Start, range),
                DataCountSection { range, .. } => section(SectionId::DataCount, range),
                CodeSectionStart { range, .. } => section(SectionId::Code, range),
                CodeSectionEntry(_) => {}

                ModuleSection { .. }
                | InstanceSection(_)
                | AliasSection(_)
                | CoreTypeSection(_)
                | ComponentSection { .. }
                | ComponentInstanceSection(_)
                | ComponentAliasSection(_)
                | ComponentTypeSection(_)
                | ComponentCanonicalSection(_)
                | ComponentStartSection(_)
                | ComponentImportSection(_)
                | ComponentExportSection(_) => unimplemented!("component model"),

                CustomSection(c) if c.name() == "name" && !self.all => {
                    module.section(&RawSection {
                        id: SectionId::Custom as u8,
                        data: &input[c.range()],
                    });
                }

                CustomSection(_) => {}

                UnknownSection {
                    id,
                    contents,
                    range: _,
                } => {
                    module.section(&RawSection { id, data: contents });
                }

                End(_) => {}
            }
        }

        self.io.output(wasm_tools::Output::Wasm {
            bytes: module.as_slice(),
            wat: self.wat,
        })?;
        Ok(())
    }
}
