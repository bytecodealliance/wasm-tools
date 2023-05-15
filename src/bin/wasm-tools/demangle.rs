use anyhow::{bail, Result};
use wasm_encoder::{IndirectNameMap, NameMap, NameSection, RawSection};
use wasmparser::{Name, NameSectionReader, Parser, Payload::*};

/// Demangle Rust and C++ symbol names in the `name` section.
///
/// This command will detect a `name` section in a wasm executable and demangle
/// any Rust and C++ symbol names found within it. Tooling for debugging a wasm
/// module which otherwise uses the `name` section but doesn't run a demangler
/// will use the demangled names since the `name` section will be replaced.
#[derive(clap::Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

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
        let mut module = wasm_encoder::Module::new();

        for payload in Parser::new(0).parse_all(&input) {
            let payload = payload?;
            match &payload {
                CustomSection(c) if c.name() == "name" => {
                    match self.demangle(c.data(), c.data_offset()) {
                        Ok(new_section) => {
                            module.section(&new_section);
                            continue;
                        }
                        Err(e) => log::debug!("error parsing name section {e:?}"),
                    }
                }
                Version { encoding, .. } if *encoding == wasmparser::Encoding::Component => {
                    bail!("demangling components is not supported");
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

    fn demangle(&self, section: &[u8], offset: usize) -> Result<NameSection> {
        let mut new_section = NameSection::new();
        for section in NameSectionReader::new(section, offset) {
            match section? {
                Name::Module { name, .. } => new_section.module(name),
                Name::Memory(names) => new_section.memories(&self.name_map(names)?),
                Name::Global(names) => new_section.globals(&self.name_map(names)?),
                Name::Function(names) => new_section.functions(&self.name_map(names)?),
                Name::Type(names) => new_section.types(&self.name_map(names)?),
                Name::Table(names) => new_section.tables(&self.name_map(names)?),
                Name::Element(names) => new_section.elements(&self.name_map(names)?),
                Name::Data(names) => new_section.data(&self.name_map(names)?),
                Name::Local(names) => new_section.locals(&self.indirect_name_map(names)?),
                Name::Label(names) => new_section.labels(&self.indirect_name_map(names)?),
                Name::Unknown { .. } => bail!("unknown name section"),
            }
        }
        Ok(new_section)
    }

    fn name_map(&self, names: wasmparser::NameMap<'_>) -> Result<NameMap> {
        let mut ret = NameMap::new();
        for naming in names {
            let naming = naming?;
            let name = match rustc_demangle::try_demangle(naming.name) {
                Ok(name) => name.to_string(),
                Err(_) => match cpp_demangle::Symbol::new(naming.name) {
                    Ok(name) => name.to_string(),
                    Err(_) => naming.name.to_string(),
                },
            };
            ret.append(naming.index, &name);
        }
        Ok(ret)
    }

    fn indirect_name_map(&self, names: wasmparser::IndirectNameMap<'_>) -> Result<IndirectNameMap> {
        let mut ret = IndirectNameMap::new();
        for naming in names {
            let naming = naming?;
            let map = self.name_map(naming.names)?;
            ret.append(naming.index, &map);
        }
        Ok(ret)
    }
}
