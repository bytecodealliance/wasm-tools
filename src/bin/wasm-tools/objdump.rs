use anyhow::Result;
use std::ops::Range;
use std::path::PathBuf;
use wasmparser::{Parser, Payload::*};

/// Dumps information about sections in a WebAssembly file.
///
/// This is a relatively incomplete subcommand and is generally intended to just
/// help poke around an object file.
#[derive(clap::Parser)]
pub struct Opts {
    /// Input WebAssembly file to dump information about.
    input: PathBuf,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        let input = wat::parse_file(&self.input)?;

        let mut printer = Printer::default();
        printer.indices.push(IndexSpace::default());

        for payload in Parser::new(0).parse_all(&input) {
            match payload? {
                Version { encoding, .. } => printer.start(encoding),

                TypeSection(s) => printer.section(s, "types"),
                ImportSection(s) => printer.section(s, "imports"),
                FunctionSection(s) => printer.section(s, "functions"),
                TableSection(s) => printer.section(s, "tables"),
                MemorySection(s) => printer.section(s, "memories"),
                TagSection(s) => printer.section(s, "tags"),
                GlobalSection(s) => printer.section(s, "globals"),
                ExportSection(s) => printer.section(s, "exports"),
                StartSection { range, .. } => printer.section_raw(range, 1, "start"),
                ElementSection(s) => printer.section(s, "elements"),
                DataCountSection { range, .. } => printer.section_raw(range, 1, "data count"),
                DataSection(s) => printer.section(s, "data"),
                CodeSectionStart { range, count, .. } => printer.section_raw(range, count, "code"),
                CodeSectionEntry(_) => {}

                ComponentTypeSection(s) => printer.section(s, "types"),
                ComponentImportSection(s) => printer.section(s, "imports"),
                ComponentFunctionSection(s) => printer.section(s, "functions"),
                ModuleSection { range, .. } => {
                    printer.section_raw(range, 1, "module");
                    if let Some(space) = printer.indices.last_mut() {
                        space.inc_module();
                    }
                }

                ComponentSection { range, .. } => {
                    let mut index_space = IndexSpace::default();
                    index_space.inc_component();
                    printer.indices.push(index_space);
                    printer.section_raw(range, 1, "component")
                }

                InstanceSection(s) => printer.section(s, "instances"),
                ComponentExportSection(s) => printer.section(s, "exports"),
                ComponentStartSection(_) => {}
                AliasSection(s) => printer.section(s, "alias"),

                CustomSection(c) => printer.section_raw(
                    c.data_offset()..c.data_offset() + c.data().len(),
                    1,
                    &format!("custom {:?}", c.name()),
                ),

                UnknownSection { .. } => {}

                End(_) => printer.end(),
            }
        }

        Ok(())
    }
}

#[derive(Default, Debug)]
struct IndexSpace {
    modules: Option<u32>,
    components: Option<u32>,
    processing: Vec<wasmparser::Encoding>,
}

impl IndexSpace {
    pub fn inc_module(&mut self) {
        match self.modules {
            None => self.modules = Some(0),
            Some(n) => self.modules = Some(n + 1),
        }
    }

    pub fn inc_component(&mut self) {
        match self.components {
            None => self.components = Some(0),
            Some(n) => self.components = Some(n + 1),
        }
    }
}

#[derive(Default)]
struct Printer {
    indices: Vec<IndexSpace>,
}

impl Printer {
    fn start(&mut self, encoding: wasmparser::Encoding) {
        if !self.indices.is_empty() {
            let space = self.indices.last_mut().unwrap();

            space.processing.push(encoding);

            match encoding {
                wasmparser::Encoding::Module => {
                    if let Some(modules) = space.modules {
                        println!(
                            "{}------ start module {} -------------",
                            self.header(),
                            modules
                        );
                    }
                }
                wasmparser::Encoding::Component => {
                    if let Some(components) = space.components {
                        println!(
                            "{}------ start component {} ----------",
                            self.header(),
                            components
                        );
                    }
                }
            }
        }
    }

    fn end(&mut self) {
        let header = self.header();

        if let Some(space) = self.indices.last_mut() {
            match space.processing.last() {
                Some(wasmparser::Encoding::Module) => {
                    if let Some(modules) = space.modules {
                        println!("{}------ end module {} -------------", header, modules);
                        space.processing.pop();
                    }
                }
                Some(wasmparser::Encoding::Component) => {
                    if let Some(components) = space.components {
                        println!("{}------ end component {} ----------", header, components);
                        space.processing.pop();
                    }
                }
                _ => {}
            }

            if space.processing.is_empty() {
                self.indices.pop();
            }
        }
    }

    fn section<T>(&mut self, section: T, name: &str)
    where
        T: wasmparser::SectionWithLimitedItems + wasmparser::SectionReader,
    {
        self.section_raw(section.range(), section.get_count(), name)
    }

    fn section_raw(&self, range: Range<usize>, count: u32, name: &str) {
        println!(
            "{:40} | {:#10x} - {:#10x} | {:9} bytes | {} count",
            format!("{}{}", self.header(), name),
            range.start,
            range.end,
            range.end - range.start,
            count,
        );
    }

    fn header(&mut self) -> String {
        let mut s = String::new();
        let depth = self
            .indices
            .last()
            .map_or(0, |space| match space.processing.last() {
                Some(wasmparser::Encoding::Module) => {
                    if space.modules.is_some() {
                        self.indices.len() + 1
                    } else {
                        self.indices.len()
                    }
                }
                _ => self.indices.len(),
            });
        for _ in 0..depth {
            s.push_str("  ");
        }
        return s;
    }
}
