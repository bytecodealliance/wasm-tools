use anyhow::Result;
use std::io::Write;
use std::ops::Range;
use termcolor::WriteColor;
use wasmparser::{Encoding, Parser, Payload::*};

/// Dumps information about sections in a WebAssembly file.
///
/// This is a relatively incomplete subcommand and is generally intended to just
/// help poke around an object file.
#[derive(clap::Parser)]
#[clap(after_help = "
Examples:

       # Dump the contents of the file `add.wasm`:
       $ wasm-tools objdump add.wasm
       types                                  |        0xa -       0x11 |         7 bytes | 1 count
       functions                              |       0x13 -       0x15 |         2 bytes | 1 count
       exports                                |       0x17 -       0x33 |        28 bytes | 1 count
       code                                   |       0x35 -       0x3e |         9 bytes | 1 count
       custom \"name\"                          |       0x45 -       0x5c |        23 bytes | 1 count

   One line is printed per section. The columns indicate:
       * The name of the section (for example, \"types\")
       * The offsets within the file for the start and end of the section (for example, \"0xa - 0x11\")
       * The size of the section (for example, \"7 bytes\")
       * The number of items within the section (for example, \"1 count\")

       # Dump the contents of the file `add.wasm` and store the text-based output in a file
       # named `add.txt`:
       $ wasm-tools objdump add.wasm -o add.txt

Exit status:
    0 if OK,
    nonzero to indicate a parse error.")]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let input = self.io.get_input_wasm(None)?;

        let mut printer = Printer {
            indices: Vec::new(),
            output: self.io.output_writer()?,
        };
        printer.indices.push(IndexSpace::default());

        for payload in Parser::new(0).parse_all(&input) {
            match payload? {
                Version { .. } => {}

                TypeSection(s) => printer.section(s, "types")?,
                ImportSection(s) => printer.section(s, "imports")?,
                FunctionSection(s) => printer.section(s, "functions")?,
                TableSection(s) => printer.section(s, "tables")?,
                MemorySection(s) => printer.section(s, "memories")?,
                TagSection(s) => printer.section(s, "tags")?,
                GlobalSection(s) => printer.section(s, "globals")?,
                ExportSection(s) => printer.section(s, "exports")?,
                StartSection { range, .. } => printer.section_raw(range, 1, "start")?,
                ElementSection(s) => printer.section(s, "elements")?,
                DataCountSection { range, .. } => printer.section_raw(range, 1, "data count")?,
                DataSection(s) => printer.section(s, "data")?,
                CodeSectionStart { range, count, .. } => {
                    printer.section_raw(range, count, "code")?
                }
                CodeSectionEntry(_) => {}

                ModuleSection {
                    unchecked_range: range,
                    ..
                } => {
                    printer.section_raw(range, 1, "module")?;
                    printer.start(Encoding::Module)?;
                }
                InstanceSection(s) => printer.section(s, "core instances")?,
                CoreTypeSection(s) => printer.section(s, "core types")?,
                ComponentSection {
                    unchecked_range: range,
                    ..
                } => {
                    printer.section_raw(range, 1, "component")?;
                    printer.indices.push(IndexSpace::default());
                    printer.start(Encoding::Component)?;
                }
                ComponentInstanceSection(s) => printer.section(s, "component instances")?,
                ComponentAliasSection(s) => printer.section(s, "component alias")?,
                ComponentTypeSection(s) => printer.section(s, "component types")?,
                ComponentCanonicalSection(s) => printer.section(s, "canonical functions")?,
                ComponentStartSection { range, .. } => {
                    printer.section_raw(range.clone(), 1, "component start")?
                }
                ComponentImportSection(s) => printer.section(s, "component imports")?,
                ComponentExportSection(s) => printer.section(s, "component exports")?,

                CustomSection(c) => printer.section_raw(
                    c.data_offset()..c.data_offset() + c.data().len(),
                    1,
                    &format!("custom {:?}", c.name()),
                )?,

                End(_) => printer.end()?,

                _ => {}
            }
        }

        Ok(())
    }
}

#[derive(Default)]
struct IndexSpace {
    modules: u32,
    components: u32,
    processing: Vec<Encoding>,
}

struct Printer {
    indices: Vec<IndexSpace>,
    output: Box<dyn WriteColor>,
}

impl Printer {
    fn start(&mut self, encoding: Encoding) -> Result<()> {
        if let Some(space) = self.indices.last_mut() {
            space.processing.push(encoding);
        }

        if let Some(space) = self.indices.last() {
            match encoding {
                Encoding::Module => {
                    writeln!(
                        self.output,
                        "{}------ start module {} -------------",
                        self.header(),
                        space.modules
                    )?;
                }
                Encoding::Component => {
                    writeln!(
                        self.output,
                        "{}------ start component {} ----------",
                        self.header(),
                        space.components
                    )?;
                }
            }
        }
        Ok(())
    }

    fn end(&mut self) -> Result<()> {
        let header = self.header();
        if let Some(space) = self.indices.last_mut() {
            match space.processing.pop() {
                Some(Encoding::Module) => {
                    writeln!(
                        self.output,
                        "{}------ end module {} -------------",
                        header, space.modules
                    )?;
                    space.modules += 1;
                }
                Some(Encoding::Component) => {
                    writeln!(
                        self.output,
                        "{}------ end component {} ----------",
                        header, space.components
                    )?;
                    self.indices.pop();

                    if let Some(space) = self.indices.last_mut() {
                        space.components += 1;
                    }
                }
                None => {
                    self.indices.pop();
                }
            }
        }
        Ok(())
    }

    fn section<'a, T>(
        &mut self,
        section: wasmparser::SectionLimited<'a, T>,
        name: &str,
    ) -> Result<()>
    where
        T: wasmparser::FromReader<'a>,
    {
        self.section_raw(section.range(), section.count(), name)
    }

    fn section_raw(&mut self, range: Range<usize>, count: u32, name: &str) -> Result<()> {
        writeln!(
            self.output,
            "{:40} | {:#10x} - {:#10x} | {:9} bytes | {} count",
            format!("{}{}", self.header(), name),
            range.start,
            range.end,
            range.end - range.start,
            count,
        )?;
        Ok(())
    }

    fn header(&self) -> String {
        let mut s = String::new();
        let depth = self
            .indices
            .last()
            .map_or(0, |space| match space.processing.last() {
                Some(Encoding::Module) => self.indices.len() + 1,
                _ => self.indices.len(),
            });

        for _ in 0..depth {
            s.push_str("  ");
        }
        return s;
    }
}
