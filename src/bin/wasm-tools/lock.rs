use anyhow::Result;
use std::io::Write;
use std::ops::Range;
use termcolor::WriteColor;
use wasmparser::{Encoding, Parser, Payload::*};
use wasm_lock;

/// Dumps information about sections in a WebAssembly file.
///
/// This is a relatively incomplete subcommand and is generally intended to just
/// help poke around an object file.
#[derive(clap::Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let mut parser = wasm_deps::DepsParser::new();
        let deps = parser.parse(&input);
        dbg!(deps);


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
