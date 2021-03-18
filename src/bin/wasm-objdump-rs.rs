use anyhow::Result;
use std::env;
use wasmparser::{Parser, Payload::*};

fn main() -> Result<()> {
    env_logger::init();

    // Use the `getopts` crate to parse the `-o` option as well as `-h`
    let program = env::args().nth(0).unwrap();
    let mut opts = getopts::Options::new();
    opts.optflag("h", "help", "print this help menu");
    let matches = opts.parse(env::args_os().skip(1))?;
    if matches.opt_present("h") {
        return Ok(print_usage(&program, opts));
    }
    let input = match matches.free.len() {
        0 => {
            print_usage(&program, opts);
            std::process::exit(1);
        }
        1 => &matches.free[0],
        _ => anyhow::bail!("more than one input file specified on command line"),
    };

    let input = std::fs::read(&input)?;

    let mut printer = Printer::default();
    for payload in Parser::new(0).parse_all(&input) {
        match payload? {
            Version { .. } => printer.start(),

            TypeSection(s) => printer.section(s, "types"),
            ImportSection(s) => printer.section(s, "imports"),
            AliasSection(s) => printer.section(s, "aliases"),
            InstanceSection(s) => printer.section(s, "instances"),
            FunctionSection(s) => printer.section(s, "functions"),
            TableSection(s) => printer.section(s, "tables"),
            MemorySection(s) => printer.section(s, "memories"),
            EventSection(s) => printer.section(s, "events"),
            GlobalSection(s) => printer.section(s, "globals"),
            ExportSection(s) => printer.section(s, "exports"),
            StartSection { range, .. } => printer.section_raw(range, 1, "start"),
            ElementSection(s) => printer.section(s, "elements"),
            DataCountSection { range, .. } => printer.section_raw(range, 1, "data count"),
            DataSection(s) => printer.section(s, "data"),

            CodeSectionStart { range, count, .. } => printer.section_raw(range, count, "code"),

            ModuleSectionStart { range, count, .. } => {
                printer.section_raw(range, count, "modules");
                printer.module_code_counts.push((0, count));
            }

            CustomSection {
                name,
                data_offset,
                data,
                range: _,
            } => printer.section_raw(
                wasmparser::Range {
                    start: data_offset,
                    end: data_offset + data.len(),
                },
                1,
                &format!("custom {:?}", name),
            ),

            CodeSectionEntry(_) => {}
            ModuleSectionEntry { .. } => {}
            UnknownSection { .. } => {}

            End => printer.end(),
        }
    }

    Ok(())
}

#[derive(Default)]
struct Printer {
    module_code_counts: Vec<(u32, u32)>,
}

impl Printer {
    fn start(&self) {
        if let Some((a, b)) = self.module_code_counts.last() {
            println!("{}------ start {}/{} ----------", self.header(), *a + 1, b);
        }
    }

    fn end(&mut self) {
        let header = self.header();
        let pop = if let Some((a, b)) = self.module_code_counts.last_mut() {
            println!("{}------   end {}/{} ----------", header, *a + 1, b);
            *a += 1;
            *a == *b
        } else {
            false
        };
        if pop {
            self.module_code_counts.pop();
        }
    }

    fn section<T>(&self, section: T, name: &str)
    where
        T: wasmparser::SectionWithLimitedItems + wasmparser::SectionReader,
    {
        self.section_raw(section.range(), section.get_count(), name)
    }

    fn section_raw(&self, range: wasmparser::Range, count: u32, name: &str) {
        println!(
            "{:40} | {:#10x} - {:#10x} | {:9} bytes | {} count",
            format!("{}{}", self.header(), name),
            range.start,
            range.end,
            range.end - range.start,
            count,
        );
    }

    fn header(&self) -> String {
        let mut s = String::new();
        for _ in 0..self.module_code_counts.len() {
            s.push_str("  ");
        }
        return s;
    }
}

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}
