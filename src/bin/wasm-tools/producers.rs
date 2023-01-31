use anyhow::Result;
use indexmap::{map::Entry, IndexMap};
use std::fmt;
use std::io::Write;
use wasmparser::{Parser, Payload::*, ProducersSectionReader};

struct Producers(IndexMap<String, IndexMap<String, String>>);

impl Producers {
    fn from_reader(section: ProducersSectionReader) -> Result<Self> {
        let mut fields = IndexMap::new();
        for field in section.into_iter() {
            let field = field?;
            let mut values = IndexMap::new();
            for value in field.values.into_iter() {
                let value = value?;
                values.insert(value.name.to_owned(), value.version.to_owned());
            }
            fields.insert(field.name.to_owned(), values);
        }
        Ok(Producers(fields))
    }
    fn add(&mut self, field: &str, name: &str, version: &str) {
        match self.0.entry(field.to_string()) {
            Entry::Occupied(e) => {
                e.into_mut().insert(name.to_owned(), version.to_owned());
            }
            Entry::Vacant(e) => {
                let mut m = IndexMap::new();
                m.insert(name.to_owned(), version.to_owned());
                e.insert(m);
            }
        }
    }
    fn into_encoder(self) -> wasm_encoder::ProducersSection {
        let mut section = wasm_encoder::ProducersSection::new();
        for (fieldname, fieldvalues) in self.0 {
            let mut field = wasm_encoder::ProducersField::new();
            for (name, version) in fieldvalues {
                field.value(&name, &version);
            }
            section.field(&fieldname, &field);
        }
        section
    }
}

impl fmt::Display for Producers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (fieldname, fieldvalues) in self.0.iter() {
            writeln!(f, "{fieldname}:")?;
            for (name, version) in fieldvalues {
                if version.is_empty() {
                    writeln!(f, "    {name}")?;
                } else {
                    writeln!(f, "    {name}: {version}")?;
                }
            }
        }
        Ok(())
    }
}

/// Dumps the contents of the "producers" custom sections in a WebAssembly file.
///
/// Spec: https://github.com/WebAssembly/tool-conventions/blob/main/ProducersSection.md
#[derive(clap::Parser)]
pub enum Opts {
    Show(ShowOpts),
    Add(AddOpts),
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        match self {
            Opts::Show(opts) => opts.run(),
            Opts::Add(opts) => opts.run(),
        }
    }
}

#[derive(clap::Parser)]
pub struct ShowOpts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl ShowOpts {
    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let mut output = self.io.output_writer()?;

        for payload in Parser::new(0).parse_all(&input) {
            match payload? {
                CustomSection(c) if c.name() == "producers" => {
                    let section = ProducersSectionReader::new(c.data(), c.data_offset())?;
                    let producers = Producers::from_reader(section)?;
                    write!(output, "{producers}")?;
                }

                _ => {}
            }
        }

        Ok(())
    }
}

#[derive(clap::Parser)]
pub struct AddOpts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Add a programming language to the producers section
    #[clap(long, value_name = "NAME")]
    language: Vec<String>,

    /// Add a tool and its version to the producers section
    #[clap(long = "processed-by", value_parser = parse_key_value, value_name="NAME=VERSION")]
    processed_by: Vec<(String, String)>,

    /// Add an SDK and its version to the producers section
    #[clap(long, value_parser = parse_key_value, value_name="NAME=VERSION")]
    sdk: Vec<(String, String)>,

    /// Output the text format of WebAssembly instead of the binary format
    #[clap(short = 't', long)]
    wat: bool,
}

fn parse_key_value(s: &str) -> Result<(String, String)> {
    s.split_once('=')
        .map(|(k, v)| (k.to_owned(), v.to_owned()))
        .ok_or_else(|| anyhow::anyhow!("expected KEY=VALUE"))
}

impl AddOpts {
    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let mut module = wasm_encoder::Module::new();

        let mut depth = 0;
        for payload in Parser::new(0).parse_all(&input) {
            let payload = payload?;

            // Track nesting depth, so that we don't mess with inner producer sections:
            match payload {
                ModuleSection { .. } | ComponentSection { .. } => depth += 1,
                End { .. } => depth -= 1,
                _ => {}
            }

            // Process the wasm sections:
            match payload {
                // Only rewrite the outermost producers section:
                CustomSection(c) if c.name() == "producers" && depth == 0 => {
                    let section = ProducersSectionReader::new(c.data(), c.data_offset())?;
                    let mut producers = Producers::from_reader(section)?;
                    // Add to the section according to the command line flags:
                    for lang in self.language.iter() {
                        producers.add("language", &lang, "");
                    }
                    for (name, version) in self.processed_by.iter() {
                        producers.add("processed-by", &name, &version);
                    }
                    for (name, version) in self.sdk.iter() {
                        producers.add("sdk", &name, &version);
                    }
                    // Encode into output:
                    let encoder = producers.into_encoder();
                    module.section(&encoder);
                }

                // All other sections get passed through unmodified:
                _ => {
                    if let Some((id, range)) = payload.as_section() {
                        module.section(&wasm_encoder::RawSection {
                            id,
                            data: &input[range],
                        });
                    }
                }
            }
        }
        self.io.output(wasm_tools::Output::Wasm {
            bytes: module.as_slice(),
            wat: self.wat,
        })?;
        Ok(())
    }
}
