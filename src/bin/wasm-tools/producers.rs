use anyhow::Result;
use indexmap::{map::Entry, IndexMap};
use std::fmt;
use std::io::Write;
use wasmparser::{NameSectionReader, Parser, Payload::*, ProducersSectionReader};

#[derive(Debug)]
struct Producers(IndexMap<String, IndexMap<String, String>>);

impl Producers {
    fn empty() -> Self {
        Producers(IndexMap::new())
    }
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
    fn add_opts(&mut self, opts: &AddOpts) {
        for lang in opts.language.iter() {
            self.add("language", &lang, "");
        }
        for (name, version) in opts.processed_by.iter() {
            self.add("processed-by", &name, &version);
        }
        for (name, version) in opts.sdk.iter() {
            self.add("sdk", &name, &version);
        }
    }
    fn into_section(self) -> wasm_encoder::ProducersSection {
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

#[derive(Debug)]
enum ProducersReport {
    Component {
        name: Option<String>,
        producers: Option<Producers>,
        children: Vec<Box<ProducersReport>>,
    },
    Module {
        name: Option<String>,
        producers: Option<Producers>,
    },
}

impl ProducersReport {
    fn component() -> Self {
        ProducersReport::Component {
            name: None,
            producers: None,
            children: Vec::new(),
        }
    }
    fn module() -> Self {
        ProducersReport::Module {
            name: None,
            producers: None,
        }
    }
    fn set_name(&mut self, n: &str) {
        match self {
            ProducersReport::Module { name, .. } => *name = Some(n.to_owned()),
            ProducersReport::Component { name, .. } => *name = Some(n.to_owned()),
        }
    }
    fn set_producers(&mut self, p: Producers) {
        match self {
            ProducersReport::Module { producers, .. } => *producers = Some(p),
            ProducersReport::Component { producers, .. } => *producers = Some(p),
        }
    }
    fn push_child(&mut self, child: Self) {
        match self {
            ProducersReport::Module { .. } => panic!("module shouldnt have children"),
            ProducersReport::Component { children, .. } => children.push(Box::new(child)),
        }
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

        let mut report_stack = Vec::new();

        for payload in Parser::new(0).parse_all(&input) {
            match payload? {
                Version { encoding, .. } => {
                    if report_stack.is_empty() {
                        match encoding {
                            wasmparser::Encoding::Module => {
                                report_stack.push(ProducersReport::module())
                            }
                            wasmparser::Encoding::Component => {
                                report_stack.push(ProducersReport::component())
                            }
                        }
                    }
                }
                ModuleSection { .. } => report_stack.push(ProducersReport::module()),
                ComponentSection { .. } => report_stack.push(ProducersReport::component()),
                End { .. } => {
                    let finished = report_stack.pop().expect("non-empty report stack");
                    if report_stack.is_empty() {
                        write!(output, "report: {finished:?}")?;
                    } else {
                        report_stack.last_mut().unwrap().push_child(finished);
                    }
                }
                CustomSection(c) if c.name() == "name" => {
                    let section = NameSectionReader::new(c.data(), c.data_offset());
                    for name in section.into_iter() {
                        let name = name?;
                        match name {
                            wasmparser::Name::Module { name, .. } => report_stack
                                .last_mut()
                                .expect("non-empty report stack")
                                .set_name(name),
                            _ => {}
                        }
                    }
                }
                CustomSection(c) if c.name() == "producers" => {
                    let section = ProducersSectionReader::new(c.data(), c.data_offset())?;
                    let producers = Producers::from_reader(section)?;
                    write!(output, "{producers}")?;
                    report_stack
                        .last_mut()
                        .expect("non-empty report stack")
                        .set_producers(producers);
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

        let mut found = false;
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
                    found = true;
                    let section = ProducersSectionReader::new(c.data(), c.data_offset())?;
                    let mut producers = Producers::from_reader(section)?;
                    // Add to the section according to the command line flags:
                    producers.add_opts(&self);
                    // Encode into output:
                    module.section(&producers.into_section());
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
        if !found {
            let mut producers = Producers::empty();
            // Add to the section according to the command line flags:
            producers.add_opts(&self);
            // Encode into output:
            module.section(&producers.into_section());
        }
        self.io.output(wasm_tools::Output::Wasm {
            bytes: module.as_slice(),
            wat: self.wat,
        })?;
        Ok(())
    }
}
