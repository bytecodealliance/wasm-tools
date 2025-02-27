use std::io::Write;

use anyhow::Result;
use comfy_table::modifiers::UTF8_ROUND_CORNERS;
use comfy_table::presets::UTF8_FULL;
use comfy_table::{ContentArrangement, Table};
use termcolor::WriteColor;
use wasm_metadata::{Metadata, Payload};

/// Manipulate metadata (module name, producers) to a WebAssembly file.
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

    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        match self {
            Opts::Show(opts) => opts.general_opts(),
            Opts::Add(opts) => opts.general_opts(),
        }
    }
}

/// Read metadata (module name, producers) from a WebAssembly file.
#[derive(clap::Parser)]
pub struct ShowOpts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Output in JSON encoding
    #[clap(long)]
    json: bool,
}

impl ShowOpts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let mut output = self.io.output_writer()?;

        let payload = wasm_metadata::Payload::from_binary(&input)?;
        if self.json {
            write!(output, "{}", serde_json::to_string(&payload)?)?;
        } else {
            write_table(&payload, &mut output)?;
        }
        Ok(())
    }
}

/// Add metadata (module name, producers) to a WebAssembly file
#[derive(clap::Parser)]
pub struct AddOpts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    #[clap(flatten)]
    add_metadata: wasm_metadata::AddMetadata,

    /// Output the text format of WebAssembly instead of the binary format
    #[clap(short = 't', long)]
    wat: bool,
}

impl AddOpts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;

        let output = self.add_metadata.to_wasm(&input)?;

        self.io.output_wasm(&output, self.wat)?;
        Ok(())
    }
}

/// Write a table containing a wasm binary's metadata to a writer
fn write_table(payload: &Payload, f: &mut Box<dyn WriteColor>) -> Result<()> {
    // Prepare a table and get the individual metadata
    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .apply_modifier(UTF8_ROUND_CORNERS)
        .set_content_arrangement(ContentArrangement::Dynamic)
        .set_width(80)
        .set_header(vec!["KIND", "VALUE"]);
    let Metadata {
        name,
        authors,
        description,
        producers,
        licenses,
        source,
        homepage,
        range,
        revision,
        version,
    } = payload.metadata();

    // Add the basic information to the table first
    let name = name.as_deref().unwrap_or("<unknown>");
    table.add_row(vec!["name", &name]);
    let kind = match payload {
        Payload::Component { .. } => "component",
        Payload::Module(_) => "module",
    };
    table.add_row(vec!["kind", &kind]);
    table.add_row(vec![
        "range",
        &format!("0x{:x}..0x{:x}", range.start, range.end),
    ]);

    // Add the OCI annotations to the table
    if let Some(description) = description {
        table.add_row(vec!["description", &description.to_string()]);
    }
    if let Some(authors) = authors {
        table.add_row(vec!["authors", &authors.to_string()]);
    }
    if let Some(version) = version {
        table.add_row(vec!["version", &version.to_string()]);
    }
    if let Some(revision) = revision {
        table.add_row(vec!["revision", &revision.to_string()]);
    }
    if let Some(licenses) = licenses {
        table.add_row(vec!["licenses", &licenses.to_string()]);
    }
    if let Some(source) = source {
        table.add_row(vec!["source", &source.to_string()]);
    }
    if let Some(homepage) = homepage {
        table.add_row(vec!["homepage", &homepage.to_string()]);
    }

    // Add the producer section to the table
    if let Some(producers) = producers {
        // Ensure the "language" fields are listed first
        let mut producers = producers
            .iter()
            .map(|(n, p)| (n.clone(), p))
            .collect::<Vec<_>>();
        producers.sort_by(|(a, _), (b, _)| {
            if a == "language" {
                std::cmp::Ordering::Less
            } else if b == "language" {
                std::cmp::Ordering::Greater
            } else {
                a.cmp(b)
            }
        });

        // Add the producers to the table
        for (name, pairs) in producers.iter() {
            for (field, version) in pairs.iter() {
                match version.len() {
                    0 => table.add_row(vec![name, &format!("{field}")]),
                    _ => table.add_row(vec![name, &format!("{field} [{version}]")]),
                };
            }
        }
    }

    // Add child relationships to the table
    if let Payload::Component { children, .. } = &payload {
        for payload in children {
            let name = payload.metadata().name.as_deref().unwrap_or("<unknown>");
            let kind = match payload {
                Payload::Component { .. } => "component",
                Payload::Module(_) => "module",
            };
            table.add_row(vec!["child", &format!("{name} [{kind}]")]);
        }
    }

    // Write the table to the writer
    writeln!(f, "{table}")?;

    // Recursively print any children
    if let Payload::Component { children, .. } = payload {
        for payload in children {
            write_table(payload, f)?;
        }
    }

    Ok(())
}
