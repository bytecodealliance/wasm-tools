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
            fmt_payload(&payload, &mut output)?;
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

fn fmt_payload(payload: &Payload, f: &mut Box<dyn WriteColor>) -> Result<()> {
    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .apply_modifier(UTF8_ROUND_CORNERS)
        .set_content_arrangement(ContentArrangement::Dynamic)
        .set_width(80)
        .set_header(vec!["KIND", "VALUE"]);
    let Metadata {
        name,
        author,
        description,
        producers,
        licenses,
        source,
        homepage,
        range,
        revision,
        version,
    } = payload.metadata();

    // Print the basic information
    let kind = match payload {
        Payload::Component { .. } => "component",
        Payload::Module(_) => "module",
    };
    table.add_row(vec!["kind", &kind]);
    let name = name.as_deref().unwrap_or("<unknown>");
    table.add_row(vec!["name", &name]);
    table.add_row(vec![
        "range",
        &format!("0x{:x}..0x{:x}", range.start, range.end),
    ]);

    // Print the OCI annotations
    if let Some(description) = description {
        table.add_row(vec!["description", &description.to_string()]);
    }
    if let Some(licenses) = licenses {
        table.add_row(vec!["licenses", &licenses.to_string()]);
    }
    if let Some(author) = author {
        table.add_row(vec!["author", &author.to_string()]);
    }
    if let Some(source) = source {
        table.add_row(vec!["source", &source.to_string()]);
    }
    if let Some(homepage) = homepage {
        table.add_row(vec!["homepage", &homepage.to_string()]);
    }
    if let Some(revision) = revision {
        table.add_row(vec!["revision", &revision.to_string()]);
    }
    if let Some(version) = version {
        table.add_row(vec!["version", &version.to_string()]);
    }

    if let Some(producers) = producers {
        for (name, pairs) in producers.iter() {
            for (field, version) in pairs.iter() {
                match version.len() {
                    0 => table.add_row(vec![name, &format!("{field}")]),
                    _ => table.add_row(vec![name, &format!("{field} [{version}]")]),
                };
            }
        }
    }

    // Write the table to the writer
    writeln!(f, "{table}")?;

    if let Payload::Component { children, .. } = payload {
        for payload in children {
            fmt_payload(payload, f)?;
        }
    }

    Ok(())
}
