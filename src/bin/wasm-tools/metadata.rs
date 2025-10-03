use bytesize::ByteSize;
use std::io::Write;

use anyhow::Result;
use comfy_table::modifiers::UTF8_ROUND_CORNERS;
use comfy_table::presets::UTF8_FULL;
use comfy_table::{CellAlignment, ContentArrangement, Table};
use termcolor::WriteColor;
use wasm_metadata::{AddMetadata, Metadata, Payload};

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
#[clap(after_help = "\
Examples:

Suppose foo.wasm has the following textual representation:

(component $my-name
  (core module $submodule)

  (core module (@name \"another submodule\"))
)

This Wasm file represents a component containing two core modules.

    # Show the metadata for foo.wasm in a tabular format.
    $ wasm-tools metadata show foo.wasm

╭───────────┬───────────────────┬──────┬───────┬───────────┬─────────╮
│ KIND      ┆ NAME              ┆ SIZE ┆ SIZE% ┆ LANGUAGES ┆ PARENT  │
╞═══════════╪═══════════════════╪══════╪═══════╪═══════════╪═════════╡
│ component ┆ my-name           ┆ 136B ┆  100% ┆ -         ┆ <root>  │
├╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌┼╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌┤
│ module    ┆ submodule         ┆  27B ┆   20% ┆ -         ┆ my-name │
├╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌┼╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌┤
│ module    ┆ another submodule ┆  35B ┆   26% ┆ -         ┆ my-name │
╰───────────┴───────────────────┴──────┴───────┴───────────┴─────────╯
╭───────┬────────────────────────────╮
│ KIND  ┆ VALUE                      │
╞═══════╪════════════════════════════╡
│ name  ┆ my-name                    │
├╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
│ kind  ┆ component                  │
├╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
│ range ┆ 0x0..0x88                  │
├╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
│ child ┆ submodule [module]         │
├╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
│ child ┆ another submodule [module] │
╰───────┴────────────────────────────╯
╭───────┬───────────╮
│ KIND  ┆ VALUE     │
╞═══════╪═══════════╡
│ name  ┆ submodule │
├╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌┤
│ kind  ┆ module    │
├╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌┤
│ range ┆ 0xa..0x25 │
╰───────┴───────────╯
╭───────┬───────────────────╮
│ KIND  ┆ VALUE             │
╞═══════╪═══════════════════╡
│ name  ┆ another submodule │
├╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
│ kind  ┆ module            │
├╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
│ range ┆ 0x27..0x4a        │
╰───────┴───────────────────╯

    The first table summarizes the contents of the file: a component and two
    core modules.

    Next, one table is shown for each component and module. The first table
    shows that a component named `my-name` is represented by the bytes in
    foo.wasm between the offsets 0x0 and 0x88. The \"child\" rows of the table
    show the names of each of its contained modules. The second two tables
    show the names, kinds, and ranges of each of the modules in the file.

    # Show the metadata for foo.wasm in JSON format.
    # The output is not pretty-printed. For clarity, it has been pretty-printed
    # here.
    $ wasm-tools metadata show foo.wasm --json
{
  \"component\": {
    \"metadata\": {
      \"name\": \"my-name\",
      \"producers\": null,
      \"authors\": null,
      \"description\": null,
      \"licenses\": null,
      \"source\": null,
      \"homepage\": null,
      \"revision\": null,
      \"version\": null,
      \"range\": {
        \"start\": 0,
        \"end\": 136
      },
      \"dependencies\": null
    },
    \"children\": [
      {
        \"module\": {
          \"name\": \"submodule\",
          \"producers\": null,
          \"authors\": null,
          \"description\": null,
          \"licenses\": null,
          \"source\": null,
          \"homepage\": null,
          \"revision\": null,
          \"version\": null,
          \"range\": {
            \"start\": 10,
            \"end\": 37
          },
          \"dependencies\": null
        }
      },
      {
        \"module\": {
          \"name\": \"another submodule\",
          \"producers\": null,
          \"authors\": null,
          \"description\": null,
          \"licenses\": null,
          \"source\": null,
          \"homepage\": null,
          \"revision\": null,
          \"version\": null,
          \"range\": {
            \"start\": 39,
            \"end\": 74
          },
          \"dependencies\": null
        }
      }
    ]
  }
}

    The JSON format contains some additional fields. In this example, the values of
    those fields are set to `null` because they are not present in the input file.
    In the tabular format, these fields were simply omitted.
    The offset ranges are also shown in decimal instead of hexadecimal.

Exit status:
    0 on success,
    nonzero if the input file fails to parse.
")]
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
        let input = self.io.get_input_wasm(None)?;
        let mut output = self.io.output_writer()?;

        let payload = wasm_metadata::Payload::from_binary(&input)?;
        if self.json {
            write!(output, "{}", serde_json::to_string(&payload)?)?;
        } else {
            write_summary_table(&payload, &mut output)?;
            write_details_table(&payload, &mut output)?;
        }
        Ok(())
    }
}

/// Add metadata (module name, producers) to a WebAssembly file
#[derive(clap::Parser)]
#[clap(after_help = "\
Examples:

Suppose foo.wasm has the following textual representation:

(component $foo
  (core module
    (func $foo)
  )
)

This Wasm file represents a component containing one core module.

    # Add a programming language \"foo\" whose version is 1
    # to the `producers` section, an \"authors\" string,
    # and a homepage URL and print the textual representation
    # to stdout
    $ wasm-tools metadata add --language foo=1 \\
          --authors \"J. Random Hacker\" \\
          --homepage \"https://example.com/\" \\
          foo.wasm -t
(component $foo
  (core module (;0;)
    (type (;0;) (func))
    (func $foo (;0;) (type 0))
  )
  (@producers
    (language \"foo\" \"1\")
  )
  (@custom \"authors\" \"J. Random Hacker\")
  (@custom \"homepage\" \"https://example.com/\")
)

   # Remove the homepage section from the file foo1.wasm
   # and write the binary output to foo2.wasm
   $ wasm-tools metadata add --clear-homepage foo1.wasm -o foo2.wasm

Exit status:
    0 on success,
    nonzero if the input file fails to parse.
")]
pub struct AddOpts {
    #[clap(flatten)]
    generate_dwarf: wasm_tools::GenerateDwarfArg,

    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    #[clap(flatten)]
    add_metadata: wasm_metadata::AddMetadataOpts,

    /// Output the text format of WebAssembly instead of the binary format
    #[clap(short = 't', long)]
    wat: bool,
}

impl AddOpts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let input = self.io.get_input_wasm(Some(&self.generate_dwarf))?;

        let add_metadata: AddMetadata = self.add_metadata.clone().into();
        let output = add_metadata.to_wasm(&input)?;

        self.io.output_wasm(&output, self.wat)?;
        Ok(())
    }
}

/// Write a table containing a summarized overview of a wasm binary's metadata to
/// a writer.
fn write_summary_table(payload: &Payload, f: &mut Box<dyn WriteColor>) -> Result<()> {
    // Prepare a table and get the individual metadata
    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .apply_modifier(UTF8_ROUND_CORNERS)
        .set_content_arrangement(ContentArrangement::Dynamic)
        .set_width(80)
        .set_header(vec!["KIND", "NAME", "SIZE", "SIZE%", "LANGUAGES", "PARENT"]);

    table
        .column_mut(2)
        .expect("This should be the SIZE column")
        .set_cell_alignment(CellAlignment::Right);

    table
        .column_mut(3)
        .expect("This should be the SIZE% column")
        .set_cell_alignment(CellAlignment::Right);

    // Get the max value of the `range` field. This is the upper memory bound.
    fn find_range_max(max: &mut usize, payload: &Payload) {
        let range = &payload.metadata().range;
        if range.end > *max {
            *max = range.end;
        }

        if let Payload::Component { children, .. } = payload {
            for child in children {
                find_range_max(max, child);
            }
        }
    }

    let mut range_max = 0;
    find_range_max(&mut range_max, payload);

    // Recursively add all children to the table
    write_summary_table_inner(&payload, "<root>", &mut 0, range_max, f, &mut table)?;

    // Write the table to the writer
    writeln!(f, "{table}")?;

    Ok(())
}

// The recursing inner function of `write_summary_table`
fn write_summary_table_inner(
    payload: &Payload,
    parent: &str,
    unknown_id: &mut u16,
    range_max: usize,
    f: &mut Box<dyn WriteColor>,
    table: &mut Table,
) -> Result<()> {
    let Metadata {
        name,
        range,
        producers,
        ..
    } = payload.metadata();

    let name = match name.as_deref() {
        Some(name) => name.to_owned(),
        None => {
            let name = format!("unknown({unknown_id})");
            *unknown_id += 1;
            name
        }
    };
    let size = ByteSize::b((range.end - range.start) as u64)
        .display()
        .si_short()
        .to_string();

    let usep = match ((range.end - range.start) as f64 / range_max as f64 * 100.0).round() as u8 {
        // If the item was truly empty, it wouldn't be part of the binary
        0..=1 => "<1%".to_string(),
        // We're hedging against the low-ends, this hedges against the high-ends.
        // Makes sure we don't see a mix of <1% and 100% in the same table, unless
        // the item is actually 100% of the binary.
        100 if range.end != range_max => ">99%".to_string(),
        usep => format!("{usep}%"),
    };
    let kind = match payload {
        Payload::Component { .. } => "component",
        Payload::Module(_) => "module",
    };
    let languages = match producers {
        Some(producers) => match producers.iter().find(|(name, _)| *name == "language") {
            Some((_, pairs)) => pairs
                .iter()
                .map(|(lang, _)| lang.to_owned())
                .collect::<Vec<_>>()
                .join(", "),
            None => "-".to_string(),
        },
        None => "-".to_string(),
    };

    table.add_row(vec![&kind, &*name, &*size, &usep, &languages, &parent]);

    // Recursively print any children
    if let Payload::Component { children, .. } = payload {
        for payload in children {
            write_summary_table_inner(payload, &name, unknown_id, range_max, f, table)?;
        }
    }

    Ok(())
}

/// Write a table containing a detailed overview of a wasm binary's metadata to
/// a writer.
fn write_details_table(payload: &Payload, f: &mut Box<dyn WriteColor>) -> Result<()> {
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
        dependencies,
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

    // Add dependency packages to the table
    if let Some(dependencies) = dependencies {
        for package in &dependencies.version_info().packages {
            table.add_row(vec![
                "dependency",
                &format!("{} [{}]", package.name, package.version),
            ]);
        }
    }

    // Write the table to the writer
    writeln!(f, "{table}")?;

    // Recursively print any children
    if let Payload::Component { children, .. } = payload {
        for payload in children {
            write_details_table(payload, f)?;
        }
    }

    Ok(())
}
