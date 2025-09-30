use anyhow::Result;
use std::mem;
use wasm_encoder::{ComponentSectionId, Encode, RawSection, Section};
use wasmparser::{Parser, Payload::*};

/// Removes custom sections from an input WebAssembly file.
///
/// This command will by default strip all custom sections such as DWARF
/// debugging information from a wasm file. It will not strip the `name`, `component-type`,
/// or `dylink.0` sections by default unless the `--all` flag is passed.
#[derive(clap::Parser)]
#[clap(after_help = "\
Examples:

Suppose foo.wasm has the following textual representation:

(module
  (type (;0;) (func))
  (func (;2;) (type 0)
    (local i32)
    global.get 0
  )
  (@custom \"linking\" (after code) \"\")
  (@custom \"reloc.CODE\" (after code) \"\")
  (@custom \"target_features\" (after code) \"\")
)

    # Remove all custom sections from foo.wasm and print the textual form of
    # the output to stdout.
    $ wasm-tools strip -a -t foo.wasm
(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    (local i32)
    global.get 0
  )
)
    # Remove only the custom sections whose names match the regexp `linking`
    # and print the textual form of the output to stdout.
    $ wasm-tools strip -d linking foo.wasm -t
(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    (local i32)
    global.get 0
  )
  (@custom \"reloc.CODE\" (after code) \"\")
  (@custom \"target_features\" (after code) \"\")
)

   # Remove all custom sections from foo.wasm and save the binary output to
   # the file out.wasm.
   $ wasm-tools strip -a foo.wasm -o out.wasm

Exit status:
    0 on success,
    nonzero if the input file fails to parse.
")]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Remove all custom sections, regardless of name.
    #[clap(long, short)]
    all: bool,

    /// Remove custom sections matching the specified regex.
    #[clap(long, short, value_name = "REGEX")]
    delete: Vec<String>,

    /// Output the text format of WebAssembly instead of the binary format.
    #[clap(short = 't', long)]
    wat: bool,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let input = self.io.get_input_wasm(None)?;
        let to_delete = regex::RegexSet::new(self.delete.iter())?;

        let strip_custom_section = |name: &str| {
            // If explicitly specified, strip everything.
            if self.all {
                return true;
            }

            // If any section was called out by name only delete those sections.
            if !to_delete.is_empty() {
                return to_delete.is_match(name);
            }

            // Finally default strip everything but:
            // * the `name` section
            // * any `component-type` sections
            // * the `dylink.0` section
            name != "name" && !name.starts_with("component-type:") && name != "dylink.0"
        };

        let mut output = Vec::new();
        let mut stack = Vec::new();

        for payload in Parser::new(0).parse_all(&input) {
            let payload = payload?;

            // Track nesting depth, so that we don't mess with inner producer sections:
            match payload {
                Version { encoding, .. } => {
                    output.extend_from_slice(match encoding {
                        wasmparser::Encoding::Component => &wasm_encoder::Component::HEADER,
                        wasmparser::Encoding::Module => &wasm_encoder::Module::HEADER,
                    });
                }
                ModuleSection { .. } | ComponentSection { .. } => {
                    stack.push(mem::take(&mut output));
                    continue;
                }
                End { .. } => {
                    let mut parent = match stack.pop() {
                        Some(c) => c,
                        None => break,
                    };
                    if output.starts_with(&wasm_encoder::Component::HEADER) {
                        parent.push(ComponentSectionId::Component as u8);
                        output.encode(&mut parent);
                    } else {
                        parent.push(ComponentSectionId::CoreModule as u8);
                        output.encode(&mut parent);
                    }
                    output = parent;
                }
                _ => {}
            }

            match &payload {
                CustomSection(c) => {
                    if strip_custom_section(c.name()) {
                        continue;
                    }
                }

                _ => {}
            }
            if let Some((id, range)) = payload.as_section() {
                RawSection {
                    id,
                    data: &input[range],
                }
                .append_to(&mut output);
            }
        }

        self.io.output_wasm(&output, self.wat)?;
        Ok(())
    }
}
