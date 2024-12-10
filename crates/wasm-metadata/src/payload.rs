use std::fmt::{self, Display};
use std::ops::Range;

use anyhow::Result;
use comfy_table::modifiers::UTF8_ROUND_CORNERS;
use comfy_table::presets::UTF8_FULL;
use comfy_table::{ContentArrangement, Table};
use serde_derive::Serialize;
use wasmparser::{KnownCustom, Parser, Payload::*};

use crate::{
    Author, ComponentNames, Description, Homepage, Licenses, Metadata, ModuleNames, Producers,
    Source,
};

/// Data representing either a Wasm Component or module
///
/// Each payload has additional [`Metadata`] associated with it,
/// but if it's a Component it may have also additional `Payloads` associated
/// with it.
#[derive(Debug, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Payload {
    /// A representation of a Wasm Component
    Component {
        /// The metadata associated with the Component
        metadata: Metadata,
        /// The metadata of nested Components or Modules
        children: Vec<Payload>,
    },
    /// A representation of a Wasm Module
    Module(Metadata),
}

impl Payload {
    /// Parse metadata from a WebAssembly binary. Supports both core WebAssembly modules, and
    /// WebAssembly components.
    pub fn from_binary(input: &[u8]) -> Result<Self> {
        let mut output = Vec::new();

        for payload in Parser::new(0).parse_all(&input) {
            match payload? {
                Version { encoding, .. } => {
                    if output.is_empty() {
                        match encoding {
                            wasmparser::Encoding::Module => {
                                output.push(Self::empty_module(0..input.len()))
                            }
                            wasmparser::Encoding::Component => {
                                output.push(Self::empty_component(0..input.len()))
                            }
                        }
                    }
                }
                ModuleSection {
                    unchecked_range: range,
                    ..
                } => output.push(Self::empty_module(range)),
                ComponentSection {
                    unchecked_range: range,
                    ..
                } => output.push(Self::empty_component(range)),
                End { .. } => {
                    let finished = output.pop().expect("non-empty metadata stack");
                    if output.is_empty() {
                        return Ok(finished);
                    } else {
                        output.last_mut().unwrap().push_child(finished);
                    }
                }
                CustomSection(c) => match c.as_known() {
                    KnownCustom::Name(_) => {
                        let names = ModuleNames::from_bytes(c.data(), c.data_offset())?;
                        if let Some(name) = names.get_name() {
                            output
                                .last_mut()
                                .expect("non-empty metadata stack")
                                .metadata_mut()
                                .name = Some(name.clone());
                        }
                    }
                    KnownCustom::ComponentName(_) => {
                        let names = ComponentNames::from_bytes(c.data(), c.data_offset())?;
                        if let Some(name) = names.get_name() {
                            output
                                .last_mut()
                                .expect("non-empty metadata stack")
                                .metadata_mut()
                                .name = Some(name.clone());
                        }
                    }
                    KnownCustom::Producers(_) => {
                        let producers = Producers::from_bytes(c.data(), c.data_offset())?;
                        output
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .metadata_mut()
                            .producers = Some(producers);
                    }
                    KnownCustom::Unknown if c.name() == "author" => {
                        let a = Author::parse_custom_section(&c)?;
                        let Metadata { author, .. } = output
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .metadata_mut();
                        *author = Some(a);
                    }
                    KnownCustom::Unknown if c.name() == "description" => {
                        let a = Description::parse_custom_section(&c)?;
                        let Metadata { description, .. } = output
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .metadata_mut();
                        *description = Some(a);
                    }
                    KnownCustom::Unknown if c.name() == "licenses" => {
                        let a = Licenses::parse_custom_section(&c)?;
                        let Metadata { licenses, .. } = output
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .metadata_mut();
                        *licenses = Some(a);
                    }
                    KnownCustom::Unknown if c.name() == "source" => {
                        let a = Source::parse_custom_section(&c)?;
                        let Metadata { source, .. } = output
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .metadata_mut();
                        *source = Some(a);
                    }
                    KnownCustom::Unknown if c.name() == "homepage" => {
                        let a = Homepage::parse_custom_section(&c)?;
                        let Metadata { homepage, .. } = output
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .metadata_mut();
                        *homepage = Some(a);
                    }
                    _ => {}
                },
                _ => {}
            }
        }
        Err(anyhow::anyhow!(
            "malformed wasm binary, should have reached end"
        ))
    }

    /// Get a reference te the metadata
    pub fn metadata(&self) -> &Metadata {
        match self {
            Payload::Component { metadata, .. } => metadata,
            Payload::Module(metadata) => metadata,
        }
    }

    /// Get a mutable reference te the metadata
    pub fn metadata_mut(&mut self) -> &mut Metadata {
        match self {
            Payload::Component { metadata, .. } => metadata,
            Payload::Module(metadata) => metadata,
        }
    }

    fn empty_component(range: Range<usize>) -> Self {
        let mut this = Self::Component {
            metadata: Metadata::default(),
            children: vec![],
        };
        this.metadata_mut().range = range;
        this
    }

    fn empty_module(range: Range<usize>) -> Self {
        let mut this = Self::Module(Metadata::default());
        this.metadata_mut().range = range;
        this
    }

    fn push_child(&mut self, child: Self) {
        match self {
            Self::Module { .. } => panic!("module shouldnt have children"),
            Self::Component { children, .. } => children.push(child),
        }
    }
}

impl Display for Payload {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
            range,
        } = self.metadata();

        // Print the basic information
        let kind = match self {
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
        if let Some(source) = source {
            table.add_row(vec!["source", &source.to_string()]);
        }
        if let Some(author) = author {
            table.add_row(vec!["author", &author.to_string()]);
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

        if let Self::Component { children, .. } = self {
            for metadata in children {
                write!(f, "{metadata}")?;
            }
        }

        Ok(())
    }
}
