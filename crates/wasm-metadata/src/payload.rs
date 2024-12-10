use std::fmt::{self, Display};
use std::ops::Range;

use anyhow::Result;
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

    fn display(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let spaces = std::iter::repeat(" ").take(indent).collect::<String>();
        match self {
            Self::Module(Metadata {
                name, producers, ..
            }) => {
                if let Some(name) = name {
                    writeln!(f, "{spaces}module {name}:")?;
                } else {
                    writeln!(f, "{spaces}module:")?;
                }
                if let Some(producers) = producers {
                    producers.display(f, indent + 4)?;
                }
                Ok(())
            }
            Self::Component {
                children,
                metadata: Metadata {
                    name, producers, ..
                },
            } => {
                if let Some(name) = name {
                    writeln!(f, "{spaces}component {name}:")?;
                } else {
                    writeln!(f, "{spaces}component:")?;
                }
                if let Some(producers) = producers {
                    producers.display(f, indent + 4)?;
                }
                for c in children {
                    c.display(f, indent + 4)?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Payload {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display(f, 0)
    }
}
