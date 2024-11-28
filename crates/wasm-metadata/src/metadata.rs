use anyhow::Result;
use serde_derive::Serialize;
use std::fmt;
use std::ops::Range;
use wasmparser::{KnownCustom, Parser, Payload::*};

use crate::{ComponentNames, ModuleNames, Producers, RegistryMetadata};

/// A tree of the metadata found in a WebAssembly binary.
#[derive(Debug, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Metadata {
    /// Metadata found inside a WebAssembly component.
    Component {
        /// The component name, if any. Found in the component-name section.
        name: Option<String>,
        /// The component's producers section, if any.
        producers: Option<Producers>,
        /// The component's registry metadata section, if any.
        registry_metadata: Option<RegistryMetadata>,
        /// All child modules and components inside the component.
        children: Vec<Box<Metadata>>,
        /// Byte range of the module in the parent binary
        range: Range<usize>,
    },
    /// Metadata found inside a WebAssembly module.
    Module {
        /// The module name, if any. Found in the name section.
        name: Option<String>,
        /// The module's producers section, if any.
        producers: Option<Producers>,
        /// The module's registry metadata section, if any.
        registry_metadata: Option<RegistryMetadata>,
        /// Byte range of the module in the parent binary
        range: Range<usize>,
    },
}

impl Metadata {
    /// Parse metadata from a WebAssembly binary. Supports both core WebAssembly modules, and
    /// WebAssembly components.
    pub fn from_binary(input: &[u8]) -> Result<Self> {
        let mut metadata = Vec::new();

        for payload in Parser::new(0).parse_all(&input) {
            match payload? {
                Version { encoding, .. } => {
                    if metadata.is_empty() {
                        match encoding {
                            wasmparser::Encoding::Module => {
                                metadata.push(Metadata::empty_module(0..input.len()))
                            }
                            wasmparser::Encoding::Component => {
                                metadata.push(Metadata::empty_component(0..input.len()))
                            }
                        }
                    }
                }
                ModuleSection {
                    unchecked_range: range,
                    ..
                } => metadata.push(Metadata::empty_module(range)),
                ComponentSection {
                    unchecked_range: range,
                    ..
                } => metadata.push(Metadata::empty_component(range)),
                End { .. } => {
                    let finished = metadata.pop().expect("non-empty metadata stack");
                    if metadata.is_empty() {
                        return Ok(finished);
                    } else {
                        metadata.last_mut().unwrap().push_child(finished);
                    }
                }
                CustomSection(c) => match c.as_known() {
                    KnownCustom::Name(_) => {
                        let names = ModuleNames::from_bytes(c.data(), c.data_offset())?;
                        if let Some(name) = names.get_name() {
                            metadata
                                .last_mut()
                                .expect("non-empty metadata stack")
                                .set_name(&name);
                        }
                    }
                    KnownCustom::ComponentName(_) => {
                        let names = ComponentNames::from_bytes(c.data(), c.data_offset())?;
                        if let Some(name) = names.get_name() {
                            metadata
                                .last_mut()
                                .expect("non-empty metadata stack")
                                .set_name(name);
                        }
                    }
                    KnownCustom::Producers(_) => {
                        let producers = Producers::from_bytes(c.data(), c.data_offset())?;
                        metadata
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .set_producers(producers);
                    }
                    KnownCustom::Unknown if c.name() == "registry-metadata" => {
                        let registry: RegistryMetadata =
                            RegistryMetadata::from_bytes(&c.data(), 0)?;
                        metadata
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .set_registry_metadata(registry);
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

    fn empty_component(range: Range<usize>) -> Self {
        Metadata::Component {
            name: None,
            producers: None,
            registry_metadata: None,
            children: Vec::new(),
            range,
        }
    }

    fn empty_module(range: Range<usize>) -> Self {
        Metadata::Module {
            name: None,
            producers: None,
            registry_metadata: None,
            range,
        }
    }
    fn set_name(&mut self, n: &str) {
        match self {
            Metadata::Module { name, .. } => *name = Some(n.to_owned()),
            Metadata::Component { name, .. } => *name = Some(n.to_owned()),
        }
    }
    fn set_producers(&mut self, p: Producers) {
        match self {
            Metadata::Module { producers, .. } => *producers = Some(p),
            Metadata::Component { producers, .. } => *producers = Some(p),
        }
    }
    fn set_registry_metadata(&mut self, r: RegistryMetadata) {
        match self {
            Metadata::Module {
                registry_metadata, ..
            } => *registry_metadata = Some(r),
            Metadata::Component {
                registry_metadata, ..
            } => *registry_metadata = Some(r),
        }
    }
    fn push_child(&mut self, child: Self) {
        match self {
            Metadata::Module { .. } => panic!("module shouldnt have children"),
            Metadata::Component { children, .. } => children.push(Box::new(child)),
        }
    }

    fn display(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let spaces = std::iter::repeat(" ").take(indent).collect::<String>();
        match self {
            Metadata::Module {
                name,
                producers,
                registry_metadata,
                ..
            } => {
                if let Some(name) = name {
                    writeln!(f, "{spaces}module {name}:")?;
                } else {
                    writeln!(f, "{spaces}module:")?;
                }
                if let Some(producers) = producers {
                    producers.display(f, indent + 4)?;
                }
                if let Some(registry_metadata) = registry_metadata {
                    registry_metadata.display(f, indent + 4)?;
                }
                Ok(())
            }
            Metadata::Component {
                name,
                producers,
                registry_metadata,
                children,
                ..
            } => {
                if let Some(name) = name {
                    writeln!(f, "{spaces}component {name}:")?;
                } else {
                    writeln!(f, "{spaces}component:")?;
                }
                if let Some(producers) = producers {
                    producers.display(f, indent + 4)?;
                }
                if let Some(registry_metadata) = registry_metadata {
                    registry_metadata.display(f, indent + 4)?;
                }
                for c in children {
                    c.display(f, indent + 4)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Metadata {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display(f, 0)
    }
}
