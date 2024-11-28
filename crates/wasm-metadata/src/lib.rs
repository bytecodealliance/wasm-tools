use anyhow::Result;
use serde_derive::Serialize;
use std::borrow::Cow;
use std::fmt;
use std::mem;
use std::ops::Range;
use wasm_encoder::ComponentSection as _;
use wasm_encoder::{ComponentSectionId, Encode, Section};
use wasmparser::{KnownCustom, Parser, Payload::*};

pub use component_name::ComponentNames;
pub use module_name::ModuleNames;
pub use producers::{Producers, ProducersField};
pub use registry::{CustomLicense, Link, LinkType, RegistryMetadata};

mod component_name;
mod module_name;
mod producers;
mod registry;

/// Add metadata (module name, producers) to a WebAssembly file.
///
/// Supports both core WebAssembly modules and components. In components,
/// metadata will be added to the outermost component.
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[derive(Debug, Clone, Default)]
pub struct AddMetadata {
    /// Add a module or component name to the names section
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    pub name: Option<String>,

    /// Add a programming language to the producers section
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    pub language: Vec<String>,

    /// Add a tool and its version to the producers section
    #[cfg_attr(feature = "clap", clap(long = "processed-by", value_parser = parse_key_value, value_name="NAME=VERSION"))]
    pub processed_by: Vec<(String, String)>,

    /// Add an SDK and its version to the producers section
    #[cfg_attr(feature="clap", clap(long, value_parser = parse_key_value, value_name="NAME=VERSION"))]
    pub sdk: Vec<(String, String)>,

    /// Add an registry metadata to the registry-metadata section
    #[cfg_attr(feature="clap", clap(long, value_parser = parse_registry_metadata_value, value_name="PATH"))]
    pub registry_metadata: Option<RegistryMetadata>,
}

#[cfg(feature = "clap")]
fn parse_key_value(s: &str) -> Result<(String, String)> {
    s.split_once('=')
        .map(|(k, v)| (k.to_owned(), v.to_owned()))
        .ok_or_else(|| anyhow::anyhow!("expected KEY=VALUE"))
}

#[cfg(feature = "clap")]
fn parse_registry_metadata_value(s: &str) -> Result<RegistryMetadata> {
    let contents = std::fs::read(s)?;

    let registry_metadata = RegistryMetadata::from_bytes(&contents, 0)?;

    Ok(registry_metadata)
}

impl AddMetadata {
    /// Process a WebAssembly binary. Supports both core WebAssembly modules, and WebAssembly
    /// components. The module and component will have, at very least, an empty name and producers
    /// section created.
    pub fn to_wasm(&self, input: &[u8]) -> Result<Vec<u8>> {
        rewrite_wasm(
            &self.name,
            &Producers::from_meta(self),
            self.registry_metadata.as_ref(),
            input,
        )
    }
}

fn rewrite_wasm(
    add_name: &Option<String>,
    add_producers: &Producers,
    add_registry_metadata: Option<&RegistryMetadata>,
    input: &[u8],
) -> Result<Vec<u8>> {
    let mut producers_found = false;
    let mut names_found = false;
    let mut stack = Vec::new();
    let mut output = Vec::new();
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

        // Only rewrite the outermost custom sections
        if let CustomSection(c) = &payload {
            if stack.len() == 0 {
                match c.as_known() {
                    KnownCustom::Producers(_) => {
                        producers_found = true;
                        let mut producers = Producers::from_bytes(c.data(), c.data_offset())?;
                        // Add to the section according to the command line flags:
                        producers.merge(&add_producers);
                        // Encode into output:
                        producers.section().append_to(&mut output);
                        continue;
                    }
                    KnownCustom::Name(_) => {
                        names_found = true;
                        let mut names = ModuleNames::from_bytes(c.data(), c.data_offset())?;
                        names.merge(&ModuleNames::from_name(add_name));

                        names.section()?.as_custom().append_to(&mut output);
                        continue;
                    }
                    KnownCustom::ComponentName(_) => {
                        names_found = true;
                        let mut names = ComponentNames::from_bytes(c.data(), c.data_offset())?;
                        names.merge(&ComponentNames::from_name(add_name));
                        names.section()?.as_custom().append_to(&mut output);
                        continue;
                    }
                    KnownCustom::Unknown if c.name() == "registry-metadata" => {
                        // Pass section through if a new registry metadata isn't provided, otherwise ignore and overwrite with new
                        if add_registry_metadata.is_none() {
                            let registry: RegistryMetadata =
                                RegistryMetadata::from_bytes(&c.data(), 0)?;

                            let registry_metadata = wasm_encoder::CustomSection {
                                name: Cow::Borrowed("registry-metadata"),
                                data: Cow::Owned(serde_json::to_vec(&registry)?),
                            };
                            registry_metadata.append_to(&mut output);
                            continue;
                        }
                    }
                    _ => {}
                }
            }
        }
        // All other sections get passed through unmodified:
        if let Some((id, range)) = payload.as_section() {
            wasm_encoder::RawSection {
                id,
                data: &input[range],
            }
            .append_to(&mut output);
        }
    }
    if !names_found && add_name.is_some() {
        if output.starts_with(&wasm_encoder::Component::HEADER) {
            let names = ComponentNames::from_name(add_name);
            names.section()?.append_to_component(&mut output);
        } else {
            let names = ModuleNames::from_name(add_name);
            names.section()?.append_to(&mut output)
        }
    }
    if !producers_found && !add_producers.is_empty() {
        let mut producers = Producers::empty();
        // Add to the section according to the command line flags:
        producers.merge(add_producers);
        // Encode into output:
        producers.section().append_to(&mut output);
    }
    if add_registry_metadata.is_some() {
        let registry_metadata = wasm_encoder::CustomSection {
            name: Cow::Borrowed("registry-metadata"),
            data: Cow::Owned(serde_json::to_vec(&add_registry_metadata)?),
        };
        registry_metadata.append_to(&mut output);
    }
    Ok(output)
}

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

pub(crate) fn name_map(map: &wasmparser::NameMap<'_>) -> Result<wasm_encoder::NameMap> {
    let mut out = wasm_encoder::NameMap::new();
    for m in map.clone().into_iter() {
        let m = m?;
        out.append(m.index, m.name);
    }
    Ok(out)
}

pub(crate) fn indirect_name_map(
    map: &wasmparser::IndirectNameMap<'_>,
) -> Result<wasm_encoder::IndirectNameMap> {
    let mut out = wasm_encoder::IndirectNameMap::new();
    for m in map.clone().into_iter() {
        let m = m?;
        out.append(m.index, &name_map(&m.names)?);
    }
    Ok(out)
}
