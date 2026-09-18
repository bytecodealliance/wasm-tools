use std::fmt::{self, Debug};

use anyhow::Result;
use wasm_encoder::Encode;
use wasm_encoder::reencode::{ReencodeComponent, RoundtripReencoder};
use wasmparser::{BinaryReader, ComponentNameSectionReader};

/// Helper for rewriting a component's component-name section with a new component name.
pub struct ComponentNames<'a> {
    component_name: Option<String>,
    names: Vec<wasmparser::ComponentName<'a>>,
}

impl<'a> Debug for ComponentNames<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ComponentNames")
            .field("component_name", &self.component_name)
            .finish_non_exhaustive()
    }
}

impl<'a> ComponentNames<'a> {
    /// Create an empty component-name section.
    pub fn empty() -> Self {
        ComponentNames {
            component_name: None,
            names: Vec::new(),
        }
    }
    /// Read a component-name section from a WebAssembly binary. Records the component name, as
    /// well as all other component name fields for later serialization.
    pub fn from_bytes(bytes: &'a [u8], offset: u64) -> Result<ComponentNames<'a>> {
        let reader = BinaryReader::new(bytes, offset);
        let section = ComponentNameSectionReader::new(reader);
        let mut s = Self::empty();
        for name in section.into_iter() {
            let name = name?;
            match name {
                wasmparser::ComponentName::Component { name, .. } => {
                    s.component_name = Some(name.to_owned())
                }
                _ => s.names.push(name),
            }
        }
        Ok(s)
    }
    /// Set component name according to [`AddMetadata`]
    pub(crate) fn from_name(name: &Option<String>) -> Self {
        let mut s = Self::empty();
        s.component_name = name.clone();
        s
    }

    /// Merge with another section
    pub(crate) fn merge(&mut self, other: &Self) {
        if other.component_name.is_some() {
            self.component_name = other.component_name.clone();
        }
        self.names.extend_from_slice(&other.names);
    }

    /// Set component name
    pub fn set_name(&mut self, name: &str) {
        self.component_name = Some(name.to_owned())
    }
    /// Get component name
    pub fn get_name(&self) -> Option<&String> {
        self.component_name.as_ref()
    }
    /// Serialize into [`wasm_encoder::ComponentNameSection`]
    pub(crate) fn section(&self) -> Result<wasm_encoder::ComponentNameSection> {
        let mut section = wasm_encoder::ComponentNameSection::new();
        if let Some(component_name) = &self.component_name {
            section.component(&component_name);
        }
        for n in self.names.iter() {
            RoundtripReencoder.parse_custom_component_name_subsection(&mut section, n.clone())?;
        }
        Ok(section)
    }

    /// Serialize into the raw bytes of a wasm custom section.
    pub fn raw_custom_section(&self) -> Result<Vec<u8>> {
        let mut ret = Vec::new();
        self.section()?.encode(&mut ret);
        Ok(ret)
    }
}
