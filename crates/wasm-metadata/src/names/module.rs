use std::fmt::{self, Debug};

use anyhow::Result;
use wasm_encoder::Encode;
use wasm_encoder::reencode::{Reencode, RoundtripReencoder};
use wasmparser::{BinaryReader, NameSectionReader};

/// Helper for rewriting a module's name section with a new module name.
pub struct ModuleNames<'a> {
    module_name: Option<String>,
    names: Vec<wasmparser::Name<'a>>,
}

impl<'a> Debug for ModuleNames<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ModuleNames")
            .field("module_name", &self.module_name)
            .finish_non_exhaustive()
    }
}

impl<'a> ModuleNames<'a> {
    /// Create an empty name section.
    pub fn empty() -> Self {
        ModuleNames {
            module_name: None,
            names: Vec::new(),
        }
    }
    /// Read a name section from a WebAssembly binary. Records the module name, and all other
    /// contents of name section, for later serialization.
    pub fn from_bytes(bytes: &'a [u8], offset: u64) -> Result<ModuleNames<'a>> {
        let reader = BinaryReader::new(bytes, offset);
        let section = NameSectionReader::new(reader);
        let mut s = Self::empty();
        for name in section.into_iter() {
            let name = name?;
            match name {
                wasmparser::Name::Module { name, .. } => s.module_name = Some(name.to_owned()),
                _ => s.names.push(name),
            }
        }
        Ok(s)
    }
    /// Update module section according to [`AddMetadata`]
    pub(crate) fn from_name(name: &Option<String>) -> Self {
        let mut s = Self::empty();
        s.module_name = name.clone();
        s
    }

    /// Merge with another section
    pub(crate) fn merge(&mut self, other: &Self) {
        if other.module_name.is_some() {
            self.module_name = other.module_name.clone();
        }
        self.names.extend_from_slice(&other.names);
    }

    /// Set module name
    pub fn set_name(&mut self, name: &str) {
        self.module_name = Some(name.to_owned())
    }
    /// Get module name
    pub fn get_name(&self) -> Option<&String> {
        self.module_name.as_ref()
    }
    /// Serialize into [`wasm_encoder::NameSection`].
    pub(crate) fn section(&self) -> Result<wasm_encoder::NameSection> {
        let mut section = wasm_encoder::NameSection::new();
        if let Some(module_name) = &self.module_name {
            section.module(&module_name);
        }
        for n in self.names.iter() {
            RoundtripReencoder.parse_custom_name_subsection(&mut section, n.clone())?;
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
