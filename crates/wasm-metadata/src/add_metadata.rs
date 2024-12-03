use crate::{rewrite_wasm, Producers, RegistryMetadata};

use anyhow::Result;

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
    #[cfg_attr(feature = "clap", clap(long, value_parser = parse_key_value, value_name = "NAME=VERSION"))]
    pub language: Vec<(String, String)>,

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
pub(crate) fn parse_key_value(s: &str) -> Result<(String, String)> {
    s.split_once('=')
        .map(|(k, v)| (k.to_owned(), v.to_owned()))
        .ok_or_else(|| anyhow::anyhow!("expected KEY=VALUE"))
}

#[cfg(feature = "clap")]
pub(crate) fn parse_registry_metadata_value(s: &str) -> Result<RegistryMetadata> {
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
