use crate::{rewrite_wasm, Producers};
use anyhow::Result;

/// Add metadata (module name, producers) to a WebAssembly file.
///
/// Supports both core WebAssembly modules and components. In components,
/// metadata will be added to the outermost component.
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[derive(Debug, Clone, Default)]
#[non_exhaustive]
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

    /// Contact details of the people or organization responsible,
    /// encoded as a freeform string.
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    #[cfg(feature = "oci")]
    pub authors: Option<crate::Authors>,

    /// A human-readable description of the binary
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    #[cfg(feature = "oci")]
    pub description: Option<crate::Description>,

    /// License(s) under which contained software is distributed as an SPDX License Expression.
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    #[cfg(feature = "oci")]
    pub licenses: Option<crate::Licenses>,

    /// URL to get source code for building the image
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    #[cfg(feature = "oci")]
    pub source: Option<crate::Source>,

    /// URL to find more information on the binary
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    #[cfg(feature = "oci")]
    pub homepage: Option<crate::Homepage>,

    /// Source control revision identifier for the packaged software.
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    #[cfg(feature = "oci")]
    pub revision: Option<crate::Revision>,

    /// Version of the packaged software
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    #[cfg(feature = "oci")]
    pub version: Option<crate::Version>,
}

#[cfg(feature = "clap")]
pub(crate) fn parse_key_value(s: &str) -> Result<(String, String)> {
    s.split_once('=')
        .map(|(k, v)| (k.to_owned(), v.to_owned()))
        .ok_or_else(|| anyhow::anyhow!("expected KEY=VALUE"))
}

impl AddMetadata {
    /// Process a WebAssembly binary. Supports both core WebAssembly modules, and WebAssembly
    /// components. The module and component will have, at very least, an empty name and producers
    /// section created.
    pub fn to_wasm(&self, input: &[u8]) -> Result<Vec<u8>> {
        let add_producers = Producers::from_meta(self);
        rewrite_wasm(self, &add_producers, input)
    }
}
