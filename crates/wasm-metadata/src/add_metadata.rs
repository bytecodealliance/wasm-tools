use crate::{rewrite_wasm, Producers};
use anyhow::Result;
use std::fmt::Debug;

/// Add metadata (module name, producers) to a WebAssembly file.
///
/// Supports both core WebAssembly modules and components. In components,
/// metadata will be added to the outermost component.
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct AddMetadata {
    /// Add a module or component name to the names section
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME", default_value_t, value_parser = parse_raw_add_metadata_field))]
    pub name: AddMetadataField<String>,

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
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME", default_value_t, value_parser = parse_add_metadata_field::<crate::Authors>))]
    #[cfg(feature = "oci")]
    pub authors: AddMetadataField<crate::Authors>,

    /// A human-readable description of the binary
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME", default_value_t, value_parser = parse_add_metadata_field::<crate::Description>))]
    #[cfg(feature = "oci")]
    pub description: AddMetadataField<crate::Description>,

    /// License(s) under which contained software is distributed as an SPDX License Expression.
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME", default_value_t, value_parser = parse_add_metadata_field::<crate::Licenses>))]
    #[cfg(feature = "oci")]
    pub licenses: AddMetadataField<crate::Licenses>,

    /// URL to get source code for building the image
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME", default_value_t, value_parser = parse_add_metadata_field::<crate::Source>))]
    #[cfg(feature = "oci")]
    pub source: AddMetadataField<crate::Source>,

    /// URL to find more information on the binary
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME", default_value_t, value_parser = parse_add_metadata_field::<crate::Homepage>))]
    #[cfg(feature = "oci")]
    pub homepage: AddMetadataField<crate::Homepage>,

    /// Source control revision identifier for the packaged software.
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME", default_value_t, value_parser = parse_add_metadata_field::<crate::Revision>))]
    #[cfg(feature = "oci")]
    pub revision: AddMetadataField<crate::Revision>,

    /// Version of the packaged software
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME", default_value_t, value_parser = parse_add_metadata_field::<crate::Version>))]
    #[cfg(feature = "oci")]
    pub version: AddMetadataField<crate::Version>,
}

#[cfg(feature = "clap")]
pub const KEEP: &str = "::keep";

#[cfg(feature = "clap")]
pub(crate) fn parse_key_value(s: &str) -> Result<(String, String)> {
    s.split_once('=')
        .map(|(k, v)| (k.to_owned(), v.to_owned()))
        .ok_or_else(|| anyhow::anyhow!("expected KEY=VALUE"))
}

#[cfg(feature = "clap")]
pub(crate) fn parse_raw_add_metadata_field(s: &str) -> Result<AddMetadataField<String>> {
    if s.is_empty() {
        Ok(AddMetadataField::Clear)
    } else if s == KEEP {
        Ok(AddMetadataField::Keep)
    } else {
        Ok(AddMetadataField::Set(s.to_owned()))
    }
}

#[cfg(feature = "clap")]
pub(crate) fn parse_add_metadata_field<
    T: Debug + Clone + std::str::FromStr<Err = anyhow::Error>,
>(
    s: &str,
) -> Result<AddMetadataField<T>> {
    if s.is_empty() {
        Ok(AddMetadataField::Clear)
    } else if s == KEEP {
        Ok(AddMetadataField::Keep)
    } else {
        Ok(AddMetadataField::Set(T::from_str(s).map_err(|err| {
            anyhow::anyhow!("failed to parse field: {err}")
        })?))
    }
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

/// Defines how to modify a field of the component/module metadata
#[derive(Debug, Clone)]
pub enum AddMetadataField<T: Debug + Clone> {
    /// Keep the existing value of the field
    Keep,
    /// Remove the existing value of the field
    Clear,
    /// Set the field to a new value
    Set(T),
}

impl<T: Debug + Clone> AddMetadataField<T> {
    /// Returns true if the field should be cleared
    pub fn is_clear(&self) -> bool {
        matches!(self, Self::Clear)
    }

    /// Returns true if the field should be kept
    pub fn is_keep(&self) -> bool {
        matches!(self, Self::Keep)
    }
}

#[cfg(feature = "clap")]
impl<T: Debug + Clone> std::fmt::Display for AddMetadataField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddMetadataField::Keep => write!(f, "{KEEP}"), // special value for the CLI value parser
            AddMetadataField::Clear => write!(f, ""), // empty string for the CLI value parser means 'clear'
            AddMetadataField::Set(value) => write!(f, "set to {value:?}"),
        }
    }
}

impl<T: Debug + Clone> Default for AddMetadataField<T> {
    fn default() -> Self {
        Self::Keep
    }
}
