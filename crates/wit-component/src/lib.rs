//! The WebAssembly component tooling.

#![deny(missing_docs)]

use std::str::FromStr;
use std::{borrow::Cow, fmt::Display};

use anyhow::{bail, Context, Result};
use wasm_encoder::{CanonicalOption, Encode, Section};
use wit_parser::{parse_use_path, PackageId, ParsedUsePath, Resolve, WorldId};

mod encoding;
mod gc;
mod linking;
mod printing;
mod targets;
mod validation;

pub use encoding::{encode, ComponentEncoder};
pub use linking::Linker;
pub use printing::*;
pub use targets::*;
pub use wit_parser::decoding::{decode, decode_reader, DecodedWasm};

pub mod metadata;

#[cfg(feature = "dummy-module")]
pub use dummy::dummy_module;
#[cfg(feature = "dummy-module")]
mod dummy;

#[cfg(feature = "semver-check")]
mod semver_check;
#[cfg(feature = "semver-check")]
pub use semver_check::*;

/// Supported string encoding formats.
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum StringEncoding {
    /// Strings are encoded with UTF-8.
    #[default]
    UTF8,
    /// Strings are encoded with UTF-16.
    UTF16,
    /// Strings are encoded with compact UTF-16 (i.e. Latin1+UTF-16).
    CompactUTF16,
}

impl Display for StringEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringEncoding::UTF8 => write!(f, "utf8"),
            StringEncoding::UTF16 => write!(f, "utf16"),
            StringEncoding::CompactUTF16 => write!(f, "compact-utf16"),
        }
    }
}

impl FromStr for StringEncoding {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "utf8" => Ok(StringEncoding::UTF8),
            "utf16" => Ok(StringEncoding::UTF16),
            "compact-utf16" => Ok(StringEncoding::CompactUTF16),
            _ => bail!("unknown string encoding `{}`", s),
        }
    }
}

impl From<StringEncoding> for wasm_encoder::CanonicalOption {
    fn from(e: StringEncoding) -> wasm_encoder::CanonicalOption {
        match e {
            StringEncoding::UTF8 => CanonicalOption::UTF8,
            StringEncoding::UTF16 => CanonicalOption::UTF16,
            StringEncoding::CompactUTF16 => CanonicalOption::CompactUTF16,
        }
    }
}

/// Handles world name resolution for cases when multiple packages may have been resolved. If this
/// is the case, and we're dealing with input that contains a user-supplied world name (like via a
/// CLI command, for instance), we want to ensure that the world name follows the following rules:
///
///   * If there is a single resolved package with a single world, the world name name MAY be
///     omitted.
///   * If there is a single resolved package with multiple worlds, the world name MUST be supplied,
///     but MAY or MAY NOT be fully-qualified.
///   * If there are multiple resolved packages, the world name MUST be fully-qualified.
pub fn resolve_world_from_name(
    resolve: &Resolve,
    resolved_packages: Vec<PackageId>,
    world_name: Option<&str>,
) -> Result<WorldId> {
    match resolved_packages.len() {
        0 => bail!("all of the supplied WIT source files were empty"),
        1 => resolve.select_world(resolved_packages[0], world_name.as_deref()),
        _ => match world_name.as_deref() {
            Some(name) => {
                let world_path = parse_use_path(name).with_context(|| {
                    format!("failed to parse world specifier `{name}`")
                })?;
                match world_path {
                        ParsedUsePath::Name(name) => bail!("the world specifier must be of the fully-qualified, id-based form (ex: \"wasi:http/proxy\" rather than \"proxy\"); you used {name}"),
                        ParsedUsePath::Package(pkg_name, _) => {
                            match resolve.package_names.get(&pkg_name) {
                                Some(pkg_id) => resolve.select_world(pkg_id.clone(), world_name.as_deref()),
                                None => bail!("the world specifier you provided named {pkg_name}, but no package with that name was found"),
                            }
                        }
                    }
            }
            None => bail!("the supplied WIT source files describe multiple packages; please provide a fully-qualified world-specifier to the `embed` command"),
        },
    }
}

/// A producer section to be added to all modules and components synthesized by
/// this crate
pub(crate) fn base_producers() -> wasm_metadata::Producers {
    let mut producer = wasm_metadata::Producers::empty();
    producer.add("processed-by", "wit-component", env!("CARGO_PKG_VERSION"));
    producer
}

/// Embed component metadata in a buffer of bytes that contains a Wasm module
pub fn embed_component_metadata(
    bytes: &mut Vec<u8>,
    wit_resolver: &Resolve,
    world: WorldId,
    encoding: StringEncoding,
) -> Result<()> {
    let encoded = metadata::encode(&wit_resolver, world, encoding, None)?;

    let section = wasm_encoder::CustomSection {
        name: "component-type".into(),
        data: Cow::Borrowed(&encoded),
    };
    bytes.push(section.id());
    section.encode(bytes);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use anyhow::Result;
    use wasmparser::Payload;
    use wit_parser::{Resolve, UnresolvedPackageGroup};

    use super::{embed_component_metadata, StringEncoding};

    const MODULE_WAT: &str = r#"
(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    nop
  )
)
"#;

    const COMPONENT_WIT: &str = r#"
package test:foo;
world test-world {}
"#;

    #[test]
    fn component_metadata_embedding_works() -> Result<()> {
        let mut bytes = wat::parse_str(MODULE_WAT)?;

        // Get original len & custom section count
        let original_len = bytes.len();
        let payloads = wasmparser::Parser::new(0).parse_all(&bytes);
        let original_custom_section_count = payloads.fold(0, |acc, payload| {
            if let Ok(Payload::CustomSection { .. }) = payload {
                acc + 1
            } else {
                acc
            }
        });

        // Parse pre-canned WIT to build resolver
        let mut resolver = Resolve::default();
        let UnresolvedPackageGroup {
            mut packages,
            source_map,
        } = UnresolvedPackageGroup::parse(&Path::new("in-code.wit"), COMPONENT_WIT)?;
        let pkg_id = resolver.push(packages.remove(0), &source_map)?;
        let world = resolver.select_world(pkg_id, Some("test-world").into())?;

        // Embed component metadata
        embed_component_metadata(&mut bytes, &resolver, world, StringEncoding::UTF8)?;

        // Re-retrieve custom section count, and search for the component-type custom section along the way
        let mut found_component_section = false;
        let new_custom_section_count =
            wasmparser::Parser::new(0)
                .parse_all(&bytes)
                .fold(0, |acc, payload| {
                    if let Ok(Payload::CustomSection(reader)) = payload {
                        if reader.name() == "component-type" {
                            found_component_section = true;
                        }
                        acc + 1
                    } else {
                        acc
                    }
                });

        assert!(original_len < bytes.len());
        assert_eq!(original_custom_section_count + 1, new_custom_section_count);
        assert!(found_component_section);

        Ok(())
    }
}
