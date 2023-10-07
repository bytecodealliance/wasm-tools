//! The WebAssembly component tooling.

#![deny(missing_docs)]

use std::str::FromStr;
use std::{borrow::Cow, fmt::Display};

use anyhow::{bail, Result};
use wasm_encoder::{CanonicalOption, Encode, Section};
use wit_parser::{Resolve, WorldId};

mod decoding;
mod encoding;
mod gc;
mod linking;
mod printing;
mod targets;
mod validation;

pub use decoding::{decode, DecodedWasm};
pub use encoding::{encode, ComponentEncoder};
pub use linking::Linker;
pub use printing::*;
pub use targets::*;

pub mod metadata;

#[cfg(feature = "dummy-module")]
pub use dummy::dummy_module;
#[cfg(feature = "dummy-module")]
mod dummy;

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

/// A producer section to be added to all modules and components synthesized by
/// this crate
pub(crate) fn base_producers() -> wasm_metadata::Producers {
    let mut producer = wasm_metadata::Producers::empty();
    producer.add("processed-by", "wit-component", env!("CARGO_PKG_VERSION"));
    producer
}

/// Parse a WIT file from a path that represents a top level 'wit' directory,
/// normally containing a 'deps' folder.
pub fn parse_wit_from_path(
    path: impl AsRef<std::path::Path>,
) -> Result<(Resolve, wit_parser::PackageId)> {
    use anyhow::Context;

    let mut resolver = Resolve::default();
    let id = match path.as_ref() {
        // Directories can be directly fed into the resolver
        p if p.is_dir() => {
            resolver
                .push_dir(p)
                .with_context(|| {
                    format!(
                        "failed to resolve directory while parsing WIT for path [{}]",
                        p.display()
                    )
                })?
                .0
        }
        // Non-directory files (including symlinks) can be either:
        // - Wasm modules (binary or WAT) that are WIT packages
        // - WIT files
        #[cfg(not(feature = "wat"))]
        p => {
            let file_contents = std::fs::read(p)
                .with_context(|| format!("failed to parse WIT from path [{}]", p.display()))?;
            match decode(&file_contents)? {
                DecodedWasm::Component(..) => {
                    bail!("specified path is a component, not a wit package")
                }
                DecodedWasm::WitPackage(resolve, pkg) => return Ok((resolve, pkg)),
            }
        }
        #[cfg(feature = "wat")]
        p => {
            use wit_parser::UnresolvedPackage;

            let file_contents = std::fs::read(p)
                .with_context(|| format!("failed to parse WIT from path [{}]", p.display()))?;

            // Check if the bytes represent a Wasm module (either binary or WAT encoded)
            if is_wasm_binary_or_wat(&file_contents) {
                let bytes = wat::parse_bytes(&file_contents).map_err(|mut e| {
                    e.set_path(p);
                    e
                })?;
                match decode(&bytes)? {
                    DecodedWasm::Component(..) => {
                        bail!("specified path is a component, not a wit package")
                    }
                    DecodedWasm::WitPackage(resolve, pkg) => return Ok((resolve, pkg)),
                }
            } else {
                // If the bytes are not a WASM module, they should be WIT that can be parsed
                // into a package by the resolver
                let text = match std::str::from_utf8(&file_contents) {
                    Ok(s) => s,
                    Err(_) => bail!("input file is not valid utf-8"),
                };
                let pkg = UnresolvedPackage::parse(p, text)?;
                resolver.push(pkg)?
            }
        }
    };
    Ok((resolver, id))
}

/// Detect quickly if supplied bytes represent a Wasm module,
/// whether binary encoded or in WAT-encoded.
///
/// This briefly lexes past whitespace and comments as a `*.wat` file to see if
/// we can find a left-paren. If that fails then it's probably `*.wit` instead.
///
///
/// Examples
/// ```
/// # use wit_component::is_wasm_binary_or_wat;
/// assert!(is_wasm_binary_or_wat(r#"
/// (module
///   (type (;0;) (func))
///   (func (;0;) (type 0)
///     nop
///   )
/// )
/// "#));
/// ```
#[cfg(feature = "wat")]
pub fn is_wasm_binary_or_wat(bytes: impl AsRef<[u8]>) -> bool {
    use wast::lexer::{Lexer, TokenKind};

    if bytes.as_ref().starts_with(b"\0asm") {
        return true;
    }
    let text = match std::str::from_utf8(bytes.as_ref()) {
        Ok(s) => s,
        Err(_) => return true,
    };

    let lexer = Lexer::new(text);
    let mut iter = lexer.iter(0);

    while let Some(next) = iter.next() {
        match next.map(|t| t.kind) {
            Ok(TokenKind::Whitespace)
            | Ok(TokenKind::BlockComment)
            | Ok(TokenKind::LineComment) => {}
            Ok(TokenKind::LParen) => return true,
            _ => break,
        }
    }

    false
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
    use wit_parser::{Resolve, UnresolvedPackage};

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
        let pkg = UnresolvedPackage::parse(&Path::new("in-code.wit"), COMPONENT_WIT)?;
        let pkg_id = resolver.push(pkg)?;
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
