//! The WebAssembly component tooling.

#![deny(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]

use std::str::FromStr;
use std::{borrow::Cow, fmt::Display};

use anyhow::{Result, bail};
use wasm_encoder::{CanonicalOption, Encode, Section};
use wit_parser::{Resolve, WorldId};

mod encoding;
mod gc;
mod linking;
mod printing;
mod targets;
mod validation;

pub use encoding::{ComponentEncoder, LibraryInfo, encode};
pub use linking::Linker;
pub use printing::*;
pub use targets::*;
pub use validation::AdapterModuleDidNotExport;
pub use wit_parser::decoding::{DecodedWasm, decode, decode_reader};

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
            _ => bail!("unknown string encoding `{s}`"),
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
    use anyhow::Result;
    use wasmparser::{Payload, WasmFeatures};
    use wit_parser::Resolve;

    use super::{StringEncoding, embed_component_metadata, encode};

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
        let pkg = resolver.push_str("in-code.wit", COMPONENT_WIT)?;
        let world = resolver.select_world(&[pkg], Some("test-world"))?;

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

    #[test]
    fn package_scope_foreign_type_encodes_without_import() -> Result<()> {
        let mut resolve = Resolve::new();
        resolve.push_str(
            "types.wit",
            r#"
package local:types;

record point {
  x: u32,
  y: u32,
}

interface unused {}
"#,
        )?;
        let pkg = resolve.push_str(
            "consumer.wit",
            r#"
package local:consumer;

use local:types/point;

interface api {
  move-to: func(p: point);
}

world w {
  export api;
}
"#,
        )?;

        let wasm = encode(&resolve, pkg)?;
        let wat = wasmprinter::print_bytes(&wasm)?;
        assert!(
            !wat.contains("(import"),
            "package-scope foreign type must be inlined, not imported:\n{wat}"
        );
        assert!(
            wat.contains("(record"),
            "expected inlined record for foreign package type:\n{wat}"
        );
        wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;

        // Text print of the resolved package should emit a toplevel use.
        let mut printer = crate::WitPrinter::default();
        printer.print(&resolve, pkg, &[])?;
        let printed = printer.output.to_string();
        assert!(
            printed.contains("use local:types/point;"),
            "expected toplevel use printback:\n{printed}"
        );

        Ok(())
    }

    #[test]
    fn package_scope_foreign_type_as_printback() -> Result<()> {
        let mut resolve = Resolve::new();
        resolve.push_str(
            "types.wit",
            r#"
package local:types;

record point {
  x: u32,
  y: u32,
}

interface unused {}
"#,
        )?;
        let pkg = resolve.push_str(
            "consumer.wit",
            r#"
package local:consumer;

use local:types/point as pt;

interface api {
  move-to: func(p: pt);
}

world w {
  export api;
}
"#,
        )?;

        let mut printer = crate::WitPrinter::default();
        printer.print(&resolve, pkg, &[])?;
        let printed = printer.output.to_string();
        // Printer canonicalizes to the original package type name rather than
        // preserving the local `as` alias.
        assert!(
            printed.contains("use local:types/point"),
            "expected toplevel use printback:\n{printed}"
        );
        assert!(
            printed.contains("func(p: point)") || printed.contains("func(p: pt)"),
            "expected func to reference the foreign type:\n{printed}"
        );

        let wasm = encode(&resolve, pkg)?;
        wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;
        Ok(())
    }

    #[test]
    fn package_scope_nested_package_encodes() -> Result<()> {
        let mut resolve = Resolve::new();
        resolve.push_str(
            "nested.wit",
            r#"
package local:root;

package local:nested {
  record point {
    x: u32,
    y: u32,
  }

  interface api {
    move-to: func(p: point);
  }

  world w {
    export api;
  }
}
"#,
        )?;
        let nested = *resolve
            .package_names
            .iter()
            .find(|(name, _)| name.name == "nested")
            .map(|(_, id)| id)
            .expect("nested package");

        let wasm = encode(&resolve, nested)?;
        let wat = wasmprinter::print_bytes(&wasm)?;
        assert!(
            wat.contains("(export (;1;) \"point\" (type 0))"),
            "expected package-scope point export:\n{wat}"
        );
        assert!(!wat.contains("(import"), "{wat}");
        wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;

        let decoded = crate::decode(&wasm)?;
        let mut printer = crate::WitPrinter::default();
        printer.print(decoded.resolve(), decoded.package(), &[])?;
        let printed = printer.output.to_string();
        assert!(printed.contains("record point"), "{printed}");
        assert!(printed.contains("interface api"), "{printed}");

        Ok(())
    }

    #[test]
    fn package_scope_type_only_package_decode_fails() -> Result<()> {
        let mut resolve = Resolve::new();
        let pkg = resolve.push_str(
            "types-only.wit",
            r#"
package local:types;

record point {
  x: u32,
  y: u32,
}
"#,
        )?;

        let wasm = encode(&resolve, pkg)?;
        match crate::decode(&wasm) {
            Ok(_) => panic!("type-only packages cannot round-trip"),
            Err(err) => {
                let msg = format!("{err:#}");
                assert!(
                    msg.contains("only package-scope types")
                        || msg.contains("no interfaces or worlds"),
                    "unexpected error: {msg}"
                );
            }
        }
        Ok(())
    }
}
