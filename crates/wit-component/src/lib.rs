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
    fn package_scope_foreign_type_encodes_as_qualified_import() -> Result<()> {
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
        // The definition is restated locally and the import names where it came
        // from, just as it is for a type projected out of a foreign interface.
        assert!(
            wat.contains("(record (field \"x\" u32) (field \"y\" u32))"),
            "expected the foreign definition to be restated:\n{wat}"
        );
        assert!(
            wat.contains("(import \"local:types/point\" (type"),
            "expected a qualified import for the foreign package type:\n{wat}"
        );
        assert!(
            !wat.contains("(import \"local:types/point\" (instance"),
            "a package-scope type must not be wrapped in an instance:\n{wat}"
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
        assert!(
            wat.contains("(import \"local:nested/point\" (type"),
            "expected the interface to import the package-scope type:\n{wat}"
        );
        wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;

        let decoded = crate::decode(&wasm)?;
        let mut printer = crate::WitPrinter::default();
        printer.print(decoded.resolve(), decoded.package(), &[])?;
        let printed = printer.output.to_string();
        assert!(printed.contains("record point"), "{printed}");
        assert!(printed.contains("interface api"), "{printed}");

        Ok(())
    }

    /// The approved WIT.md "a package-scope type may refer to another one"
    /// example: since an exported type may only refer to types named by an
    /// import or export, the referenced definition is exported under its own id
    /// first and the dependent one refers back to that export.
    #[test]
    fn package_scope_type_referring_to_another_exports_before_use() -> Result<()> {
        let mut resolve = Resolve::new();
        let pkg = resolve.push_str(
            "demo.wit",
            r#"
package local:demo;

record point {
  x: u32,
  y: u32,
}

type point-list = list<point>;
"#,
        )?;

        let wasm = encode(&resolve, pkg)?;
        let wat = wasmprinter::print_bytes(&wasm)?;
        // `point` is exported first as a record and, since this package has
        // no interfaces or worlds, re-imported under its fully-qualified name
        // so the package name is recoverable...
        assert!(
            wat.contains("(record (field \"x\" u32) (field \"y\" u32))")
                && wat.contains("\"point\" (type 0)")
                && wat.contains("(import \"local:demo/point\" (type (;2;) (eq 1)))"),
            "expected `point` to be exported and self-imported:\n{wat}"
        );
        // ...and `point-list` is a `list` of the imported `point`, exported
        // next.
        assert!(
            wat.contains("(type (;3;) (list 2))") && wat.contains("\"point-list\" (type 3)"),
            "expected `point-list` to reference the imported `point`:\n{wat}"
        );
        wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;
        Ok(())
    }

    /// A package without interfaces or worlds has no fully-qualified export
    /// to recover the package name from, so each of its types is additionally
    /// bound with a self-referential `eq` import named by the type's
    /// fully-qualified name, and decoding recovers the package name from
    /// those imports.
    #[test]
    fn package_scope_type_only_package_round_trips() -> Result<()> {
        let mut resolve = Resolve::new();
        let pkg = resolve.push_str(
            "types-only.wit",
            r#"
package local:types;

record point {
  x: u32,
  y: u32,
}

type path = list<point>;
"#,
        )?;

        let wasm = encode(&resolve, pkg)?;
        wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;

        let wat = wasmprinter::print_bytes(&wasm)?;
        assert!(
            wat.contains("(import \"local:types/point\""),
            "expected a self-import binding `point`:\n{wat}"
        );
        assert!(
            wat.contains("(import \"local:types/path\""),
            "expected a self-import binding `path`:\n{wat}"
        );

        let decoded = crate::decode(&wasm)?;
        let resolve = decoded.resolve();
        let package = &resolve.packages[decoded.package()];
        assert_eq!(package.name.to_string(), "local:types");
        assert_eq!(package.types.len(), 2);
        assert!(package.types.contains_key("point"));
        assert!(package.types.contains_key("path"));
        assert!(package.interfaces.is_empty());
        assert!(package.worlds.is_empty());
        Ok(())
    }

    /// Same as above, but the types-only package also depends on a foreign
    /// package-scope type: the self-imports and the foreign import coexist at
    /// the package root and decoding tells them apart.
    #[test]
    fn package_scope_type_only_package_with_foreign_dep_round_trips() -> Result<()> {
        let mut resolve = Resolve::new();
        resolve.push_str(
            "types.wit",
            r#"
package local:types;

record point {
  x: u32,
  y: u32,
}
"#,
        )?;
        let pkg = resolve.push_str(
            "consumer.wit",
            r#"
package local:consumer;

use local:types/point;

record bin {
  p: point,
}
"#,
        )?;

        let wasm = encode(&resolve, pkg)?;
        wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;

        let decoded = crate::decode(&wasm)?;
        let resolve = decoded.resolve();
        let package = &resolve.packages[decoded.package()];
        assert_eq!(package.name.to_string(), "local:consumer");
        assert!(package.types.contains_key("bin"));

        // The foreign `point` stays owned by `local:types`.
        let types_pkg = resolve
            .packages
            .iter()
            .find(|(_, p)| p.name.to_string() == "local:types")
            .map(|(_, p)| p)
            .expect("foreign package should be present");
        assert!(types_pkg.types.contains_key("point"));
        Ok(())
    }

    /// An `interface` or `world` names a foreign package-scope type with an
    /// `import` on its wrapping component-type. A package-scope definition has
    /// no such wrapper, so its import goes on the package's component itself.
    #[test]
    fn package_scope_type_depending_on_foreign_package_type() -> Result<()> {
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

record bin {
  p: point,
}

interface api {
  wrap: func(b: bin);
}
"#,
        )?;

        let wasm = encode(&resolve, pkg)?;
        let wat = wasmprinter::print_bytes(&wasm)?;
        assert!(
            wat.contains("(import \"local:types/point\" (type"),
            "expected a qualified import on the package itself:\n{wat}"
        );
        wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;

        // The import must not be mistaken for a concrete component's, and
        // `point` must stay owned by `local:types` rather than being claimed by
        // the consumer.
        let decoded = crate::decode(&wasm)?;
        let mut printer = crate::WitPrinter::default();
        printer.print(decoded.resolve(), decoded.package(), &[])?;
        let printed = printer.output.to_string();
        assert!(printed.contains("use local:types/point;"), "{printed}");
        assert!(printed.contains("record bin {"), "{printed}");
        assert!(
            !printed.contains("record point"),
            "the foreign definition must not be restated as our own:\n{printed}"
        );

        Ok(())
    }

    /// A foreign package-scope type carries its version in the fully-qualified
    /// import name and that version survives a decode round-trip.
    #[test]
    fn package_scope_foreign_type_preserves_version() -> Result<()> {
        let mut resolve = Resolve::new();
        resolve.push_str(
            "types.wit",
            r#"
package local:types@1.2.3;

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
package local:consumer@0.1.0;

use local:types/point@1.2.3;

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
            wat.contains("local:types/point@1.2.3"),
            "expected the import name to carry the version:\n{wat}"
        );
        wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;

        let decoded = crate::decode(&wasm)?;
        let mut printer = crate::WitPrinter::default();
        printer.print(decoded.resolve(), decoded.package(), &[])?;
        let printed = printer.output.to_string();
        assert!(
            printed.contains("use local:types/point@1.2.3;"),
            "expected the decoded use to keep the version:\n{printed}"
        );

        Ok(())
    }

    /// `resource` is excluded from package scope: its bound is abstract, so it
    /// can't be restated and bound with `eq`. A component that nonetheless names
    /// a resource where a package-scope type would go must be rejected rather
    /// than silently decoded.
    #[test]
    fn package_scope_resource_import_is_rejected() -> Result<()> {
        let wasm = wat::parse_str(
            r#"
(component
  (import "local:types/thing" (type (;0;) (sub resource)))
  (type (;1;) (record (field "x" u32)))
  (export (;2;) "bin" (type 1))
)
"#,
        )?;

        let err = match crate::decode(&wasm) {
            Ok(_) => panic!("resource at package scope must fail to decode"),
            Err(err) => format!("{err:#}"),
        };
        assert!(
            err.contains("not a structural type"),
            "unexpected error: {err}"
        );
        Ok(())
    }
}

#[cfg(all(test, feature = "dummy-module"))]
mod component_tests {
    use crate::{
        ComponentEncoder, DecodedWasm, StringEncoding, dummy_module, embed_component_metadata,
    };
    use anyhow::Result;
    use wit_parser::{ManglingAndAbi, Resolve};

    // Note: `wit_parser::{Type, TypeOwner, WorldItem}` are referenced via full
    // paths in the assertions below to keep this module's imports minimal.

    /// Building a real component from a world that uses a package-scope type
    /// must not fabricate an interface just to hold the type: the export is the
    /// interface itself. This is the outcome motivating the feature (issue
    /// #694). The produced artifact must also decode as a component, not be
    /// mistaken for a WIT package now that packages may carry type imports.
    #[test]
    fn package_scope_component_has_no_invented_interface() -> Result<()> {
        let mut resolve = Resolve::new();
        let pkg = resolve.push_str(
            "demo.wit",
            r#"
package local:demo;

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
"#,
        )?;
        let world = resolve.select_world(&[pkg], Some("w"))?;

        let mut module = dummy_module(&resolve, world, ManglingAndAbi::Standard32);
        embed_component_metadata(&mut module, &resolve, world, StringEncoding::UTF8)?;
        let component = ComponentEncoder::default()
            .module(&module)?
            .validate(true)
            .encode()?;

        let decoded = crate::decode(&component)?;
        let (resolve, world) = match &decoded {
            DecodedWasm::Component(resolve, world) => (resolve, *world),
            DecodedWasm::WitPackage(..) => {
                panic!("a real component must not be sniffed as a WIT package")
            }
        };

        // The world exports the interface directly, with no import fabricated
        // to carry the package-scope type.
        assert!(
            resolve.worlds[world].imports.is_empty(),
            "no import should be invented to hold the package-scope type"
        );
        let exports: Vec<_> = resolve.worlds[world].exports.values().collect();
        assert_eq!(
            exports.len(),
            1,
            "world should export exactly the interface"
        );
        let iface = match exports[0] {
            wit_parser::WorldItem::Interface { id, .. } => *id,
            other => panic!("expected an interface export, got {other:?}"),
        };

        // That interface still carries `move-to`, and its parameter is the
        // package-scope `point`, owned by a package rather than by the
        // interface.
        let func = resolve.interfaces[iface]
            .functions
            .get("move-to")
            .expect("api interface should keep its function");
        let point = match func.params.as_slice() {
            [param] => match param.ty {
                wit_parser::Type::Id(id) => id,
                other => panic!("expected a named type param, got {other:?}"),
            },
            params => panic!("expected one param, got {}", params.len()),
        };
        // Decoding a component doesn't reconstruct package scope, but the type
        // must still resolve to the sole `api` interface rather than to any
        // separate interface fabricated to hold it.
        assert_eq!(
            resolve.types[point].owner,
            wit_parser::TypeOwner::Interface(iface),
            "point should belong to the api interface, not an invented one"
        );
        assert_eq!(resolve.types[point].name.as_deref(), Some("point"));

        Ok(())
    }
}
