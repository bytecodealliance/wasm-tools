//! Definition for encoding of custom sections within core wasm modules of
//! component-model related data.
//!
//! When creating a component from a source language the high-level process for
//! doing this is that code will be generated into the source language by
//! `wit-bindgen` or a similar tool which will be compiled down to core wasm.
//! The core wasm file is then fed into `wit-component` and a component is
//! created. This means that the componentization process is decoupled from the
//! binding generation process and intentionally affords for linking together
//! libraries into the main core wasm module that import different interfaces.
//!
//! The purpose of this module is to define an intermediate format to reside in
//! a custom section in the core wasm output. This intermediate format is
//! carried through the wasm linker through a custom section whose name starts
//! with `component-type`. This custom section is created
//! per-language-binding-generation and consumed by slurping up all the
//! sections during the component creation process.
//!
//! Currently the encoding of this custom section is:
//!
//! * First, a version byte (`CURRENT_VERSION`). This is intended to detect
//!   mismatches between different versions of the binding generator and
//!   `wit-component` which may or may not become a problem over time.
//!
//! * Next a string encoding byte.
//!
//! * Next, three strings are encoded. These are the names of the root package,
//!   document, and world that the bindings were generated for. These strings
//!   are used as lookups into the next field.
//!
//! * Finally the Wasm-encoded representation of a `Resolve` is included in its
//!   binary form. This is the encoding of a package into wasm, and the bound
//!   world for the bindings is specified from the prior strings.

use crate::validation::BARE_FUNC_MODULE_NAME;
use crate::{DecodedWasm, StringEncoding};
use anyhow::{bail, Context, Result};
use indexmap::IndexMap;
use wasm_encoder::Encode;
use wasm_metadata::Producers;
use wasmparser::BinaryReader;
use wit_parser::{Document, Package, Resolve, World, WorldId, WorldItem};

const CURRENT_VERSION: u8 = 0x02;

/// The result of decoding binding information from a WebAssembly binary.
///
/// This structure is returned by [`decode`] and represents the interface of a
/// WebAssembly binary.
pub struct Bindgen {
    /// Interface and type information for this binary.
    pub resolve: Resolve,
    /// The world that was bound.
    pub world: WorldId,
    /// Metadata about this specific module that was bound.
    pub metadata: ModuleMetadata,
    /// Producer information about tools used to produce this specific module.
    pub producers: Option<Producers>,
}

impl Default for Bindgen {
    fn default() -> Bindgen {
        let mut resolve = Resolve::default();
        let package = resolve.packages.alloc(Package {
            name: "root".to_string(),
            url: None,
            documents: Default::default(),
        });
        let document = resolve.documents.alloc(Document {
            name: "root".to_string(),
            interfaces: Default::default(),
            worlds: Default::default(),
            default_world: None,
            default_interface: None,
            package: Some(package),
        });
        let world = resolve.worlds.alloc(World {
            name: "root".to_string(),
            docs: Default::default(),
            imports: Default::default(),
            exports: Default::default(),
            document,
        });
        Bindgen {
            resolve,
            world,
            metadata: ModuleMetadata::default(),
            producers: None,
        }
    }
}

/// Module-level metadata that's specific to one core WebAssembly module. This
/// is extracted with a [`Bindgen`].
#[derive(Default)]
pub struct ModuleMetadata {
    /// Per-function options imported into the core wasm module, currently only
    /// related to string encoding.
    pub import_encodings: IndexMap<(String, String), StringEncoding>,

    /// Per-function options exported from the core wasm module, currently only
    /// related to string encoding.
    pub export_encodings: IndexMap<String, StringEncoding>,
}

/// This function will parse the `wasm` binary given as input and return a
/// [`Bindgen`] which extracts the custom sections describing component-level
/// types from within the binary itself.
///
/// This is used to parse the output of `wit-bindgen`-generated modules and is
/// one of the earliest phases in transitioning such a module to a component.
/// The extraction here provides the metadata necessary to continue the process
/// later on.
///
/// Note that a "stripped" binary where `component-type` sections are removed
/// is returned as well to embed within a component.
pub fn decode(wasm: &[u8]) -> Result<(Vec<u8>, Bindgen)> {
    let mut ret = Bindgen::default();
    let mut new_module = wasm_encoder::Module::new();

    for payload in wasmparser::Parser::new(0).parse_all(wasm) {
        let payload = payload.context("decoding item in module")?;
        match payload {
            wasmparser::Payload::CustomSection(cs) if cs.name().starts_with("component-type") => {
                let data = Bindgen::decode(cs.data())
                    .with_context(|| format!("decoding custom section {}", cs.name()))?;
                ret.merge(data)
                    .with_context(|| format!("updating metadata for section {}", cs.name()))?;
            }
            _ => {
                if let Some((id, range)) = payload.as_section() {
                    new_module.section(&wasm_encoder::RawSection {
                        id,
                        data: &wasm[range],
                    });
                }
            }
        }
    }

    Ok((new_module.finish(), ret))
}

/// Creates a `component-type*` custom section to be decoded by `decode` above.
///
/// This is primarily created by wit-bindgen-based guest generators to embed
/// into the final core wasm binary. The core wasm binary is later fed
/// through `wit-component` to produce the actual component where this returned
/// section will be decoded.
pub fn encode(
    resolve: &Resolve,
    world: WorldId,
    encoding: StringEncoding,
    producers: Option<&Producers>,
) -> Result<Vec<u8>> {
    let world = &resolve.worlds[world];
    let doc = &resolve.documents[world.document];
    let pkg = &resolve.packages[doc.package.unwrap()];

    assert!(
        resolve
            .packages
            .iter()
            .filter(|(_, p)| p.name == pkg.name)
            .count()
            == 1
    );

    let mut ret = Vec::new();
    ret.push(CURRENT_VERSION);
    ret.push(match encoding {
        StringEncoding::UTF8 => 0x00,
        StringEncoding::UTF16 => 0x01,
        StringEncoding::CompactUTF16 => 0x02,
    });
    pkg.name.encode(&mut ret);
    doc.name.encode(&mut ret);
    world.name.encode(&mut ret);
    // This appends a wasm binary encoded Component to the ret:
    let mut component_builder = crate::encoding::encode_component(resolve, doc.package.unwrap())?;

    if let Some(p) = producers {
        component_builder.add_producers(p);
    }
    ret.extend(component_builder.finish());
    Ok(ret)
}

impl Bindgen {
    fn decode(data: &[u8]) -> Result<Bindgen> {
        let mut reader = BinaryReader::new(data);
        let version = reader.read_u8()?;
        if version != CURRENT_VERSION {
            bail!("component-type version {version} does not match supported version {CURRENT_VERSION}");
        }
        let encoding = match reader.read_u8()? {
            0x00 => StringEncoding::UTF8,
            0x01 => StringEncoding::UTF16,
            0x02 => StringEncoding::CompactUTF16,
            byte => bail!("invalid string encoding {byte:#x}"),
        };
        let pkg_name = reader.read_string()?;
        let doc_name = reader.read_string()?;
        let world_name = reader.read_string()?;

        let (resolve, pkg) = match crate::decode(pkg_name, &data[reader.original_position()..])? {
            DecodedWasm::WitPackage(resolve, pkg) => (resolve, pkg),
            DecodedWasm::Component(..) => bail!("expected an encoded wit package"),
        };
        let doc = resolve.packages[pkg].documents[doc_name];
        let world = resolve.documents[doc].worlds[world_name];
        let metadata = ModuleMetadata::new(&resolve, world, encoding);
        let producers = wasm_metadata::Producers::from_wasm(&data[reader.original_position()..])?;
        Ok(Bindgen {
            resolve,
            world,
            metadata,
            producers,
        })
    }

    /// Merges another `BindgenMetadata` into this one.
    ///
    /// This operation is intended to be akin to "merging worlds" when the
    /// abstraction level for that is what we're working at here. For now the
    /// merge operation only succeeds if the two metadata descriptions are
    /// entirely disjoint.
    ///
    /// Note that at this time there's no support for changing string encodings
    /// between metadata.
    pub fn merge(&mut self, other: Bindgen) -> Result<()> {
        let Bindgen {
            resolve,
            world,
            metadata:
                ModuleMetadata {
                    import_encodings,
                    export_encodings,
                },
            producers,
        } = other;

        let world = self.resolve.merge(resolve).worlds[world.index()];
        self.resolve
            .merge_worlds(world, self.world)
            .context("failed to merge worlds from two documents")?;

        for (name, encoding) in export_encodings {
            let prev = self
                .metadata
                .export_encodings
                .insert(name.clone(), encoding);
            if let Some(prev) = prev {
                if prev != encoding {
                    bail!("conflicting string encodings specified for export `{name}`");
                }
            }
        }
        for ((module, name), encoding) in import_encodings {
            let prev = self
                .metadata
                .import_encodings
                .insert((module.clone(), name.clone()), encoding);
            if let Some(prev) = prev {
                if prev != encoding {
                    bail!("conflicting string encodings specified for import `{module}::{name}`");
                }
            }
        }
        if let Some(producers) = producers {
            if let Some(mine) = &mut self.producers {
                mine.merge(&producers);
            } else {
                self.producers = Some(producers);
            }
        }

        Ok(())
    }
}

impl ModuleMetadata {
    /// Creates a new `ModuleMetadata` instance holding the given set of
    /// interfaces which are expected to all use the `encoding` specified.
    pub fn new(resolve: &Resolve, world: WorldId, encoding: StringEncoding) -> ModuleMetadata {
        let mut ret = ModuleMetadata::default();

        let world = &resolve.worlds[world];
        for (name, item) in world.imports.iter() {
            match item {
                WorldItem::Function(_) => {
                    let prev = ret
                        .import_encodings
                        .insert((BARE_FUNC_MODULE_NAME.to_string(), name.clone()), encoding);
                    assert!(prev.is_none());
                }
                WorldItem::Interface(i) => {
                    for (func, _) in resolve.interfaces[*i].functions.iter() {
                        let prev = ret
                            .import_encodings
                            .insert((name.clone(), func.clone()), encoding);
                        assert!(prev.is_none());
                    }
                }
                WorldItem::Type(_) => {}
            }
        }

        for (name, item) in world.exports.iter() {
            match item {
                WorldItem::Function(func) => {
                    let name = func.core_export_name(None).into_owned();
                    let prev = ret.export_encodings.insert(name, encoding);
                    assert!(prev.is_none());
                }
                WorldItem::Interface(i) => {
                    for (_, func) in resolve.interfaces[*i].functions.iter() {
                        let name = func.core_export_name(Some(name)).into_owned();
                        let prev = ret.export_encodings.insert(name, encoding);
                        assert!(prev.is_none());
                    }
                }
                WorldItem::Type(_) => {}
            }
        }

        ret
    }
}
