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
//! The custom section here contains `World`, the interpretation of a "world"
//! of a component, along with how strings are encoded for all the specified
//! interfaces. Currently the encoding is:
//!
//! * First, a version byte (`CURRENT_VERSION`). This is intended to detect
//!   mismatches between different versions of the binding generator and
//!   `wit-component` which may or may not become a problem over time.
//!
//! * Next a string encoding byte.
//!
//! * Afterwards a "types only" component encoding of a `World`
//!   package through the `ComponentEncoder::types_only` configuration.

use crate::{decode_world, ComponentEncoder, StringEncoding};
use anyhow::{bail, Context, Result};
use indexmap::IndexMap;
use wasm_encoder::Encode;
use wasmparser::BinaryReader;
use wit_parser::{Resolve, World, WorldId};

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
}

impl Default for Bindgen {
    fn default() -> Bindgen {
        let mut resolve = Resolve::default();
        let world = resolve.worlds.alloc(World::default());
        Bindgen {
            resolve,
            world,
            metadata: ModuleMetadata::default(),
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
pub fn encode(resolve: &Resolve, world: WorldId, encoding: StringEncoding) -> Vec<u8> {
    let component = ComponentEncoder::default()
        .types_only(true)
        .document(doc.clone(), encoding)
        .unwrap()
        .encode()
        .unwrap();

    let mut ret = Vec::new();
    ret.push(CURRENT_VERSION);
    ret.push(match encoding {
        StringEncoding::UTF8 => 0x00,
        StringEncoding::UTF16 => 0x01,
        StringEncoding::CompactUTF16 => 0x02,
    });
    doc.worlds[world].name.encode(&mut ret);
    ret.extend(component);
    ret
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
        let name = reader.read_string()?;

        let (doc, world) = decode_world(name, &data[reader.original_position()..])?;
        let metadata = ModuleMetadata::new(&doc, world, encoding);
        Ok(Bindgen {
            doc,
            world,
            metadata,
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
            doc,
            world,
            metadata:
                ModuleMetadata {
                    import_encodings,
                    export_encodings,
                },
        } = other;

        let world = self.doc.merge(doc).world_map[world.index()];
        self.doc
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

        Ok(())
    }
}

impl ModuleMetadata {
    /// Creates a new `ModuleMetadata` instance holding the given set of
    /// interfaces which are expected to all use the `encoding` specified.
    pub fn new(doc: &Document, world: WorldId, encoding: StringEncoding) -> ModuleMetadata {
        let mut ret = ModuleMetadata::default();

        if let Some(iface) = doc.worlds[world].default {
            for func in doc.interfaces[iface].functions.iter() {
                let name = func.core_export_name(None);
                let prev = ret.export_encodings.insert(name.to_string(), encoding);
                assert!(prev.is_none());
            }
        }

        for (name, import) in doc.worlds[world].imports.iter() {
            for func in doc.interfaces[*import].functions.iter() {
                let key = (name.clone(), func.name.clone());
                let prev = ret.import_encodings.insert(key, encoding);
                assert!(prev.is_none());
            }
        }
        for (name, export) in doc.worlds[world].exports.iter() {
            for func in doc.interfaces[*export].functions.iter() {
                let name = func.core_export_name(Some(name));
                let prev = ret.export_encodings.insert(name.to_string(), encoding);
                assert!(prev.is_none());
            }
        }
        ret
    }
}
