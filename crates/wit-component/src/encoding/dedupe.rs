//! Routines to rewrite a wasm core module to remove duplicate imports, which
//! are illegal in components
//!
//! Thus far, this supports only duplicatively named *functions*, because our
//! goal is to make `wasm-tools component new` work, and the only imports that
//! supports is functions from adapters.

use anyhow::{bail, Context, Result};
use std::collections::hash_map::{Entry, HashMap};
use wasm_encoder;
use wasmparser::{BinaryReaderError, Encoding, ExternalKind, Import, Parser, Payload, TypeRef};

/// The module/name pair of a wasm import
#[derive(Hash, Eq, PartialEq, Debug)]
struct ImportPath {
    module: String,
    field: String,
}

struct Remappings {
    /// Map of old func indices to new
    indices: Vec<u32>,
    /// Number of duplicate imports removed
    num_dupes: u32,
    /// Which imports indices were duplicates
    dupes: Vec<bool>,
}

impl Remappings {
    /// Return a fresh Remappings, initialized to a state as if the import
    /// section were entirely absent.
    pub fn new() -> Self {
        Self {
            indices: Vec::new(),
            num_dupes: 0,
            dupes: Vec::new(),
        }
    }

    /// Works out the mappings of a core module's func idxs to new ones that
    /// take into account the compacting-out of duplicate imports. The indices
    /// can then be looked up using `new_index_for()`.
    ///
    /// Calling this twice replaces any previous state.
    pub fn imports<'a, T>(&'a mut self, import_section: T) -> Result<()>
    where
        T: IntoIterator<Item = Result<Import<'a>, BinaryReaderError>>,
    {
        let mut num_dupes = 0;
        // A map of module/field pairs to the first import idx at which each
        // occurs
        let mut canonicals = HashMap::new();
        let mut indices = Vec::new();
        let mut dupes = Vec::new();

        // import_section.clone() lets us iterate over the section a 2nd time
        // without consuming the original iterator or copying the bytes.
        let mut func_idx = 0u32; // Imports are a "vec" of imports and thus bounded to u32.
        for import in import_section.into_iter() {
            let import = import?;
            let TypeRef::Func(_) = import.ty else {
                continue;
            };
            let path = ImportPath {
                module: import.module.to_string(),
                field: import.name.to_string(),
            };
            match canonicals.entry(path) {
                Entry::Occupied(slot) => {
                    indices.push(*slot.get());
                    num_dupes += 1;
                    dupes.push(true);
                }
                Entry::Vacant(slot) => {
                    indices.push(*slot.insert(func_idx - num_dupes));
                    dupes.push(false);
                }
            }
            func_idx += 1;
        }
        self.indices = indices;
        self.num_dupes = num_dupes;
        self.dupes = dupes;
        Ok(())
    }

    /// Given an original-to-the-module func index, returns the one that should
    /// be used once duplicate imports are removed.
    fn new_index_for(&self, idx: u32) -> u32 {
        match self.indices.get(idx as usize) {
            // Return a remapping using the index vector, if the idx is within
            // bounds of the imports.
            Some(new_idx) => *new_idx,
            // Otherwise, it's a local idx (as from the local function section),
            // and we need only subtract the number of duplicates removed. (The
            // spec says "The index space for functions, tables, memories and
            // globals includes respective imports declared in the same module.
            // The indices of these imports precede the indices of other
            // definitions in the same index space.")
            None => idx - self.num_dupes,
        }
    }

    /// Returns whether we contain no remappings to apply.
    fn is_empty(&self) -> bool {
        self.num_dupes == 0
    }

    /// Returns whether the import with the given index has been eliminated and
    /// redirected to a preceding one.
    fn is_duplicate(&self, import_idx: u32) -> bool {
        match self.dupes.get(import_idx as usize) {
            Some(b) => *b,
            None => false,
        }
    }
}

/// Given a core wasm module that may contain duplicate function imports
/// (repeats of module/name pairs), returns an equivalent one without the
/// duplicates, rewriting references to those functions elsewhere in the module
/// to compensate.
pub fn dedupe_imports(module: &[u8]) -> Result<Vec<u8>> {
    let mut new_module = wasm_encoder::Module::new();
    let mut remappings = Remappings::new();

    for payload in Parser::new(0).parse_all(module) {
        // Fortunately, the import section comes before any section we need to
        // modify. Thus, we don't have to decode the wasm twice.
        let payload = payload.context("decoding item in module")?;
        match payload {
            Payload::Version { encoding, .. } if encoding != Encoding::Module => {
                bail!("decoding a component is not supported")
            }
            Payload::ExportSection(exports) => {
                let mut new_section = wasm_encoder::ExportSection::new();
                for export in exports {
                    let export = export?;
                    let index = match export.kind {
                        ExternalKind::Func => remappings.new_index_for(export.index),
                        _ => export.index,
                    };
                    new_section.export(export.name, export.kind.into(), index);
                }

                new_module.section(&new_section);
                //info.section(SectionId::Export.into(), reader.range(), input_wasm);
            }
            Payload::ImportSection(imports) => {
                remappings.imports(imports.clone())?;
                if remappings.is_empty() {
                    // No duplicates; take the fast path.
                    return Ok(module.to_vec());
                }

                let mut new_section = wasm_encoder::ImportSection::new();
                for (idx, import) in imports.into_iter().enumerate() {
                    let import = import?;
                    if !remappings.is_duplicate(idx as u32) {
                        new_section.import(
                            import.module,
                            import.name,
                            wasm_encoder::reencode::utils::entity_type(
                                &mut wasm_encoder::reencode::RoundtripReencoder,
                                import.ty,
                            )?,
                        );
                    }
                }
                new_module.section(&new_section);
            }
            Payload::GlobalSection(_globals) => {}
            // TODO: Explicitly match each remaining variant so we don't get
            // quiet failures later on.
            // TODO: Must rewrite: imports, globals (initializer expressions
            // only), √ exports (funcs only), starts, elements (many cases),
            // codes, datas (offset expression only).
            // TODO: Leave in producers and "target features" custom sections,
            // and strip the rest out, because they could be debug info or
            // otherwise dependent on the byte offests we're changing.
            // https://calabro.io/dwarf/die
            _ => {
                if let Some((id, range)) = payload.as_section() {
                    new_module.section(&wasm_encoder::RawSection {
                        id,
                        data: &module[range],
                    });
                }
            }
        }
    }
    Ok(new_module.finish())
}

#[cfg(test)]
mod test {
    use super::*;
    use wasmparser::{GlobalType, ValType};

    #[test]
    fn remappings_empty_state() {
        let remappings = Remappings::new();
        assert!(remappings.is_empty());
        assert!(
            !remappings.is_duplicate(0),
            "func idxs should default to not being duplicates."
        );
        assert_eq!(
            remappings.new_index_for(7),
            7,
            "new_index_for() should return unchanged indices unless it knows a better mapping."
        );
    }

    /// Convenience to construct function imports for test data
    fn func<'a>(module: &'a str, name: &'a str) -> Result<Import<'a>, BinaryReaderError> {
        Ok(Import {
            module,
            name,
            ty: TypeRef::Func(0), // We don't care about this.
        })
    }

    #[test]
    fn remappings_populated_state() -> Result<()> {
        // We test contiguous and discontiguous duplicates, singletons, and
        // indices outside the mapping table (which represent local, unimported
        // functions).
        let imports = vec![
            func("A", "a"),
            func("A", "a"),
            // Include a non-function import to make sure those are skipped and
            // don't take up a slot in our newly constructed func index space.
            Ok(Import {
                module: "non",
                name: "function",
                ty: TypeRef::Global(GlobalType {
                    content_type: ValType::I32,
                    mutable: false,
                    shared: false,
                }),
            }),
            func("C", "c"),
            func("D", "d"),
            func("D", "d"),
            func("D", "d"),
            func("G", "g"),
            func("D", "d"),
            func("I", "i"),
        ];
        let mut remappings = Remappings::new();
        remappings.imports(imports)?;
        assert!(!remappings.is_empty());
        assert!(!remappings.is_duplicate(0));
        assert!(remappings.is_duplicate(1));
        assert_eq!(
            (0..=9) // Go one beyond the length of the input.
                .map(|idx| remappings.new_index_for(idx))
                .collect::<Vec<_>>(),
            vec![0, 0, 1, 2, 2, 2, 3, 2, 4, 5]
        );
        Ok(())
    }
}
