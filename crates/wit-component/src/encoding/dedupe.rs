//! Routines to rewrite a wasm core module to remove duplicate imports, which
//! are illegal in components
//!
//! Thus far, this supports only duplicatively named *functions*, because our
//! goal is to make `wasm-tools component new` work, and the only imports that
//! supports are functions from adapters.

use anyhow::{anyhow, Error};
use std::{
    borrow::Cow,
    collections::hash_map::{Entry, HashMap},
    fmt::{self, Display, Formatter},
};
use wasm_encoder::{
    self,
    reencode::{self, utils::parse_custom_section, Reencode},
};
use wasmparser::{self, BinaryReaderError, Import, KnownCustom, Parser, TypeRef};

/// The module/name pair of a wasm import
#[derive(Hash, Eq, PartialEq, Debug)]
struct ImportPath {
    module: String,
    field: String,
}

/// Info which lets us quickly replace references to duplicate-imported
/// functions with refs to the canonical ones once the duplicates have been
/// removed
///
/// Default represents a state as if the import section were entirely absent
/// from the module.
#[derive(Default)]
struct Remappings {
    /// Map of old func indices to new
    indices: Vec<u32>,
    /// Number of duplicate imports removed
    num_dupes: u32,
    /// Which imports indices were duplicates
    dupes: Vec<bool>,
}

impl Remappings {
    /// Works out the mappings of a core module's func idxs to new ones that
    /// take into account the compacting-out of duplicate imports. The indices
    /// can then be looked up using `new_index_for()`.
    ///
    /// Calling this twice replaces any previous state iff it returns `Ok`.
    fn imports<'a, T>(&'a mut self, import_section: T) -> Result<(), BinaryReaderError>
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
                dupes.push(false);
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
    /// references to it redirected to a preceding one.
    fn is_duplicate(&self, import_idx: u32) -> bool {
        match self.dupes.get(import_idx as usize) {
            Some(b) => *b,
            None => false,
        }
    }
}

/// No duplicate imports were found, so I have nothing to do.
struct NoDuplicatesFound;

impl Display for NoDuplicatesFound {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "no duplicate functions were found, so there was nothing to do"
        )
    }
}

#[derive(Default)]
struct DedupingReencoder {
    remappings: Remappings,
}

type DeduperError = reencode::Error<NoDuplicatesFound>;

impl Reencode for DedupingReencoder {
    type Error = NoDuplicatesFound;

    // Fortunately, the import section comes before any section we need to
    // modify. Thus, we don't have to decode the wasm twice.
    fn parse_import_section(
        &mut self,
        imports: &mut wasm_encoder::ImportSection,
        section: wasmparser::ImportSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        self.remappings.imports(section.clone())?;

        // If no duplicates, take the fast path out.
        if self.remappings.is_empty() {
            return Err(DeduperError::UserError(NoDuplicatesFound));
        }

        for (idx, import) in section.into_iter().enumerate() {
            if !self.remappings.is_duplicate(idx as u32) {
                self.parse_import(imports, import?)?;
            }
        }
        Ok(())
    }

    fn function_index(&mut self, func_idx: u32) -> Result<u32, reencode::Error<Self::Error>> {
        Ok(self.remappings.new_index_for(func_idx))
    }

    // Strip all but known-safe custom sections.
    //
    // Because custom sections are opaque to us, they may contain information
    // (like debugging info) which depends on byte offsets we've invalidated by
    // removing imports or on function indices that we didn't patch up. To be
    // conservative, we strip all such sections except those we know are okay.
    // At the moment, we retain producers and target_features sections, as well
    // as name sections (which Reencode does patch up).
    fn parse_custom_section(
        &mut self,
        module: &mut wasm_encoder::Module,
        section: wasmparser::CustomSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        match section.as_known() {
            KnownCustom::Name(_) | KnownCustom::Producers(_) => {
                // Keep this section verbatim:
                parse_custom_section(self, module, section)
            }
            KnownCustom::Unknown if section.name() == "target_features" => {
                parse_custom_section(self, module, section)
            }
            // Strip others:
            _ => Ok(()),
        }
    }
}

/// Given a core wasm module that may contain duplicate function imports
/// (repeats of module/name pairs), returns an equivalent one without the
/// duplicates, rewriting references to those functions elsewhere in the module
/// to compensate.
pub fn dedupe_imports(module: &[u8]) -> Result<Cow<[u8]>, Error> {
    let mut new_module = wasm_encoder::Module::new();
    let result =
        DedupingReencoder::default().parse_core_module(&mut new_module, Parser::new(0), module);
    match result {
        // Fast path: return module verbatim
        Err(DeduperError::UserError(NoDuplicatesFound)) => Ok(Cow::Borrowed(module)),
        // Rewrite module
        Ok(_) => Ok(Cow::Owned(new_module.finish())),
        Err(err) => Err(anyhow!("reencoding failed: {}", err)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use wasmparser::{GlobalType, ValType};

    #[test]
    fn remappings_empty_state() -> Result<(), BinaryReaderError> {
        let mut remappings = Remappings::default();
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
        let imports = vec![func("A", "a"), func("B", "b"), func("C", "c")];
        remappings.imports(imports)?;
        assert!(
            remappings.is_empty(),
            "remappings should show as empty when there are function imports but no duplicates."
        );
        Ok(())
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
    fn remappings_populated_state() -> Result<(), BinaryReaderError> {
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
        let mut remappings = Remappings::default();
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

    /// Show that non-function imports don't throw off the `is_duplicate()`
    /// positions of later imports.
    #[test]
    fn remappings_non_function_imports() -> Result<(), BinaryReaderError> {
        let imports = vec![
            func("A", "a"),
            Ok(Import {
                module: "non",
                name: "function",
                ty: TypeRef::Global(GlobalType {
                    content_type: ValType::I32,
                    mutable: false,
                    shared: false,
                }),
            }),
            func("A", "a"),
        ];
        let mut remappings = Remappings::default();
        remappings.imports(imports)?;
        assert!(!remappings.is_duplicate(1));
        assert!(remappings.is_duplicate(2));
        Ok(())
    }
}
