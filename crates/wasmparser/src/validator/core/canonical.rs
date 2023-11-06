//! Canonicalization of types.
//!
//! The unit of canonicalization is a recursion group. Having "unnecessary"
//! types in a recursion group can "break" canonicalization of other types
//! within that same recursion group, as can reordering types within a recursion
//! group.
//!
//! It is an invariant that all types defined before the recursion group we are
//! currently canonicalizing have already been canonicalized themselves.
//!
//! Canonicalizing a recursion group then proceeds as follows:
//!
//! * First we walk each of its `SubType` elements and put their type references
//!   (i.e. their `PackedIndex`es) into canonical form. Canonicalizing a
//!   `PackedIndex` means switching it from indexing into the Wasm module's
//!   types space into either
//!
//!   1. Referencing an already-canonicalized type, for types outside of this
//!      recursion group. Because inter-group type references can only go
//!      towards types defined before this recursion group, we know the type is
//!      already canonicalized and we have a `CoreTypeId` for each of those
//!      types. This updates the `PackedIndex` into a `CoreTypeId`.
//!
//!   2. Indexing into the current recursion group, for intra-group type
//!      references.
//!
//!   Note that (2) has the effect of making the "same" structure of mutual type
//!   recursion look identical across recursion groups:
//!
//!   ```wat
//!   ;; Before
//!   (rec (struct (field (module-type 1))) (struct (field (module-type 0))))
//!   (rec (struct (field (module-type 3))) (struct (field (module-type 2))))
//!
//!   ;; After
//!   (rec (struct (field (rec-group-type 1))) (struct (field (rec-group-type 0))))
//!   (rec (struct (field (rec-group-type 1))) (struct (field (rec-group-type 0))))
//!   ```
//!
//! * Now that the recursion group's elements are in canonical form, we can
//!   "simply" hash cons whole rec groups at a time. The `TypesList` morally
//!   maintains a hash map from `Vec<SubType>` to `RecGroupId` and we can do
//!   get-or-create operations on it. I say "morally" because we don't actually
//!   duplicate the `Vec<SubType>` key in that hash map since those elements are
//!   already stored in the `TypeList`'s internal `SnapshotList<CoreType>`. This
//!   means we need to do some low-level hash table fiddling with the
//!   `hashbrown` crate.
//!
//! And that's it! That is the whole canonicalization algorithm.
//!
//! Some more random things to note:
//!
//! * Because we essentially already have to do the check to canonicalize, and
//!   to avoid additional passes over the types, the canonicalization pass also
//!   checks that type references are in bounds. These are the only errors that
//!   can be returned from canonicalization.
//!
//! * Canonicalizing requires the `Module` to translate type indices to
//!   actual `CoreTypeId`s.
//!
//! * It is important that *after* we have canonicalized all types, we don't
//!   need the `Module` anymore. This makes sure that we can, for example,
//!   intern all types from the same store into the same `TypeList`. Which in
//!   turn lets us type check function imports of a same-store instance's
//!   exported functions and we don't need to translate from one module's
//!   canonical representation to another module's canonical representation or
//!   perform additional expensive checks to see if the types match or not
//!   (since the whole point of canonicalization is to avoid that!).

use super::{Module, RecGroupId, TypeAlloc};
use crate::{
    ArrayType, CompositeType, FieldType, FuncType, HeapType, PackedIndex, RecGroup, RefType,
    Result, StorageType, StructType, SubType, ValType, WasmFeatures,
};

/// Canonicalize the rec group and return its id and whether it is a new group
/// (we added its types to the `TypeAlloc`) or not (we deduplicated it with an
/// existing canonical rec group).
pub(crate) fn canonicalize_and_intern_rec_group(
    features: &WasmFeatures,
    types: &mut TypeAlloc,
    module: &Module,
    mut rec_group: RecGroup,
    offset: usize,
) -> Result<(bool, RecGroupId)> {
    TypeCanonicalizer::new(module, offset)
        .with_features(features)
        .canonicalize_rec_group(&mut rec_group)?;
    types.intern_canonical_rec_group(rec_group)
}

pub(crate) struct TypeCanonicalizer<'a> {
    module: &'a Module,
    features: Option<&'a WasmFeatures>,
    rec_group_start: u32,
    rec_group_len: u32,
    offset: usize,
}

impl<'a> TypeCanonicalizer<'a> {
    pub fn new(module: &'a Module, offset: usize) -> Self {
        // These defaults will work for when we are canonicalizing types from
        // outside of a rec group definition, forcing all `PackedIndex`es to be
        // canonicalized to `CoreTypeId`s.
        let rec_group_start = u32::MAX;
        let rec_group_len = 0;

        Self {
            module,
            features: None,
            rec_group_start,
            rec_group_len,
            offset,
        }
    }

    pub fn with_features(&mut self, features: &'a WasmFeatures) -> &mut Self {
        debug_assert!(self.features.is_none());
        self.features = Some(features);
        self
    }

    fn allow_gc(&self) -> bool {
        self.features.map_or(true, |f| f.gc)
    }

    fn canonicalize_rec_group(&mut self, rec_group: &mut RecGroup) -> Result<()> {
        // Re-initialize these fields so that we properly canonicalize
        // intra-rec-group type references into indices into the rec group
        // rather than as `CoreTypeId`s.
        self.rec_group_start = u32::try_from(self.module.types.len()).unwrap();
        self.rec_group_len = u32::try_from(rec_group.types().len()).unwrap();

        for (rec_group_index, ty) in rec_group.types_mut().iter_mut().enumerate() {
            let rec_group_index = u32::try_from(rec_group_index).unwrap();
            let type_index = self.rec_group_start + rec_group_index;
            self.canonicalize_sub_type(ty, type_index)?;
        }

        Ok(())
    }

    fn canonicalize_type_index(&self, ty: &mut PackedIndex) -> Result<()> {
        let index = match ty.as_module_index() {
            None => return Ok(()),
            Some(i) => i,
        };

        if index < self.rec_group_start {
            let id = self.module.type_id_at(index, self.offset)?;
            if let Some(id) = PackedIndex::from_id(id) {
                *ty = id;
                return Ok(());
            } else {
                bail!(
                    self.offset,
                    "implementation limit: too many types in `TypeList`"
                )
            }
        }

        // When GC is not enabled the `rec_group_len == 1` so any rec group
        // local type references will be direct self references. But any kind of
        // type recursion, including self references, is not allowed in the
        // typed function references proposal, only the GC proposal.
        debug_assert!(self.allow_gc() || self.rec_group_len == 1);
        let local = index - self.rec_group_start;
        if self.allow_gc() && local < self.rec_group_len {
            if let Some(id) = PackedIndex::from_rec_group_index(local) {
                *ty = id;
                return Ok(());
            } else {
                bail!(
                    self.offset,
                    "implementation limit: too many types in a recursion group"
                )
            }
        }

        bail!(
            self.offset,
            "unknown type {index}: type index out of bounds"
        )
    }

    fn canonicalize_sub_type(&self, ty: &mut SubType, index: u32) -> Result<()> {
        if let Some(sup) = ty.supertype_idx.as_mut() {
            if sup.as_module_index().map_or(false, |i| i >= index) {
                bail!(self.offset, "supertypes must be defined before subtypes");
            }

            self.canonicalize_type_index(sup)?;
        }

        self.canonicalize_composite_type(&mut ty.composite_type)
    }

    fn canonicalize_composite_type(&self, ty: &mut CompositeType) -> Result<()> {
        match ty {
            CompositeType::Func(f) => self.canonicalize_func_type(f),
            CompositeType::Array(a) => self.canonicalize_array_type(a),
            CompositeType::Struct(s) => self.canonicalize_struct_type(s),
        }
    }

    fn canonicalize_func_type(&self, ty: &mut FuncType) -> Result<()> {
        for ty in ty.params_mut() {
            self.canonicalize_val_type(ty)?;
        }
        for ty in ty.results_mut() {
            self.canonicalize_val_type(ty)?;
        }
        Ok(())
    }

    fn canonicalize_array_type(&self, ty: &mut ArrayType) -> Result<()> {
        self.canonicalize_field_type(&mut ty.0)
    }

    fn canonicalize_struct_type(&self, ty: &mut StructType) -> Result<()> {
        for ty in ty.fields.iter_mut() {
            self.canonicalize_field_type(ty)?;
        }
        Ok(())
    }

    fn canonicalize_field_type(&self, ty: &mut FieldType) -> Result<()> {
        self.canonicalize_storage_type(&mut ty.element_type)
    }

    fn canonicalize_storage_type(&self, ty: &mut StorageType) -> Result<()> {
        match ty {
            StorageType::I8 | StorageType::I16 => Ok(()),
            StorageType::Val(ty) => self.canonicalize_val_type(ty),
        }
    }

    pub fn canonicalize_val_type(&self, ty: &mut ValType) -> Result<()> {
        match ty {
            ValType::I32 | ValType::I64 | ValType::F32 | ValType::F64 | ValType::V128 => Ok(()),
            ValType::Ref(ty) => self.canonicalize_ref_type(ty),
        }
    }

    fn canonicalize_ref_type(&self, ty: &mut RefType) -> Result<()> {
        match ty.heap_type() {
            HeapType::Concrete(unpacked_index) => {
                let mut packed_index = unpacked_index
                    .pack()
                    .expect("it was just packed in the `RefType` so we know it fits");
                self.canonicalize_type_index(&mut packed_index)?;
                *ty = RefType::concrete(ty.is_nullable(), packed_index);
                Ok(())
            }
            HeapType::Func
            | HeapType::Extern
            | HeapType::Any
            | HeapType::None
            | HeapType::NoExtern
            | HeapType::NoFunc
            | HeapType::Eq
            | HeapType::Struct
            | HeapType::Array
            | HeapType::I31 => Ok(()),
        }
    }
}
