//! Implementation of matching (subtyping) for core Wasm types.

use crate::{
    types::{CoreTypeId, RecGroupId, TypeList},
    ArrayType, BinaryReaderError, CompositeType, FieldType, FuncType, HeapType, PackedIndex,
    RefType, Result, StorageType, StructType, SubType, UnpackedIndex, ValType,
};

/// Wasm type matching.
pub trait Matches {
    /// Does `a` match `b`?
    ///
    /// Both `a` and `b` must be canonicalized already.
    ///
    /// Returns `Err` when we require `RecGroupId` context but are missing it.
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool>;
}

/// A `T` with its containing `RecGroupId` (when available).
///
/// The `RecGroupId`, when present, can be used to resolve canonicalized type
/// references that are indices into the local rec group.
#[derive(Debug, Copy, Clone)]
pub(crate) struct WithRecGroup<T> {
    inner: T,
    rec_group_id: Option<RecGroupId>,
}

impl<T: std::fmt::Debug> WithRecGroup<T> {
    #[inline]
    fn unwrap_rec_group(x: Self) -> RecGroupId {
        match x.rec_group_id {
            Some(id) => id,
            _ => panic!("WithRecGroup::rec_group({x:?}): missing rec group context"),
        }
    }
}

impl<T> std::ops::Deref for WithRecGroup<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> std::ops::DerefMut for WithRecGroup<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T> WithRecGroup<T> {
    /// Construct a new `WithRecGroup` that does not have `RecGroupId` context.
    ///
    /// This means that resolving indices into the local rec group will fail,
    /// but this is fine for some usages where we know that we are only dealing
    /// with type references that have been canonicalized to `CoreTypeId`s
    /// rather than rec group local indices (e.g. any reference to the type from
    /// outside of a rec group definition).
    pub(crate) fn without_rec_group(inner: T) -> Self {
        WithRecGroup {
            inner,
            rec_group_id: None,
        }
    }
}

impl WithRecGroup<CoreTypeId> {
    /// Construct a new `WithRecGroup<CoreTypeId>` by looking up the
    /// `CoreTypeId`'s rec group id in the `TypeList`.
    pub(crate) fn new(types: &TypeList, id: CoreTypeId) -> Self {
        let rec_group_id = Some(types.rec_group_id_of(id));
        WithRecGroup {
            inner: id,
            rec_group_id,
        }
    }
}

impl<T> WithRecGroup<T> {
    /// Project into a field of the inner value, while maintaining the
    /// `RecGroupId` context.
    pub(crate) fn map<U>(x: Self, f: impl FnOnce(T) -> U) -> WithRecGroup<U> {
        WithRecGroup {
            inner: f(x.inner),
            rec_group_id: x.rec_group_id,
        }
    }
}

fn core_type_id(
    types: &TypeList,
    index: WithRecGroup<PackedIndex>,
    offset: usize,
) -> Result<CoreTypeId> {
    // NB: if we already have `CoreTypeId`s, just use those directly. This
    // avoids unwrapping the `WithRecGroup`'s `RecGroupId`, which may not be
    // available. These two cases happen together frequently, and we want to
    // support them: whenever we are referencing an already-canonicalized
    // type from outside its rec group (e.g. from a global type's inner
    // value type) then we will be given a `CoreTypeId` but not a
    // `RecGroupId`. With our internal access to the `TypeList`, we can
    // always recover the `RecGroupId` later (see `WithRecGroup::new`).
    if let Some(id) = index.as_core_type_id() {
        Ok(id)
    } else {
        let group = WithRecGroup::unwrap_rec_group(index);
        types.at_canonicalized_packed_index(group, *index, offset)
    }
}

impl Matches for WithRecGroup<UnpackedIndex> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        let a_packed = a.pack().ok_or_else(|| {
            BinaryReaderError::new("implementation limit: type index too large", offset)
        })?;
        let b_packed = b.pack().ok_or_else(|| {
            BinaryReaderError::new("implementation limit: type index too large", offset)
        })?;
        types.matches(
            WithRecGroup::map(a, |_| a_packed),
            WithRecGroup::map(b, |_| b_packed),
            offset,
        )
    }
}

impl Matches for WithRecGroup<PackedIndex> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        // Matching relies on canonicalization to avoid exponential run time.
        debug_assert!(a.is_canonical());
        debug_assert!(b.is_canonical());

        if *a == *b {
            return Ok(true);
        }

        types.matches(
            core_type_id(types, a, offset)?,
            core_type_id(types, b, offset)?,
            offset,
        )
    }
}

impl Matches for CoreTypeId {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        if a == b {
            return Ok(true);
        }

        let a = WithRecGroup::new(types, a);
        let a = WithRecGroup::map(a, |a| &types[a]);

        let b = WithRecGroup::new(types, b);
        let b = WithRecGroup::map(b, |b| &types[b]);

        types.matches(a, b, offset)
    }
}

impl<'a> Matches for WithRecGroup<&'a SubType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        types.matches(
            WithRecGroup::map(a, |a| &a.composite_type),
            WithRecGroup::map(b, |b| &b.composite_type),
            offset,
        )
    }
}

impl<'a> Matches for WithRecGroup<&'a CompositeType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        match (&*a, &*b) {
            (CompositeType::Func(fa), CompositeType::Func(fb)) => types.matches(
                WithRecGroup::map(a, |_| fa),
                WithRecGroup::map(b, |_| fb),
                offset,
            ),
            (CompositeType::Func(_), _) => Ok(false),

            (CompositeType::Array(aa), CompositeType::Array(ab)) => types.matches(
                WithRecGroup::map(a, |_| *aa),
                WithRecGroup::map(b, |_| *ab),
                offset,
            ),
            (CompositeType::Array(_), _) => Ok(false),

            (CompositeType::Struct(sa), CompositeType::Struct(sb)) => types.matches(
                WithRecGroup::map(a, |_| sa),
                WithRecGroup::map(b, |_| sb),
                offset,
            ),
            (CompositeType::Struct(_), _) => Ok(false),
        }
    }
}

impl<'a> Matches for WithRecGroup<&'a FuncType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        if a.params().len() != b.params().len() || a.results().len() != b.results().len() {
            return Ok(false);
        }

        // A quick recap of covariance, contravariance, and how it applies to
        // subtyping function types, because I (and almost everyone else, it
        // seems) always need a reminder:
        //
        // *Covariance* is when `a <: b` and `a' <: b'`. That is, the subtyping
        // checks on the nested things (`a'` and `b'`) goes the same way as the
        // outer things (`a` and `b`).
        //
        // *Contravariance* is when `a <: b` and `b' <: a'`. That is, the
        // subtyping on the nested things is reversed compared to the outer
        // things.
        //
        // Now, when we are checking subtyping for function types, we have the
        // following variance:
        //
        // * Parameters are contravariant: `(a -> c) <: (b -> c)` when `b <: a`.
        //
        //   For example, we can substitute a `Cat -> Cat` function with a
        //   `Animal -> Cat` function because `Cat <: Animal` and so all
        //   arguments that could be given to the function are still valid.
        //
        //   We can't do the opposite and replace an `Animal -> Cat` function
        //   with a `Cat -> Cat` function. What would the `Cat -> Cat` function
        //   do when given a `Dog`? It is unsound.
        //
        // * Results are covariant: `(a -> b) <: (a -> c)` when `b <: c`.
        //
        //   For example, we can substitute a `Cat -> Animal` function with a
        //   `Cat -> Cat` function because callers expect to be returned an
        //   `Animal` and all `Cat`s are `Animal`s. (Also: all `Cat`s are
        //   `Beautiful`!)
        //
        //   We cannot do the opposite and substitute a `Cat -> Cat` function
        //   with a `Cat -> Animal` function, since callers expect a `Cat` but
        //   the new function could return a `Pig`.
        //
        // As always, Wikipedia is also helpful:
        // https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)

        let params_match =
            a.params()
                .iter()
                .zip(b.params())
                .try_fold(true, |matches, (pa, pb)| {
                    // Parameters are contravariant.
                    Ok(matches
                        && types.matches(
                            WithRecGroup::map(b, |_| *pb),
                            WithRecGroup::map(a, |_| *pa),
                            offset,
                        )?)
                })?;
        if !params_match {
            return Ok(false);
        }

        let results_match =
            a.results()
                .iter()
                .zip(b.results())
                .try_fold(true, |matches, (ra, rb)| {
                    // Results are covariant.
                    Ok(matches
                        && types.matches(
                            WithRecGroup::map(a, |_| *ra),
                            WithRecGroup::map(b, |_| *rb),
                            offset,
                        )?)
                })?;
        Ok(results_match)
    }
}

impl Matches for WithRecGroup<ArrayType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        types.matches(
            WithRecGroup::map(a, |a| a.0),
            WithRecGroup::map(b, |b| b.0),
            offset,
        )
    }
}

impl<'a> Matches for WithRecGroup<&'a StructType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        // Note: Struct types support width and depth subtyping.
        Ok(a.fields.len() >= b.fields.len()
            && a.fields
                .iter()
                .zip(b.fields.iter())
                .try_fold(true, |matches, (fa, fb)| {
                    Ok(matches
                        && types.matches(
                            WithRecGroup::map(a, |_| *fa),
                            WithRecGroup::map(b, |_| *fb),
                            offset,
                        )?)
                })?)
    }
}

impl Matches for WithRecGroup<FieldType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        Ok((b.mutable || !a.mutable)
            && types.matches(
                WithRecGroup::map(a, |a| a.element_type),
                WithRecGroup::map(b, |b| b.element_type),
                offset,
            )?)
    }
}

impl Matches for WithRecGroup<StorageType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        use StorageType as ST;
        match (*a, *b) {
            (ST::I8, ST::I8) | (ST::I16, ST::I16) => Ok(true),
            (ST::I8 | ST::I16, _) => Ok(false),

            (ST::Val(va), ST::Val(vb)) => types.matches(
                WithRecGroup::map(a, |_| va),
                WithRecGroup::map(b, |_| vb),
                offset,
            ),
            (ST::Val(_), _) => Ok(false),
        }
    }
}

impl Matches for WithRecGroup<ValType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        match (*a, *b) {
            (ValType::Ref(ra), ValType::Ref(rb)) => types.matches(
                WithRecGroup::map(a, |_| ra),
                WithRecGroup::map(b, |_| rb),
                offset,
            ),
            (ValType::Ref(_), _) => Ok(false),

            (ValType::I32 | ValType::I64 | ValType::F32 | ValType::F64 | ValType::V128, _) => {
                Ok(*a == *b)
            }
        }
    }
}

impl Matches for WithRecGroup<RefType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        if *a == *b {
            return Ok(true);
        }
        if a.is_nullable() && !b.is_nullable() {
            return Ok(false);
        }
        types.matches(
            WithRecGroup::map(a, |a| a.heap_type()),
            WithRecGroup::map(b, |b| b.heap_type()),
            offset,
        )
    }
}

impl Matches for WithRecGroup<HeapType> {
    fn matches(types: &TypeList, a: Self, b: Self, offset: usize) -> Result<bool> {
        let subtype = |x: Self, index: UnpackedIndex| -> Result<&SubType> {
            let index = index.pack().ok_or_else(|| {
                BinaryReaderError::new("implementation limit: index too large", offset)
            })?;
            let id = core_type_id(types, WithRecGroup::map(x, |_| index), offset)?;
            Ok(&types[id])
        };

        use HeapType as HT;
        match (*a, *b) {
            (a, b) if a == b => Ok(true),

            (HT::Eq | HT::I31 | HT::Struct | HT::Array | HT::None, HT::Any) => Ok(true),
            (HT::I31 | HT::Struct | HT::Array | HT::None, HT::Eq) => Ok(true),
            (HT::NoExtern, HT::Extern) => Ok(true),
            (HT::NoFunc, HT::Func) => Ok(true),
            (HT::None, HT::I31 | HT::Array | HT::Struct) => Ok(true),

            (HT::Concrete(ia), HT::Eq | HT::Any) => Ok(matches!(
                subtype(a, ia)?.composite_type,
                CompositeType::Array(_) | CompositeType::Struct(_)
            )),

            (HT::Concrete(ia), HT::Struct) => Ok(matches!(
                subtype(a, ia)?.composite_type,
                CompositeType::Struct(_)
            )),

            (HT::Concrete(ia), HT::Array) => Ok(matches!(
                subtype(a, ia)?.composite_type,
                CompositeType::Array(_)
            )),

            (HT::Concrete(ia), HT::Func) => Ok(matches!(
                subtype(a, ia)?.composite_type,
                CompositeType::Func(_)
            )),

            (HT::Concrete(ia), HT::Concrete(ib)) => types.matches(
                WithRecGroup::map(a, |_| ia),
                WithRecGroup::map(b, |_| ib),
                offset,
            ),

            (HT::None, HT::Concrete(ib)) => Ok(matches!(
                subtype(b, ib)?.composite_type,
                CompositeType::Array(_) | CompositeType::Struct(_)
            )),

            (HT::NoFunc, HT::Concrete(ib)) => Ok(matches!(
                subtype(b, ib)?.composite_type,
                CompositeType::Func(_)
            )),

            // Nothing else matches. (Avoid full wildcard matches so that
            // adding/modifying variants is easier in the future.)
            (HT::Concrete(_), _)
            | (HT::Func, _)
            | (HT::Extern, _)
            | (HT::Any, _)
            | (HT::None, _)
            | (HT::NoExtern, _)
            | (HT::NoFunc, _)
            | (HT::Eq, _)
            | (HT::Struct, _)
            | (HT::Array, _)
            | (HT::I31, _) => Ok(false),
        }
    }
}
