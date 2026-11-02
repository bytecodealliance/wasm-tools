use crate::{
    Function, Handle, Int, Resolve, Type, TypeDefKind, TypeId, TypeOwner, WorldId, WorldItem,
};
use alloc::vec::Vec;

/// A core WebAssembly signature with params and results.
#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct WasmSignature {
    /// The WebAssembly parameters of this function.
    pub params: Vec<WasmType>,

    /// The WebAssembly results of this function.
    pub results: Vec<WasmType>,

    /// Whether or not this signature is passing all of its parameters
    /// indirectly through a pointer within `params`.
    ///
    /// Note that `params` still reflects the true wasm parameters of this
    /// function, this is auxiliary information for code generators if
    /// necessary.
    pub indirect_params: bool,

    /// Whether or not this signature is using a return pointer to store the
    /// result of the function, which is reflected either in `params` or
    /// `results` depending on the context this function is used (e.g. an import
    /// or an export).
    pub retptr: bool,
}

/// Enumerates wasm types used by interface types when lowering/lifting.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WasmType {
    I32,
    I64,
    F32,
    F64,

    /// A pointer type. In core Wasm this typically lowers to either `i32` or
    /// `i64` depending on the index type of the exported linear memory,
    /// however bindings can use different source-level types to preserve
    /// provenance.
    ///
    /// Users that don't do anything special for pointers can treat this as
    /// `i32`.
    Pointer,

    /// A type for values which can be either pointers or 64-bit integers.
    /// This occurs in variants, when pointers and non-pointers are unified.
    ///
    /// Users that don't do anything special for pointers can treat this as
    /// `i64`.
    PointerOrI64,

    /// An array length type. In core Wasm this lowers to either `i32` or `i64`
    /// depending on the index type of the exported linear memory.
    ///
    /// Users that don't do anything special for pointers can treat this as
    /// `i32`.
    Length,
    // NOTE: we don't lower interface types to any other Wasm type,
    // e.g. externref, so we don't need to define them here.
}

fn join(a: WasmType, b: WasmType) -> WasmType {
    use WasmType::*;

    match (a, b) {
        (I32, I32)
        | (I64, I64)
        | (F32, F32)
        | (F64, F64)
        | (Pointer, Pointer)
        | (PointerOrI64, PointerOrI64)
        | (Length, Length) => a,

        (I32, F32) | (F32, I32) => I32,

        // A length is at least an `i32`, maybe more, so it wins over
        // 32-bit types.
        (Length, I32 | F32) => Length,
        (I32 | F32, Length) => Length,

        // A length might be an `i64`, but might not be, so if we have
        // 64-bit types, they win.
        (Length, I64 | F64) => I64,
        (I64 | F64, Length) => I64,

        // Pointers have provenance and are at least an `i32`, so they
        // win over 32-bit and length types.
        (Pointer, I32 | F32 | Length) => Pointer,
        (I32 | F32 | Length, Pointer) => Pointer,

        // If we need 64 bits and provenance, we need to use the special
        // `PointerOrI64`.
        (Pointer, I64 | F64) => PointerOrI64,
        (I64 | F64, Pointer) => PointerOrI64,

        // PointerOrI64 wins over everything.
        (PointerOrI64, _) => PointerOrI64,
        (_, PointerOrI64) => PointerOrI64,

        // Otherwise, `i64` wins.
        (_, I64 | F64) | (I64 | F64, _) => I64,
    }
}

impl From<Int> for WasmType {
    fn from(i: Int) -> WasmType {
        match i {
            Int::U8 | Int::U16 | Int::U32 => WasmType::I32,
            Int::U64 => WasmType::I64,
        }
    }
}

/// We use a different ABI for wasm importing functions exported by the host
/// than for wasm exporting functions imported by the host.
///
/// Note that this reflects the flavor of ABI we generate, and not necessarily
/// the way the resulting bindings will be used by end users. See the comments
/// on the `Direction` enum in gen-core for details.
///
/// The bindings ABI has a concept of a "guest" and a "host". There are two
/// variants of the ABI, one specialized for the "guest" importing and calling
/// a function defined and exported in the "host", and the other specialized for
/// the "host" importing and calling a function defined and exported in the "guest".
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum AbiVariant {
    /// The guest is importing and calling the function.
    GuestImport,
    /// The guest is defining and exporting the function.
    GuestExport,
    GuestImportAsync,
    GuestExportAsync,
    GuestExportAsyncStackful,
}

impl AbiVariant {
    pub fn is_async(&self) -> bool {
        match self {
            Self::GuestImport | Self::GuestExport => false,
            Self::GuestImportAsync | Self::GuestExportAsync | Self::GuestExportAsyncStackful => {
                true
            }
        }
    }
}

pub struct FlatTypes<'a> {
    types: &'a mut [WasmType],
    cur: usize,
    overflow: bool,
}

impl<'a> FlatTypes<'a> {
    pub fn new(types: &'a mut [WasmType]) -> FlatTypes<'a> {
        FlatTypes {
            types,
            cur: 0,
            overflow: false,
        }
    }

    pub fn push(&mut self, ty: WasmType) -> bool {
        match self.types.get_mut(self.cur) {
            Some(next) => {
                *next = ty;
                self.cur += 1;
                true
            }
            None => {
                self.overflow = true;
                false
            }
        }
    }

    pub fn to_vec(&self) -> Vec<WasmType> {
        self.types[..self.cur].to_vec()
    }
}

impl Resolve {
    pub const MAX_FLAT_PARAMS: usize = 16;
    pub const MAX_FLAT_ASYNC_PARAMS: usize = 4;
    pub const MAX_FLAT_RESULTS: usize = 1;

    /// Get the WebAssembly type signature for this interface function
    ///
    /// This function will convert the component-model-typed WIT `func` specified
    /// into its core wasm equivalent using the `variant` ABI specified. The
    /// `variant` can specify whether what's happening here, such as a WIT
    /// function being lowered with `canon lower` or a core function which
    /// should be lifted into the WIT function with `canon `lift`.
    ///
    /// The `world` is the world that `func` is imported into or exported from.
    /// This is used to handle `borrow<T>` arguments. The representation of a
    /// borrow in core wasm is either `WasmType::I32` for a generic handle or
    /// `WasmType::Pointer` for a resource owned by the `world` specified. The
    /// `world` indicates which ones should be `Pointer`.
    ///
    /// Note that correctly assigning types to `borrow<T>` requires that the
    /// [`Resolve::generate_nominal_type_ids`] method has been called.
    pub fn wasm_signature(
        &self,
        world: WorldId,
        variant: AbiVariant,
        func: &Function,
    ) -> WasmSignature {
        // Note that one extra parameter is allocated in case a return pointer
        // is needed down below for imports.
        let mut storage = [WasmType::I32; Self::MAX_FLAT_PARAMS + 1];
        let mut params = FlatTypes::new(&mut storage);
        let ok = self.push_flat_list(world, func.params.iter().map(|p| &p.ty), &mut params);
        assert_eq!(ok, !params.overflow);

        let max = match variant {
            AbiVariant::GuestImport
            | AbiVariant::GuestExport
            | AbiVariant::GuestExportAsync
            | AbiVariant::GuestExportAsyncStackful => Self::MAX_FLAT_PARAMS,
            AbiVariant::GuestImportAsync => Self::MAX_FLAT_ASYNC_PARAMS,
        };

        let indirect_params = !ok || params.cur > max;
        if indirect_params {
            params.types[0] = WasmType::Pointer;
            params.cur = 1;
        }

        let mut storage = [WasmType::I32; Self::MAX_FLAT_RESULTS];
        let mut results = FlatTypes::new(&mut storage);
        let mut retptr = false;
        match variant {
            AbiVariant::GuestImport | AbiVariant::GuestExport => {
                if let Some(ty) = &func.result {
                    self.push_flat(world, ty, &mut results);
                }
                retptr = results.overflow;

                // Rust/C don't support multi-value well right now, so if a
                // function would have multiple results then instead truncate
                // it. Imports take a return pointer to write into and exports
                // return a pointer they wrote into.
                if retptr {
                    results.cur = 0;
                    match variant {
                        AbiVariant::GuestImport => {
                            assert!(params.push(WasmType::Pointer));
                        }
                        AbiVariant::GuestExport => {
                            assert!(results.push(WasmType::Pointer));
                        }
                        _ => unreachable!(),
                    }
                }
            }
            AbiVariant::GuestImportAsync => {
                // If this function has a result, a pointer must be passed to
                // get filled in by the async runtime.
                if func.result.is_some() {
                    assert!(params.push(WasmType::Pointer));
                    retptr = true;
                }

                // The result of this function is a status code.
                assert!(results.push(WasmType::I32));
            }
            AbiVariant::GuestExportAsync => {
                // The result of this function is a status code. Note that the
                // function results are entirely ignored here as they aren't
                // part of the ABI and are handled in the `task.return`
                // intrinsic.
                assert!(results.push(WasmType::I32));
            }
            AbiVariant::GuestExportAsyncStackful => {
                // No status code, and like async exports no result handling.
            }
        }

        WasmSignature {
            params: params.to_vec(),
            indirect_params,
            results: results.to_vec(),
            retptr,
        }
    }

    /// Returns the flat representation of a `borrow` of the resource `id`.
    ///
    /// Resources defined in interfaces exported by a component are passed
    /// as their representation, which hardcoded as a pointer for now. All
    /// other resources are passed as an `i32` handle.
    fn flat_borrow(&self, world: WorldId, mut id: TypeId) -> WasmType {
        while let TypeDefKind::Type(Type::Id(next)) = self.types[id].kind {
            id = next;
        }
        let TypeOwner::Interface(owner) = self.types[id].owner else {
            return WasmType::I32;
        };
        let exported = self.worlds[world]
            .exports
            .values()
            .any(|item| matches!(item, WorldItem::Interface { id, .. } if *id == owner));
        if exported {
            WasmType::Pointer
        } else {
            WasmType::I32
        }
    }

    fn push_flat_list<'a>(
        &self,
        world: WorldId,
        mut list: impl Iterator<Item = &'a Type>,
        result: &mut FlatTypes<'_>,
    ) -> bool {
        list.all(|ty| self.push_flat(world, ty, result))
    }

    /// Appends the flat wasm types representing `ty` onto the `result`
    /// list provided.
    ///
    /// The `world` is used to determine how `borrow<T>` handles are flattened,
    /// as described in [`Resolve::wasm_signature`].
    pub fn push_flat(&self, world: WorldId, ty: &Type, result: &mut FlatTypes<'_>) -> bool {
        match ty {
            Type::Bool
            | Type::S8
            | Type::U8
            | Type::S16
            | Type::U16
            | Type::S32
            | Type::U32
            | Type::Char
            | Type::ErrorContext => result.push(WasmType::I32),

            Type::U64 | Type::S64 => result.push(WasmType::I64),
            Type::F32 => result.push(WasmType::F32),
            Type::F64 => result.push(WasmType::F64),
            Type::String => result.push(WasmType::Pointer) && result.push(WasmType::Length),

            Type::Id(id) => match &self.types[*id].kind {
                TypeDefKind::Type(t) => self.push_flat(world, t, result),

                TypeDefKind::Handle(Handle::Own(_)) => result.push(WasmType::I32),
                TypeDefKind::Handle(Handle::Borrow(id)) => {
                    result.push(self.flat_borrow(world, *id))
                }

                TypeDefKind::Resource => todo!(),

                TypeDefKind::Record(r) => {
                    self.push_flat_list(world, r.fields.iter().map(|f| &f.ty), result)
                }

                TypeDefKind::Tuple(t) => self.push_flat_list(world, t.types.iter(), result),

                TypeDefKind::Flags(r) => {
                    self.push_flat_list(world, (0..r.repr().count()).map(|_| &Type::U32), result)
                }

                TypeDefKind::List(_) => {
                    result.push(WasmType::Pointer) && result.push(WasmType::Length)
                }

                TypeDefKind::Map(_, _) => {
                    result.push(WasmType::Pointer) && result.push(WasmType::Length)
                }

                TypeDefKind::FixedLengthList(ty, size) => {
                    self.push_flat_list(world, (0..*size).map(|_| ty), result)
                }

                TypeDefKind::Variant(v) => {
                    result.push(v.tag().into())
                        && self.push_flat_variants(
                            world,
                            v.cases.iter().map(|c| c.ty.as_ref()),
                            result,
                        )
                }

                TypeDefKind::Enum(e) => result.push(e.tag().into()),

                TypeDefKind::Option(t) => {
                    result.push(WasmType::I32)
                        && self.push_flat_variants(world, [None, Some(t)], result)
                }

                TypeDefKind::Result(r) => {
                    result.push(WasmType::I32)
                        && self.push_flat_variants(world, [r.ok.as_ref(), r.err.as_ref()], result)
                }

                TypeDefKind::Future(_) => result.push(WasmType::I32),
                TypeDefKind::Stream(_) => result.push(WasmType::I32),

                TypeDefKind::Unknown => unreachable!(),
            },
        }
    }

    fn push_flat_variants<'a>(
        &self,
        world: WorldId,
        tys: impl IntoIterator<Item = Option<&'a Type>>,
        result: &mut FlatTypes<'_>,
    ) -> bool {
        let mut temp = result.types[result.cur..].to_vec();
        let mut temp = FlatTypes::new(&mut temp);
        let start = result.cur;

        // Push each case's type onto a temporary vector, and then
        // merge that vector into our final list starting at
        // `start`. Note that this requires some degree of
        // "unification" so we can handle things like `Result<i32,
        // f32>` where that turns into `[i32 i32]` where the second
        // `i32` might be the `f32` bitcasted.
        for ty in tys {
            if let Some(ty) = ty {
                if !self.push_flat(world, ty, &mut temp) {
                    result.overflow = true;
                    return false;
                }

                for (i, ty) in temp.types[..temp.cur].iter().enumerate() {
                    let i = i + start;
                    if i < result.cur {
                        result.types[i] = join(result.types[i], *ty);
                    } else if result.cur == result.types.len() {
                        result.overflow = true;
                        return false;
                    } else {
                        result.types[i] = *ty;
                        result.cur += 1;
                    }
                }
                temp.cur = 0;
            }
        }

        true
    }
}
