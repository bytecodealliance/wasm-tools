use crate::{Function, Handle, Int, Resolve, Type, TypeDefKind};

/// A raw WebAssembly signature with params and results.
#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct WasmSignature {
    /// The WebAssembly parameters of this function.
    pub params: Vec<WasmType>,

    /// The WebAssembly results of this function.
    pub results: Vec<WasmType>,

    /// Whether or not this signature is passing all of its parameters
    /// indirectly through a pointer within `params`.
    ///
    /// Note that `params` still reflects the true wasm paramters of this
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
    // NOTE: we don't lower interface types to any other Wasm type,
    // e.g. externref, so we don't need to define them here.
}

fn join(a: WasmType, b: WasmType) -> WasmType {
    use WasmType::*;

    match (a, b) {
        (I32, I32) | (I64, I64) | (F32, F32) | (F64, F64) => a,

        (I32, F32) | (F32, I32) => I32,

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
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AbiVariant {
    /// The guest is importing and calling the function.
    GuestImport,
    /// The guest is defining and exporting the function.
    GuestExport,
}

impl Resolve {
    /// Get the WebAssembly type signature for this interface function
    ///
    /// The first entry returned is the list of parameters and the second entry
    /// is the list of results for the wasm function signature.
    pub fn wasm_signature(&self, variant: AbiVariant, func: &Function) -> WasmSignature {
        const MAX_FLAT_PARAMS: usize = 16;
        const MAX_FLAT_RESULTS: usize = 1;

        let mut params = Vec::new();
        let mut indirect_params = false;
        for (_, param) in func.params.iter() {
            self.push_flat(param, &mut params);
        }

        if params.len() > MAX_FLAT_PARAMS {
            params.truncate(0);
            params.push(WasmType::I32);
            indirect_params = true;
        }

        let mut results = Vec::new();
        for ty in func.results.iter_types() {
            self.push_flat(ty, &mut results)
        }

        let mut retptr = false;

        // Rust/C don't support multi-value well right now, so if a function
        // would have multiple results then instead truncate it. Imports take a
        // return pointer to write into and exports return a pointer they wrote
        // into.
        if results.len() > MAX_FLAT_RESULTS {
            retptr = true;
            results.truncate(0);
            match variant {
                AbiVariant::GuestImport => {
                    params.push(WasmType::I32);
                }
                AbiVariant::GuestExport => {
                    results.push(WasmType::I32);
                }
            }
        }

        WasmSignature {
            params,
            indirect_params,
            results,
            retptr,
        }
    }

    /// Appends the flat wasm types representing `ty` onto the `result`
    /// list provided.
    pub fn push_flat(&self, ty: &Type, result: &mut Vec<WasmType>) {
        match ty {
            Type::Bool
            | Type::S8
            | Type::U8
            | Type::S16
            | Type::U16
            | Type::S32
            | Type::U32
            | Type::Char => result.push(WasmType::I32),

            Type::U64 | Type::S64 => result.push(WasmType::I64),
            Type::Float32 => result.push(WasmType::F32),
            Type::Float64 => result.push(WasmType::F64),
            Type::String => {
                result.push(WasmType::I32);
                result.push(WasmType::I32);
            }

            Type::Id(id) => match &self.types[*id].kind {
                TypeDefKind::Type(t) => self.push_flat(t, result),

                TypeDefKind::Handle(Handle::Own(_) | Handle::Borrow(_)) => {
                    result.push(WasmType::I32);
                }

                TypeDefKind::Resource => todo!(),

                TypeDefKind::Record(r) => {
                    for field in r.fields.iter() {
                        self.push_flat(&field.ty, result);
                    }
                }

                TypeDefKind::Tuple(t) => {
                    for ty in t.types.iter() {
                        self.push_flat(ty, result);
                    }
                }

                TypeDefKind::Flags(r) => {
                    for _ in 0..r.repr().count() {
                        result.push(WasmType::I32);
                    }
                }

                TypeDefKind::List(_) => {
                    result.push(WasmType::I32);
                    result.push(WasmType::I32);
                }

                TypeDefKind::Variant(v) => {
                    result.push(v.tag().into());
                    self.push_flat_variants(v.cases.iter().map(|c| c.ty.as_ref()), result);
                }

                TypeDefKind::Enum(e) => result.push(e.tag().into()),

                TypeDefKind::Option(t) => {
                    result.push(WasmType::I32);
                    self.push_flat_variants([None, Some(t)], result);
                }

                TypeDefKind::Result(r) => {
                    result.push(WasmType::I32);
                    self.push_flat_variants([r.ok.as_ref(), r.err.as_ref()], result);
                }

                TypeDefKind::Union(u) => {
                    result.push(WasmType::I32);
                    self.push_flat_variants(u.cases.iter().map(|c| Some(&c.ty)), result);
                }

                TypeDefKind::Future(_) => {
                    result.push(WasmType::I32);
                }

                TypeDefKind::Stream(_) => {
                    result.push(WasmType::I32);
                }

                TypeDefKind::Unknown => unreachable!(),
            },
        }
    }

    fn push_flat_variants<'a>(
        &self,
        tys: impl IntoIterator<Item = Option<&'a Type>>,
        result: &mut Vec<WasmType>,
    ) {
        let mut temp = Vec::new();
        let start = result.len();

        // Push each case's type onto a temporary vector, and then
        // merge that vector into our final list starting at
        // `start`. Note that this requires some degree of
        // "unification" so we can handle things like `Result<i32,
        // f32>` where that turns into `[i32 i32]` where the second
        // `i32` might be the `f32` bitcasted.
        for ty in tys {
            if let Some(ty) = ty {
                self.push_flat(ty, &mut temp);

                for (i, ty) in temp.drain(..).enumerate() {
                    match result.get_mut(start + i) {
                        Some(prev) => *prev = join(*prev, ty),
                        None => result.push(ty),
                    }
                }
            }
        }
    }
}
