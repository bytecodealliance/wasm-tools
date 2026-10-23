//! Generation of well-formed WebAssembly components from the component model's
//! [binary grammar].
//!
//! Each method emits the production it is named after. The fuzzer input picks
//! between alternatives and decides how many times a `vec(...)` or `section*`
//! repeats, so every component is a derivation of `component`. Size and count
//! prefixes are written once the bytes they cover exist, and the grammar's side
//! conditions, including the text format's grammar for names, are generated
//! directly. Generation also follows the validation rules that `wasmparser`
//! checks while decoding, so that decoding never fails.
//!
//! The grammar alone would make nearly every index out of bounds, so each
//! scope (component, component type, instance type and core module type)
//! tracks the length of each sort's index space, and indices are in bounds
//! whenever their index space isn't empty.
//!
//! Beyond that, [`Config::component_steering_percent`] of the choices that can
//! lead to a valid component do, so that the input decides how far components
//! get into validation. For that, each scope tracks what it knows about its
//! items as they're defined:
//!
//! - The kind of each type (function, instance, resource and so on), so that
//!   type indices refer to types of the right kind.
//! - What each value type flattens to and whether it's passed in memory, the
//!   type of each core function, and which memories can be canonical ABI
//!   memories, so that lifts and lowers pair functions with core functions of
//!   the right type and have the canonical options that they need.
//! - Which types imports and exports have named, so that imports and exports
//!   only refer to named types.
//! - What core modules, components, instances and their types import and
//!   export, so that aliases name exports that exist and instantiations pass
//!   arguments that match the imports.
//!
//! Core modules come from wasm-smith's `Module`. When steered, they import only
//! functions that a core instance exports, and export a memory, a `realloc`
//! and functions of the types that lifting the scope's function types needs.
//! Some sections are emitted together, as a component that exports a function
//! whatever the rest of the input does.
//!
//! Names are drawn from pools, as [`Config::name_pool_size`],
//! [`Config::name_reuse_percent`] and [`Config::name_collision_percent`] say,
//! so that references by name line up and names collide.
//!
//! Every loop stops once the input is exhausted and the first alternative of
//! each choice doesn't recurse, so generation terminates.
//!
//! [binary grammar]: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Binary.md

use crate::core::{
    DuplicateImportsBehavior, ExternType, FuncType, Shape, arbitrary_memtype, arbitrary_table_type,
    configured_valtypes,
};
use crate::{Config, Module, arbitrary_loop, limited_str};
use arbitrary::{Arbitrary, Result, Unstructured};
use std::collections::{BTreeMap, HashSet};
use std::mem;
use std::rc::Rc;
use wasm_encoder::{Encode, ExportKind, GlobalType, HeapType, RefType, ValType};

/// A pseudo-random well-formed WebAssembly [component].
///
/// The component is a derivation of the `component` production of the
/// component model's [binary grammar], so it always decodes. It is steered
/// toward a valid component as often as
/// [`Config::component_steering_percent`] says, so it is not necessarily
/// valid.
///
/// Productions that `wasmparser` doesn't decode as specified are not
/// generated: 🪙 value definitions and `valuebound`s, and the 🧵② built-ins,
/// whose `sh?` immediate `wasmparser` doesn't read. Neither are 🪙 start
/// definitions, since the text format can't hold the names that `wasmprinter`
/// prints for their results.
///
/// [component]: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md
/// [binary grammar]: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Binary.md
#[derive(Debug)]
pub struct WellFormedComponent {
    bytes: Vec<u8>,
}

impl<'a> Arbitrary<'a> for WellFormedComponent {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        WellFormedComponent::new(Config::default(), u)
    }
}

impl WellFormedComponent {
    /// Generates a component.
    ///
    /// Nesting is bounded by [`Config::max_nesting_depth`], the number of
    /// nested core modules and components by [`Config::max_modules`] and
    /// [`Config::max_components`], and the length of each section by the
    /// corresponding `max_*` field. Core modules are generated with `config`.
    pub fn new(config: Config, u: &mut Unstructured) -> Result<Self> {
        let mut builder = Builder {
            core_valtypes: configured_valtypes(&config),
            config,
            scopes: Vec::new(),
            names: Vec::new(),
            core_names: Vec::new(),
            labels: Vec::new(),
            modules: 0,
            components: 0,
            exportable: false,
            no_borrows: false,
            no_chars: false,
            no_ptrs: false,
        };
        let mut bytes = Vec::new();
        builder.component(u, &mut bytes)?;
        Ok(WellFormedComponent { bytes })
    }

    /// Returns the encoded component.
    pub fn to_bytes(&self) -> Vec<u8> {
        self.bytes.clone()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Sort {
    CoreFunc,
    CoreTable,
    CoreMemory,
    CoreGlobal,
    CoreTag,
    CoreType,
    CoreModule,
    CoreInstance,
    Func,
    Value,
    Type,
    Component,
    Instance,
}

const SORTS: usize = 13;

const ALL_SORTS: &[Sort] = &[
    Sort::CoreFunc,
    Sort::CoreTable,
    Sort::CoreMemory,
    Sort::CoreGlobal,
    Sort::CoreTag,
    Sort::CoreType,
    Sort::CoreModule,
    Sort::CoreInstance,
    Sort::Func,
    Sort::Value,
    Sort::Type,
    Sort::Component,
    Sort::Instance,
];

// The grammar allows any sort in a `sortidx` and an `export` alias, and any
// core sort in a `core:sortidx` and a `core export` alias, but `wasmparser`
// rejects the sorts that validation would reject while decoding.
const EXTERN_SORTS: &[Sort] = &[
    Sort::CoreModule,
    Sort::Func,
    Sort::Value,
    Sort::Type,
    Sort::Component,
    Sort::Instance,
];
const CORE_EXTERN_SORTS: &[Sort] = &[
    Sort::CoreFunc,
    Sort::CoreTable,
    Sort::CoreMemory,
    Sort::CoreGlobal,
    Sort::CoreTag,
];

const OUTER_ALIAS_SORTS: &[Sort] = &[
    Sort::CoreModule,
    Sort::CoreType,
    Sort::Component,
    Sort::Type,
];

// Aliases in component and instance types may only refer to these.
const DECL_EXPORT_ALIAS_SORTS: &[Sort] = &[Sort::Instance, Sort::Type];
const DECL_OUTER_ALIAS_SORTS: &[Sort] = &[Sort::CoreType, Sort::Type];

impl Sort {
    fn core(self) -> Option<u8> {
        Some(match self {
            Sort::CoreFunc => 0x00,
            Sort::CoreTable => 0x01,
            Sort::CoreMemory => 0x02,
            Sort::CoreGlobal => 0x03,
            Sort::CoreTag => 0x04,
            Sort::CoreType => 0x10,
            Sort::CoreModule => 0x11,
            Sort::CoreInstance => 0x12,
            _ => return None,
        })
    }

    fn from_export(kind: ExportKind) -> Sort {
        match kind {
            ExportKind::Func => Sort::CoreFunc,
            ExportKind::Table => Sort::CoreTable,
            ExportKind::Memory => Sort::CoreMemory,
            ExportKind::Global => Sort::CoreGlobal,
            ExportKind::Tag => Sort::CoreTag,
        }
    }

    fn encode(self, out: &mut Vec<u8>) {
        match self.core() {
            Some(sort) => out.extend([0x00, sort]),
            None => out.push(match self {
                Sort::Func => 0x01,
                Sort::Value => 0x02,
                Sort::Type => 0x03,
                Sort::Component => 0x04,
                Sort::Instance => 0x05,
                _ => unreachable!(),
            }),
        }
    }
}

/// What a type index refers to, as far as validation of its uses goes.
#[derive(Clone, Copy, PartialEq)]
enum Kind {
    /// A type whose kind isn't known, such as one aliased from an instance,
    /// which is used as any kind.
    Unknown,
    Value,
    Stream,
    Future,
    Func,
    Component,
    Instance,
    /// A resource type, which is defined in the current component if `local`.
    Resource {
        local: bool,
    },
    CoreModule,
    /// A `core:subtype` whose `core:comptype` has the `comptype` opcode.
    CoreSub {
        comptype: u8,
        is_final: bool,
    },
}

/// The most core values that a function's parameters flatten to before
/// they're passed in memory instead.
const MAX_FLAT_PARAMS: usize = 16;
/// The same for lowering async functions.
const MAX_FLAT_ASYNC_PARAMS: usize = 4;

/// What's known about a value type.
#[derive(Clone)]
struct Val {
    /// The core values that it flattens to, up to one more than
    /// `MAX_FLAT_PARAMS`.
    flat: Vec<ValType>,
    /// Whether it contains a `string`, `list` or `map`, which live in memory.
    ptr: bool,
    /// Whether it's a record, variant, flags or enum, which imports and
    /// exports can only refer to once it's named.
    nominal: bool,
    /// The same for equal types, if they don't involve resources.
    desc: Option<String>,
    /// The payload of a stream or future.
    payload: Option<Rc<Val>>,
    /// Whether it's an `own` (or a `borrow`) handle, and of which resource
    /// type index.
    handle: Option<(bool, u32)>,
    /// Whether it contains handles, and borrows, which results and payloads
    /// can't contain.
    handles: bool,
    borrows: bool,
}

impl Val {
    fn prim(ty: u8) -> Rc<Val> {
        let flat = match ty {
            0x73 => vec![ValType::I32; 2],
            0x78 | 0x77 => vec![ValType::I64],
            0x76 => vec![ValType::F32],
            0x75 => vec![ValType::F64],
            _ => vec![ValType::I32],
        };
        Rc::new(Val {
            flat,
            ptr: ty == 0x73,
            nominal: false,
            desc: Some(format!("{ty:x}")),
            payload: None,
            handle: None,
            handles: false,
            borrows: false,
        })
    }
}

/// The concatenated flattenings of `vals`.
fn flatten<'a>(vals: impl IntoIterator<Item = &'a Rc<Val>>) -> Vec<ValType> {
    vals.into_iter()
        .flat_map(|val| val.flat.iter().copied())
        .take(MAX_FLAT_PARAMS + 1)
        .collect()
}

/// The flattening of a variant whose cases have the payloads `cases`.
fn flatten_variant<'a>(cases: impl IntoIterator<Item = &'a Rc<Val>>) -> Vec<ValType> {
    let mut flat = vec![ValType::I32];
    for case in cases {
        for (i, ty) in case.flat.iter().enumerate() {
            match flat.get_mut(i + 1) {
                Some(prev) if prev == ty => {}
                Some(prev) => {
                    *prev = match (*prev, *ty) {
                        (ValType::I32, ValType::F32) | (ValType::F32, ValType::I32) => ValType::I32,
                        _ => ValType::I64,
                    }
                }
                None => flat.push(*ty),
            }
        }
    }
    flat.truncate(MAX_FLAT_PARAMS + 1);
    flat
}

/// The descriptions of `vals` joined with commas, if they all have one.
fn descs<'a>(vals: impl IntoIterator<Item = (&'a str, &'a Rc<Val>)>) -> Option<String> {
    let mut descs = Vec::new();
    for (label, val) in vals {
        descs.push(format!("{label}{}", val.desc.as_ref()?));
    }
    Some(descs.join(","))
}

/// The value type that the `defvaltype` opcode `op` defines with the labeled
/// `parts`, whose value types are those of their payloads, and `len` elements
/// for fixed-length lists.
fn defval(op: u8, parts: &[(String, Option<Rc<Val>>)], len: u32) -> Rc<Val> {
    let vals: Vec<&Rc<Val>> = parts.iter().filter_map(|(_, val)| val.as_ref()).collect();
    let ptr = vals.iter().any(|val| val.ptr);
    let desc = |name: &str| -> Option<String> {
        let mut descs = Vec::new();
        for (label, val) in parts {
            descs.push(match val {
                Some(val) => format!("{label}:{}", val.desc.as_ref()?),
                None => label.clone(),
            });
        }
        Some(format!("{name}({})", descs.join(",")))
    };
    let vals = vals.into_iter();
    let (flat, ptr, nominal, desc) = match op {
        0x72 => (flatten(vals), ptr, true, desc("record")),
        0x71 => (flatten_variant(vals), ptr, true, desc("variant")),
        0x70 => (vec![ValType::I32; 2], true, false, desc("list")),
        0x67 => {
            let flat =
                (0..len).flat_map(|_| parts[0].1.iter().flat_map(|val| val.flat.iter().copied()));
            let flat = flat.take(MAX_FLAT_PARAMS + 1).collect();
            (flat, ptr, false, desc(&format!("list{len}")))
        }
        0x6f => (flatten(vals), ptr, false, desc("tuple")),
        0x6e => {
            let flat = vec![ValType::I32; parts.len().div_ceil(32)];
            (flat, false, true, desc("flags"))
        }
        0x6d => (vec![ValType::I32], false, true, desc("enum")),
        0x6b => (flatten_variant(vals), ptr, false, desc("option")),
        0x6a => (flatten_variant(vals), ptr, false, desc("result")),
        0x66 => (vec![ValType::I32], false, false, desc("stream")),
        0x65 => (vec![ValType::I32], false, false, desc("future")),
        0x63 => (vec![ValType::I32; 2], true, false, desc("map")),
        // `own` and `borrow`, whose resources aren't described.
        _ => (vec![ValType::I32], false, false, None),
    };
    let payload = match op {
        0x66 | 0x65 => parts[0].1.clone(),
        _ => None,
    };
    let vals = parts.iter().filter_map(|(_, val)| val.as_ref());
    let handles = matches!(op, 0x69 | 0x68) || vals.clone().any(|val| val.handles);
    let borrows = op == 0x68 || vals.clone().any(|val| val.borrows);
    Rc::new(Val {
        flat,
        ptr,
        nominal,
        desc,
        payload,
        handle: None,
        handles,
        borrows,
    })
}

/// What's known about a function type.
struct Func {
    params: Vec<Rc<Val>>,
    result: Option<Rc<Val>>,
    is_async: bool,
    /// The same for equal types, if they don't involve resources.
    desc: Option<String>,
    /// The resource type index of a first parameter `self` that borrows it,
    /// and of a result that owns it, which methods and constructors of it
    /// need.
    self_borrow: Option<u32>,
    returns_own: Option<u32>,
}

/// How a function is lifted or lowered.
#[derive(Clone, Copy, PartialEq)]
enum Abi {
    Sync,
    Async,
    /// Async with a `callback`, which only lifts can have.
    Callback,
}

/// The core function type of lifting or lowering a function, and whether that
/// needs a `memory` and a `realloc`.
struct Lowered {
    ty: FuncType,
    memory: bool,
    realloc: bool,
}

impl Func {
    fn new(params: Vec<(String, Rc<Val>)>, result: Option<Rc<Val>>, is_async: bool) -> Func {
        let desc = descs(params.iter().map(|(l, v)| (&l[..], v))).and_then(|params| {
            let result = match &result {
                Some(result) => result.desc.clone()?,
                None => String::new(),
            };
            Some(format!("func{is_async}({params}){result}"))
        });
        let self_borrow = match params.first() {
            Some((label, val)) if label == "self" => match val.handle {
                Some((false, idx)) => Some(idx),
                _ => None,
            },
            _ => None,
        };
        let returns_own = match result.as_ref().and_then(|result| result.handle) {
            Some((true, idx)) => Some(idx),
            _ => None,
        };
        Func {
            params: params.into_iter().map(|(_, val)| val).collect(),
            result,
            is_async,
            desc,
            self_borrow,
            returns_own,
        }
    }

    fn lift(&self, abi: Abi) -> Lowered {
        let mut params = flatten(&self.params);
        let mut realloc = self.params.iter().any(|param| param.ptr);
        let mut memory = false;
        if params.len() > MAX_FLAT_PARAMS {
            params = vec![ValType::I32];
            realloc = true;
        }
        let results = match (abi, &self.result) {
            (Abi::Sync, Some(result)) if result.flat.len() > 1 => {
                memory = true;
                vec![ValType::I32]
            }
            (Abi::Sync, Some(result)) => result.flat.clone(),
            (Abi::Sync, None) => Vec::new(),
            // The result is passed to `task.return` instead.
            (_, result) => {
                memory = result
                    .as_ref()
                    .is_some_and(|result| result.ptr || result.flat.len() > MAX_FLAT_PARAMS);
                match abi {
                    Abi::Callback => vec![ValType::I32],
                    _ => Vec::new(),
                }
            }
        };
        Lowered {
            ty: FuncType { params, results },
            memory: memory || realloc,
            realloc,
        }
    }

    fn lower(&self, is_async: bool) -> Lowered {
        let max = match is_async {
            true => MAX_FLAT_ASYNC_PARAMS,
            false => MAX_FLAT_PARAMS,
        };
        let mut params = flatten(&self.params);
        let mut memory = self.params.iter().any(|param| param.ptr);
        if params.len() > max {
            params = vec![ValType::I32];
            memory = true;
        }
        let realloc = self.result.as_ref().is_some_and(|result| result.ptr);
        let mut results = Vec::new();
        match &self.result {
            Some(result) if is_async || result.flat.len() > 1 => {
                params.push(ValType::I32);
                memory = true;
            }
            Some(result) => results = result.flat.clone(),
            None => {}
        }
        if is_async {
            results = vec![ValType::I32];
        }
        Lowered {
            ty: FuncType { params, results },
            memory: memory || realloc,
            realloc,
        }
    }
}

fn realloc_type() -> FuncType {
    FuncType {
        params: vec![ValType::I32; 4],
        results: vec![ValType::I32],
    }
}

fn callback_type() -> FuncType {
    FuncType {
        params: vec![ValType::I32; 3],
        results: vec![ValType::I32],
    }
}

/// The core function type of the built-in `op`, taking a pointer to be `i32`,
/// where it doesn't depend on its immediates.
fn builtin_type(op: u8) -> Option<FuncType> {
    use ValType::{I32, I64};
    let (params, results) = match op {
        0x02 | 0x04 | 0x06 | 0x11 | 0x12 | 0x18 | 0x19 | 0x2a..=0x2d => (vec![I32], vec![I32]),
        0x03 | 0x0d | 0x13 | 0x14 | 0x1a | 0x1b | 0x1e | 0x22 | 0x28 => (vec![I32], vec![]),
        0x05 | 0x24 | 0x25 => (vec![], vec![]),
        0x0c | 0x1f | 0x26 | 0x29 => (vec![], vec![I32]),
        0x0e | 0x15 => (vec![], vec![I64]),
        0x0f | 0x10 => (vec![I32; 3], vec![I32]),
        0x16 | 0x17 | 0x20 | 0x21 => (vec![I32; 2], vec![I32]),
        0x23 | 0x2e | 0x2f => (vec![I32; 2], vec![]),
        _ => return None,
    };
    Some(FuncType { params, results })
}

/// What a type index refers to.
#[derive(Clone)]
struct Type {
    kind: Kind,
    /// The value type, for the kinds `Value`, `Stream` and `Future`.
    val: Option<Rc<Val>>,
    /// The function type, for the kind `Func`.
    func: Option<Rc<Func>>,
    /// The types that must be named for an import or export of this type.
    requires: Rc<[u32]>,
    /// The type index of a function, component or instance type that this
    /// one is the same as, which exports of either name.
    same: Option<u32>,
}

impl Type {
    fn of(kind: Kind) -> Type {
        Type {
            kind,
            val: None,
            func: None,
            requires: Rc::from([]),
            same: None,
        }
    }

    /// Whether it refers to resources.
    fn handles(&self) -> bool {
        let val = |val: &Rc<Val>| val.handles;
        match (&self.val, &self.func) {
            (Some(v), _) => val(v),
            (_, Some(func)) => func.params.iter().chain(&func.result).any(val),
            _ => false,
        }
    }

    /// This type as seen from another scope, where its indices don't apply.
    fn outside(&self) -> Type {
        Type {
            kind: match self.kind {
                Kind::Resource { .. } => Kind::Resource { local: false },
                kind => kind,
            },
            requires: Rc::from([]),
            same: None,
            ..self.clone()
        }
    }
}

/// An import or export of a core module, component, core instance or
/// instance, or of one of their types.
#[derive(Clone)]
struct Item {
    name: String,
    sort: Sort,
    ty: ItemTy,
}

/// What's known about the type of an item.
#[derive(Clone)]
enum ItemTy {
    Unknown,
    Type(Type),
    Func(Rc<Func>),
    CoreFunc(Rc<FuncType>),
    /// A core memory, which can be a canonical ABI memory if `true`.
    CoreMemory(bool),
    /// A core instance that a module imports, with what it must export.
    CoreInstance(Rc<[Item]>),
}

impl ItemTy {
    fn core(ty: ExternType<'_>) -> (Sort, ItemTy) {
        match ty {
            ExternType::Func(ty) => (
                Sort::CoreFunc,
                ty.map_or(ItemTy::Unknown, |ty| ItemTy::CoreFunc(Rc::clone(ty))),
            ),
            ExternType::Memory(ty) => (
                Sort::CoreMemory,
                ItemTy::CoreMemory(!ty.memory64 && !ty.shared),
            ),
            ExternType::Other(kind) => (Sort::from_export(kind), ItemTy::Unknown),
        }
    }
}

/// What a core module, component, core instance or instance imports and
/// exports, or what one of a type does.
#[derive(Default)]
struct Items {
    imports: Vec<Item>,
    exports: Vec<Item>,
}

impl Items {
    /// These items as seen from another scope.
    fn outside(self) -> Items {
        let outside = |items: Vec<Item>| {
            items
                .into_iter()
                .map(|item| match item.ty {
                    ItemTy::Type(ty) => Item {
                        ty: ItemTy::Type(ty.outside()),
                        ..item
                    },
                    _ => item,
                })
                .collect()
        };
        Items {
            imports: outside(self.imports),
            exports: outside(self.exports),
        }
    }
}

/// Whether each of `fields` is among `exports`, with the same type where both
/// are known.
fn satisfies(exports: &[Item], fields: &[Item]) -> bool {
    fields.iter().all(|field| {
        exports.iter().any(|export| {
            export.name == field.name
                && export.sort == field.sort
                && match (&export.ty, &field.ty) {
                    (ItemTy::CoreFunc(a), ItemTy::CoreFunc(b)) => a == b,
                    _ => true,
                }
        })
    })
}

/// A component, component type, instance type or core module type.
#[derive(Default)]
struct Scope {
    /// Whether this is a component or instance type.
    decl: bool,
    /// Whether this is an instance type, whose imports and exports can refer
    /// to types that aren't named.
    instance: bool,
    /// Whether this is a core module type.
    module: bool,
    /// The length of each sort's index space.
    lens: [u32; SORTS],
    types: Vec<Type>,
    /// The kind of each core type.
    core_types: Vec<Kind>,
    /// The core function types that core functions can have.
    core_sigs: BTreeMap<u32, Rc<FuncType>>,
    /// The encoded `core:comptype` of each core type defined in this scope.
    comptypes: BTreeMap<u32, Rc<[u8]>>,
    /// The types introduced by imports, and by exports, which name them.
    imported: HashSet<u32>,
    exported: HashSet<u32>,
    /// The function, component and instance types that exports named and
    /// imports didn't, by the index that they're the same as.
    exported_only: HashSet<u32>,
    /// The imported instances, whose exported types are named.
    imported_instances: HashSet<u32>,
    /// The names of the resource types that imports and exports introduced,
    /// which annotated function names of imports and exports refer to.
    resources: [Vec<(String, u32)>; 2],
    /// What core modules, components, core instances, instances and their
    /// types import and export, where that's known.
    items: BTreeMap<(Sort, u32), Rc<Items>>,
    /// What this scope imports and exports.
    own: Items,
    /// The fields that a core module type imports from each module.
    fields: Vec<(String, Vec<Item>)>,
    /// The type of each function whose type is known, and the types that must
    /// be named for an import or export of it.
    funcs: BTreeMap<u32, (Rc<Func>, Rc<[u32]>)>,
    /// The type of each core function whose type is known.
    core_funcs: BTreeMap<u32, Rc<FuncType>>,
    /// Whether each memory whose type is known can be a canonical ABI memory.
    memories: BTreeMap<u32, bool>,
    /// The canonical forms of the import and export names so far.
    imports: HashSet<String>,
    exports: HashSet<String>,
}

impl Scope {
    fn kind(&self, sort: Sort, idx: u32) -> Option<Kind> {
        match sort {
            Sort::Type => self.types.get(idx as usize).map(|ty| ty.kind),
            Sort::CoreType => self.core_types.get(idx as usize).copied(),
            _ => None,
        }
    }

    /// The indices of `sort`, which has kinds, whose kind satisfies `ok`.
    fn indices(&self, sort: Sort, ok: impl Fn(Kind) -> bool) -> Vec<u32> {
        (0..self.lens[sort as usize])
            .filter(|i| match self.kind(sort, *i) {
                Some(Kind::Unknown) | None => true,
                Some(kind) => ok(kind),
            })
            .collect()
    }

    /// Whether imports (or exports) can refer to all of `types`.
    fn named(&self, types: &[u32], import: bool) -> bool {
        self.instance
            || types
                .iter()
                .all(|ty| self.imported.contains(ty) || (!import && self.exported.contains(ty)))
    }

    /// The index of the function, component or instance type that `idx` is
    /// the same as, or `idx`.
    fn same(&self, idx: u32) -> u32 {
        self.types[idx as usize].same.unwrap_or(idx)
    }

    /// A type that is the same as `idx`, if `idx` is a function, component or
    /// instance type.
    fn alias(&self, idx: u32, mut ty: Type) -> Type {
        if matches!(
            ty.kind,
            Kind::Func | Kind::Component | Kind::Instance | Kind::Unknown
        ) {
            ty.same = Some(self.same(idx));
        }
        ty
    }

    /// The types that must be named where a value type refers to `idx`.
    fn needs(&self, idx: u32) -> Vec<u32> {
        let ty = &self.types[idx as usize];
        match &ty.val {
            Some(val) if val.nominal => vec![idx],
            _ => ty.requires.to_vec(),
        }
    }

    /// The core functions of type `ty`.
    fn core_funcs_of(&self, ty: &FuncType) -> Vec<u32> {
        self.core_funcs
            .iter()
            .filter(|(_, sig)| ***sig == *ty)
            .map(|(idx, _)| *idx)
            .collect()
    }
}

fn defined(kind: Kind) -> bool {
    matches!(kind, Kind::Value | Kind::Stream | Kind::Future)
}

fn resource(kind: Kind) -> bool {
    matches!(kind, Kind::Resource { .. })
}

fn core_func(kind: Kind) -> bool {
    matches!(kind, Kind::CoreSub { comptype: 0x60, .. })
}

fn core_sub(kind: Kind) -> bool {
    matches!(kind, Kind::CoreSub { .. })
}

/// The kind of a `core:subtype`, the function type that core functions of it
/// have if it is a function type that components can match, and its encoded
/// `core:comptype`.
type Subtype = (Kind, Option<FuncType>, Rc<[u8]>);

/// The `canonopt`s that validation allows on a canonical definition.
#[derive(Clone, Copy, PartialEq)]
enum Opts {
    Lift,
    Lower,
    Sync,
}

/// The extern names that are valid in some position.
#[derive(Clone, Copy, PartialEq)]
enum Names {
    All,
    Unannotated,
    Labels,
}

/// The pools that names are drawn from.
#[derive(Clone, Copy, PartialEq)]
enum Pool {
    Extern,
    Core,
    Label,
}

struct Builder {
    config: Config,
    core_valtypes: Vec<ValType>,
    /// Enclosing scopes, innermost last.
    scopes: Vec<Scope>,
    /// The pools of extern names, core names and labels.
    names: Vec<String>,
    core_names: Vec<String>,
    labels: Vec<String>,
    modules: usize,
    components: usize,
    /// Whether value types may only refer to types that exports can name.
    exportable: bool,
    /// Whether value types can't contain borrows, and be `char`s, as in
    /// results and payloads, and streams.
    no_borrows: bool,
    no_chars: bool,
    /// Whether value types can't be passed in memory.
    no_ptrs: bool,
}

/// Whether to keep repeating something, which is false once `u` is exhausted
/// and otherwise false with probability `1/n`.
fn more(u: &mut Unstructured, n: u8) -> Result<bool> {
    Ok(u.arbitrary::<u8>()? % n != 0)
}

/// True `percent` percent of the time, and once `u` is exhausted unless
/// `percent` is 0.
fn percent(u: &mut Unstructured, percent: u8) -> Result<bool> {
    Ok(percent > 0 && u.int_in_range(1..=100)? <= percent)
}

fn shuffle<T>(u: &mut Unstructured, items: &mut [T]) -> Result<()> {
    for i in (1..items.len()).rev() {
        let j = u.int_in_range(0..=i)?;
        items.swap(i, j);
    }
    Ok(())
}

impl Builder {
    fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn current(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    /// Whether to steer a choice toward a valid component.
    fn steer(&self, u: &mut Unstructured) -> Result<bool> {
        percent(u, self.config.component_steering_percent)
    }

    fn len(&self, sort: Sort) -> u32 {
        self.current().lens[sort as usize]
    }

    fn has(&self, sort: Sort) -> bool {
        self.len(sort) > 0
    }

    fn has_any(&self, sorts: &[Sort]) -> bool {
        sorts.iter().any(|s| self.has(*s))
    }

    fn add(&mut self, sort: Sort, n: u32) {
        for _ in 0..n {
            self.add_kind(sort, Kind::Unknown);
        }
    }

    fn add_kind(&mut self, sort: Sort, kind: Kind) {
        self.add_item(sort, kind, None);
    }

    /// Adds an item of `sort`, with `kind` if it's a type and what it imports
    /// and exports if that's known.
    fn add_item(&mut self, sort: Sort, kind: Kind, items: Option<Rc<Items>>) {
        match sort {
            Sort::Type => self.add_type(Type::of(kind), items),
            _ => self.push(sort, kind, items),
        }
    }

    fn add_type(&mut self, ty: Type, items: Option<Rc<Items>>) {
        self.scope().types.push(ty);
        self.push(Sort::Type, Kind::Unknown, items);
    }

    fn push(&mut self, sort: Sort, kind: Kind, items: Option<Rc<Items>>) {
        let scope = self.scope();
        let idx = scope.lens[sort as usize];
        scope.lens[sort as usize] = idx.saturating_add(1);
        if sort == Sort::CoreType {
            scope.core_types.push(kind);
        }
        if let Some(items) = items {
            scope.items.insert((sort, idx), items);
        }
    }

    fn add_core_type(&mut self, kind: Kind, sig: Option<FuncType>) {
        let idx = self.len(Sort::CoreType);
        self.add_kind(Sort::CoreType, kind);
        if let Some(sig) = sig {
            self.scope().core_sigs.insert(idx, Rc::new(sig));
        }
    }

    fn add_func(&mut self, func: Option<Rc<Func>>, requires: Rc<[u32]>) {
        let idx = self.len(Sort::Func);
        if let Some(func) = func {
            self.scope().funcs.insert(idx, (func, requires));
        }
        self.add(Sort::Func, 1);
    }

    fn add_core_func(&mut self, ty: Option<FuncType>) {
        let idx = self.len(Sort::CoreFunc);
        if let Some(ty) = ty {
            self.scope().core_funcs.insert(idx, Rc::new(ty));
        }
        self.add(Sort::CoreFunc, 1);
    }

    /// Adds an item of `sort` whose type is `ty`, and which must have
    /// `requires` named for an import or export of it if it's a function.
    fn add_typed(
        &mut self,
        sort: Sort,
        ty: &ItemTy,
        requires: Rc<[u32]>,
        items: Option<Rc<Items>>,
    ) {
        let idx = self.len(sort);
        match (sort, ty) {
            (Sort::Type, ItemTy::Type(ty)) => return self.add_type(ty.clone(), items),
            (Sort::Func, ItemTy::Func(func)) => {
                self.scope().funcs.insert(idx, (Rc::clone(func), requires));
            }
            (Sort::CoreFunc, ItemTy::CoreFunc(ty)) => {
                self.scope().core_funcs.insert(idx, Rc::clone(ty));
            }
            (Sort::CoreMemory, ItemTy::CoreMemory(ok)) => {
                self.scope().memories.insert(idx, *ok);
            }
            _ => {}
        }
        self.add_item(sort, Kind::Unknown, items);
    }

    /// What's known about the type of the item `idx` of `sort`.
    fn item_ty(&self, sort: Sort, idx: u32) -> (ItemTy, Rc<[u32]>) {
        let scope = self.current();
        let none = Rc::from([]);
        match sort {
            Sort::Type => match scope.types.get(idx as usize) {
                Some(ty) => (ItemTy::Type(ty.clone()), none),
                None => (ItemTy::Unknown, none),
            },
            Sort::Func => match scope.funcs.get(&idx) {
                Some((func, requires)) => (ItemTy::Func(Rc::clone(func)), Rc::clone(requires)),
                None => (ItemTy::Unknown, none),
            },
            Sort::CoreFunc => match scope.core_funcs.get(&idx) {
                Some(ty) => (ItemTy::CoreFunc(Rc::clone(ty)), none),
                None => (ItemTy::Unknown, none),
            },
            Sort::CoreMemory => match scope.memories.get(&idx) {
                Some(ok) => (ItemTy::CoreMemory(*ok), none),
                None => (ItemTy::Unknown, none),
            },
            _ => (ItemTy::Unknown, none),
        }
    }

    fn items(&self, ct: usize, sort: Sort, idx: u32) -> Option<Rc<Items>> {
        let scope = &self.scopes[self.scopes.len() - 1 - ct];
        scope.items.get(&(sort, idx)).cloned()
    }

    /// The indices of `sort` whose exports include one of `sorts`, and those
    /// exports.
    fn exporters(&self, sort: Sort, sorts: &[Sort]) -> Vec<(u32, Vec<Item>)> {
        let scope = self.current();
        (0..scope.lens[sort as usize])
            .filter_map(|idx| {
                let exports: Vec<_> = scope
                    .items
                    .get(&(sort, idx))?
                    .exports
                    .iter()
                    .filter(|item| sorts.contains(&item.sort))
                    .cloned()
                    .collect();
                (!exports.is_empty()).then_some((idx, exports))
            })
            .collect()
    }

    fn has_kind(&self, sort: Sort, ok: impl Fn(Kind) -> bool) -> bool {
        !self.current().indices(sort, ok).is_empty()
    }

    /// Writes an index of `sort`, which has kinds, whose kind satisfies `ok`
    /// when steered and there is one, and returns it and its kind.
    fn kind_index(
        &self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        sort: Sort,
        ok: impl Fn(Kind) -> bool,
    ) -> Result<(u32, Kind)> {
        let indices = self.current().indices(sort, ok);
        let idx = match !indices.is_empty() && self.steer(u)? {
            true => *u.choose(&indices)?,
            false => self.idx(u, sort, 0)?,
        };
        self.u32(out, idx);
        Ok((idx, self.kind_at(0, sort, idx)))
    }

    /// One of `candidates` when steered and there is one, and otherwise any
    /// index of `sort`.
    fn steered(&self, u: &mut Unstructured, sort: Sort, candidates: &[u32]) -> Result<u32> {
        match !candidates.is_empty() && self.steer(u)? {
            true => Ok(*u.choose(candidates)?),
            false => self.idx(u, sort, 0),
        }
    }

    fn kind_at(&self, ct: usize, sort: Sort, idx: u32) -> Kind {
        let scope = &self.scopes[self.scopes.len() - 1 - ct];
        scope.kind(sort, idx).unwrap_or(Kind::Unknown)
    }

    fn type_at(&self, ct: usize, idx: u32) -> Type {
        let scope = &self.scopes[self.scopes.len() - 1 - ct];
        match scope.types.get(idx as usize) {
            Some(ty) if ct == 0 => ty.clone(),
            Some(ty) => ty.outside(),
            None => Type::of(Kind::Unknown),
        }
    }

    fn can_nest(&self) -> bool {
        self.scopes.len() <= self.config.max_nesting_depth
    }

    /// Returns an index into the index space of `sort` in the scope `ct` scopes
    /// out, which is out of bounds only when that index space is empty.
    fn idx(&self, u: &mut Unstructured, sort: Sort, ct: usize) -> Result<u32> {
        let len = self.scopes[self.scopes.len() - 1 - ct].lens[sort as usize];
        match len.checked_sub(1) {
            Some(max) => u.int_in_range(0..=max),
            None => Ok(0),
        }
    }

    fn index(&self, u: &mut Unstructured, out: &mut Vec<u8>, sort: Sort) -> Result<()> {
        let idx = self.idx(u, sort, 0)?;
        self.u32(out, idx);
        Ok(())
    }

    /// Picks one of `sorts`, preferring those with a non-empty index space.
    fn pick_sort(&self, u: &mut Unstructured, sorts: &[Sort]) -> Result<Sort> {
        let nonempty: Vec<Sort> = sorts.iter().copied().filter(|s| self.has(*s)).collect();
        Ok(*u.choose(if nonempty.is_empty() {
            sorts
        } else {
            &nonempty
        })?)
    }

    /// A LEB128 `u32`, padded to [`Config::min_uleb_size`] bytes.
    fn u32(&self, out: &mut Vec<u8>, mut value: u32) {
        let min = usize::from(self.config.min_uleb_size).min(5);
        let mut len = 0;
        loop {
            let mut byte = (value & 0x7f) as u8;
            value >>= 7;
            len += 1;
            let more = value != 0 || len < min;
            if more {
                byte |= 0x80;
            }
            out.push(byte);
            if !more {
                break;
            }
        }
    }

    fn string(&self, out: &mut Vec<u8>, s: &str) {
        self.u32(out, s.len() as u32);
        out.extend_from_slice(s.as_bytes());
    }

    /// `vec(T)` of between `min` and `max` elements emitted by `f`.
    fn vec(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        min: usize,
        max: usize,
        mut f: impl FnMut(&mut Self, &mut Unstructured, &mut Vec<u8>) -> Result<()>,
    ) -> Result<()> {
        let mut items = Vec::new();
        let mut n = 0;
        while n < min || (n < max && more(u, 4)?) {
            f(self, u, &mut items)?;
            n += 1;
        }
        self.u32(out, n as u32);
        out.extend(items);
        Ok(())
    }

    /// `<T>?` where `f` emits `T`, which is absent when `present` is false.
    fn optional<T>(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        present: bool,
        f: impl FnOnce(&mut Self, &mut Unstructured, &mut Vec<u8>) -> Result<T>,
    ) -> Result<Option<T>> {
        if present && u.arbitrary()? {
            out.push(0x01);
            Ok(Some(f(self, u, out)?))
        } else {
            out.push(0x00);
            Ok(None)
        }
    }

    /// `section_N(B)` or `namesubsection_N(B)` where `f` emits `B`.
    fn subsection(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        id: u8,
        f: impl FnOnce(&mut Self, &mut Unstructured, &mut Vec<u8>) -> Result<()>,
    ) -> Result<()> {
        let mut body = Vec::new();
        f(self, u, &mut body)?;
        out.push(id);
        self.u32(out, body.len() as u32);
        out.extend(body);
        Ok(())
    }

    /// `component`, returning what it imports and exports.
    fn component(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<Items> {
        out.extend([0x00, 0x61, 0x73, 0x6d, 0x0d, 0x00, 0x01, 0x00]);
        self.scopes.push(Scope::default());
        while more(u, 16)? {
            self.section(u, out)?;
        }
        Ok(self.scopes.pop().unwrap().own.outside())
    }

    fn section(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        // Not section ids, but the sections of `callable_export`,
        // `lifted_export` and `lowered_import`.
        const CALLABLE_EXPORT: u8 = 0xfd;
        const LIFTED_EXPORT: u8 = 0xfe;
        const LOWERED_IMPORT: u8 = 0xff;

        let mut ids = vec![2, 3, 5, 7, 8, 10];
        if self.config.generate_custom_sections {
            ids.push(0);
        }
        if self.modules < self.config.max_modules {
            ids.push(1);
        }
        if self.components < self.config.max_components && self.can_nest() {
            ids.push(4);
        }
        let steer = self.steer(u)?;
        if self.can_alias(steer) {
            ids.push(6);
        }
        if self.has_any(EXTERN_SORTS) {
            ids.push(11);
        }
        if steer {
            ids.push(CALLABLE_EXPORT);
            if self.modules < self.config.max_modules {
                ids.push(LIFTED_EXPORT);
                if !self.lowerable().is_empty() {
                    ids.push(LOWERED_IMPORT);
                }
            }
        }
        let id = *u.choose(&ids)?;
        match id {
            CALLABLE_EXPORT => return self.callable_export(u, out),
            LIFTED_EXPORT => return self.lifted_export(u, out),
            LOWERED_IMPORT => return self.lowered_import(u, out),
            _ => {}
        }
        let c = &self.config;
        let (types, imports, exports, aliases) =
            (c.max_types, c.max_imports, c.max_exports, c.max_aliases);
        let (instances, funcs) = (c.max_instances, c.max_funcs);
        self.subsection(u, out, id, |me, u, out| match id {
            0 => me.custom(u, out),
            1 => me.core_module(u, out),
            2 => me.vec(u, out, 0, instances, Self::core_instance),
            3 => me.vec(u, out, 0, types, Self::core_type),
            4 => {
                me.components += 1;
                let items = me.component(u, out)?;
                me.add_item(Sort::Component, Kind::Unknown, Some(Rc::new(items)));
                Ok(())
            }
            5 => me.vec(u, out, 0, instances, Self::instance),
            6 => me.vec(u, out, 0, aliases, Self::alias),
            7 => me.vec(u, out, 0, types, Self::ty),
            8 => me.vec(u, out, 0, funcs, Self::canon),
            10 => me.vec(u, out, 0, imports, Self::import),
            _ => me.vec(u, out, 0, exports, Self::export),
        })
    }

    /// A type section with a function type of scalars, an import of a
    /// function of that type, a canonical section that lowers it without
    /// options and lifts that back with the same type, which is valid whatever
    /// the type, and an export of the lifted function, so that components
    /// export functions that call into embedders.
    fn callable_export(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let ty = self.len(Sort::Type);
        let mut func = None;
        self.subsection(u, out, 7, |me, u, out| {
            me.u32(out, 1);
            let def = me.scalar_functype(u, out)?;
            func = def.func.clone();
            me.add_type(def, None);
            Ok(())
        })?;
        let func = func.unwrap();

        let import = self.len(Sort::Func);
        self.subsection(u, out, 10, |me, u, out| {
            me.u32(out, 1);
            me.label_nameattributes(u, out, true, ItemTy::Func(Rc::clone(&func)))?;
            out.push(0x01);
            me.u32(out, ty);
            me.add_func(Some(Rc::clone(&func)), Rc::from([]));
            Ok(())
        })?;

        let core_func = self.len(Sort::CoreFunc);
        let lifted = import + 1;
        self.subsection(u, out, 8, |me, _, out| {
            me.u32(out, 2);
            out.extend([0x01, 0x00]);
            me.u32(out, import);
            me.u32(out, 0);
            me.add_core_func(Some(func.lower(false).ty));
            out.extend([0x00, 0x00]);
            me.u32(out, core_func);
            me.u32(out, 0);
            me.u32(out, ty);
            me.add_func(Some(Rc::clone(&func)), Rc::from([]));
            Ok(())
        })?;

        self.export_func(u, out, lifted, func)
    }

    /// An export section with an export of the function `idx`, of type `func`,
    /// under a label.
    fn export_func(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        idx: u32,
        func: Rc<Func>,
    ) -> Result<()> {
        let requires = self.item_ty(Sort::Func, idx).1;
        self.subsection(u, out, 11, |me, u, out| {
            me.u32(out, 1);
            me.label_nameattributes(u, out, false, ItemTy::Func(Rc::clone(&func)))?;
            Sort::Func.encode(out);
            me.u32(out, idx);
            out.push(0x00);
            me.add_func(Some(func), requires);
            Ok(())
        })
    }

    /// A type section with a function type, a core module that exports a
    /// function of the type that lifting it has, and the memory and functions
    /// that its canonical options need, an instance of the module, aliases of
    /// those exports, a canonical section that lifts the function, and an
    /// export of it, so that components export functions of any type.
    fn lifted_export(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let ty = self.len(Sort::Type);
        let mut func = None;
        self.subsection(u, out, 7, |me, u, out| {
            me.u32(out, 1);
            me.exportable = true;
            let def = me.functype(u, out)?;
            me.exportable = false;
            func = def.func.clone();
            me.add_type(def, None);
            Ok(())
        })?;
        // The type refers to a type whose structure isn't known.
        let Some(func) = func else {
            return Ok(());
        };
        let abi = match func.is_async {
            true => *u.choose(&[Abi::Sync, Abi::Async, Abi::Callback])?,
            false => Abi::Sync,
        };
        let lowered = func.lift(abi);

        // The module's function exports, in the order that they're aliased,
        // and the canonical options that refer to them.
        let mut used = HashSet::new();
        let mut funcs = vec![(self.module_export_name(u, &mut used)?, lowered.ty.clone())];
        let mut opts = Vec::new();
        // With a memory there's also a `realloc`, so that the lifted function
        // can be lowered again.
        let memory = lowered.memory || u.arbitrary()?;
        let realloc = memory;
        let post_return = abi == Abi::Sync && u.arbitrary()?;
        let post_return_type = FuncType {
            params: lowered.ty.results.clone(),
            results: Vec::new(),
        };
        for (present, opt, ty) in [
            (realloc, 0x04, realloc_type()),
            (post_return, 0x05, post_return_type),
            (abi == Abi::Callback, 0x07, callback_type()),
        ] {
            if present {
                let mut encoded = vec![opt];
                self.u32(&mut encoded, self.len(Sort::CoreFunc) + funcs.len() as u32);
                opts.push(encoded);
                funcs.push((self.module_export_name(u, &mut used)?, ty));
            }
        }
        let memory = match memory {
            true => {
                let mut encoded = vec![0x03];
                self.u32(&mut encoded, self.len(Sort::CoreMemory));
                opts.push(encoded);
                Some(self.module_export_name(u, &mut used)?)
            }
            false => None,
        };
        if abi != Abi::Sync {
            opts.push(vec![0x06]);
        }
        if u.arbitrary()? {
            opts.push(vec![u.int_in_range(0x00..=0x02)?]);
        }
        let shape = Shape {
            imports: Vec::new(),
            funcs: funcs
                .iter()
                .map(|(name, ty)| (name.clone(), Rc::new(ty.clone())))
                .collect(),
            memory: memory.clone(),
        };

        let module = self.len(Sort::CoreModule);
        self.subsection(u, out, 1, |me, u, out| me.emit_module(u, out, Some(shape)))?;
        let instance = self.len(Sort::CoreInstance);
        self.subsection(u, out, 2, |me, _, out| {
            me.u32(out, 1);
            out.push(0x00);
            me.u32(out, module);
            me.u32(out, 0);
            let items = me.items(0, Sort::CoreModule, module);
            me.add_item(Sort::CoreInstance, Kind::Unknown, items);
            Ok(())
        })?;
        let core_func = self.len(Sort::CoreFunc);
        self.subsection(u, out, 6, |me, _, out| {
            me.u32(out, (funcs.len() + usize::from(memory.is_some())) as u32);
            for (name, ty) in funcs {
                me.core_export_alias(out, Sort::CoreFunc, instance, &name);
                me.add_core_func(Some(ty));
            }
            if let Some(name) = &memory {
                me.core_export_alias(out, Sort::CoreMemory, instance, name);
                let idx = me.len(Sort::CoreMemory);
                me.scope().memories.insert(idx, true);
                me.add(Sort::CoreMemory, 1);
            }
            Ok(())
        })?;
        let lift = self.len(Sort::Func);
        self.subsection(u, out, 8, |me, u, out| {
            me.u32(out, 1);
            out.extend([0x00, 0x00]);
            me.u32(out, core_func);
            me.write_opts(u, out, opts)?;
            me.u32(out, ty);
            let requires = Rc::clone(&me.current().types[ty as usize].requires);
            me.add_func(Some(Rc::clone(&func)), requires);
            Ok(())
        })?;
        self.export_func(u, out, lift, func)
    }

    /// A canonical section that lowers a function whose type is known with
    /// the options that it needs, a core instance that exports the lowered
    /// function, a core module that imports it, and an instance of the module,
    /// so that core code calls functions of any type that the component lifts,
    /// imports or gets from another component.
    fn lowered_import(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let func = *u.choose(&self.lowerable())?;
        let info = Rc::clone(&self.current().funcs[&func].0);
        let is_async = info.is_async && self.can_lower(&info.lower(true)) && u.arbitrary()?;
        let lowered = info.lower(is_async);
        let abi = if is_async { Abi::Async } else { Abi::Sync };
        let core_func = self.len(Sort::CoreFunc);
        self.subsection(u, out, 8, |me, u, out| {
            me.u32(out, 1);
            out.extend([0x01, 0x00]);
            me.u32(out, func);
            me.canon_opts(u, out, &lowered, abi, Opts::Lower)?;
            me.add_core_func(Some(lowered.ty.clone()));
            Ok(())
        })?;

        let ty = Rc::new(lowered.ty);
        let name = self.module_export_name(u, &mut HashSet::new())?;
        let exports = self.len(Sort::CoreInstance);
        self.subsection(u, out, 2, |me, _, out| {
            me.u32(out, 1);
            out.push(0x01);
            me.u32(out, 1);
            me.string(out, &name);
            out.push(Sort::CoreFunc.core().unwrap());
            me.u32(out, core_func);
            let exports = vec![Item {
                name: name.clone(),
                sort: Sort::CoreFunc,
                ty: ItemTy::CoreFunc(Rc::clone(&ty)),
            }];
            let items = Items {
                imports: Vec::new(),
                exports,
            };
            me.add_item(Sort::CoreInstance, Kind::Unknown, Some(Rc::new(items)));
            Ok(())
        })?;

        let from = self.module_export_name(u, &mut HashSet::new())?;
        let shape = Shape {
            imports: vec![(from.clone(), name, ty)],
            ..Shape::default()
        };
        let module = self.len(Sort::CoreModule);
        self.subsection(u, out, 1, |me, u, out| me.emit_module(u, out, Some(shape)))?;
        self.subsection(u, out, 2, |me, _, out| {
            me.u32(out, 1);
            out.push(0x00);
            me.u32(out, module);
            me.u32(out, 1);
            me.string(out, &from);
            out.push(0x12);
            me.u32(out, exports);
            let items = me.items(0, Sort::CoreModule, module);
            me.add_item(Sort::CoreInstance, Kind::Unknown, items);
            Ok(())
        })
    }

    /// A `core export` alias of `name` of `sort` from the core instance
    /// `instance`.
    fn core_export_alias(&self, out: &mut Vec<u8>, sort: Sort, instance: u32, name: &str) {
        sort.encode(out);
        out.push(0x01);
        self.u32(out, instance);
        self.string(out, name);
    }

    /// A core name for an export of a generated module, which is unique in
    /// `used`.
    fn module_export_name(
        &mut self,
        u: &mut Unstructured,
        used: &mut HashSet<String>,
    ) -> Result<String> {
        let mut name = self.pooled(u, Pool::Core, |_| true, |u| Ok(limited_str(20, u)?.into()))?;
        let mut i = used.len();
        while !used.insert(name.clone()) {
            name = format!("x{i}");
            i += 1;
        }
        Ok(name)
    }

    /// A liftable `functype` whose parameters and result are scalars.
    fn scalar_functype(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<Type> {
        const SCALARS: &[u8] = &[
            0x7f, 0x7e, 0x7d, 0x7c, 0x7b, 0x7a, 0x79, 0x78, 0x77, 0x76, 0x75, 0x74,
        ];
        out.push(0x40);
        let mut labels = HashSet::new();
        let mut params = Vec::new();
        self.vec(u, out, 0, 16, |me, u, out| {
            let label = me.labellit(u, out, &mut labels)?;
            let ty = *u.choose(SCALARS)?;
            out.push(ty);
            params.push((label, Val::prim(ty)));
            Ok(())
        })?;
        let result = if u.arbitrary()? {
            let ty = *u.choose(SCALARS)?;
            out.extend([0x00, ty]);
            Some(Val::prim(ty))
        } else {
            out.extend([0x01, 0x00]);
            None
        };
        Ok(Type {
            func: Some(Rc::new(Func::new(params, result, false))),
            ..Type::of(Kind::Func)
        })
    }

    /// `nameattributes` of a function import or export of the current scope,
    /// with a label as its `externname`, since annotated plain names need
    /// resources and interface names need instances.
    fn label_nameattributes(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        import: bool,
        ty: ItemTy,
    ) -> Result<()> {
        let scope = self.scope();
        let mut used = mem::take(match import {
            true => &mut scope.imports,
            false => &mut scope.exports,
        });
        let name = self.pooled_name(u, Names::Labels);
        let name = name.and_then(|name| self.unique(u, name, &mut used));
        let scope = self.scope();
        let (names, own) = match import {
            true => (&mut scope.imports, &mut scope.own.imports),
            false => (&mut scope.exports, &mut scope.own.exports),
        };
        *names = used;
        let name = name?;
        own.push(Item {
            name: name.clone(),
            sort: Sort::Func,
            ty,
        });
        out.push(0x00);
        self.string(out, &name);
        Ok(())
    }

    fn custom(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let name = limited_str(20, u)?;
        // Arbitrary contents are only well-formed in other custom sections.
        if name == "component-name" || u.arbitrary()? {
            return self.namedata(u, out);
        }
        self.string(out, name);
        let len = u.arbitrary_len::<u8>()?.min(100);
        out.extend_from_slice(u.bytes(len)?);
        Ok(())
    }

    fn namedata(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        self.string(out, "component-name");
        if u.arbitrary()? {
            self.subsection(u, out, 0, |me, u, out| {
                let name = limited_str(20, u)?;
                me.string(out, name);
                Ok(())
            })?;
        }
        while more(u, 4)? {
            let sort = *u.choose(ALL_SORTS)?;
            self.subsection(u, out, 1, |me, u, out| {
                sort.encode(out);
                me.namemap(u, out, sort)
            })?;
        }
        Ok(())
    }

    /// `namemap`, whose indices are in increasing order.
    fn namemap(&mut self, u: &mut Unstructured, out: &mut Vec<u8>, sort: Sort) -> Result<()> {
        let mut names = Vec::new();
        let mut n = 0;
        for idx in 0..self.len(sort) {
            if !more(u, 8)? {
                break;
            }
            if u.arbitrary()? {
                self.u32(&mut names, idx);
                let name = limited_str(20, u)?;
                self.string(&mut names, name);
                n += 1;
            }
        }
        self.u32(out, n);
        out.extend(names);
        Ok(())
    }

    fn core_module(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let shape = match self.steer(u)? {
            true => Some(self.shape(u)?),
            false => None,
        };
        self.emit_module(u, out, shape)
    }

    /// Imports of some of the functions that a core instance exports, and
    /// exports of functions of the types that lifting function types of this
    /// scope has, and of a memory and a `realloc`.
    fn shape(&mut self, u: &mut Unstructured) -> Result<Shape> {
        let mut shape = Shape::default();
        let exporters: Vec<Vec<(String, Rc<FuncType>)>> = self
            .exporters(Sort::CoreInstance, &[Sort::CoreFunc])
            .into_iter()
            .map(|(_, exports)| {
                exports
                    .into_iter()
                    .filter_map(|item| match item.ty {
                        ItemTy::CoreFunc(ty) => Some((item.name, ty)),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            })
            .filter(|funcs| !funcs.is_empty())
            .collect();
        if !exporters.is_empty() && u.arbitrary()? {
            let funcs = u.choose(&exporters)?.clone();
            let module =
                self.pooled(u, Pool::Core, |_| true, |u| Ok(limited_str(20, u)?.into()))?;
            let mut used = HashSet::new();
            for (name, ty) in funcs {
                if used.insert(name.clone()) && u.arbitrary()? {
                    shape.imports.push((module.clone(), name, ty));
                }
            }
        }

        let funcs: Vec<Rc<Func>> = self
            .current()
            .types
            .iter()
            .filter_map(|ty| ty.func.clone())
            .collect();
        let mut used = HashSet::new();
        while !funcs.is_empty() && more(u, 2)? {
            let func = Rc::clone(u.choose(&funcs)?);
            let abi = match func.is_async {
                true => *u.choose(&[Abi::Sync, Abi::Async, Abi::Callback])?,
                false => Abi::Sync,
            };
            let name = self.module_export_name(u, &mut used)?;
            shape.funcs.push((name, Rc::new(func.lift(abi).ty)));
        }
        if u.arbitrary()? {
            shape.memory = Some(self.module_export_name(u, &mut used)?);
            let name = self.module_export_name(u, &mut used)?;
            shape.funcs.push((name, Rc::new(realloc_type())));
        }
        Ok(shape)
    }

    /// A core module, with `shape` if given.
    fn emit_module(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        shape: Option<Shape>,
    ) -> Result<()> {
        self.modules += 1;
        let config = self.config.clone();
        let module = match shape {
            Some(shape) => Module::new_shaped(config, u, shape)?,
            None => Module::new_internal(config, u, DuplicateImportsBehavior::Disallowed)?,
        };
        let mut imports: Vec<(String, Vec<Item>)> = Vec::new();
        for (from, name, ty) in module.import_types() {
            let (sort, ty) = ItemTy::core(ty);
            let item = Item {
                name: name.to_string(),
                sort,
                ty,
            };
            match imports.iter_mut().find(|(m, _)| m == from) {
                Some((_, fields)) => fields.push(item),
                None => imports.push((from.to_string(), vec![item])),
            }
            self.core_names.extend([from.to_string(), name.to_string()]);
        }
        let mut items = Items::default();
        items.imports = imports
            .into_iter()
            .map(|(name, fields)| Item {
                name,
                sort: Sort::CoreInstance,
                ty: ItemTy::CoreInstance(fields.into()),
            })
            .collect();
        for (name, ty) in module.export_types() {
            let (sort, ty) = ItemTy::core(ty);
            items.exports.push(Item {
                name: name.to_string(),
                sort,
                ty,
            });
            self.core_names.push(name.to_string());
        }
        out.extend(module.to_bytes());
        self.add_item(Sort::CoreModule, Kind::Unknown, Some(Rc::new(items)));
        Ok(())
    }

    /// A `core:name`, which is unique in `used` under the given prefix unless
    /// it collides on purpose.
    fn core_name(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        used: Option<(&mut HashSet<String>, &str)>,
    ) -> Result<String> {
        let mut name = self.pooled(u, Pool::Core, |_| true, |u| Ok(limited_str(20, u)?.into()))?;
        if let Some((used, prefix)) = used {
            let key = |name: &str| format!("{prefix}{name}");
            if !used.insert(key(&name)) && !percent(u, self.config.name_collision_percent)? {
                let mut i = used.len();
                while !used.insert(key(&name)) {
                    name = format!("x{i}");
                    i += 1;
                }
            }
        }
        self.string(out, &name);
        Ok(name)
    }

    fn core_instance(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let mut used = HashSet::new();
        let steer = self.steer(u)?;
        let mut module = None;
        if self.has(Sort::CoreModule) && u.arbitrary()? {
            let m = self.idx(u, Sort::CoreModule, 0)?;
            let items = self.items(0, Sort::CoreModule, m);
            // When steered, only modules whose imports some instance provides.
            let ok = match &items {
                Some(items) if steer => items
                    .imports
                    .iter()
                    .all(|arg| !self.providers(arg).is_empty()),
                _ => true,
            };
            if ok {
                module = Some((m, items));
            }
        }
        let items = if let Some((m, module)) = module {
            out.push(0x00);
            self.u32(out, m);
            let max = match self.has(Sort::CoreInstance) {
                true => usize::MAX,
                false => 0,
            };
            match &module {
                // An argument for each module that the module imports from.
                Some(module) if steer => {
                    let mut args: Vec<Option<&Item>> = module.imports.iter().map(Some).collect();
                    used.extend(module.imports.iter().map(|item| item.name.clone()));
                    while max > 0 && !more(u, 8)? && !u.is_empty() {
                        args.push(None);
                    }
                    shuffle(u, &mut args)?;
                    self.u32(out, args.len() as u32);
                    for arg in args {
                        let fields = match arg {
                            Some(arg) => {
                                self.string(out, &arg.name);
                                match &arg.ty {
                                    ItemTy::CoreInstance(fields) => Rc::clone(fields),
                                    _ => Rc::from([]),
                                }
                            }
                            None => {
                                self.core_name(u, out, Some((&mut used, "")))?;
                                Rc::from([])
                            }
                        };
                        out.push(0x12);
                        let candidates = self.providers_of(&fields);
                        let idx = match candidates.is_empty() {
                            true => self.idx(u, Sort::CoreInstance, 0)?,
                            false => *u.choose(&candidates)?,
                        };
                        self.u32(out, idx);
                    }
                }
                _ => {
                    self.vec(u, out, 0, max, |me, u, out| {
                        me.core_name(u, out, Some((&mut used, "")))?;
                        out.push(0x12);
                        me.index(u, out, Sort::CoreInstance)
                    })?;
                }
            }
            module
        } else {
            out.push(0x01);
            let max = match self.has_any(CORE_EXTERN_SORTS) {
                true => usize::MAX,
                false => 0,
            };
            let mut exports = Vec::new();
            self.vec(u, out, 0, max, |me, u, out| {
                let name = me.core_name(u, out, Some((&mut used, "")))?;
                let sort = me.pick_sort(u, CORE_EXTERN_SORTS)?;
                out.push(sort.core().unwrap());
                let idx = me.idx(u, sort, 0)?;
                me.u32(out, idx);
                let ty = me.item_ty(sort, idx).0;
                exports.push(Item { name, sort, ty });
                Ok(())
            })?;
            let imports = Vec::new();
            Some(Rc::new(Items { imports, exports }))
        };
        self.add_item(Sort::CoreInstance, Kind::Unknown, items);
        Ok(())
    }

    /// The core instances that can be the argument for the module import
    /// `arg`.
    fn providers(&self, arg: &Item) -> Vec<u32> {
        match &arg.ty {
            ItemTy::CoreInstance(fields) => self.providers_of(fields),
            _ => (0..self.len(Sort::CoreInstance)).collect(),
        }
    }

    /// The core instances that export each of `fields`.
    fn providers_of(&self, fields: &[Item]) -> Vec<u32> {
        (0..self.len(Sort::CoreInstance))
            .filter(|i| {
                self.items(0, Sort::CoreInstance, *i)
                    .is_some_and(|items| satisfies(&items.exports, fields))
            })
            .collect()
    }

    fn core_type(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let mut choices = vec![0];
        if self.config.gc_enabled {
            choices.push(1);
        }
        if self.can_nest() {
            choices.push(2);
        }
        match *u.choose(&choices)? {
            0 => self.core_rectype(u, out, false),
            1 => {
                out.extend([0x00, 0x50]);
                let comptype = self.core_comptype_opcode(u)?;
                let supertype = self.core_supertypes(u, out, comptype, &[])?;
                let bytes = self.core_comptype_of(u, comptype, 1, supertype, &[])?.0;
                out.extend(bytes.iter());
                let is_final = false;
                let idx = self.len(Sort::CoreType);
                self.add_core_type(Kind::CoreSub { comptype, is_final }, None);
                self.scope().comptypes.insert(idx, bytes);
                Ok(())
            }
            _ => self.core_moduletype(u, out),
        }
    }

    /// `core:rectype`, where a lone `0x50` subtype is only allowed with
    /// `open_ok` since it is ambiguous with `core:moduletype` otherwise.
    fn core_rectype(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        open_ok: bool,
    ) -> Result<()> {
        // `wast` doesn't parse the `(core rec ...)` that `wasmprinter` prints
        // for these in component and instance types.
        let decl = self.current().decl;
        if self.config.gc_enabled && !decl && u.arbitrary()? {
            out.push(0x4e);
            let n = u.int_in_range(0..=4)?;
            self.u32(out, n);
            let mut group = Vec::new();
            for _ in 0..n {
                let subtype = self.core_subtype(u, out, n, true, &group)?;
                group.push(subtype);
            }
            for subtype in group {
                self.add_core_subtype(subtype);
            }
        } else {
            let subtype = self.core_subtype(u, out, 1, open_ok, &[])?;
            self.add_core_subtype(subtype);
        }
        Ok(())
    }

    fn add_core_subtype(&mut self, (kind, sig, bytes): Subtype) {
        let idx = self.len(Sort::CoreType);
        self.add_core_type(kind, sig);
        self.scope().comptypes.insert(idx, bytes);
    }

    /// `core:subtype` in a recursion group of `group` types, after the subtypes
    /// `earlier` in it.
    fn core_subtype(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        group: u32,
        open_ok: bool,
        earlier: &[Subtype],
    ) -> Result<Subtype> {
        let comptype = self.core_comptype_opcode(u)?;
        let mut is_final = true;
        let mut supertype = None;
        if self.config.gc_enabled {
            match u.int_in_range(0..=2)? {
                0 if open_ok => {
                    out.push(0x50);
                    is_final = false;
                    supertype = self.core_supertypes(u, out, comptype, earlier)?;
                }
                1 => {
                    out.push(0x4f);
                    supertype = self.core_supertypes(u, out, comptype, earlier)?;
                }
                _ => {}
            }
        }
        let (bytes, sig) = self.core_comptype_of(u, comptype, group, supertype, earlier)?;
        out.extend(bytes.iter());
        let sig = sig.filter(|_| is_final && supertype.is_none() && group == 1);
        Ok((Kind::CoreSub { comptype, is_final }, sig, bytes))
    }

    /// The encoded `core:comptype` of a subtype whose opcode is `comptype`,
    /// in a recursion group of `group` types after `earlier`, and the function
    /// type if it is one. When steered, a subtype of `supertype` has the same
    /// structure, which is a valid subtype of it.
    fn core_comptype_of(
        &mut self,
        u: &mut Unstructured,
        comptype: u8,
        group: u32,
        supertype: Option<Option<u32>>,
        earlier: &[Subtype],
    ) -> Result<(Rc<[u8]>, Option<FuncType>)> {
        if let Some(Some(idx)) = supertype {
            let len = self.len(Sort::CoreType);
            let bytes = match idx.checked_sub(len) {
                Some(i) => earlier
                    .get(i as usize)
                    .map(|(_, _, bytes)| Rc::clone(bytes)),
                None => self.current().comptypes.get(&idx).cloned(),
            };
            if let Some(bytes) = bytes {
                return Ok((bytes, None));
            }
        }
        let mut bytes = Vec::new();
        let sig = self.core_comptype(u, &mut bytes, comptype, group)?;
        Ok((bytes.into(), sig))
    }

    /// The supertypes of a subtype whose `core:comptype` has the `comptype`
    /// opcode, after the subtypes `earlier` in its recursion group, which when
    /// steered can only be non-final types with the same opcode. Returns
    /// whether there is one, and which when steered.
    fn core_supertypes(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        comptype: u8,
        earlier: &[Subtype],
    ) -> Result<Option<Option<u32>>> {
        let is_final = false;
        let supertype = Kind::CoreSub { comptype, is_final };
        let len = self.len(Sort::CoreType);
        let scope = self.current();
        let mut supertypes: Vec<u32> = scope
            .indices(Sort::CoreType, |kind| kind == supertype)
            .into_iter()
            .filter(|idx| scope.comptypes.contains_key(idx))
            .collect();
        for (i, (kind, _, _)) in earlier.iter().enumerate() {
            if *kind == supertype {
                supertypes.push(len + i as u32);
            }
        }
        let steer = self.steer(u)?;
        let max = usize::from(!supertypes.is_empty() || (!steer && self.has(Sort::CoreType)));
        let mut chosen = None;
        self.vec(u, out, 0, max, |me, u, out| {
            let idx = match steer {
                true => *u.choose(&supertypes)?,
                false => me.idx(u, Sort::CoreType, 0)?,
            };
            me.u32(out, idx);
            chosen = Some(steer.then_some(idx));
            Ok(())
        })?;
        Ok(chosen)
    }

    fn core_comptype_opcode(&mut self, u: &mut Unstructured) -> Result<u8> {
        if self.config.gc_enabled {
            Ok(*u.choose(&[0x60, 0x5f, 0x5e])?)
        } else {
            Ok(0x60)
        }
    }

    /// `core:comptype`, returning the function type if it is one.
    fn core_comptype(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        comptype: u8,
        group: u32,
    ) -> Result<Option<FuncType>> {
        out.push(comptype);
        match comptype {
            0x60 => {
                let mut types = [Vec::new(), Vec::new()];
                for types in &mut types {
                    self.vec(u, out, 0, usize::MAX, |me, u, out| {
                        let ty = me.core_valtype(u, group)?;
                        ty.encode(out);
                        types.push(ty);
                        Ok(())
                    })?;
                }
                let [params, results] = types;
                Ok(Some(FuncType { params, results }))
            }
            0x5f => {
                self.vec(u, out, 0, usize::MAX, |me, u, out| {
                    me.core_fieldtype(u, out, group)
                })?;
                Ok(None)
            }
            _ => {
                self.core_fieldtype(u, out, group)?;
                Ok(None)
            }
        }
    }

    fn core_fieldtype(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        group: u32,
    ) -> Result<()> {
        match u.int_in_range(0..=3)? {
            0 => out.push(0x78),
            1 => out.push(0x77),
            _ => self.core_valtype(u, group)?.encode(out),
        }
        out.push(u.int_in_range(0..=1)?);
        Ok(())
    }

    /// A `core:valtype`, which may refer to any of the `group` types of the
    /// recursion group being defined.
    fn core_valtype(&mut self, u: &mut Unstructured, group: u32) -> Result<ValType> {
        if self.config.gc_enabled && u.ratio(1, 4)? {
            let len = self.len(Sort::CoreType);
            let mut types = match self.steer(u)? {
                true => self.current().indices(Sort::CoreType, core_sub),
                false => (0..len).collect(),
            };
            types.extend(len..len.saturating_add(group));
            if !types.is_empty() {
                return Ok(ValType::Ref(RefType {
                    nullable: u.arbitrary()?,
                    heap_type: HeapType::Concrete(*u.choose(&types)?),
                }));
            }
        }
        Ok(*u.choose(&self.core_valtypes)?)
    }

    /// A `core:valtype` that validation requires to be `i32` where it is used.
    fn core_valtype_i32(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<ValType> {
        let ty = match self.steer(u)? {
            true => ValType::I32,
            false => self.core_valtype(u, 0)?,
        };
        ty.encode(out);
        Ok(ty)
    }

    fn core_moduletype(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        out.push(0x50);
        self.scopes.push(Scope {
            module: true,
            ..Scope::default()
        });
        let max = self.config.max_types;
        self.vec(u, out, 0, max, Self::core_moduledecl)?;
        let mut scope = self.scopes.pop().unwrap();
        scope.own.imports = mem::take(&mut scope.fields)
            .into_iter()
            .map(|(name, fields)| Item {
                name,
                sort: Sort::CoreInstance,
                ty: ItemTy::CoreInstance(fields.into()),
            })
            .collect();
        let items = Rc::new(scope.own);
        self.add_item(Sort::CoreType, Kind::CoreModule, Some(items));
        Ok(())
    }

    fn core_moduledecl(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let mut choices = vec![0, 1, 3];
        if !self.outer_targets(&[Sort::CoreType]).is_empty() {
            choices.push(2);
        }
        match *u.choose(&choices)? {
            0 => {
                out.push(0x00);
                // Validation requires the module and field name pairs to be
                // unique.
                let module = self.core_name(u, out, None)?;
                let mut used = mem::take(&mut self.scope().imports);
                let prefix = format!("{module}\0");
                let name = self.core_name(u, out, Some((&mut used, &prefix)));
                self.scope().imports = used;
                let name = name?;
                let (sort, ty) = self.core_externtype(u, out)?;
                let item = Item { name, sort, ty };
                let fields = &mut self.scope().fields;
                match fields.iter_mut().find(|(m, _)| *m == module) {
                    Some((_, fields)) => fields.push(item),
                    None => fields.push((module, vec![item])),
                }
                Ok(())
            }
            1 => {
                out.push(0x01);
                self.core_rectype(u, out, true)
            }
            2 => {
                out.extend([0x02, 0x10, 0x01]);
                let (ct, sort, idx) = self.outer(u, out, &[Sort::CoreType])?;
                self.add_outer(ct, sort, idx);
                Ok(())
            }
            _ => {
                out.push(0x03);
                let mut used = mem::take(&mut self.scope().exports);
                let name = self.core_name(u, out, Some((&mut used, "")));
                self.scope().exports = used;
                let name = name?;
                let (sort, ty) = self.core_externtype(u, out)?;
                self.scope().own.exports.push(Item { name, sort, ty });
                Ok(())
            }
        }
    }

    /// `core:externtype`, returning the sort it describes and its type.
    fn core_externtype(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
    ) -> Result<(Sort, ItemTy)> {
        let mut choices = vec![3];
        if self.config.max_tables > 0 {
            choices.push(1);
        }
        if self.config.max_memories > 0 {
            choices.push(2);
        }
        if self.has_kind(Sort::CoreType, core_func) {
            choices.push(0);
            if self.config.exceptions_enabled {
                choices.push(4);
            }
        }
        match *u.choose(&choices)? {
            0 => {
                out.push(0x00);
                let (idx, _) = self.kind_index(u, out, Sort::CoreType, core_func)?;
                let ty = match self.current().core_sigs.get(&idx) {
                    Some(ty) => ItemTy::CoreFunc(Rc::clone(ty)),
                    None => ItemTy::Unknown,
                };
                Ok((Sort::CoreFunc, ty))
            }
            1 => {
                out.push(0x01);
                arbitrary_table_type(u, &self.config, None)?.encode(out);
                Ok((Sort::CoreTable, ItemTy::Unknown))
            }
            2 => {
                out.push(0x02);
                let ty = arbitrary_memtype(u, &self.config)?;
                ty.encode(out);
                let ok = !ty.memory64 && !ty.shared;
                Ok((Sort::CoreMemory, ItemTy::CoreMemory(ok)))
            }
            3 => {
                out.push(0x03);
                GlobalType {
                    val_type: self.core_valtype(u, 0)?,
                    mutable: u.arbitrary()?,
                    shared: false,
                }
                .encode(out);
                Ok((Sort::CoreGlobal, ItemTy::Unknown))
            }
            _ => {
                out.extend([0x04, 0x00]);
                self.kind_index(u, out, Sort::CoreType, core_func)?;
                Ok((Sort::CoreTag, ItemTy::Unknown))
            }
        }
    }

    /// Whether an outer alias of `sort` from `ct` scopes out can refer to an
    /// item of `kind`: types can't refer to resources across scopes, and core
    /// module types can't alias core module types.
    fn outer_ok(&self, ct: usize, sort: Sort, kind: Kind) -> bool {
        match sort {
            Sort::Type => ct == 0 || !resource(kind),
            Sort::CoreType => !self.current().module || kind != Kind::CoreModule,
            _ => true,
        }
    }

    /// The indices that an outer alias of `sort` from `ct` scopes out can
    /// refer to.
    fn outer_indices(&self, ct: usize, sort: Sort) -> Vec<u32> {
        let scope = &self.scopes[self.scopes.len() - 1 - ct];
        // Types that refer to resources can't be aliased into another
        // component.
        let len = self.scopes.len();
        let component = self.scopes[len - ct..].iter().any(|s| !s.decl && !s.module);
        match sort {
            Sort::Type | Sort::CoreType => scope
                .indices(sort, |kind| self.outer_ok(ct, sort, kind))
                .into_iter()
                .filter(|idx| {
                    !component || sort != Sort::Type || !scope.types[*idx as usize].handles()
                })
                .collect(),
            _ => (0..scope.lens[sort as usize]).collect(),
        }
    }

    /// The enclosing scopes, as outer alias `ct`s, and the sorts among `sorts`
    /// that an outer alias can refer to in them.
    fn outer_targets(&self, sorts: &[Sort]) -> Vec<(usize, Sort)> {
        let mut targets = Vec::new();
        for ct in 0..self.scopes.len() {
            for &sort in sorts {
                if !self.outer_indices(ct, sort).is_empty() {
                    targets.push((ct, sort));
                }
            }
        }
        targets
    }

    /// The `ct` and `idx` of an outer alias of one of `sorts`, returning them
    /// and the sort.
    fn outer(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        sorts: &[Sort],
    ) -> Result<(usize, Sort, u32)> {
        let targets = self.outer_targets(sorts);
        let (ct, sort) = if targets.is_empty() {
            (0, *u.choose(sorts)?)
        } else {
            *u.choose(&targets)?
        };
        self.u32(out, ct as u32);
        let indices = self.outer_indices(ct, sort);
        let idx = match indices.is_empty() {
            true => 0,
            false => *u.choose(&indices)?,
        };
        self.u32(out, idx);
        Ok((ct, sort, idx))
    }

    /// Adds the item that an outer alias of `idx` of `sort` from `ct` scopes
    /// out refers to.
    fn add_outer(&mut self, ct: usize, sort: Sort, idx: u32) {
        let items = self.items(ct, sort, idx);
        match sort {
            Sort::Type => {
                let new = self.len(Sort::Type);
                let ty = match ct {
                    0 => self.current().alias(idx, self.type_at(0, idx)),
                    _ => self.type_at(ct, idx),
                };
                self.add_type(ty, items);
                // An alias in the same scope is named if the type is.
                let scope = self.scope();
                if ct == 0 && scope.imported.contains(&idx) {
                    scope.imported.insert(new);
                }
                if ct == 0 && scope.exported.contains(&idx) {
                    scope.exported.insert(new);
                }
            }
            Sort::CoreType => {
                let scope = &self.scopes[self.scopes.len() - 1 - ct];
                let sig = scope.core_sigs.get(&idx).map(|sig| (**sig).clone());
                self.add_core_type(self.kind_at(ct, sort, idx), sig);
                if let Some(items) = items {
                    let idx = self.len(Sort::CoreType) - 1;
                    self.scope().items.insert((sort, idx), items);
                }
            }
            _ => self.add_item(sort, Kind::Unknown, items),
        }
    }

    fn instance(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let max = if self.has_any(EXTERN_SORTS) {
            usize::MAX
        } else {
            0
        };
        let mut used = HashSet::new();
        let steer = self.steer(u)?;
        let mut component = None;
        if self.has(Sort::Component) && u.arbitrary()? {
            let c = self.idx(u, Sort::Component, 0)?;
            let items = self.items(0, Sort::Component, c);
            // When steered, only components whose imports can all be passed.
            let ok = match &items {
                Some(items) if steer => items.imports.iter().all(|arg| {
                    self.has(arg.sort)
                        && match &arg.ty {
                            ItemTy::Func(func) if func.desc.is_some() => {
                                !self.matching(arg.sort, &arg.ty).is_empty()
                            }
                            _ => true,
                        }
                }),
                _ => true,
            };
            if ok {
                component = Some((c, items));
            }
        }
        let items = if let Some((c, component)) = component {
            out.push(0x00);
            self.u32(out, c);
            match &component {
                // An argument for each import, of its sort and type.
                Some(component) if steer => {
                    let mut args: Vec<Option<&Item>> = component.imports.iter().map(Some).collect();
                    used.extend(component.imports.iter().map(|item| canonical(&item.name)));
                    while max > 0 && !more(u, 8)? && !u.is_empty() {
                        args.push(None);
                    }
                    shuffle(u, &mut args)?;
                    self.u32(out, args.len() as u32);
                    for arg in args {
                        match arg {
                            Some(arg) => {
                                self.string(out, &arg.name);
                                let sort =
                                    match self.has(arg.sort) && EXTERN_SORTS.contains(&arg.sort) {
                                        true => arg.sort,
                                        false => self.pick_sort(u, EXTERN_SORTS)?,
                                    };
                                sort.encode(out);
                                let candidates = self.matching(sort, &arg.ty);
                                let idx = self.steered(u, sort, &candidates)?;
                                self.u32(out, idx);
                            }
                            None => {
                                let name = self.pooled_name(u, Names::All)?;
                                let name = self.unique(u, name, &mut used)?;
                                self.string(out, &name);
                                self.sortidx(u, out)?;
                            }
                        }
                    }
                }
                _ => {
                    self.vec(u, out, 0, max, |me, u, out| {
                        let name = me.pooled_name(u, Names::All)?;
                        let name = me.unique(u, name, &mut used)?;
                        me.string(out, &name);
                        me.sortidx(u, out)?;
                        Ok(())
                    })?;
                }
            }
            component
        } else {
            out.push(0x01);
            let mut exports = Vec::new();
            self.vec(u, out, 0, max, |me, u, out| {
                let mut idx = Vec::new();
                let (sort, i) = me.sortidx(u, &mut idx)?;
                let name = me.nameattributes(u, out, &mut used, sort, None)?;
                let ty = me.item_ty(sort, i).0;
                exports.push(Item { name, sort, ty });
                out.extend(idx);
                Ok(())
            })?;
            let imports = Vec::new();
            Some(Rc::new(Items { imports, exports }))
        };
        self.add_item(Sort::Instance, Kind::Unknown, items);
        Ok(())
    }

    /// The indices of `sort` whose type matches `ty`, if that is known.
    fn matching(&self, sort: Sort, ty: &ItemTy) -> Vec<u32> {
        let scope = self.current();
        match (sort, ty) {
            (Sort::Func, ItemTy::Func(func)) if func.desc.is_some() => scope
                .funcs
                .iter()
                .filter(|(_, (f, _))| f.desc == func.desc)
                .map(|(idx, _)| *idx)
                .collect(),
            (Sort::Type, ItemTy::Type(ty)) => match ty.kind {
                Kind::Resource { .. } => scope.indices(Sort::Type, resource),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    fn sortidx(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<(Sort, u32)> {
        let sort = self.pick_sort(u, EXTERN_SORTS)?;
        sort.encode(out);
        let idx = self.idx(u, sort, 0)?;
        self.u32(out, idx);
        Ok((sort, idx))
    }

    fn alias_sorts(&self) -> (&'static [Sort], &'static [Sort]) {
        if self.current().decl {
            (DECL_EXPORT_ALIAS_SORTS, DECL_OUTER_ALIAS_SORTS)
        } else {
            (EXTERN_SORTS, OUTER_ALIAS_SORTS)
        }
    }

    /// Whether an alias can refer to something, which when `steer`ed must be
    /// a known export.
    fn can_alias(&self, steer: bool) -> bool {
        let (exports, outer) = self.alias_sorts();
        let instances = match steer {
            true => {
                !self.exporters(Sort::Instance, exports).is_empty()
                    || (!self.current().decl
                        && !self
                            .exporters(Sort::CoreInstance, CORE_EXTERN_SORTS)
                            .is_empty())
            }
            false => self.has(Sort::Instance) || self.has(Sort::CoreInstance),
        };
        instances || !self.outer_targets(outer).is_empty()
    }

    fn alias(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let (export_sorts, outer_sorts) = self.alias_sorts();
        let decl = self.current().decl;
        let steer = self.steer(u)?;
        // When steered, only instances whose exports are known.
        let exporters = |instance: Sort, sorts: &[Sort]| match steer {
            true => !self.exporters(instance, sorts).is_empty(),
            false => self.has(instance),
        };
        let mut targets = Vec::new();
        if exporters(Sort::Instance, export_sorts) {
            targets.push(0x00);
        }
        if !decl && exporters(Sort::CoreInstance, CORE_EXTERN_SORTS) {
            targets.push(0x01);
        }
        if !self.outer_targets(outer_sorts).is_empty() {
            targets.push(0x02);
        }
        if targets.is_empty() {
            if self.has(Sort::Instance) {
                targets.push(0x00);
            }
            if !decl && self.has(Sort::CoreInstance) {
                targets.push(0x01);
            }
            if targets.is_empty() {
                targets.push(0x02);
            }
        }
        let target = *u.choose(&targets)?;
        let mut body = Vec::new();
        let sort = match target {
            0x00 | 0x01 => {
                let (instance, sorts) = match target {
                    0x00 => (Sort::Instance, export_sorts),
                    _ => (Sort::CoreInstance, CORE_EXTERN_SORTS),
                };
                let mut exporters = self.exporters(instance, sorts);
                // Lifting and lowering need a memory, which only these aliases
                // provide.
                let memories = self.exporters(instance, &[Sort::CoreMemory]);
                if !self.has(Sort::CoreMemory) && !memories.is_empty() && u.arbitrary()? {
                    exporters = memories;
                }
                let (idx, sort, ty) = if !exporters.is_empty() && steer {
                    let (idx, exports) = u.choose(&exporters)?;
                    let item = u.choose(exports)?;
                    self.u32(&mut body, *idx);
                    self.string(&mut body, &item.name);
                    (*idx, item.sort, item.ty.clone())
                } else {
                    let idx = self.idx(u, instance, 0)?;
                    self.u32(&mut body, idx);
                    if target == 0x00 {
                        let name = self.pooled_name(u, Names::All)?;
                        self.string(&mut body, &name);
                    } else {
                        self.core_name(u, &mut body, None)?;
                    }
                    (idx, *u.choose(sorts)?, ItemTy::Unknown)
                };
                let new = self.len(sort);
                self.add_typed(sort, &ty, Rc::from([]), None);
                // The types that imported instances export are named.
                let scope = self.scope();
                if sort == Sort::Type && target == 0x00 && scope.imported_instances.contains(&idx) {
                    scope.imported.insert(new);
                }
                sort
            }
            _ => {
                let (ct, sort, idx) = self.outer(u, &mut body, outer_sorts)?;
                self.add_outer(ct, sort, idx);
                sort
            }
        };
        sort.encode(out);
        out.push(target);
        out.extend(body);
        Ok(())
    }

    fn ty(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let (ty, items) = self.deftype(u, out)?;
        self.add_type(ty, items.map(Rc::new));
        Ok(())
    }

    /// `deftype`, returning it and what component and instance types import
    /// and export.
    fn deftype(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
    ) -> Result<(Type, Option<Items>)> {
        let mut choices = vec![0, 1];
        // Resources can only be defined in components.
        if !self.current().decl {
            choices.push(2);
        }
        if self.can_nest() {
            choices.extend([3, 4]);
        }
        Ok(match *u.choose(&choices)? {
            0 => (self.defvaltype(u, out)?, None),
            1 => (self.functype(u, out)?, None),
            2 => {
                self.resourcetype(u, out)?;
                (Type::of(Kind::Resource { local: true }), None)
            }
            3 => {
                let items = self.decls(u, out, 0x41, Self::componentdecl)?;
                (Type::of(Kind::Component), Some(items))
            }
            _ => {
                let items = self.decls(u, out, 0x42, Self::instancedecl)?;
                (Type::of(Kind::Instance), Some(items))
            }
        })
    }

    fn primvaltype(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<u8> {
        let ty = *u.choose(&[
            0x7f, 0x7e, 0x7d, 0x7c, 0x7b, 0x7a, 0x79, 0x78, 0x77, 0x76, 0x75, 0x74, 0x73, 0x64,
        ])?;
        out.push(ty);
        Ok(ty)
    }

    fn defvaltype(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<Type> {
        // `None` stands for any `primvaltype`.
        let mut ops = vec![
            None,
            Some(0x72),
            Some(0x71),
            Some(0x70),
            Some(0x67),
            Some(0x6f),
            Some(0x6e),
            Some(0x6d),
            Some(0x6b),
            Some(0x6a),
            Some(0x66),
            Some(0x65),
            Some(0x63),
        ];
        if self.has_kind(Sort::Type, resource) {
            ops.extend([Some(0x69), Some(0x68)]);
        }
        let Some(op) = *u.choose(&ops)? else {
            let ty = self.primvaltype(u, out)?;
            return Ok(Type {
                val: Some(Val::prim(ty)),
                ..Type::of(Kind::Value)
            });
        };
        out.push(op);
        let kind = match op {
            0x66 => Kind::Stream,
            0x65 => Kind::Future,
            _ => Kind::Value,
        };
        let mut labels = HashSet::new();
        let mut needs = Vec::new();
        // The labels of the parts and their value types, if they have one,
        // which are only used if they're all known.
        let mut parts: Vec<(String, Option<Rc<Val>>)> = Vec::new();
        let mut known = true;
        let mut len = 0;
        match op {
            0x72 => self.vec(u, out, 1, usize::MAX, |me, u, out| {
                let label = me.labellit(u, out, &mut labels)?;
                let val = me.part(u, out, &mut needs, &mut known)?;
                parts.push((label, val));
                Ok(())
            })?,
            0x71 => self.vec(u, out, 1, usize::MAX, |me, u, out| {
                let label = me.labellit(u, out, &mut labels)?;
                let val = me.optional(u, out, true, |me, u, out| {
                    me.part(u, out, &mut needs, &mut known)
                })?;
                parts.push((label, val.flatten()));
                out.push(0x00);
                Ok(())
            })?,
            0x67 => {
                let val = self.part(u, out, &mut needs, &mut known)?;
                parts.push((String::new(), val));
                len = u.int_in_range(1..=16)?;
                self.u32(out, len);
            }
            0x6f => self.vec(u, out, 1, usize::MAX, |me, u, out| {
                let val = me.part(u, out, &mut needs, &mut known)?;
                parts.push((String::new(), val));
                Ok(())
            })?,
            0x6e | 0x6d => {
                let max = if op == 0x6e { 32 } else { usize::MAX };
                self.vec(u, out, 1, max, |me, u, out| {
                    let label = me.labellit(u, out, &mut labels)?;
                    parts.push((label, None));
                    Ok(())
                })?
            }
            0x6a | 0x66 | 0x65 => {
                let labels: &[&str] = if op == 0x6a { &["ok", "err"] } else { &[""] };
                // Payloads can't contain borrows, nor be `char`s in streams.
                let steer = op != 0x6a && self.steer(u)?;
                let prev = (self.no_borrows, self.no_chars);
                self.no_borrows |= steer;
                self.no_chars = op == 0x66 && steer;
                for label in labels {
                    let val = self.optional(u, out, true, |me, u, out| {
                        me.part(u, out, &mut needs, &mut known)
                    })?;
                    parts.push((label.to_string(), val.flatten()));
                }
                (self.no_borrows, self.no_chars) = prev;
            }
            0x63 => {
                // `keytype`
                let key = *u.choose(&[
                    0x7f, 0x7e, 0x7d, 0x7c, 0x7b, 0x7a, 0x79, 0x78, 0x77, 0x74, 0x73,
                ])?;
                out.push(key);
                parts.push((String::new(), Some(Val::prim(key))));
                let val = self.part(u, out, &mut needs, &mut known)?;
                parts.push((String::new(), val));
            }
            0x69 | 0x68 => {
                let (idx, _) = self.kind_index(u, out, Sort::Type, resource)?;
                needs.push(idx);
            }
            // `list` and `option`
            _ => {
                let val = self.part(u, out, &mut needs, &mut known)?;
                parts.push((String::new(), val));
            }
        }
        let val = known.then(|| match op {
            0x69 | 0x68 => Rc::new(Val {
                handle: Some((op == 0x69, needs[0])),
                ..(*defval(op, &parts, len)).clone()
            }),
            _ => defval(op, &parts, len),
        });
        Ok(Type {
            val,
            requires: needs.into(),
            ..Type::of(kind)
        })
    }

    /// A `valtype` that is part of a defined value type which needs `needs`
    /// named, and is only `known` if this is.
    fn part(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        needs: &mut Vec<u32>,
        known: &mut bool,
    ) -> Result<Option<Rc<Val>>> {
        let (val, n) = self.valtype(u, out)?;
        needs.extend(n);
        *known &= val.is_some();
        Ok(val)
    }

    /// `valtype`, whose `typeidx` is a non-negative `s33` so that it doesn't
    /// overlap the type opcodes, returning the value type if it's known and the
    /// types that must be named for imports and exports to refer to it.
    fn valtype(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
    ) -> Result<(Option<Rc<Val>>, Vec<u32>)> {
        let scope = self.current();
        let mut types = match self.steer(u)? {
            true => scope.indices(Sort::Type, defined),
            false => (0..self.len(Sort::Type)).collect(),
        };
        if self.exportable {
            types.retain(|ty| scope.named(&scope.needs(*ty), false));
        }
        if self.no_borrows || self.no_ptrs {
            types.retain(|ty| {
                let val = &scope.types[*ty as usize].val;
                !val.as_ref().is_some_and(|val| {
                    (self.no_borrows && val.borrows)
                        || (self.no_ptrs && (val.ptr || val.flat.len() > MAX_FLAT_PARAMS))
                })
            });
        }
        if !types.is_empty() && u.arbitrary()? {
            let idx = *u.choose(&types)?;
            i64::from(idx).encode(out);
            let scope = self.current();
            Ok((scope.types[idx as usize].val.clone(), scope.needs(idx)))
        } else {
            let mut ty = self.primvaltype(u, out)?;
            if (self.no_chars && ty == 0x74) || (self.no_ptrs && ty == 0x73) {
                ty = 0x7d;
                *out.last_mut().unwrap() = ty;
            }
            Ok((Some(Val::prim(ty)), Vec::new()))
        }
    }

    fn resourcetype(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        out.push(0x3f);
        let rep = self.core_valtype_i32(u, out)?;
        let ty = FuncType {
            params: vec![rep],
            results: Vec::new(),
        };
        let dtors = match self.steer(u)? {
            true => self.current().core_funcs_of(&ty),
            false => (0..self.len(Sort::CoreFunc)).collect(),
        };
        self.optional(u, out, !dtors.is_empty(), |me, u, out| {
            me.u32(out, *u.choose(&dtors)?);
            Ok(())
        })?;
        Ok(())
    }

    /// `functype`. When steered, its first parameter is sometimes a `self`
    /// that borrows a resource and its result sometimes owns one, as methods
    /// and constructors need.
    fn functype(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<Type> {
        let is_async = u.ratio(1, 4)?;
        out.push(if is_async { 0x43 } else { 0x40 });
        let handles = |own: bool| -> Vec<u32> {
            let scope = self.current();
            (0..scope.types.len() as u32)
                .filter(|idx| {
                    let ty = &scope.types[*idx as usize];
                    ty.val
                        .as_ref()
                        .is_some_and(|val| val.handle.is_some_and(|(o, _)| o == own))
                        && (!self.exportable || scope.named(&scope.needs(*idx), false))
                })
                .collect()
        };
        let (borrows, owns) = match self.steer(u)? {
            true => (handles(false), handles(true)),
            false => (Vec::new(), Vec::new()),
        };
        let mut labels = HashSet::new();
        let mut params = Vec::new();
        let mut needs = Vec::new();
        self.vec(u, out, 0, usize::MAX, |me, u, out| {
            let (label, (val, n)) = if params.is_empty() && !borrows.is_empty() && u.ratio(1, 4)? {
                me.string(out, "self");
                labels.insert(canonical("self"));
                let idx = *u.choose(&borrows)?;
                i64::from(idx).encode(out);
                let scope = me.current();
                let val = scope.types[idx as usize].val.clone();
                ("self".to_string(), (val, scope.needs(idx)))
            } else {
                let label = me.labellit(u, out, &mut labels)?;
                (label, me.valtype(u, out)?)
            };
            needs.extend(n);
            params.push((label, val));
            Ok(())
        })?;
        let result = if !owns.is_empty() && u.ratio(1, 4)? {
            out.push(0x00);
            let idx = *u.choose(&owns)?;
            i64::from(idx).encode(out);
            let scope = self.current();
            Some((scope.types[idx as usize].val.clone(), scope.needs(idx)))
        } else {
            self.resultlist(u, out)?
        };
        let result = match result {
            Some((val, n)) => {
                needs.extend(n);
                Some(val)
            }
            None => None,
        };
        let params: Option<Vec<_>> = params
            .into_iter()
            .map(|(label, val)| Some((label, val?)))
            .collect();
        let func = match (params, result) {
            (Some(params), Some(Some(result))) => Some(Func::new(params, Some(result), is_async)),
            (Some(params), None) => Some(Func::new(params, None, is_async)),
            _ => None,
        };
        Ok(Type {
            func: func.map(Rc::new),
            requires: needs.into(),
            ..Type::of(Kind::Func)
        })
    }

    /// `resultlist`, returning the result's value type, if it's known, and
    /// the types it needs named.
    fn resultlist(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
    ) -> Result<Option<(Option<Rc<Val>>, Vec<u32>)>> {
        if u.arbitrary()? {
            out.push(0x00);
            // Results can't contain borrows.
            let steer = self.steer(u)?;
            let prev = mem::replace(&mut self.no_borrows, steer);
            let result = self.valtype(u, out)?;
            self.no_borrows = prev;
            Ok(Some(result))
        } else {
            out.extend([0x01, 0x00]);
            Ok(None)
        }
    }

    /// `componenttype` or `instancetype` with declarations emitted by `decl`,
    /// returning what it imports and exports.
    fn decls(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        opcode: u8,
        decl: fn(&mut Self, &mut Unstructured, &mut Vec<u8>) -> Result<()>,
    ) -> Result<Items> {
        out.push(opcode);
        self.scopes.push(Scope {
            decl: true,
            instance: opcode == 0x42,
            ..Scope::default()
        });
        let max = self.config.max_types;
        self.vec(u, out, 0, max, decl)?;
        Ok(self.scopes.pop().unwrap().own.outside())
    }

    fn componentdecl(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        if u.arbitrary()? {
            out.push(0x03);
            self.externdecl(u, out, true)
        } else {
            self.instancedecl(u, out)
        }
    }

    fn instancedecl(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let mut choices = vec![0x00, 0x01, 0x04];
        if self.can_alias(self.steer(u)?) {
            choices.push(0x02);
        }
        let choice = *u.choose(&choices)?;
        out.push(choice);
        match choice {
            0x00 => self.core_type(u, out),
            0x01 => self.ty(u, out),
            0x02 => self.alias(u, out),
            _ => self.externdecl(u, out, false),
        }
    }

    /// `import`, `importdecl` or `exportdecl`.
    fn externdecl(&mut self, u: &mut Unstructured, out: &mut Vec<u8>, import: bool) -> Result<()> {
        let mut encoded = Vec::new();
        let (sort, ty, requires, items) = self.externtype(u, &mut encoded, import)?;
        self.scope_nameattributes(u, out, import, sort, ty.clone())?;
        out.extend(encoded);
        let idx = self.len(sort);
        self.add_typed(sort, &ty, requires, items);
        let scope = self.scope();
        match (sort, import) {
            (Sort::Type, true) => {
                scope.imported.insert(idx);
                let same = scope.same(idx);
                scope.exported_only.remove(&same);
            }
            (Sort::Type, false) => {
                scope.exported.insert(idx);
                if scope.types[idx as usize].same.is_some() {
                    let same = scope.same(idx);
                    scope.exported_only.insert(same);
                }
            }
            (Sort::Instance, true) => {
                scope.imported_instances.insert(idx);
            }
            _ => {}
        }
        Ok(())
    }

    /// The types that an import (or export) can bound with `eq`: those whose
    /// parts are named, and which imports can refer to.
    fn bounds(&self, import: bool) -> Vec<u32> {
        let scope = self.current();
        (0..scope.types.len() as u32)
            .filter(|idx| {
                let ty = &scope.types[*idx as usize];
                let exported = (scope.exported.contains(idx) && !scope.imported.contains(idx))
                    || scope.exported_only.contains(&scope.same(*idx));
                ty.kind != (Kind::Resource { local: true })
                    && (scope.instance || !(import && exported))
                    && scope.named(&ty.requires, import)
            })
            .collect()
    }

    /// `externtype`, returning the sort it describes, its type, the types
    /// that must be named for an import or export of a function, and what it
    /// imports and exports where that's known.
    fn externtype(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        import: bool,
    ) -> Result<(Sort, ItemTy, Rc<[u32]>, Option<Rc<Items>>)> {
        let steer = self.steer(u)?;
        let scope = self.current();
        // The type indices that each `externtype` opcode can refer to.
        let candidates = |kind: Kind| -> Vec<u32> {
            let indices = scope.indices(Sort::Type, |k| k == kind).into_iter();
            match steer {
                true => indices
                    .filter(|idx| {
                        let ty = &scope.types[*idx as usize];
                        kind != Kind::Func || scope.named(&ty.requires, import)
                    })
                    .collect(),
                false => indices.collect(),
            }
        };
        let types = [
            (0x01, Sort::Func, candidates(Kind::Func)),
            (0x04, Sort::Component, candidates(Kind::Component)),
            (0x05, Sort::Instance, candidates(Kind::Instance)),
        ];
        let bounds = match steer {
            true => self.bounds(import),
            false => (0..self.len(Sort::Type)).collect(),
        };
        let mut ops = vec![0x03];
        if self.has_kind(Sort::CoreType, |kind| kind == Kind::CoreModule) {
            ops.push(0x00);
        }
        for (op, _, candidates) in &types {
            if !candidates.is_empty() {
                ops.push(*op);
            }
        }
        let op = *u.choose(&ops)?;
        out.push(op);
        let none = Rc::from([]);
        Ok(match op {
            0x00 => {
                out.push(0x11);
                let module = |k| k == Kind::CoreModule;
                let (idx, _) = self.kind_index(u, out, Sort::CoreType, module)?;
                let items = self.items(0, Sort::CoreType, idx);
                (Sort::CoreModule, ItemTy::Unknown, none, items)
            }
            0x03 => {
                if !bounds.is_empty() && u.arbitrary()? {
                    out.push(0x00);
                    let idx = *u.choose(&bounds)?;
                    self.u32(out, idx);
                    let ty = self.current().alias(idx, self.type_at(0, idx));
                    (Sort::Type, ItemTy::Type(ty), none, None)
                } else {
                    out.push(0x01);
                    let ty = ItemTy::Type(Type::of(Kind::Resource { local: false }));
                    (Sort::Type, ty, none, None)
                }
            }
            _ => {
                let (_, sort, candidates) = types.iter().find(|(o, _, _)| *o == op).unwrap();
                let idx = *u.choose(candidates)?;
                self.u32(out, idx);
                let ty = self.type_at(0, idx);
                let items = self.items(0, Sort::Type, idx);
                match ty.func {
                    Some(func) if *sort == Sort::Func => {
                        (*sort, ItemTy::Func(func), ty.requires, items)
                    }
                    _ => (*sort, ItemTy::Unknown, none, items),
                }
            }
        })
    }

    fn canon(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        const NONE: &[u8] = &[0x24, 0x25, 0x05, 0x0d, 0x1e, 0x1f, 0x22, 0x23, 0x26, 0x28];
        const ZERO: &[u8] = &[0x29, 0x0c, 0x2a, 0x2b, 0x2c, 0x2d];
        const TYPED: &[u8] = &[
            0x02, 0x03, 0x04, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x2e, 0x15, 0x16, 0x17,
            0x18, 0x19, 0x1a, 0x1b, 0x2f,
        ];

        /// The kinds of the `typeidx` that the built-in `op` takes.
        fn type_kind(op: u8) -> fn(Kind) -> bool {
            match op {
                0x02 | 0x04 => |kind| kind == Kind::Resource { local: true },
                0x03 => resource,
                0x0e..=0x14 | 0x2e => |kind| kind == Kind::Stream,
                _ => |kind| kind == Kind::Future,
            }
        }

        let steer = self.steer(u)?;
        let lifts = match steer {
            true => self.lift_choices(),
            false => Vec::new(),
        };
        // Lifting and lowering are the bulk of real components, so pick them
        // half of the time.
        let mut lift_lower = Vec::new();
        if match steer {
            true => !lifts.is_empty(),
            false => self.has(Sort::CoreFunc) && self.has_kind(Sort::Type, |k| k == Kind::Func),
        } {
            lift_lower.push(0x00);
        }
        if match steer {
            true => !self.lowerable().is_empty(),
            false => self.has(Sort::Func),
        } {
            lift_lower.push(0x01);
        }
        let op = if !lift_lower.is_empty() && u.arbitrary()? {
            *u.choose(&lift_lower)?
        } else {
            let mut ops = lift_lower;
            ops.extend(NONE.iter().chain(ZERO));
            ops.extend([0x09, 0x0a, 0x0b, 0x06]);
            // These need a memory, and `debug-message` a `realloc` too.
            if !steer || self.has(Sort::CoreMemory) {
                ops.push(0x1c);
            }
            let reallocs = self.current().core_funcs_of(&realloc_type());
            if !steer || (self.has(Sort::CoreMemory) && !reallocs.is_empty()) {
                ops.push(0x1d);
            }
            for &op in TYPED {
                if self.has_kind(Sort::Type, type_kind(op)) {
                    ops.push(op);
                }
            }
            if self.has(Sort::CoreMemory) {
                ops.extend([0x20, 0x21]);
            }
            // Reading and writing values needs a memory.
            if steer && !self.has(Sort::CoreMemory) {
                ops.retain(|op| !matches!(op, 0x0f | 0x10 | 0x16 | 0x17));
            }
            if self.has_kind(Sort::CoreType, core_func) && self.has(Sort::CoreTable) {
                ops.push(0x27);
            }
            *u.choose(&ops)?
        };

        out.push(op);
        let mut payload = None;
        if TYPED.contains(&op) {
            let (idx, _) = self.kind_index(u, out, Sort::Type, type_kind(op))?;
            let val = self
                .current()
                .types
                .get(idx as usize)
                .and_then(|ty| ty.val.clone());
            payload = val.map(|val| val.payload.clone());
        }
        let mut ty = builtin_type(op);
        match op {
            0x00 => {
                out.push(0x00);
                if !lifts.is_empty() {
                    let (idx, core_func, abi) = *u.choose(&lifts)?;
                    self.u32(out, core_func);
                    let ty = self.current().types[idx as usize].clone();
                    let func = ty.func.unwrap();
                    self.canon_opts(u, out, &func.lift(abi), abi, Opts::Lift)?;
                    self.u32(out, idx);
                    self.add_func(Some(func), ty.requires);
                } else {
                    self.index(u, out, Sort::CoreFunc)?;
                    self.opts(u, out, Opts::Lift)?;
                    let func = |kind| kind == Kind::Func;
                    let (idx, _) = self.kind_index(u, out, Sort::Type, func)?;
                    let ty = self.type_at(0, idx);
                    self.add_func(ty.func, ty.requires);
                }
                return Ok(());
            }
            0x01 => {
                out.push(0x00);
                let known = self.lowerable();
                let func = match steer && !known.is_empty() {
                    true => *u.choose(&known)?,
                    false => self.idx(u, Sort::Func, 0)?,
                };
                self.u32(out, func);
                ty = None;
                let known = self
                    .current()
                    .funcs
                    .get(&func)
                    .map(|(func, _)| Rc::clone(func));
                match known {
                    Some(func) if steer => {
                        let is_async =
                            func.is_async && self.can_lower(&func.lower(true)) && u.arbitrary()?;
                        let lowered = func.lower(is_async);
                        let abi = if is_async { Abi::Async } else { Abi::Sync };
                        self.canon_opts(u, out, &lowered, abi, Opts::Lower)?;
                        ty = Some(lowered.ty);
                    }
                    _ => self.opts(u, out, Opts::Lower)?,
                }
            }
            0x09 => {
                // Without a memory, results can't be passed in memory.
                let no_ptrs = steer && !self.has(Sort::CoreMemory);
                let prev = mem::replace(&mut self.no_ptrs, no_ptrs);
                let result = self.resultlist(u, out)?;
                self.no_ptrs = prev;
                let params = match result {
                    Some((Some(val), _)) => Some(vec![(String::new(), val)]),
                    Some((None, _)) => None,
                    None => Some(Vec::new()),
                };
                match params {
                    Some(params) if steer => {
                        let lowered = Func::new(params, None, false).lower(false);
                        self.canon_opts(u, out, &lowered, Abi::Sync, Opts::Sync)?;
                        ty = Some(lowered.ty);
                    }
                    _ => self.opts(u, out, Opts::Sync)?,
                }
            }
            0x0a | 0x0b => {
                let val = self.core_valtype_i32(u, out)?;
                let slot = u.int_in_range(0..=1)?;
                self.u32(out, slot);
                let (params, results) = match op {
                    0x0a => (Vec::new(), vec![val]),
                    _ => (vec![val], Vec::new()),
                };
                ty = Some(FuncType { params, results });
            }
            // `async?`
            0x06 | 0x11 | 0x12 | 0x18 | 0x19 => out.push(u.int_in_range(0..=1)?),
            0x0f | 0x10 | 0x16 | 0x17 => match payload {
                Some(payload) if steer => {
                    let needs = Lowered {
                        ty: FuncType {
                            params: Vec::new(),
                            results: Vec::new(),
                        },
                        memory: payload.is_some(),
                        realloc: payload.is_some_and(|payload| payload.ptr),
                    };
                    let abi = match u.arbitrary()? {
                        true => Abi::Async,
                        false => Abi::Sync,
                    };
                    self.canon_opts(u, out, &needs, abi, Opts::Lower)?;
                }
                _ => self.opts(u, out, Opts::Lower)?,
            },
            // `error-context.new` needs a memory, and `debug-message` a
            // `realloc` too.
            0x1c | 0x1d if steer => {
                let needs = Lowered {
                    ty: FuncType {
                        params: Vec::new(),
                        results: Vec::new(),
                    },
                    memory: true,
                    realloc: op == 0x1d,
                };
                let allowed = if op == 0x1d { Opts::Lower } else { Opts::Sync };
                self.canon_opts(u, out, &needs, Abi::Sync, allowed)?;
            }
            0x1c | 0x1d => self.opts(u, out, Opts::Sync)?,
            0x20 | 0x21 => {
                out.push(0x00);
                self.index(u, out, Sort::CoreMemory)?;
            }
            0x27 => {
                self.kind_index(u, out, Sort::CoreType, core_func)?;
                self.index(u, out, Sort::CoreTable)?;
            }
            op if ZERO.contains(&op) => out.push(0x00),
            _ => {}
        }
        self.add_core_func(ty);
        Ok(())
    }

    /// Whether the scope has what lowering with `lowered` needs.
    fn can_lower(&self, lowered: &Lowered) -> bool {
        let scope = self.current();
        (!lowered.memory || self.has(Sort::CoreMemory))
            && (!lowered.realloc || !scope.core_funcs_of(&realloc_type()).is_empty())
    }

    /// The functions whose type is known that the scope can lower
    /// synchronously.
    fn lowerable(&self) -> Vec<u32> {
        let scope = self.current();
        scope
            .funcs
            .iter()
            .filter(|(_, (func, _))| self.can_lower(&func.lower(false)))
            .map(|(idx, _)| *idx)
            .collect()
    }

    /// The function types, core functions and ABIs that lifting can pair,
    /// with what their canonical options need.
    fn lift_choices(&self) -> Vec<(u32, u32, Abi)> {
        let scope = self.current();
        let reallocs = !scope.core_funcs_of(&realloc_type()).is_empty();
        let callbacks = !scope.core_funcs_of(&callback_type()).is_empty();
        let mut choices = Vec::new();
        for (idx, ty) in scope.types.iter().enumerate() {
            let Some(func) = &ty.func else { continue };
            for abi in [Abi::Sync, Abi::Async, Abi::Callback] {
                if (abi != Abi::Sync && !func.is_async) || (abi == Abi::Callback && !callbacks) {
                    continue;
                }
                let lowered = func.lift(abi);
                if (lowered.memory && !self.has(Sort::CoreMemory)) || (lowered.realloc && !reallocs)
                {
                    continue;
                }
                for core_func in scope.core_funcs_of(&lowered.ty) {
                    choices.push((idx as u32, core_func, abi));
                }
            }
        }
        choices
    }

    /// `opts` with the `canonopt`s that lifting or lowering with `abi` needs,
    /// and others that `allowed` allows, with core functions of the right
    /// types where there are some.
    fn canon_opts(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        lowered: &Lowered,
        abi: Abi,
        allowed: Opts,
    ) -> Result<()> {
        let scope = self.current();
        let memories: Vec<u32> = scope
            .memories
            .iter()
            .filter(|(_, ok)| **ok)
            .map(|(idx, _)| *idx)
            .collect();
        let reallocs = scope.core_funcs_of(&realloc_type());
        let callbacks = scope.core_funcs_of(&callback_type());
        let post_returns = scope.core_funcs_of(&FuncType {
            params: lowered.ty.results.clone(),
            results: Vec::new(),
        });
        let mut opts = Vec::new();
        let mut opt = |me: &Self, u: &mut Unstructured, opt: u8, sort: Sort, candidates: &[u32]| {
            let mut encoded = vec![opt];
            let idx = me.steered(u, sort, candidates)?;
            me.u32(&mut encoded, idx);
            opts.push(encoded);
            Result::Ok(())
        };
        let memory = lowered.memory || (self.has(Sort::CoreMemory) && u.arbitrary()?);
        if memory {
            opt(self, u, 0x03, Sort::CoreMemory, &memories)?;
            let realloc = lowered.realloc || (!reallocs.is_empty() && u.arbitrary()?);
            if realloc && allowed != Opts::Sync {
                opt(self, u, 0x04, Sort::CoreFunc, &reallocs)?;
            }
        }
        if abi == Abi::Callback {
            opt(self, u, 0x07, Sort::CoreFunc, &callbacks)?;
        }
        if allowed == Opts::Lift && abi == Abi::Sync && !post_returns.is_empty() && u.arbitrary()? {
            opt(self, u, 0x05, Sort::CoreFunc, &post_returns)?;
        }
        if abi != Abi::Sync {
            opts.push(vec![0x06]);
        }
        if u.arbitrary()? {
            opts.push(vec![u.int_in_range(0x00..=0x02)?]);
        }
        self.write_opts(u, out, opts)
    }

    /// `opts` made of the encoded `canonopt`s `opts`, in any order.
    fn write_opts(
        &self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        mut opts: Vec<Vec<u8>>,
    ) -> Result<()> {
        shuffle(u, &mut opts)?;
        self.u32(out, opts.len() as u32);
        out.extend(opts.into_iter().flatten());
        Ok(())
    }

    /// `opts`, with the combinations of `canonopt`s that validation allows.
    fn opts(&mut self, u: &mut Unstructured, out: &mut Vec<u8>, allowed: Opts) -> Result<()> {
        // `None` stands for any `string-encoding`.
        let mut kinds = vec![None];
        if self.has(Sort::CoreMemory) {
            kinds.push(Some(0x03));
            if allowed != Opts::Sync && self.has(Sort::CoreFunc) {
                kinds.push(Some(0x04));
            }
        }
        if allowed != Opts::Sync {
            kinds.push(Some(0x06));
        }
        if allowed == Opts::Lift && self.has(Sort::CoreFunc) {
            kinds.extend([Some(0x05), Some(0x07)]);
        }
        let mut opts = Vec::new();
        // Lifting and lowering strings and lists needs `memory` and `realloc`.
        if allowed != Opts::Sync && kinds.contains(&Some(0x03)) && u.ratio(3, 4)? {
            opts.push(Some(0x03));
            if kinds.contains(&Some(0x04)) && u.ratio(3, 4)? {
                opts.push(Some(0x04));
            }
            kinds.retain(|kind| !opts.contains(kind));
        }
        while !kinds.is_empty() && more(u, 2)? {
            opts.push(kinds.swap_remove(u.choose_index(kinds.len())?));
        }
        // `realloc` requires `memory`, and `callback` requires `async`, which
        // rejects `post-return`.
        for (opt, requires) in [(0x04, 0x03), (0x07, 0x06)] {
            if opts.contains(&Some(opt)) && !opts.contains(&Some(requires)) {
                opts.push(Some(requires));
            }
        }
        if opts.contains(&Some(0x06)) {
            opts.retain(|opt| *opt != Some(0x05));
        }

        self.u32(out, opts.len() as u32);
        for opt in opts {
            match opt {
                None => out.push(u.int_in_range(0x00..=0x02)?),
                Some(0x03) => {
                    out.push(0x03);
                    self.index(u, out, Sort::CoreMemory)?;
                }
                Some(0x06) => out.push(0x06),
                Some(opt) => {
                    out.push(opt);
                    self.index(u, out, Sort::CoreFunc)?;
                }
            }
        }
        Ok(())
    }

    fn import(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        self.externdecl(u, out, true)
    }

    fn export(&mut self, u: &mut Unstructured, out: &mut Vec<u8>) -> Result<()> {
        let mut encoded = Vec::new();
        let sort = self.pick_sort(u, EXTERN_SORTS)?;
        sort.encode(&mut encoded);
        // Exports can only refer to types that are named.
        let scope = self.current();
        let candidates: Vec<u32> = (0..self.len(sort))
            .filter(|idx| match sort {
                Sort::Func => scope
                    .funcs
                    .get(idx)
                    .is_none_or(|(_, requires)| scope.named(requires, false)),
                Sort::Type => scope.named(&scope.types[*idx as usize].requires, false),
                _ => true,
            })
            .collect();
        let idx = self.steered(u, sort, &candidates)?;
        self.u32(&mut encoded, idx);
        let (mut ty, requires) = self.item_ty(sort, idx);
        if let ItemTy::Type(t) = ty {
            ty = ItemTy::Type(self.current().alias(idx, t));
        }
        self.scope_nameattributes(u, out, false, sort, ty.clone())?;
        out.extend(encoded);
        // When steered, only types are ascribed, with their own type.
        let ascribe = u.ratio(1, 16)?;
        if self.steer(u)? {
            self.optional(u, out, ascribe && sort == Sort::Type, |me, _, out| {
                out.extend([0x03, 0x00]);
                me.u32(out, idx);
                Ok(())
            })?;
        } else {
            self.optional(u, out, ascribe, |me, u, out| me.externtype(u, out, false))?;
        }
        let items = self.items(0, sort, idx);
        let new = self.len(sort);
        self.add_typed(sort, &ty, requires, items);
        if sort == Sort::Type {
            let scope = self.scope();
            scope.exported.insert(new);
            if scope.types[new as usize].same.is_some() && !scope.imported.contains(&idx) {
                let same = scope.same(new);
                scope.exported_only.insert(same);
            }
        }
        Ok(())
    }

    /// `nameattributes` of an import or export of `sort` and type `ty` in the
    /// current scope.
    fn scope_nameattributes(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        import: bool,
        sort: Sort,
        ty: ItemTy,
    ) -> Result<()> {
        fn used(scope: &mut Scope, import: bool) -> &mut HashSet<String> {
            if import {
                &mut scope.imports
            } else {
                &mut scope.exports
            }
        }
        let mut names = mem::take(used(self.scope(), import));
        let name = self.nameattributes(u, out, &mut names, sort, Some((import, &ty)));
        *used(self.scope(), import) = names;
        let name = name?;
        let idx = self.len(sort);
        let scope = self.scope();
        if let ItemTy::Type(Type {
            kind: Kind::Resource { .. },
            ..
        }) = ty
        {
            scope.resources[usize::from(!import)].push((name.clone(), idx));
        }
        let item = Item { name, sort, ty };
        match import {
            true => scope.own.imports.push(item),
            false => scope.own.exports.push(item),
        }
        Ok(())
    }

    /// `nameattributes` of `sort` whose `externname` is strongly-unique in
    /// `used` unless it collides on purpose, returning the `externname`.
    fn nameattributes(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        used: &mut HashSet<String>,
        sort: Sort,
        ctx: Option<(bool, &ItemTy)>,
    ) -> Result<String> {
        // 🏷️ `implements` is only valid for instances with a label name, and
        // 🔗 `version-suffix` only for instances, completing the
        // `canonversion` of the name or of `implements` into a `valid semver`.
        let instance = sort == Sort::Instance;
        let implements = instance && u.ratio(1, 4)?;
        let version = if instance && u.ratio(1, 4)? {
            Some(split_semver(u)?)
        } else {
            None
        };
        let name = if implements {
            self.pooled_name(u, Names::Labels)?
        } else if let Some((version, _)) = &version {
            // Not pooled since it is only valid with this suffix.
            interfacename(u, Some(version))?
        } else if sort == Sort::Func {
            self.func_name(u, ctx)?
        } else {
            self.pooled_name(u, Names::Unannotated)?
        };
        let name = self.unique(u, name, used)?;

        let mut attributes = Vec::new();
        if implements {
            let version = version.as_ref().map(|(version, _)| &version[..]);
            attributes.push((0x00, interfacename(u, version)?));
        }
        if let Some((_, suffix)) = version {
            attributes.push((0x01, suffix));
        }
        if u.ratio(1, 8)? {
            attributes.push((0x02, limited_str(20, u)?.to_string()));
        }

        let opcode = if attributes.is_empty() {
            u.int_in_range(0x00..=0x02)?
        } else {
            0x02
        };
        out.push(opcode);
        self.string(out, &name);
        if opcode == 0x02 {
            self.u32(out, attributes.len() as u32);
            while !attributes.is_empty() {
                let (kind, value) = attributes.swap_remove(u.choose_index(attributes.len())?);
                out.push(kind);
                self.string(out, &value);
            }
        }
        Ok(name)
    }

    /// The name of a function, of type `ty`, that the current scope imports
    /// (or exports) when `ctx` says so. When steered, annotated names are only
    /// those that validation accepts: `[constructor]`s that return an `own`,
    /// `[method]`s that borrow `self`, `[static]`s of resources named in the
    /// scope, and accessors with the parameters and results they need.
    fn func_name(&mut self, u: &mut Unstructured, ctx: Option<(bool, &ItemTy)>) -> Result<String> {
        if !self.steer(u)? {
            return self.pooled_name(u, Names::All);
        }
        let (import, func) = match ctx {
            Some((import, ItemTy::Func(func))) => (import, Some(Rc::clone(func))),
            Some((import, _)) => (import, None),
            None => return self.pooled_name(u, Names::Unannotated),
        };
        let mut resources = self.current().resources[usize::from(!import)].clone();
        resources.retain(|(name, _)| !name.contains(':') && !name.starts_with('['));
        // Prefixes that a label completes, unless they end in a resource.
        let mut names = Vec::new();
        let accessor = func.as_ref().filter(|func| !func.is_async);
        let get = |func: &Func, params| func.params.len() == params && func.result.is_some();
        for (resource, idx) in &resources {
            names.push(format!("[static]{resource}."));
            if let Some(func) = accessor {
                if func.returns_own == Some(*idx) {
                    names.push(format!("[constructor]{resource}"));
                }
                if get(func, 0) {
                    names.push(format!("[static][get]{resource}."));
                }
            }
            if let Some(func) = &func {
                if func.self_borrow == Some(*idx) {
                    names.push(format!("[method]{resource}."));
                    if !func.is_async && get(func, 1) {
                        names.push(format!("[method][get]{resource}."));
                    }
                }
            }
        }
        if let Some(func) = accessor {
            if get(func, 0) {
                names.push("[get]".to_string());
            }
            // Setters of the getters so far whose result type their last
            // parameter has.
            let scope = self.current();
            let getters = match import {
                true => &scope.own.imports,
                false => &scope.own.exports,
            };
            for getter in getters {
                let ItemTy::Func(getter_func) = &getter.ty else {
                    continue;
                };
                if !getter.name.contains("[get]") || func.result.is_some() {
                    continue;
                }
                let params = match getter_func.self_borrow {
                    Some(_) => 2,
                    None => 1,
                };
                let value = func.params.last().and_then(|val| val.desc.as_ref());
                let result = getter_func
                    .result
                    .as_ref()
                    .and_then(|val| val.desc.as_ref());
                if func.params.len() == params
                    && func.self_borrow == getter_func.self_borrow
                    && value.is_some()
                    && value == result
                {
                    names.push(getter.name.replacen("[get]", "[set]", 1));
                }
            }
        }
        if names.is_empty() || u.arbitrary()? {
            return self.pooled_name(u, Names::Unannotated);
        }
        let mut name = u.choose(&names)?.clone();
        if name.ends_with(['.', ']']) {
            name.push_str(&self.pooled(u, Pool::Label, |_| true, label)?);
        }
        Ok(name)
    }

    /// A name from `pool` that satisfies `ok`, or a new one, which is made by
    /// `new` or is a variant of a pooled one.
    fn pooled(
        &mut self,
        u: &mut Unstructured,
        pool: Pool,
        ok: impl Fn(&str) -> bool,
        new: impl FnOnce(&mut Unstructured) -> Result<String>,
    ) -> Result<String> {
        let names = match pool {
            Pool::Extern => &self.names,
            Pool::Core => &self.core_names,
            Pool::Label => &self.labels,
        };
        let names: Vec<&String> = names.iter().filter(|name| ok(name)).collect();
        if !names.is_empty() && percent(u, self.config.name_reuse_percent)? {
            return Ok(u.choose(&names)?.to_string());
        }
        let name = if !names.is_empty() && percent(u, self.config.name_collision_percent)? {
            let name = u.choose(&names)?.to_string();
            match pool {
                Pool::Core => name,
                _ => similar(u, name)?,
            }
        } else {
            new(u)?
        };
        let size = self.config.name_pool_size;
        let names = match pool {
            Pool::Extern => &mut self.names,
            Pool::Core => &mut self.core_names,
            Pool::Label => &mut self.labels,
        };
        if names.len() < size {
            names.push(name.clone());
        }
        Ok(name)
    }

    /// An extern name, of the kind that `names` allows.
    fn pooled_name(&mut self, u: &mut Unstructured, names: Names) -> Result<String> {
        let ok = |name: &str| match names {
            Names::All => true,
            Names::Unannotated => !name.starts_with('['),
            Names::Labels => !name.starts_with('[') && !name.contains(':'),
        };
        self.pooled(u, Pool::Extern, ok, |u| {
            if names == Names::Labels || u.arbitrary()? {
                plainname(u, names == Names::All)
            } else {
                interfacename(u, None)
            }
        })
    }

    /// Returns `name` and adds it to `used` if it is strongly-unique in
    /// `used`, or it collides on purpose, and otherwise a name that is.
    fn unique(
        &self,
        u: &mut Unstructured,
        mut name: String,
        used: &mut HashSet<String>,
    ) -> Result<String> {
        if used.insert(canonical(&name)) || percent(u, self.config.name_collision_percent)? {
            return Ok(name);
        }
        let mut i = used.len();
        while !used.insert(canonical(&name)) {
            name = format!("x{i}");
            i += 1;
        }
        Ok(name)
    }

    fn labellit(
        &mut self,
        u: &mut Unstructured,
        out: &mut Vec<u8>,
        used: &mut HashSet<String>,
    ) -> Result<String> {
        let name = self.pooled(u, Pool::Label, |_| true, label)?;
        let name = self.unique(u, name, used)?;
        self.string(out, &name);
        Ok(name)
    }
}

/// `interfacename`, whose `interfaceversion` is `version` if given.
fn interfacename(u: &mut Unstructured, version: Option<&str>) -> Result<String> {
    // 🪺 allows more than one namespace and projection.
    let max = if u.ratio(1, 8)? { 3 } else { 1 };
    let mut name = String::new();
    arbitrary_loop(u, 1, max, |u| {
        words(u, &mut name)?;
        name.push(':');
        Ok(true)
    })?;
    words(u, &mut name)?;
    arbitrary_loop(u, 1, max, |u| {
        name.push('/');
        name.push_str(&label(u)?);
        Ok(true)
    })?;
    if let Some(version) = version {
        name.push('@');
        name.push_str(version);
    } else if u.arbitrary()? {
        name.push('@');
        semver(u, &mut name)?;
    }
    Ok(name)
}

/// A variant of the extern name or label `name` that has the same canonical
/// form, with the case of one of the fragments of its last label changed.
fn similar(u: &mut Unstructured, name: String) -> Result<String> {
    let start = name.rfind(['/', ']']).map_or(0, |i| i + 1);
    let end = name[start..].find('@').map_or(name.len(), |i| start + i);
    let mut fragments = Vec::new();
    let mut from = start;
    for (i, c) in name[start..end].char_indices() {
        if c == '-' || c == '.' {
            fragments.push(from..start + i);
            from = start + i + 1;
        }
    }
    fragments.push(from..end);
    fragments.retain(|range| name[range.clone()].bytes().any(|b| b.is_ascii_alphabetic()));
    if fragments.is_empty() {
        return Ok(name);
    }
    let range = u.choose(&fragments)?.clone();
    let fragment = &name[range.clone()];
    let flipped = match fragment.bytes().any(|b| b.is_ascii_lowercase()) {
        true => fragment.to_ascii_uppercase(),
        false => fragment.to_ascii_lowercase(),
    };
    let mut similar = name.clone();
    similar.replace_range(range, &flipped);
    Ok(similar)
}

/// The canonical form of a name under which names in the same scope must be
/// [strongly-unique].
///
/// [strongly-unique]: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md#name-uniqueness
fn canonical(name: &str) -> String {
    let name = name.replace('-', "").to_lowercase();
    let mut rest = &name[..];
    let mut annotations = String::new();
    while let Some(end) = rest.strip_prefix('[').and_then(|r| r.find(']')) {
        let annotation = &rest[..end + 2];
        if annotation == "[constructor]" || annotation == "[set]" {
            annotations.push_str(annotation);
        }
        rest = &rest[end + 2..];
    }
    match rest.split_once('.') {
        Some((a, b)) if a == b => a.to_string(),
        _ => annotations + rest,
    }
}

/// `plainname`, which is annotated only if `annotated`.
fn plainname(u: &mut Unstructured, annotated: bool) -> Result<String> {
    if !annotated || u.arbitrary()? {
        return label(u);
    }
    Ok(match u.int_in_range(0..=4)? {
        0 => format!("[constructor]{}", label(u)?),
        // 📡
        1 => format!("[get]{}", label(u)?),
        2 => format!("[set]{}", label(u)?),
        n => {
            let kind = if n == 3 { "[method]" } else { "[static]" };
            // 📡
            let accessor = *u.choose(&["", "[get]", "[set]"])?;
            format!("{kind}{accessor}{}.{}", label(u)?, label(u)?)
        }
    })
}

/// `label ::= first-fragment ('-' fragment)*`
fn label(u: &mut Unstructured) -> Result<String> {
    let mut label = String::new();
    let acronym = u.arbitrary()?;
    fragment(u, &mut label, acronym, true)?;
    arbitrary_loop(u, 0, 3, |u| {
        label.push('-');
        let acronym = u.arbitrary()?;
        fragment(u, &mut label, acronym, false)?;
        Ok(true)
    })?;
    Ok(label)
}

/// `words ::= first-word ('-' word)*`
fn words(u: &mut Unstructured, out: &mut String) -> Result<()> {
    fragment(u, out, false, true)?;
    arbitrary_loop(u, 0, 2, |u| {
        out.push('-');
        fragment(u, out, false, false)?;
        Ok(true)
    })
}

/// A `word` (`[0-9a-z]+`) or an `acronym` (`[0-9A-Z]+`), which starts with a
/// letter when it is `first`.
fn fragment(u: &mut Unstructured, out: &mut String, acronym: bool, first: bool) -> Result<()> {
    let a = if acronym { b'A' } else { b'a' };
    for i in 0..u.int_in_range(1..=4)? {
        let c = match u.int_in_range(0..=35)? {
            n @ 0..=9 if !(first && i == 0) => b'0' + n,
            n => a + n % 26,
        };
        out.push(char::from(c));
    }
    Ok(())
}

/// `valid semver`
fn semver(u: &mut Unstructured, out: &mut String) -> Result<()> {
    let (major, minor, patch): (u8, u8, u8) = u.arbitrary()?;
    out.push_str(&format!("{major}.{minor}.{patch}"));
    if u.ratio(1, 4)? {
        out.push('-');
        identifiers(u, out)?;
    }
    if u.ratio(1, 4)? {
        out.push('+');
        identifiers(u, out)?;
    }
    Ok(())
}

/// Dot-separated alphanumeric identifiers that don't start with a digit, so
/// that they never have leading zeros.
fn identifiers(u: &mut Unstructured, out: &mut String) -> Result<()> {
    let acronym = u.arbitrary()?;
    fragment(u, out, acronym, true)?;
    arbitrary_loop(u, 0, 2, |u| {
        out.push('.');
        let acronym = u.arbitrary()?;
        fragment(u, out, acronym, true)?;
        Ok(true)
    })
}

/// A 🔗 `canonversion` and the `semversuffix` that completes it into a
/// `valid semver` that canonicalizes into exactly these two parts.
fn split_semver(u: &mut Unstructured) -> Result<(String, String)> {
    let (major, minor, patch): (u8, u8, u8) = u.arbitrary()?;
    let mut build = String::new();
    if u.arbitrary()? {
        build.push('+');
        identifiers(u, &mut build)?;
    }
    let (version, mut suffix) = if u.ratio(1, 4)? {
        let mut version = format!("{major}.{minor}.{patch}-");
        identifiers(u, &mut version)?;
        (version, build)
    } else if major > 0 {
        (major.to_string(), format!(".{minor}.{patch}{build}"))
    } else if minor > 0 {
        (format!("0.{minor}"), format!(".{patch}{build}"))
    } else {
        (format!("0.0.{patch}"), build)
    };
    if suffix.is_empty() {
        suffix.push('+');
        identifiers(u, &mut suffix)?;
    }
    Ok((version, suffix))
}
