use crate::graph::{
    type_desc, CompositionGraph, EncodeOptions, ExportIndex, ImportIndex, InstanceId,
};
use anyhow::{anyhow, bail, Result};
use heck::ToKebabCase;
use indexmap::{IndexMap, IndexSet};
use petgraph::EdgeDirection;
use smallvec::SmallVec;
use std::mem;
use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap},
};
use wasm_encoder::*;
use wasmparser::{
    names::KebabString,
    types::{self, ComponentEntityType, Type, TypeId},
    ComponentExternalKind,
};

fn type_ref_to_export_kind(ty: wasmparser::ComponentTypeRef) -> ComponentExportKind {
    match ty {
        wasmparser::ComponentTypeRef::Module(_) => ComponentExportKind::Module,
        wasmparser::ComponentTypeRef::Func(_) => ComponentExportKind::Func,
        wasmparser::ComponentTypeRef::Value(_) => ComponentExportKind::Value,
        wasmparser::ComponentTypeRef::Type { .. } => ComponentExportKind::Type,
        wasmparser::ComponentTypeRef::Instance(_) => ComponentExportKind::Instance,
        wasmparser::ComponentTypeRef::Component(_) => ComponentExportKind::Component,
    }
}

enum Encodable {
    Component(ComponentType),
    Instance(InstanceType),
    Builder(ComponentBuilder),
}

impl Encodable {
    fn type_count(&self) -> u32 {
        match self {
            Encodable::Component(t) => t.type_count(),
            Encodable::Instance(t) => t.type_count(),
            Encodable::Builder(t) => t.type_count(),
        }
    }

    fn instance_count(&self) -> u32 {
        match self {
            Encodable::Component(t) => t.instance_count(),
            Encodable::Instance(t) => t.instance_count(),
            Encodable::Builder(t) => t.instance_count(),
        }
    }

    fn core_type_count(&self) -> u32 {
        match self {
            Encodable::Component(t) => t.core_type_count(),
            Encodable::Instance(t) => t.core_type_count(),
            Encodable::Builder(t) => t.core_type_count(),
        }
    }

    fn ty(&mut self) -> ComponentTypeEncoder {
        match self {
            Encodable::Component(t) => t.ty(),
            Encodable::Instance(t) => t.ty(),
            Encodable::Builder(t) => t.ty().1,
        }
    }

    fn core_type(&mut self) -> CoreTypeEncoder {
        match self {
            Encodable::Component(t) => t.core_type(),
            Encodable::Instance(t) => t.core_type(),
            Encodable::Builder(t) => t.core_type().1,
        }
    }

    fn alias(&mut self, alias: Alias<'_>) {
        match self {
            Encodable::Component(t) => {
                t.alias(alias);
            }
            Encodable::Instance(t) => {
                t.alias(alias);
            }
            Encodable::Builder(t) => {
                t.alias(alias);
            }
        }
    }
}

/// Metadata necessary to track type definitions across both instances and also
/// across unioning components.
///
/// This state is used when assembling the imports into a composed component.
/// The imported instances will have types that are intended to mirror the
/// original source components which means that the type structure, such as
/// aliases, additionally needs to be mirrored. This structure keeps track of
/// type scopes and is used throughout type translation to facilitate this.
pub(crate) struct TypeState<'a> {
    // Current outer scopes of this state which can be referred to with
    // `alias outer`
    scopes: Vec<TypeScope<'a>>,

    // Current type scope that's being translated into.
    cur: TypeScope<'a>,
}

/// A unique key identifying a type.
///
/// Note that this has two components: the first is the component that a type
/// comes from and the second is the wasmparser-unique id for within that
/// component.
type TypeKey<'a> = (PtrKey<'a, crate::graph::Component<'a>>, TypeId);

/// A scope that types can be defined into.
///
/// This is stored within `TypeState` and contains all the relevant information
/// for mirroring a preexisting wasmparser-defined structure of types into a
/// new component type (such as an instance for an instance import).
struct TypeScope<'a> {
    /// Types defined in this current scope.
    ///
    /// Contains the type index that the type is defined at.
    type_defs: HashMap<TypeKey<'a>, u32>,

    /// Types exported in the current scope.
    ///
    /// This is filled out during `export` and indicates that a particular type
    /// is exported with the specified name.
    type_exports: HashMap<TypeKey<'a>, &'a str>,

    /// Reverse of the `type_exports` map, indicating which name exports which
    /// types.
    ///
    /// Note that this can export a "list" of types which represents that
    /// multiple components may be "unioned" together to create a single
    /// instance import, so exporting a type can have different names as each
    /// original component may have ID for the same export name.
    ///
    /// This enables translating type-use situations where one component
    /// translates the base type and then a second component refers to that.
    type_exports_rev: HashMap<&'a str, Vec<TypeKey<'a>>>,

    /// Instances that are available to alias from in this scope.
    ///
    /// This map is keyed by the types that are available to be referred to.
    /// The value here is the instance index that the type is defined in along
    /// with its export name. This is used to generate `alias export` items.
    instance_exports: HashMap<TypeKey<'a>, (u32, &'a str)>,

    /// Encoded representation of this type scope, a `wasm-encoder` structure.
    encodable: Encodable,
}

impl<'a> TypeState<'a> {
    fn new() -> TypeState<'a> {
        TypeState {
            scopes: Vec::new(),
            cur: TypeScope {
                type_exports: HashMap::new(),
                type_exports_rev: HashMap::new(),
                instance_exports: HashMap::new(),
                type_defs: HashMap::new(),
                encodable: Encodable::Builder(Default::default()),
            },
        }
    }

    /// Pushes a new scope which will be written to the `encodable` provided.
    fn push(&mut self, encodable: Encodable) {
        let prev = mem::replace(
            &mut self.cur,
            TypeScope {
                type_exports: HashMap::new(),
                type_exports_rev: HashMap::new(),
                instance_exports: HashMap::new(),
                type_defs: HashMap::new(),
                encodable,
            },
        );
        self.scopes.push(prev);
    }

    /// Pops a previously pushed scope and returns the encoding.
    fn pop(&mut self) -> Encodable {
        let prev = mem::replace(&mut self.cur, self.scopes.pop().unwrap());

        // If the previous scope was an instance then assume that the instance
        // type is about to be used as an import or export which defines a new
        // instance that this previously-outer-now-current scope has access to.
        //
        // This scope then takes all of the type exports of the created instance
        // and registers them as available at this next instance index.
        if let Encodable::Instance(_) = &prev.encodable {
            let idx = self.cur.encodable.instance_count();
            for (id, name) in prev.type_exports {
                let prev = self.cur.instance_exports.insert(id, (idx, name));
                assert!(prev.is_none());
            }
        }

        prev.encodable
    }
}

impl Default for TypeState<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> TypeScope<'a> {
    /// Registers that `ty` is exported as `name`, filling in both type export
    /// maps at the same time.
    fn add_type_export(&mut self, ty: TypeKey<'a>, name: &'a str) {
        let prev = self.type_exports.insert(ty, name);
        assert!(prev.is_none());
        self.type_exports_rev.entry(name).or_default().push(ty);
    }
}

pub struct PtrKey<'a, T>(&'a T);

impl<T> PartialEq for PtrKey<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<T> Eq for PtrKey<'_, T> {}

impl<T> Copy for PtrKey<'_, T> {}

impl<T> Clone for PtrKey<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> std::hash::Hash for PtrKey<'_, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state);
    }
}

pub(crate) struct TypeEncoder<'a>(&'a crate::graph::Component<'a>);

impl<'a> TypeEncoder<'a> {
    pub fn new(component: &'a crate::graph::Component) -> Self {
        Self(component)
    }

    pub fn component<I, E>(
        &self,
        state: &mut TypeState<'a>,
        imports: I,
        exports: E,
    ) -> ComponentType
    where
        I: IntoIterator<Item = (&'a str, wasmparser::types::ComponentEntityType)>,
        E: IntoIterator<Item = (&'a str, wasmparser::types::ComponentEntityType)>,
    {
        state.push(Encodable::Component(ComponentType::new()));

        for (name, ty) in imports {
            let ty = self.component_entity_type(state, ty);
            let c = match &mut state.cur.encodable {
                Encodable::Component(c) => c,
                _ => unreachable!(),
            };
            c.import(name, ty);
        }

        for (name, ty) in exports {
            let export = self.export(name, ty, state);
            let c = match &mut state.cur.encodable {
                Encodable::Component(c) => c,
                _ => unreachable!(),
            };
            c.export(name, export);
        }

        match state.pop() {
            Encodable::Component(c) => c,
            _ => unreachable!(),
        }
    }

    pub fn instance<E>(&self, state: &mut TypeState<'a>, exports: E) -> InstanceType
    where
        E: IntoIterator<Item = (&'a str, wasmparser::types::ComponentEntityType)>,
    {
        state.push(Encodable::Instance(InstanceType::new()));

        for (name, ty) in exports {
            let export = self.export(name, ty, state);
            let c = match &mut state.cur.encodable {
                Encodable::Instance(c) => c,
                _ => unreachable!(),
            };
            c.export(name, export);
        }

        match state.pop() {
            Encodable::Instance(c) => c,
            _ => unreachable!(),
        }
    }

    pub fn module<I, E>(&self, imports: I, exports: E) -> ModuleType
    where
        I: IntoIterator<Item = (&'a str, &'a str, wasmparser::types::EntityType)>,
        E: IntoIterator<Item = (&'a str, wasmparser::types::EntityType)>,
    {
        let mut encoded = ModuleType::new();
        let mut types = HashMap::default();

        for (module, name, ty) in imports {
            let ty = self.entity_type(&mut encoded, &mut types, ty);
            encoded.import(module, name, ty);
        }

        for (name, ty) in exports {
            let ty = self.entity_type(&mut encoded, &mut types, ty);
            encoded.export(name, ty);
        }

        encoded
    }

    fn entity_type(
        &self,
        encodable: &mut ModuleType,
        types: &mut HashMap<TypeId, u32>,
        ty: wasmparser::types::EntityType,
    ) -> EntityType {
        match ty {
            wasmparser::types::EntityType::Func(id) => {
                let ty = &self.0.types[id];
                let idx = match types.entry(id) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let ty = ty.unwrap_func();
                        let index = encodable.type_count();
                        encodable.ty().function(
                            ty.params().iter().copied().map(Self::val_type),
                            ty.results().iter().copied().map(Self::val_type),
                        );
                        *e.insert(index)
                    }
                };
                EntityType::Function(idx)
            }
            wasmparser::types::EntityType::Table(ty) => EntityType::Table(Self::table_type(ty)),
            wasmparser::types::EntityType::Memory(ty) => EntityType::Memory(Self::memory_type(ty)),
            wasmparser::types::EntityType::Global(ty) => EntityType::Global(Self::global_type(ty)),
            wasmparser::types::EntityType::Tag(id) => {
                let ty = &self.0.types[id];
                let idx = match types.entry(id) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let ty = ty.unwrap_func();
                        let index = encodable.type_count();
                        encodable.ty().function(
                            ty.params().iter().copied().map(Self::val_type),
                            ty.results().iter().copied().map(Self::val_type),
                        );
                        *e.insert(index)
                    }
                };
                EntityType::Tag(TagType {
                    kind: TagKind::Exception,
                    func_type_idx: idx,
                })
            }
        }
    }

    fn component_entity_type(
        &self,
        state: &mut TypeState<'a>,
        ty: wasmparser::types::ComponentEntityType,
    ) -> ComponentTypeRef {
        match ty {
            wasmparser::types::ComponentEntityType::Module(id) => {
                ComponentTypeRef::Module(self.ty(state, id))
            }
            wasmparser::types::ComponentEntityType::Func(id) => {
                ComponentTypeRef::Func(self.ty(state, id))
            }
            wasmparser::types::ComponentEntityType::Value(ty) => {
                ComponentTypeRef::Value(self.component_val_type(state, ty))
            }
            wasmparser::types::ComponentEntityType::Type { referenced, .. } => {
                ComponentTypeRef::Type(TypeBounds::Eq(self.ty(state, referenced)))
            }
            wasmparser::types::ComponentEntityType::Instance(id) => {
                ComponentTypeRef::Instance(self.ty(state, id))
            }
            wasmparser::types::ComponentEntityType::Component(id) => {
                ComponentTypeRef::Component(self.ty(state, id))
            }
        }
    }

    fn val_type(ty: wasmparser::ValType) -> ValType {
        match ty {
            wasmparser::ValType::I32 => ValType::I32,
            wasmparser::ValType::I64 => ValType::I64,
            wasmparser::ValType::F32 => ValType::F32,
            wasmparser::ValType::F64 => ValType::F64,
            wasmparser::ValType::V128 => ValType::V128,
            wasmparser::ValType::Ref(ty) => ValType::Ref(Self::ref_type(ty)),
        }
    }

    fn ref_type(ty: wasmparser::RefType) -> RefType {
        RefType {
            nullable: ty.is_nullable(),
            heap_type: match ty.heap_type() {
                wasmparser::HeapType::Func => HeapType::Func,
                wasmparser::HeapType::Extern => HeapType::Extern,
                wasmparser::HeapType::Any => HeapType::Any,
                wasmparser::HeapType::None => HeapType::None,
                wasmparser::HeapType::NoExtern => HeapType::NoExtern,
                wasmparser::HeapType::NoFunc => HeapType::NoFunc,
                wasmparser::HeapType::Eq => HeapType::Eq,
                wasmparser::HeapType::Struct => HeapType::Struct,
                wasmparser::HeapType::Array => HeapType::Array,
                wasmparser::HeapType::I31 => HeapType::I31,
                wasmparser::HeapType::Indexed(i) => HeapType::Indexed(i),
            },
        }
    }

    fn table_type(ty: wasmparser::TableType) -> TableType {
        TableType {
            element_type: Self::ref_type(ty.element_type),
            minimum: ty.initial,
            maximum: ty.maximum,
        }
    }

    fn memory_type(ty: wasmparser::MemoryType) -> MemoryType {
        MemoryType {
            minimum: ty.initial,
            maximum: ty.maximum,
            memory64: ty.memory64,
            shared: ty.shared,
        }
    }

    fn global_type(ty: wasmparser::GlobalType) -> GlobalType {
        GlobalType {
            val_type: Self::val_type(ty.content_type),
            mutable: ty.mutable,
        }
    }

    fn primitive(ty: wasmparser::PrimitiveValType) -> PrimitiveValType {
        match ty {
            wasmparser::PrimitiveValType::Bool => PrimitiveValType::Bool,
            wasmparser::PrimitiveValType::S8 => PrimitiveValType::S8,
            wasmparser::PrimitiveValType::U8 => PrimitiveValType::U8,
            wasmparser::PrimitiveValType::S16 => PrimitiveValType::S16,
            wasmparser::PrimitiveValType::U16 => PrimitiveValType::U16,
            wasmparser::PrimitiveValType::S32 => PrimitiveValType::S32,
            wasmparser::PrimitiveValType::U32 => PrimitiveValType::U32,
            wasmparser::PrimitiveValType::S64 => PrimitiveValType::S64,
            wasmparser::PrimitiveValType::U64 => PrimitiveValType::U64,
            wasmparser::PrimitiveValType::Float32 => PrimitiveValType::Float32,
            wasmparser::PrimitiveValType::Float64 => PrimitiveValType::Float64,
            wasmparser::PrimitiveValType::Char => PrimitiveValType::Char,
            wasmparser::PrimitiveValType::String => PrimitiveValType::String,
        }
    }

    fn module_type(&self, state: &mut TypeState<'a>, id: TypeId) -> u32 {
        let ty = &self.0.types[id];
        let ty = ty.unwrap_module();

        let module = self.module(
            ty.imports
                .iter()
                .map(|((m, n), t)| (m.as_str(), n.as_str(), *t)),
            ty.exports.iter().map(|(n, t)| (n.as_str(), *t)),
        );

        let index = state.cur.encodable.core_type_count();
        state.cur.encodable.core_type().module(&module);
        index
    }

    fn component_instance_type(&self, state: &mut TypeState<'a>, id: TypeId) -> u32 {
        let ty = &self.0.types[id];
        let ty = ty.unwrap_component_instance();
        let instance = self.instance(state, ty.exports.iter().map(|(n, t)| (n.as_str(), *t)));
        let index = state.cur.encodable.type_count();
        state.cur.encodable.ty().instance(&instance);
        index
    }

    fn component_type(&self, state: &mut TypeState<'a>, id: TypeId) -> u32 {
        let ty = &self.0.types[id];
        let ty = ty.unwrap_component();

        let component = self.component(
            state,
            ty.imports.iter().map(|(n, t)| (n.as_str(), *t)),
            ty.exports.iter().map(|(n, t)| (n.as_str(), *t)),
        );

        let index = state.cur.encodable.type_count();
        state.cur.encodable.ty().component(&component);
        index
    }

    fn component_func_type(&self, state: &mut TypeState<'a>, id: TypeId) -> u32 {
        let ty = &self.0.types[id];
        let func_ty = ty.unwrap_component_func();

        let params = func_ty
            .params
            .iter()
            .map(|(name, ty)| (name.as_str(), self.component_val_type(state, *ty)))
            .collect::<Vec<_>>();

        let results = func_ty
            .results
            .iter()
            .map(|(name, ty)| (name.as_deref(), self.component_val_type(state, *ty)))
            .collect::<Vec<_>>();

        let index = state.cur.encodable.type_count();
        let mut f = state.cur.encodable.ty().function();

        f.params(params);

        if results.len() == 1 && results[0].0.is_none() {
            f.result(results[0].1);
        } else {
            f.results(
                results
                    .into_iter()
                    .map(|(name, ty)| (name.unwrap().as_str(), ty)),
            );
        }

        index
    }

    /// Translates a type `id` provided, returning the index that it is defined
    /// at.
    ///
    /// This is the main point at which type translation flows through. This
    /// performs everything necessary such as:
    ///
    /// * Each type is translated only once
    /// * If `id` comes from a different instance it's aliased
    /// * Dispatching to the correct translation internally.
    fn ty(&self, state: &mut TypeState<'a>, id: TypeId) -> u32 {
        // Consult our scope's `type_defs` map, and if it's not present then
        // generate the type and fill it in.
        let key = (PtrKey(self.0), id);
        if let Some(ret) = state.cur.type_defs.get(&key) {
            return *ret;
        }
        let idx = self._ty(state, id);
        let prev = state.cur.type_defs.insert(key, idx);
        assert!(prev.is_none());
        idx
    }

    // Inner version of `ty` above which is a separate method to make it easier
    // to use `return` and not thwart the caching above.
    fn _ty(&self, state: &mut TypeState<'a>, id: TypeId) -> u32 {
        // If `id` is not an alias to anything else then it's a defined type in
        // this scope meaning that it needs to be translated and represented
        // here.
        if self.0.types.peel_alias(id).is_none() {
            return match &self.0.types[id] {
                Type::Sub(_) | Type::Instance(_) => unreachable!(),
                Type::Module(_) => self.module_type(state, id),
                Type::Component(_) => self.component_type(state, id),
                Type::ComponentInstance(_) => self.component_instance_type(state, id),
                Type::ComponentFunc(_) => self.component_func_type(state, id),
                Type::Defined(_) => self.defined_type(state, id),
                Type::Resource(_) => unimplemented!(),
            };
        }

        // Otherwise at this point we know that `id` is an alias to something.
        // This basically means that it's a reference to an exported type which
        // is an alias to a different underlying type.
        //
        // Before attempting to find something to alias first determine if the
        // type has already been created but under a different name. This
        // can happen where one component's view of an imported instance
        // may be partial and then unioned with another component's view of an
        // imported instance. The second instance should reuse all the types
        // defined/exported by the first.
        //
        // Here the reverse-export map is consulted where if `key` is an
        // exported type from this instance (which is registered in "parallel"
        // when one of those is encountered for all components) then search
        // with the exported name if any other component has a type definition
        // for their own version of our `id`.
        let key = (PtrKey(self.0), id);
        if let Some(name) = state.cur.type_exports.get(&key) {
            for key in state.cur.type_exports_rev[name].iter() {
                if let Some(ret) = state.cur.type_defs.get(key) {
                    log::trace!("id already defined through a different component");
                    return *ret;
                }
            }
        }

        // Failing all that it seems that this type is defined elsewhere and
        // no one has created an alias yet for it. That's our job so follow the
        // alias chain and search all our outer scopes for one that has an
        // instance that exports `id`. When found it's aliased out from
        // that instance and then aliased into the current scope.
        let mut cur = id;
        loop {
            for (i, scope) in state.scopes.iter_mut().rev().enumerate() {
                let key = (PtrKey(self.0), cur);
                let (instance, name) = match scope.instance_exports.get(&key) {
                    Some(pair) => *pair,
                    None => continue,
                };
                let scope_idx = scope.encodable.type_count();
                scope.encodable.alias(Alias::InstanceExport {
                    instance,
                    name,
                    kind: ComponentExportKind::Type,
                });
                match &scope.encodable {
                    Encodable::Instance(_) => log::trace!("instance"),
                    Encodable::Component(_) => log::trace!("component"),
                    Encodable::Builder(_) => log::trace!("builder"),
                }
                let ret = state.cur.encodable.type_count();
                state.cur.encodable.alias(Alias::Outer {
                    count: i as u32 + 1,
                    index: scope_idx,
                    kind: ComponentOuterAliasKind::Type,
                });
                log::trace!("id defined in a different instance");
                return ret;
            }

            // If the end of the alias chain has been reached then that's a bug
            // in this type translation. It should be the case that we know
            // how to find all types at this point, so reaching the end means
            // there's a missing case one way or another.
            cur = self.0.types.peel_alias(cur).unwrap_or_else(|| {
                panic!("failed to find alias of {id:?}");
            });
        }
    }

    fn component_val_type(
        &self,
        state: &mut TypeState<'a>,
        ty: wasmparser::types::ComponentValType,
    ) -> ComponentValType {
        match ty {
            wasmparser::types::ComponentValType::Primitive(ty) => {
                ComponentValType::Primitive(Self::primitive(ty))
            }
            wasmparser::types::ComponentValType::Type(id) => {
                ComponentValType::Type(self.ty(state, id))
            }
        }
    }

    fn defined_type(&self, state: &mut TypeState<'a>, id: TypeId) -> u32 {
        let ty = &self.0.types[id];
        let defined_ty = ty.unwrap_defined();

        match defined_ty {
            wasmparser::types::ComponentDefinedType::Primitive(ty) => {
                let index = state.cur.encodable.type_count();
                state
                    .cur
                    .encodable
                    .ty()
                    .defined_type()
                    .primitive(Self::primitive(*ty));
                index
            }
            wasmparser::types::ComponentDefinedType::Record(r) => self.record(state, r),
            wasmparser::types::ComponentDefinedType::Variant(v) => self.variant(state, v),
            wasmparser::types::ComponentDefinedType::List(ty) => self.list(state, *ty),
            wasmparser::types::ComponentDefinedType::Tuple(t) => self.tuple(state, t),
            wasmparser::types::ComponentDefinedType::Flags(names) => {
                Self::flags(&mut state.cur.encodable, names)
            }
            wasmparser::types::ComponentDefinedType::Enum(cases) => {
                Self::enum_type(&mut state.cur.encodable, cases)
            }
            wasmparser::types::ComponentDefinedType::Option(ty) => self.option(state, *ty),
            wasmparser::types::ComponentDefinedType::Result { ok, err } => {
                self.result(state, *ok, *err)
            }
            wasmparser::types::ComponentDefinedType::Own(id) => {
                let i = self.ty(state, *id);
                let index = state.cur.encodable.type_count();
                state.cur.encodable.ty().defined_type().own(i);
                index
            }
            wasmparser::types::ComponentDefinedType::Borrow(id) => {
                let i = self.ty(state, *id);
                let index = state.cur.encodable.type_count();
                state.cur.encodable.ty().defined_type().borrow(i);
                index
            }
        }
    }

    fn record(&self, state: &mut TypeState<'a>, record: &wasmparser::types::RecordType) -> u32 {
        let fields = record
            .fields
            .iter()
            .map(|(n, ty)| (n.as_str(), self.component_val_type(state, *ty)))
            .collect::<Vec<_>>();

        let index = state.cur.encodable.type_count();
        state.cur.encodable.ty().defined_type().record(fields);
        index
    }

    fn variant(&self, state: &mut TypeState<'a>, variant: &wasmparser::types::VariantType) -> u32 {
        let cases = variant
            .cases
            .iter()
            .map(|(n, c)| {
                (
                    n.as_str(),
                    c.ty.map(|ty| self.component_val_type(state, ty)),
                    c.refines
                        .as_deref()
                        .map(|r| variant.cases.iter().position(|(n, _)| n == r).unwrap() as u32),
                )
            })
            .collect::<Vec<_>>();
        let index = state.cur.encodable.type_count();
        state.cur.encodable.ty().defined_type().variant(cases);
        index
    }

    fn list(&self, state: &mut TypeState<'a>, ty: wasmparser::types::ComponentValType) -> u32 {
        let ty = self.component_val_type(state, ty);
        let index = state.cur.encodable.type_count();
        state.cur.encodable.ty().defined_type().list(ty);
        index
    }

    fn tuple(&self, state: &mut TypeState<'a>, tuple: &wasmparser::types::TupleType) -> u32 {
        let types = tuple
            .types
            .iter()
            .map(|ty| self.component_val_type(state, *ty))
            .collect::<Vec<_>>();
        let index = state.cur.encodable.type_count();
        state.cur.encodable.ty().defined_type().tuple(types);
        index
    }

    fn flags(encodable: &mut Encodable, names: &IndexSet<KebabString>) -> u32 {
        let index = encodable.type_count();
        encodable
            .ty()
            .defined_type()
            .flags(names.iter().map(|n| n.as_str()));
        index
    }

    fn enum_type(encodable: &mut Encodable, cases: &IndexSet<KebabString>) -> u32 {
        let index = encodable.type_count();
        encodable
            .ty()
            .defined_type()
            .enum_type(cases.iter().map(|c| c.as_str()));
        index
    }

    fn option(&self, state: &mut TypeState<'a>, ty: wasmparser::types::ComponentValType) -> u32 {
        let ty = self.component_val_type(state, ty);

        let index = state.cur.encodable.type_count();
        state.cur.encodable.ty().defined_type().option(ty);
        index
    }

    fn result(
        &self,
        state: &mut TypeState<'a>,
        ok: Option<wasmparser::types::ComponentValType>,
        err: Option<wasmparser::types::ComponentValType>,
    ) -> u32 {
        let ok = ok.map(|ty| self.component_val_type(state, ty));
        let err = err.map(|ty| self.component_val_type(state, ty));

        let index = state.cur.encodable.type_count();
        state.cur.encodable.ty().defined_type().result(ok, err);
        index
    }

    fn export(
        &self,
        name: &'a str,
        export: ComponentEntityType,
        state: &mut TypeState<'a>,
    ) -> ComponentTypeRef {
        // Check if the export is a type; if so, we need to update the index of the
        // type to point to the export instead of the original definition
        let id = match export {
            ComponentEntityType::Type { created: id, .. } => Some(id),
            _ => None,
        };
        let export = self.component_entity_type(state, export);
        if let Some(id) = id {
            // Update the index in the type map to point to this export
            let key = (PtrKey(self.0), id);
            let prev = state
                .cur
                .type_defs
                .insert(key, state.cur.encodable.type_count());
            assert!(prev.is_none());
            state.cur.add_type_export(key, name);
        }
        export
    }
}

/// Represents an instance index in a composition graph.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct InstanceIndex(usize);

enum ArgumentImportKind<'a> {
    /// An item is being imported.
    ///
    /// The item may be an instance that does not need to be merged.
    Item(&'a crate::graph::Component<'a>, ComponentEntityType),
    /// A merged instance is being imported.
    ///
    /// Instances are unioned together to form a single instance to
    /// import that will satisfy all instantiation arguments that
    /// reference the import.
    Instance(IndexMap<&'a str, Vec<(&'a crate::graph::Component<'a>, ComponentEntityType)>>),
}

/// Represents an import for an instantiation argument.
struct ArgumentImport<'a> {
    // The name of the import.
    name: &'a str,
    /// The kind of import.
    kind: ArgumentImportKind<'a>,
    /// The instances that will use the import for an argument.
    instances: SmallVec<[(InstanceIndex, ImportIndex); 1]>,
}

impl ArgumentImport<'_> {
    fn merge(&mut self, arg: Self) -> Result<()> {
        assert_eq!(self.name, arg.name);
        self.instances.extend(arg.instances);

        // If the existing import is an instance, convert this argument import to
        // a merged instance import.
        if let ArgumentImportKind::Item(component, ComponentEntityType::Instance(id)) = &self.kind {
            let exports = component.types[*id]
                .unwrap_component_instance()
                .exports
                .iter();

            let mut map = IndexMap::with_capacity(exports.len());
            for (name, ty) in exports {
                map.insert(name.as_str(), vec![(*component, *ty)]);
            }

            self.kind = ArgumentImportKind::Instance(map);
        }

        match (&mut self.kind, arg.kind) {
            // The new item should never be a merged instance
            (_, ArgumentImportKind::Instance(..)) => {
                unreachable!("expected an item import to merge with")
            }
            // If the existing import is a merged instance, merge with an instance item import
            (
                ArgumentImportKind::Instance(exports),
                ArgumentImportKind::Item(new_component, ComponentEntityType::Instance(id)),
            ) => {
                for (name, new_type) in new_component.types[id]
                    .unwrap_component_instance()
                    .exports
                    .iter()
                {
                    let dst = exports.entry(name.as_str()).or_default();
                    for (existing_component, existing_type) in dst.iter_mut() {
                        if Self::types_compatible(
                            existing_component,
                            *existing_type,
                            new_component,
                            *new_type,
                        ) {
                            continue;
                        }
                        bail!(
                            "cannot import instance with name `{name}` for \
                             an instantiation argument of component \
                             `{cname}` because it conflicts with an \
                             imported instantiation argument of component \
                             `{ecname}`",
                            name = self.name,
                            cname = new_component.name,
                            ecname = existing_component.name,
                        )
                    }
                    dst.push((new_component, *new_type));
                }
            }
            // Otherwise, an attempt to merge an instance with a non-instance is an error
            (ArgumentImportKind::Instance(_), ArgumentImportKind::Item(component, ty)) => {
                bail!(
                    "cannot import {ty} with name `{name}` for an instantiation \
                     argument of component `{cname}` because it conflicts with \
                     an instance imported with the same name",
                    name = self.name,
                    ty = type_desc(ty),
                    cname = component.name,
                );
            }
            // Finally, merge two item imports together by finding the most-compatible type
            (
                ArgumentImportKind::Item(existing_component, existing_type),
                ArgumentImportKind::Item(new_component, new_type),
            ) => {
                if !Self::types_compatible(
                    existing_component,
                    *existing_type,
                    new_component,
                    new_type,
                ) {
                    bail!(
                        "cannot import {ty} with name `{name}` for an \
                         instantiation argument of component `{cname}` \
                         because it conflicts with an imported instantiation \
                         argument of component `{ecname}`",
                        ty = type_desc(new_type),
                        name = self.name,
                        cname = new_component.name,
                        ecname = existing_component.name,
                    )
                }
            }
        }

        Ok(())
    }

    /// Tests whether two types from different components are compatible with
    /// one another.
    ///
    /// For now this performs a subtype check in both directions, only
    /// considering them compatible if both checks are "true". This means
    /// that it effectively requires the types to be structured the same way.
    fn types_compatible<'a>(
        existing_component: &'a crate::graph::Component<'a>,
        existing_type: ComponentEntityType,
        new_component: &'a crate::graph::Component<'a>,
        new_type: ComponentEntityType,
    ) -> bool {
        ComponentEntityType::is_subtype_of(
            &existing_type,
            existing_component.types(),
            &new_type,
            new_component.types(),
        ) && ComponentEntityType::is_subtype_of(
            &new_type,
            new_component.types(),
            &existing_type,
            existing_component.types(),
        )
    }
}

/// Represents an entry in the import map built up during
/// the encoding of a composition graph.
///
/// A map entry is either an import of a component or
/// an import of an item to satisfy an instantiation argument.
enum ImportMapEntry<'a> {
    /// An import of a component.
    Component(&'a crate::graph::Component<'a>),
    /// An import to satisfy one or more instantiation arguments.
    Argument(ArgumentImport<'a>),
}

/// Represents the import map built during the encoding
/// of a composition graph.
#[derive(Default)]
struct ImportMap<'a>(IndexMap<Cow<'a, str>, ImportMapEntry<'a>>);

impl<'a> ImportMap<'a> {
    fn new(import_components: bool, graph: &'a CompositionGraph) -> Result<Self> {
        let mut imports = Self::default();

        if import_components {
            imports.add_component_imports(graph);
        }

        imports.add_instantiation_imports(graph)?;

        Ok(imports)
    }

    fn add_component_imports(&mut self, graph: &'a CompositionGraph) {
        for entry in graph
            .components
            .values()
            .filter(|e| !e.instances.is_empty())
        {
            assert!(self
                .0
                .insert(
                    entry.component.name.to_kebab_case().into(),
                    ImportMapEntry::Component(&entry.component),
                )
                .is_none());
        }
    }

    fn add_instantiation_imports(&mut self, graph: &'a CompositionGraph) -> Result<()> {
        let mut imported = HashMap::new();

        // Metadata about dependency edges used below during sorting (see
        // below).
        let mut component_defining_instance = HashMap::new();
        let mut deps = HashMap::new();

        for (instance_index, instance) in graph.instances.values().enumerate() {
            let (component_index, _, entry) =
                graph.components.get_full(&instance.component).unwrap();
            let defining_instances = component_defining_instance
                .entry(component_index)
                .or_insert(HashMap::new());

            let instance_index = InstanceIndex(instance_index);

            // Import any unconnected instantiation arguments for the instance
            for (import_index, name, _) in entry.component.imports() {
                if instance.connected.contains(&import_index) {
                    continue;
                }

                let (_, ty) = entry.component.import_entity_type(import_index).unwrap();

                let arg = ArgumentImport {
                    name,
                    kind: ArgumentImportKind::Item(&entry.component, ty),
                    instances: smallvec::smallvec![(instance_index, import_index)],
                };

                // Check to see if we've seen this import before; if so, don't bother with
                // type checking, just add the instance to the list of instances
                match imported.entry((component_index, import_index)) {
                    Entry::Occupied(e) => match self.0.get_index_mut(*e.get()).unwrap().1 {
                        ImportMapEntry::Component(_) => {
                            unreachable!("import should not be for a component")
                        }
                        ImportMapEntry::Argument(arg) => {
                            arg.instances.push((instance_index, import_index));
                        }
                    },
                    Entry::Vacant(e) => {
                        // If this is the first time an instance import is seen
                        // then register all of its dependencies on other
                        // imports.
                        //
                        // (see more below on this)
                        DependencyRegistrar {
                            types: &entry.component.types,
                            defining_instances,
                            cur: name,
                            deps: &mut deps,
                        }
                        .entity(ty);
                        let index = match self.0.entry(name.into()) {
                            indexmap::map::Entry::Occupied(mut e) => match e.get_mut() {
                                ImportMapEntry::Component(_) => {
                                    bail!(
                                            "cannot import {ty} `{name}` for an instantiation argument of component `{cname}` because it conflicts with a component imported with the same name",
                                            ty = type_desc(ty),
                                            cname = entry.component.name,
                                        );
                                }
                                ImportMapEntry::Argument(existing) => {
                                    existing.merge(arg)?;
                                    e.index()
                                }
                            },
                            indexmap::map::Entry::Vacant(e) => {
                                let index = e.index();
                                e.insert(ImportMapEntry::Argument(arg));
                                index
                            }
                        };
                        e.insert(index);
                    }
                }
            }
        }

        // Before returning perform a topological sort of all dependencies to
        // ensure that they're translated in the correct order for types to be
        // connected.
        //
        // Why, might you be asking, is this necessary? Each component
        // individually already has topologically sorted dependencies and
        // they're all iterated through above, so what's the problem?
        //
        // The reason this loop arises is for when two different components
        // are unioned together and a topological ordering between the union
        // of the imports is required. While each component is sorted that's
        // not enough to ensure everything is sorted.
        //
        // For example let's say there's three interfaces A, B, and C. C
        // depends on both A and B. Then let's have one component import both A
        // and C and a second component imports B and C. This can be valid where
        // C can be "sliced up" where only the portions that depend on A can be
        // separated from the portions that depend on B.
        // When walking over the instances above we'll first generate imports
        // of A and C. Next though the next instance will add and import for B,
        // meaning that the import list is A, C, B. This is not a correct
        // topological ordering, which is what this sort is solving.
        let mut order = IndexSet::new();
        for (name, _) in self.0.iter() {
            toposort(&name[..], &deps, &mut order);
        }
        let order = order
            .into_iter()
            .map(|s| s.to_string())
            .collect::<IndexSet<_>>();
        self.0
            .sort_by_cached_key(|name, _| order.get_full(&name[..]).unwrap().0);

        Ok(())
    }
}

/// Helper structure used to fill out the `deps` and `defining_instances` maps
/// when building the imports to a component.
///
/// The goal of this structure is to take types defined within `self.cur` and
/// draw edges in the `deps` map from `cur` to those names in
/// `defining_instances`.
///
/// For example if a type refers to a name defined in a different instance then
/// that previous instance will already be registered in `defining_instances`
/// and that'll draw a dependency from `self.cur` to that name.
struct DependencyRegistrar<'a, 'b> {
    types: &'a types::Types,
    defining_instances: &'b mut HashMap<TypeId, &'a str>,
    cur: &'a str,
    deps: &'b mut HashMap<&'a str, Vec<&'a str>>,
}

impl DependencyRegistrar<'_, '_> {
    fn entity(&mut self, ty: ComponentEntityType) {
        match ty {
            ComponentEntityType::Type {
                created,
                referenced,
            } => {
                let prev = self.defining_instances.insert(created, self.cur);
                assert!(prev.is_none());
                self.ty(referenced);
            }
            ComponentEntityType::Module(_) => {}
            ComponentEntityType::Value(v) => self.val_type(v),
            ComponentEntityType::Instance(e) => self.instance(e),
            ComponentEntityType::Component(e) => self.component(e),
            ComponentEntityType::Func(e) => self.func(e),
        }
    }

    fn ty(&mut self, ty: TypeId) {
        match self.defining_instances.entry(ty) {
            // If it's known where `ty` is defined then there's nothing else to
            // do here beyond drawing a new dependency edge. Note though that
            // "edges to self" are skipped explicitly here.
            Entry::Occupied(e) => {
                if *e.get() != self.cur {
                    self.deps.entry(self.cur).or_default().push(*e.get());
                }
                return;
            }

            // Otherwise `ty` is now registered as defined by `self.cur` and we
            // continue below.
            Entry::Vacant(e) => {
                e.insert(self.cur);
            }
        }

        // Recurse for aliases to see edges across components, and otherwise
        // recurse on the structure of the type below.
        if let Some(ty) = self.types.peel_alias(ty) {
            return self.ty(ty);
        }

        match &self.types[ty] {
            Type::Instance(_) | Type::Sub(_) | Type::Module(_) => {}
            Type::Component(_) => self.component(ty),
            Type::ComponentInstance(_) => self.instance(ty),
            Type::ComponentFunc(_) => self.func(ty),
            Type::Defined(_) => self.defined(ty),
            Type::Resource(_) => {}
        }
    }

    fn val_type(&mut self, ty: types::ComponentValType) {
        match ty {
            types::ComponentValType::Type(t) => self.ty(t),
            types::ComponentValType::Primitive(_) => {}
        }
    }

    fn component(&mut self, ty: TypeId) {
        let ty = &self.types[ty].unwrap_component();
        for (_, ty) in ty.imports.iter().chain(&ty.exports) {
            self.entity(*ty);
        }
    }

    fn instance(&mut self, ty: TypeId) {
        for (_, ty) in self.types[ty].unwrap_component_instance().exports.iter() {
            self.entity(*ty);
        }
    }

    fn func(&mut self, ty: TypeId) {
        let ty = &self.types[ty].unwrap_component_func();
        for ty in ty
            .params
            .iter()
            .map(|p| p.1)
            .chain(ty.results.iter().map(|p| p.1))
        {
            self.val_type(ty);
        }
    }

    fn defined(&mut self, ty: TypeId) {
        match &self.types[ty].unwrap_defined() {
            types::ComponentDefinedType::Primitive(_)
            | types::ComponentDefinedType::Enum(_)
            | types::ComponentDefinedType::Flags(_) => {}
            types::ComponentDefinedType::List(t) | types::ComponentDefinedType::Option(t) => {
                self.val_type(*t)
            }
            types::ComponentDefinedType::Own(t) | types::ComponentDefinedType::Borrow(t) => {
                self.ty(*t)
            }
            types::ComponentDefinedType::Record(r) => {
                for (_, ty) in r.fields.iter() {
                    self.val_type(*ty);
                }
            }
            types::ComponentDefinedType::Tuple(r) => {
                for ty in r.types.iter() {
                    self.val_type(*ty);
                }
            }
            types::ComponentDefinedType::Variant(r) => {
                for (_, case) in r.cases.iter() {
                    if let Some(ty) = case.ty {
                        self.val_type(ty);
                    }
                }
            }
            types::ComponentDefinedType::Result { ok, err } => {
                if let Some(ok) = ok {
                    self.val_type(*ok);
                }
                if let Some(err) = err {
                    self.val_type(*err);
                }
            }
        }
    }
}

fn toposort<'a>(
    cur: &'a str,
    deps: &HashMap<&'a str, Vec<&'a str>>,
    order: &mut IndexSet<&'a str>,
) {
    if order.contains(cur) {
        return;
    }
    if let Some(list) = deps.get(cur) {
        for dep in list {
            toposort(dep, deps, order);
        }
    }
    let ok = order.insert(cur);
    assert!(ok);
}

/// Used to encode a composition graph as a new WebAssembly component.
pub(crate) struct CompositionGraphEncoder<'a> {
    /// The options for the encoding.
    options: EncodeOptions,
    /// The graph being encoded.
    graph: &'a CompositionGraph<'a>,
    /// Map from graph component index to encoded component index.
    encoded_components: HashMap<PtrKey<'a, crate::graph::Component<'a>>, u32>,
    /// Map from graph instance id to encoded instance index.
    encoded_instances: HashMap<InstanceId, u32>,
    /// Map from instance and import index to encoded item index.
    ///
    /// This is used for instantiation arguments that are imported.
    imported_args: HashMap<(InstanceIndex, ImportIndex), u32>,
    /// Map from instance id and export index to encoded item index.
    ///
    /// This is used to track instantiation arguments aliased from
    /// other instances.
    aliases: HashMap<(InstanceId, ExportIndex), u32>,
}

impl<'a> CompositionGraphEncoder<'a> {
    pub(crate) fn new(options: EncodeOptions, graph: &'a CompositionGraph) -> Self {
        Self {
            options,
            graph,
            encoded_components: Default::default(),
            encoded_instances: Default::default(),
            imported_args: Default::default(),
            aliases: Default::default(),
        }
    }

    pub(crate) fn encode(mut self) -> Result<Vec<u8>> {
        let mut encoded = ComponentBuilder::default();

        self.encode_imports(&mut encoded)?;
        self.encode_components(&mut encoded);
        self.encode_instantiations(&mut encoded)?;

        if let Some(id) = self.options.export {
            self.encode_exports(&mut encoded, id)?;
        }

        Ok(encoded.finish())
    }

    fn encode_imports(&mut self, encoded: &mut ComponentBuilder) -> Result<()> {
        let imports = ImportMap::new(!self.options.define_components, self.graph)?;

        // Create new state to track where types are defined and in which
        // instances. Temporarily "move" the `encoded` builder into this state.
        let mut state = TypeState::new();
        state.cur.encodable = Encodable::Builder(mem::take(encoded));

        for (name, entry) in imports.0 {
            log::trace!("encoding instance import {name}");
            match entry {
                ImportMapEntry::Component(component) => {
                    let encoded = match &mut state.cur.encodable {
                        Encodable::Builder(builder) => builder,
                        _ => unreachable!(),
                    };
                    self.encode_component_import(encoded, name.as_ref(), component);
                }
                ImportMapEntry::Argument(arg) => {
                    let index = match arg.kind {
                        ArgumentImportKind::Item(component, ty) => {
                            self.encode_item_import(&mut state, name.as_ref(), component, ty)
                        }
                        ArgumentImportKind::Instance(exports) => {
                            self.encode_instance_import(&mut state, name.as_ref(), exports)
                        }
                    };

                    self.imported_args
                        .extend(arg.instances.into_iter().map(|k| (k, index)));
                }
            }
        }

        // "Move" the builder back out from the state into this function's
        // parameter.
        match &mut state.cur.encodable {
            Encodable::Builder(builder) => *encoded = mem::take(builder),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn encode_component_import(
        &mut self,
        encoded: &mut ComponentBuilder,
        name: &str,
        component: &'a crate::graph::Component,
    ) -> u32 {
        let type_index = self.define_component_type(encoded, component);
        let index = self.import(encoded, name, ComponentTypeRef::Component(type_index));

        assert!(self
            .encoded_components
            .insert(PtrKey(component), index)
            .is_none());

        index
    }

    fn encode_item_import(
        &mut self,
        state: &mut TypeState<'a>,
        name: &str,
        component: &'a crate::graph::Component,
        ty: ComponentEntityType,
    ) -> u32 {
        log::trace!("encoding item import {name}");
        let encoder = TypeEncoder::new(component);
        let ty = encoder.component_entity_type(state, ty);

        let encoded = match &mut state.cur.encodable {
            Encodable::Builder(builder) => builder,
            _ => unreachable!(),
        };
        self.import(encoded, name, ty)
    }

    fn encode_instance_import(
        &mut self,
        state: &mut TypeState<'a>,
        name: &str,
        exports: IndexMap<&'a str, Vec<(&'a crate::graph::Component, ComponentEntityType)>>,
    ) -> u32 {
        log::trace!("encoding instance import {name}");
        state.push(Encodable::Instance(InstanceType::new()));
        for (name, types) in exports {
            let (component, ty) = types[0];
            log::trace!("export {name}");
            let export = TypeEncoder::new(component).export(name, ty, state);
            let t = match &mut state.cur.encodable {
                Encodable::Instance(c) => c,
                _ => unreachable!(),
            };
            t.export(name, export);

            for (component, ty) in types.iter().skip(1) {
                if let ComponentEntityType::Type { created, .. } = ty {
                    state
                        .cur
                        .add_type_export((PtrKey(component), *created), name);
                }
            }
        }
        let instance_type = match state.pop() {
            Encodable::Instance(c) => c,
            _ => unreachable!(),
        };

        let encoded = match &mut state.cur.encodable {
            Encodable::Builder(builder) => builder,
            _ => unreachable!(),
        };
        let index = encoded.type_instance(&instance_type);
        self.import(encoded, name, ComponentTypeRef::Instance(index))
    }

    fn encode_instantiations(&mut self, encoded: &mut ComponentBuilder) -> Result<()> {
        let ordering = self.graph.instantiation_order()?;

        // Encode the independent instances first
        for id in self
            .graph
            .instances
            .keys()
            .filter(|id| !ordering.contains(*id))
        {
            self.encode_instantiation(encoded, *id)?;
        }

        // Encode the dependent instances last
        for id in ordering {
            self.encode_instantiation(encoded, id)?;
        }

        Ok(())
    }

    fn encode_exports(
        &mut self,
        encoded: &mut ComponentBuilder,
        instance_id: InstanceId,
    ) -> Result<()> {
        let instance = self.graph.instances.get(&instance_id).ok_or_else(|| {
            anyhow!("cannot export specified instance because it does not exist in the graph")
        })?;
        let entry = self.graph.components.get(&instance.component).unwrap();

        let encoded_instance_index = self.encoded_instances[&instance_id];

        for (export_index, export_name, kind, _) in entry.component.exports() {
            let kind = match kind {
                ComponentExternalKind::Module => ComponentExportKind::Module,
                ComponentExternalKind::Func => ComponentExportKind::Func,
                ComponentExternalKind::Value => ComponentExportKind::Value,
                ComponentExternalKind::Type => ComponentExportKind::Type,
                ComponentExternalKind::Instance => ComponentExportKind::Instance,
                ComponentExternalKind::Component => ComponentExportKind::Component,
            };

            let index = match self.aliases.get(&(instance_id, export_index)) {
                Some(index) => *index,
                None => {
                    let index = self.alias(encoded, encoded_instance_index, export_name, kind);
                    self.aliases.insert((instance_id, export_index), index);
                    index
                }
            };

            encoded.export(export_name, kind, index, None);
        }

        Ok(())
    }

    fn encode_instantiation(
        &mut self,
        encoded: &mut ComponentBuilder,
        instance_id: InstanceId,
    ) -> Result<()> {
        let (instance_index, _, instance) = self.graph.instances.get_full(&instance_id).unwrap();
        let entry = &self.graph.components.get(&instance.component).unwrap();

        let instance_index = InstanceIndex(instance_index);
        let encoded_component_index = self.encoded_components[&PtrKey(&entry.component)];

        let args = self.instantiation_args(encoded, instance_index, &entry.component);

        log::debug!(
            "instantiating component `{name}` with {args:?}",
            name = entry.component.name,
        );

        let encoded_instance_index = encoded.instantiate(encoded_component_index, args);

        self.encoded_instances
            .insert(instance_id, encoded_instance_index);

        Ok(())
    }

    fn encode_components(&mut self, encoded: &mut ComponentBuilder) {
        if !self.options.define_components {
            return;
        }

        for entry in self
            .graph
            .components
            .values()
            .filter(|e| !e.instances.is_empty())
        {
            let index = self.define_component(encoded, &entry.component);
            assert!(self
                .encoded_components
                .insert(PtrKey(&entry.component), index)
                .is_none());
        }
    }

    fn define_component_type(
        &mut self,
        encoded: &mut ComponentBuilder,
        component: &crate::graph::Component,
    ) -> u32 {
        encoded.type_component(&component.ty())
    }

    fn define_component(
        &mut self,
        encoded: &mut ComponentBuilder,
        component: &crate::graph::Component,
    ) -> u32 {
        log::debug!(
            "defining component `{name}` in composed component",
            name = component.name,
        );
        encoded.component_raw(component.bytes())
    }

    fn instantiation_args(
        &mut self,
        encoded: &mut ComponentBuilder,
        instance_index: InstanceIndex,
        component: &'a crate::graph::Component,
    ) -> Vec<(&'a str, ComponentExportKind, u32)> {
        let (instance_id, instance) = self.graph.instances.get_index(instance_index.0).unwrap();
        let mut args = Vec::with_capacity(component.imports.len());

        // Add the arguments that are aliased exports from other instances
        for (source_id, _, map) in self
            .graph
            .graph
            .edges_directed(*instance_id, EdgeDirection::Incoming)
        {
            assert!(source_id != *instance_id);
            let source_index = self.encoded_instances[&source_id];
            let (_, source_component) = &self.graph.get_component_of_instance(source_id).unwrap();

            for (import_index, export_index) in map {
                // Check to see if we need to alias the item from the source instance
                let (name, ty) = component.import(*import_index).unwrap();
                let index = match export_index {
                    Some(export_index) => {
                        let (export_name, _, _) = source_component.export(*export_index).unwrap();
                        match self.aliases.get(&(source_id, *export_index)) {
                            Some(index) => *index,
                            None => {
                                let index = self.alias(
                                    encoded,
                                    source_index,
                                    export_name,
                                    type_ref_to_export_kind(ty),
                                );
                                self.aliases.insert((source_id, *export_index), index);
                                index
                            }
                        }
                    }
                    None => source_index,
                };
                args.push((name, type_ref_to_export_kind(ty), index));
            }
        }

        // Finally, add any instantiation arguments that are being imported
        for (i, (name, ty)) in component.imports.iter().enumerate() {
            let import_index = ImportIndex(i);
            if instance.connected.contains(&import_index) {
                continue;
            }

            let index = self.imported_args[&(instance_index, import_index)];
            args.push((name.as_str(), type_ref_to_export_kind(*ty), index));
        }

        args
    }

    fn import(&mut self, encoded: &mut ComponentBuilder, name: &str, ty: ComponentTypeRef) -> u32 {
        log::debug!("importing {ty:?} with `{name}` in composed component");
        encoded.import(name, ty)
    }

    fn alias(
        &mut self,
        encoded: &mut ComponentBuilder,
        instance: u32,
        name: &str,
        kind: ComponentExportKind,
    ) -> u32 {
        log::debug!(
            "aliasing {kind:?} export `{name}` from encoded index {instance} in composed component"
        );
        encoded.alias_export(instance, name, kind)
    }
}
