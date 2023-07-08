use crate::graph::{
    type_desc, CompositionGraph, EncodeOptions, ExportIndex, ImportIndex, InstanceId,
};
use anyhow::{anyhow, bail, Result};
use heck::ToKebabCase;
use indexmap::{IndexMap, IndexSet};
use petgraph::EdgeDirection;
use smallvec::SmallVec;
use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap},
};
use wasm_encoder::*;
use wasmparser::{
    names::KebabString,
    types::{ComponentEntityType, Type, TypeId, Types},
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

// Utility trait implement on component and instance types
// to abstract their encoding.
trait Encodable {
    fn type_count(&self) -> u32;
    fn core_type_count(&self) -> u32;
    fn ty(&mut self) -> ComponentTypeEncoder;
    fn core_type(&mut self) -> CoreTypeEncoder;
}

impl Encodable for ComponentType {
    fn type_count(&self) -> u32 {
        self.type_count()
    }

    fn core_type_count(&self) -> u32 {
        self.core_type_count()
    }

    fn ty(&mut self) -> ComponentTypeEncoder {
        self.ty()
    }

    fn core_type(&mut self) -> CoreTypeEncoder {
        self.core_type()
    }
}

impl Encodable for InstanceType {
    fn type_count(&self) -> u32 {
        self.type_count()
    }

    fn core_type_count(&self) -> u32 {
        self.core_type_count()
    }

    fn ty(&mut self) -> ComponentTypeEncoder {
        self.ty()
    }

    fn core_type(&mut self) -> CoreTypeEncoder {
        self.core_type()
    }
}

struct EncodableEntityType<'a> {
    start_type_count: u32,
    core_types: &'a mut CoreTypeSection,
    component_types: &'a mut ComponentTypeSection,
}

impl<'a> Encodable for EncodableEntityType<'a> {
    fn type_count(&self) -> u32 {
        self.start_type_count + self.component_types.len()
    }

    fn core_type_count(&self) -> u32 {
        self.core_types.len()
    }

    fn ty(&mut self) -> ComponentTypeEncoder {
        self.component_types.ty()
    }

    fn core_type(&mut self) -> CoreTypeEncoder {
        self.core_types.ty()
    }
}

#[derive(Copy, Clone)]
pub struct PtrKey<'a, T>(&'a T);

impl<T> PartialEq for PtrKey<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<T> Eq for PtrKey<'_, T> {}

impl<T> std::hash::Hash for PtrKey<'_, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state);
    }
}

/// Represents a type map used in encoding.
///
/// This implementation prevents encoding of duplicate types from the same
/// validator, but not from different validators.
///
/// TODO: implement proper equality checks in `wasmparser` as to eliminate
/// all type duplicates?
type TypeMap<'a> = HashMap<PtrKey<'a, Type>, u32>;

pub(crate) struct TypeEncoder<'a>(&'a Types);

impl<'a> TypeEncoder<'a> {
    pub fn new(types: &'a Types) -> Self {
        Self(types)
    }

    pub fn component<I, E>(&self, imports: I, exports: E) -> ComponentType
    where
        I: IntoIterator<Item = (&'a str, wasmparser::types::ComponentEntityType)>,
        E: IntoIterator<Item = (&'a str, wasmparser::types::ComponentEntityType)>,
    {
        let mut encoded = ComponentType::new();
        let mut types = TypeMap::new();

        for (name, ty) in imports {
            let ty = self.component_entity_type(&mut encoded, &mut types, ty);
            encoded.import(name, ty);
        }

        for (name, ty) in exports {
            let export = self.export(ty, &mut encoded, &mut types);
            encoded.export(name, export);
        }

        encoded
    }

    pub fn instance<E>(&self, exports: E) -> InstanceType
    where
        E: IntoIterator<Item = (&'a str, wasmparser::types::ComponentEntityType)>,
    {
        let mut encoded = InstanceType::new();
        let mut types = TypeMap::new();

        for (name, ty) in exports {
            let export = self.export(ty, &mut encoded, &mut types);
            encoded.export(name, export);
        }

        encoded
    }

    pub fn module<I, E>(&self, imports: I, exports: E) -> ModuleType
    where
        I: IntoIterator<Item = (&'a str, &'a str, wasmparser::types::EntityType)>,
        E: IntoIterator<Item = (&'a str, wasmparser::types::EntityType)>,
    {
        let mut encoded = ModuleType::new();
        let mut types = TypeMap::new();

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
        types: &mut TypeMap<'a>,
        ty: wasmparser::types::EntityType,
    ) -> EntityType {
        match ty {
            wasmparser::types::EntityType::Func(id) => {
                let ty = self.0.type_from_id(id).unwrap();
                let idx = match types.entry(PtrKey(ty)) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let ty = ty.as_func_type().unwrap();
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
                let ty = self.0.type_from_id(id).unwrap();
                let idx = match types.entry(PtrKey(ty)) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let ty = ty.as_func_type().unwrap();
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
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        ty: wasmparser::types::ComponentEntityType,
    ) -> ComponentTypeRef {
        match ty {
            wasmparser::types::ComponentEntityType::Module(id) => {
                ComponentTypeRef::Module(self.module_type(encodable, types, id))
            }
            wasmparser::types::ComponentEntityType::Func(id) => {
                ComponentTypeRef::Func(self.component_func_type(encodable, types, id))
            }
            wasmparser::types::ComponentEntityType::Value(ty) => {
                ComponentTypeRef::Value(self.component_val_type(encodable, types, ty))
            }
            wasmparser::types::ComponentEntityType::Type { created, .. } => {
                ComponentTypeRef::Type(TypeBounds::Eq(self.ty(encodable, types, created)))
            }
            wasmparser::types::ComponentEntityType::Instance(id) => {
                ComponentTypeRef::Instance(self.component_instance_type(encodable, types, id))
            }
            wasmparser::types::ComponentEntityType::Component(id) => {
                ComponentTypeRef::Component(self.component_type(encodable, types, id))
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

    fn module_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        id: TypeId,
    ) -> u32 {
        let ty = self.0.type_from_id(id).unwrap();
        match types.entry(PtrKey(ty)) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let ty = ty.as_module_type().unwrap();

                let module = self.module(
                    ty.imports
                        .iter()
                        .map(|((m, n), t)| (m.as_str(), n.as_str(), *t)),
                    ty.exports.iter().map(|(n, t)| (n.as_str(), *t)),
                );

                let index = encodable.core_type_count();
                encodable.core_type().module(&module);
                *e.insert(index)
            }
        }
    }

    fn component_instance_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        id: TypeId,
    ) -> u32 {
        let ty = self.0.type_from_id(id).unwrap();
        match types.entry(PtrKey(ty)) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let ty = ty.as_component_instance_type().unwrap();
                let instance = self.instance(ty.exports.iter().map(|(n, t)| (n.as_str(), *t)));
                let index = encodable.type_count();
                encodable.ty().instance(&instance);
                *e.insert(index)
            }
        }
    }

    fn component_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        id: TypeId,
    ) -> u32 {
        let ty = self.0.type_from_id(id).unwrap();
        match types.entry(PtrKey(ty)) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let ty = ty.as_component_type().unwrap();

                let component = self.component(
                    ty.imports.iter().map(|(n, t)| (n.as_str(), *t)),
                    ty.exports.iter().map(|(n, t)| (n.as_str(), *t)),
                );

                let index = encodable.type_count();
                encodable.ty().component(&component);
                *e.insert(index)
            }
        }
    }

    fn component_func_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        id: TypeId,
    ) -> u32 {
        let ty = self.0.type_from_id(id).unwrap();
        if let Some(idx) = types.get(&PtrKey(ty)) {
            return *idx;
        }

        let func_ty = ty.as_component_func_type().unwrap();

        let params = func_ty
            .params
            .iter()
            .map(|(name, ty)| {
                (
                    name.as_str(),
                    self.component_val_type(encodable, types, *ty),
                )
            })
            .collect::<Vec<_>>();

        let results = func_ty
            .results
            .iter()
            .map(|(name, ty)| {
                (
                    name.as_deref(),
                    self.component_val_type(encodable, types, *ty),
                )
            })
            .collect::<Vec<_>>();

        let index = encodable.type_count();
        let mut f = encodable.ty().function();

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

        types.insert(PtrKey(ty), index);
        index
    }

    fn ty(&self, encodable: &mut impl Encodable, types: &mut TypeMap<'a>, id: TypeId) -> u32 {
        let ty = self.0.type_from_id(id).unwrap();

        match ty {
            Type::Sub(_) | Type::Instance(_) => {
                unreachable!()
            }
            Type::Module(_) => self.module_type(encodable, types, id),
            Type::Component(_) => self.component_type(encodable, types, id),
            Type::ComponentInstance(_) => self.component_instance_type(encodable, types, id),
            Type::ComponentFunc(_) => self.component_func_type(encodable, types, id),
            Type::Defined(_) => self.defined_type(encodable, types, id),
            Type::Resource(_) => unimplemented!(),
        }
    }

    fn component_val_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        ty: wasmparser::types::ComponentValType,
    ) -> ComponentValType {
        match ty {
            wasmparser::types::ComponentValType::Primitive(ty) => {
                ComponentValType::Primitive(Self::primitive(ty))
            }
            wasmparser::types::ComponentValType::Type(id) => {
                ComponentValType::Type(self.defined_type(encodable, types, id))
            }
        }
    }

    fn defined_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        id: TypeId,
    ) -> u32 {
        let ty = self.0.type_from_id(id).unwrap();
        if let Some(idx) = types.get(&PtrKey(ty)) {
            return *idx;
        }

        let defined_ty = ty.as_defined_type().unwrap();

        let index = match defined_ty {
            wasmparser::types::ComponentDefinedType::Primitive(ty) => {
                let index = encodable.type_count();
                encodable
                    .ty()
                    .defined_type()
                    .primitive(Self::primitive(*ty));
                index
            }
            wasmparser::types::ComponentDefinedType::Record(r) => self.record(encodable, types, r),
            wasmparser::types::ComponentDefinedType::Variant(v) => {
                self.variant(encodable, types, v)
            }
            wasmparser::types::ComponentDefinedType::List(ty) => self.list(encodable, types, *ty),
            wasmparser::types::ComponentDefinedType::Tuple(t) => self.tuple(encodable, types, t),
            wasmparser::types::ComponentDefinedType::Flags(names) => Self::flags(encodable, names),
            wasmparser::types::ComponentDefinedType::Enum(cases) => {
                Self::enum_type(encodable, cases)
            }
            wasmparser::types::ComponentDefinedType::Union(u) => self.union(encodable, types, u),
            wasmparser::types::ComponentDefinedType::Option(ty) => {
                self.option(encodable, types, *ty)
            }
            wasmparser::types::ComponentDefinedType::Result { ok, err } => {
                self.result(encodable, types, *ok, *err)
            }
            wasmparser::types::ComponentDefinedType::Own(id) => {
                let i = self.defined_type(encodable, types, *id);
                let index = encodable.type_count();
                encodable.ty().defined_type().own(i);
                index
            }
            wasmparser::types::ComponentDefinedType::Borrow(id) => {
                let i = self.defined_type(encodable, types, *id);
                let index = encodable.type_count();
                encodable.ty().defined_type().borrow(i);
                index
            }
        };

        types.insert(PtrKey(ty), index);
        index
    }

    fn record(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        record: &wasmparser::types::RecordType,
    ) -> u32 {
        let fields = record
            .fields
            .iter()
            .map(|(n, ty)| (n.as_str(), self.component_val_type(encodable, types, *ty)))
            .collect::<Vec<_>>();

        let index = encodable.type_count();
        encodable.ty().defined_type().record(fields);
        index
    }

    fn variant(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        variant: &wasmparser::types::VariantType,
    ) -> u32 {
        let cases = variant
            .cases
            .iter()
            .map(|(n, c)| {
                (
                    n.as_str(),
                    c.ty.map(|ty| self.component_val_type(encodable, types, ty)),
                    c.refines
                        .as_deref()
                        .map(|r| variant.cases.iter().position(|(n, _)| n == r).unwrap() as u32),
                )
            })
            .collect::<Vec<_>>();
        let index = encodable.type_count();
        encodable.ty().defined_type().variant(cases);
        index
    }

    fn list(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        ty: wasmparser::types::ComponentValType,
    ) -> u32 {
        let ty = self.component_val_type(encodable, types, ty);
        let index = encodable.type_count();
        encodable.ty().defined_type().list(ty);
        index
    }

    fn tuple(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        tuple: &wasmparser::types::TupleType,
    ) -> u32 {
        let types = tuple
            .types
            .iter()
            .map(|ty| self.component_val_type(encodable, types, *ty))
            .collect::<Vec<_>>();
        let index = encodable.type_count();
        encodable.ty().defined_type().tuple(types);
        index
    }

    fn flags(encodable: &mut impl Encodable, names: &IndexSet<KebabString>) -> u32 {
        let index = encodable.type_count();
        encodable
            .ty()
            .defined_type()
            .flags(names.iter().map(|n| n.as_str()));
        index
    }

    fn enum_type(encodable: &mut impl Encodable, cases: &IndexSet<KebabString>) -> u32 {
        let index = encodable.type_count();
        encodable
            .ty()
            .defined_type()
            .enum_type(cases.iter().map(|c| c.as_str()));
        index
    }

    fn union(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        union: &wasmparser::types::UnionType,
    ) -> u32 {
        let types = union
            .types
            .iter()
            .map(|ty| self.component_val_type(encodable, types, *ty))
            .collect::<Vec<_>>();

        let index = encodable.type_count();
        encodable.ty().defined_type().union(types);
        index
    }

    fn option(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        ty: wasmparser::types::ComponentValType,
    ) -> u32 {
        let ty = self.component_val_type(encodable, types, ty);

        let index = encodable.type_count();
        encodable.ty().defined_type().option(ty);
        index
    }

    fn result(
        &self,
        encodable: &mut impl Encodable,
        types: &mut TypeMap<'a>,
        ok: Option<wasmparser::types::ComponentValType>,
        err: Option<wasmparser::types::ComponentValType>,
    ) -> u32 {
        let ok = ok.map(|ty| self.component_val_type(encodable, types, ty));
        let err = err.map(|ty| self.component_val_type(encodable, types, ty));

        let index = encodable.type_count();
        encodable.ty().defined_type().result(ok, err);
        index
    }

    fn export(
        &self,
        export: ComponentEntityType,
        encoded: &mut impl Encodable,
        types: &mut TypeMap<'a>,
    ) -> ComponentTypeRef {
        // Check if the export is a type; if so, we need to update the index of the
        // type to point to the export instead of the original definition
        let id = match export {
            ComponentEntityType::Type { created: id, .. } => Some(id),
            _ => None,
        };
        let export = self.component_entity_type(encoded, types, export);
        if let Some(id) = id {
            // Update the index in the type map to point to this export
            let ty = self.0.type_from_id(id).unwrap();
            types.insert(PtrKey(ty), encoded.type_count());
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
    Instance(IndexMap<&'a str, (&'a crate::graph::Component<'a>, ComponentEntityType)>),
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
            let exports = component
                .types
                .type_from_id(*id)
                .unwrap()
                .as_component_instance_type()
                .unwrap()
                .exports
                .iter();

            let mut map = IndexMap::with_capacity(exports.len());
            for (name, ty) in exports {
                map.insert(name.as_str(), (*component, *ty));
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
                for (name, new_type) in new_component
                    .types
                    .type_from_id(id)
                    .unwrap()
                    .as_component_instance_type()
                    .unwrap()
                    .exports
                    .iter()
                {
                    match exports.entry(name.as_str()) {
                        indexmap::map::Entry::Occupied(mut e) => {
                            let (existing_component, existing_type) = e.get_mut();
                            match Self::compatible_type(
                                existing_component,
                                *existing_type,
                                new_component,
                                *new_type,
                            ) {
                                Some((c, ty)) => {
                                    *existing_component = c;
                                    *existing_type = ty;
                                }
                                None => bail!(
                                    "cannot import instance with name `{name}` for an instantiation argument of component `{cname}` because it conflicts with an imported instantiation argument of component `{ecname}`",
                                    name = self.name,
                                    cname = new_component.name,
                                    ecname = existing_component.name,
                                ),
                            }
                        }
                        indexmap::map::Entry::Vacant(e) => {
                            e.insert((new_component, *new_type));
                        }
                    }
                }
            }
            // Otherwise, an attempt to merge an instance with a non-instance is an error
            (ArgumentImportKind::Instance(_), ArgumentImportKind::Item(component, ty)) => {
                bail!(
                    "cannot import {ty} with name `{name}` for an instantiation argument of component `{cname}` because it conflicts with an instance imported with the same name",
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
                match Self::compatible_type(
                    existing_component,
                    *existing_type,
                    new_component,
                    new_type,
                ) {
                    Some((c, ty)) => {
                        *existing_component = c;
                        *existing_type = ty;
                    }
                    None => bail!(
                        "cannot import {ty} with name `{name}` for an instantiation argument of component `{cname}` because it conflicts with an imported instantiation argument of component `{ecname}`",
                        ty = type_desc(new_type),
                        name = self.name,
                        cname = new_component.name,
                        ecname = existing_component.name,
                    ),
                }
            }
        }

        Ok(())
    }

    fn compatible_type<'a>(
        existing_component: &'a crate::graph::Component<'a>,
        existing_type: ComponentEntityType,
        new_component: &'a crate::graph::Component<'a>,
        new_type: ComponentEntityType,
    ) -> Option<(&'a crate::graph::Component<'a>, ComponentEntityType)> {
        if ComponentEntityType::is_subtype_of(
            &existing_type,
            existing_component.types(),
            &new_type,
            new_component.types(),
        ) {
            return Some((existing_component, existing_type));
        }

        if ComponentEntityType::is_subtype_of(
            &new_type,
            new_component.types(),
            &existing_type,
            existing_component.types(),
        ) {
            return Some((new_component, new_type));
        }

        None
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

        for (instance_index, instance) in graph.instances.values().enumerate() {
            let (component_index, _, entry) =
                graph.components.get_full(&instance.component).unwrap();

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

        Ok(())
    }
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
    /// The number of modules encoded (i.e. next module index).
    modules: u32,
    /// The number of component functions encoded (i.e. next function index).
    funcs: u32,
    /// The number of values encoded (i.e. next value index).
    values: u32,
    /// The number of types encoded (i.e. next type index).
    types: u32,
    /// The number of component instances encoded (i.e. next instance index).
    instances: u32,
    /// The number of components encoded (i.e. next component index).
    components: u32,
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
            modules: 0,
            funcs: 0,
            values: 0,
            types: 0,
            instances: 0,
            components: 0,
        }
    }

    pub(crate) fn encode(mut self) -> Result<Vec<u8>> {
        let mut encoded = Component::new();

        self.encode_imports(&mut encoded)?;
        self.encode_components(&mut encoded);
        self.encode_instantiations(&mut encoded)?;

        if let Some(id) = self.options.export {
            self.encode_exports(&mut encoded, id)?;
        }

        Ok(encoded.finish())
    }

    fn encode_imports(&mut self, encoded: &mut Component) -> Result<()> {
        let imports = ImportMap::new(!self.options.define_components, self.graph)?;
        let mut type_map = TypeMap::default();
        for (name, entry) in imports.0 {
            match entry {
                ImportMapEntry::Component(component) => {
                    self.encode_component_import(encoded, name.as_ref(), component);
                }
                ImportMapEntry::Argument(arg) => {
                    let index = match arg.kind {
                        ArgumentImportKind::Item(component, ty) => self.encode_item_import(
                            encoded,
                            &mut type_map,
                            name.as_ref(),
                            component,
                            ty,
                        ),
                        ArgumentImportKind::Instance(exports) => {
                            self.encode_instance_import(encoded, name.as_ref(), exports)
                        }
                    };

                    self.imported_args
                        .extend(arg.instances.into_iter().map(|k| (k, index)));
                }
            }
        }

        Ok(())
    }

    fn encode_component_import(
        &mut self,
        encoded: &mut Component,
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
        encoded: &mut Component,
        type_map: &mut TypeMap<'a>,
        name: &str,
        component: &'a crate::graph::Component,
        ty: ComponentEntityType,
    ) -> u32 {
        let mut core_type_section = CoreTypeSection::new();
        let mut type_section = ComponentTypeSection::new();

        let encoder = TypeEncoder::new(&component.types);

        let mut encodable = EncodableEntityType {
            start_type_count: self.types,
            core_types: &mut core_type_section,
            component_types: &mut type_section,
        };
        let ty = encoder.component_entity_type(&mut encodable, type_map, ty);

        if !core_type_section.is_empty() {
            encoded.section(&core_type_section);
        }

        if !type_section.is_empty() {
            encoded.section(&type_section);
            self.types += type_section.len();
        }

        self.import(encoded, name, ty)
    }

    fn encode_instance_import(
        &mut self,
        encoded: &mut Component,
        name: &str,
        exports: IndexMap<&'a str, (&'a crate::graph::Component, ComponentEntityType)>,
    ) -> u32 {
        let mut instance_type = InstanceType::new();
        let mut types = TypeMap::new();
        for (name, (component, ty)) in exports {
            let encoder = TypeEncoder::new(&component.types);
            let export = encoder.export(ty, &mut instance_type, &mut types);
            instance_type.export(name, export);
        }

        let index = self.types;
        let mut type_section = ComponentTypeSection::new();
        type_section.instance(&instance_type);
        encoded.section(&type_section);
        self.types += 1;

        self.import(encoded, name, ComponentTypeRef::Instance(index))
    }

    fn encode_instantiations(&mut self, encoded: &mut Component) -> Result<()> {
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

    fn encode_exports(&mut self, encoded: &mut Component, instance_id: InstanceId) -> Result<()> {
        let instance = self.graph.instances.get(&instance_id).ok_or_else(|| {
            anyhow!("cannot export specified instance because it does not exist in the graph")
        })?;
        let entry = self.graph.components.get(&instance.component).unwrap();

        let encoded_instance_index = self.encoded_instances[&instance_id];

        let mut alias_section = ComponentAliasSection::new();
        let mut export_section = ComponentExportSection::new();
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
                    let index = self.alias(
                        &mut alias_section,
                        encoded_instance_index,
                        export_name,
                        kind,
                    );
                    self.aliases.insert((instance_id, export_index), index);
                    index
                }
            };

            export_section.export(export_name, kind, index, None);
        }

        if !alias_section.is_empty() {
            encoded.section(&alias_section);
        }

        if !export_section.is_empty() {
            encoded.section(&export_section);
        }

        Ok(())
    }

    fn encode_instantiation(
        &mut self,
        encoded: &mut Component,
        instance_id: InstanceId,
    ) -> Result<()> {
        let (instance_index, _, instance) = self.graph.instances.get_full(&instance_id).unwrap();
        let entry = &self.graph.components.get(&instance.component).unwrap();

        let instance_index = InstanceIndex(instance_index);
        let encoded_component_index = self.encoded_components[&PtrKey(&entry.component)];

        let mut alias_section = ComponentAliasSection::new();
        let args = self.instantiation_args(instance_index, &entry.component, &mut alias_section);

        if !alias_section.is_empty() {
            encoded.section(&alias_section);
        }

        log::debug!(
            "instantiating component `{name}` (encoded index {instance_index}) with {args:?}",
            name = entry.component.name,
            instance_index = self.instances,
        );

        let mut instance_section = ComponentInstanceSection::new();
        let encoded_instance_index = self.instances;
        instance_section.instantiate(encoded_component_index, args);
        self.instances += 1;
        encoded.section(&instance_section);

        self.encoded_instances
            .insert(instance_id, encoded_instance_index);

        Ok(())
    }

    fn encode_components(&mut self, encoded: &mut Component) {
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
        encoded: &mut Component,
        component: &crate::graph::Component,
    ) -> u32 {
        let mut type_section = ComponentTypeSection::new();
        type_section.component(&component.ty());
        encoded.section(&type_section);

        let type_index = self.types;
        self.types += 1;
        type_index
    }

    fn define_component(
        &mut self,
        encoded: &mut Component,
        component: &crate::graph::Component,
    ) -> u32 {
        let index = self.components;

        log::debug!(
            "defining component `{name}` (encoded index {index}) in composed component",
            name = component.name,
        );

        encoded.section(&RawSection {
            id: ComponentSectionId::Component.into(),
            data: component.bytes(),
        });

        self.components += 1;
        index
    }

    fn instantiation_args(
        &mut self,
        instance_index: InstanceIndex,
        component: &'a crate::graph::Component,
        alias_section: &mut ComponentAliasSection,
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
                                    alias_section,
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

    fn import(&mut self, encoded: &mut Component, name: &str, ty: ComponentTypeRef) -> u32 {
        let (desc, count) = match ty {
            ComponentTypeRef::Module(_) => ("module", &mut self.modules),
            ComponentTypeRef::Func(_) => ("function", &mut self.funcs),
            ComponentTypeRef::Value(_) => ("value", &mut self.values),
            ComponentTypeRef::Type(_) => ("type", &mut self.types),
            ComponentTypeRef::Instance(_) => ("instance", &mut self.instances),
            ComponentTypeRef::Component(_) => ("component", &mut self.components),
        };

        log::debug!("importing {desc} with `{name}` (encoded index {count}) in composed component");

        let mut import_section = ComponentImportSection::new();
        import_section.import(name, ty);
        encoded.section(&import_section);

        let index = *count;
        *count += 1;
        index
    }

    fn alias(
        &mut self,
        aliases: &mut ComponentAliasSection,
        instance: u32,
        name: &str,
        kind: ComponentExportKind,
    ) -> u32 {
        let (desc, count) = match kind {
            ComponentExportKind::Module => ("module", &mut self.modules),
            ComponentExportKind::Func => ("function", &mut self.funcs),
            ComponentExportKind::Value => ("value", &mut self.values),
            ComponentExportKind::Type => ("type", &mut self.types),
            ComponentExportKind::Instance => ("instance", &mut self.instances),
            ComponentExportKind::Component => ("component", &mut self.components),
        };

        log::debug!("aliasing {desc} export `{name}` from encoded index {instance} (encoded index {count}) in composed component");

        aliases.alias(Alias::InstanceExport {
            instance,
            kind,
            name,
        });

        let index = *count;
        *count += 1;
        index
    }
}
