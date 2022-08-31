use crate::{
    composer::{
        Component, ComponentIndex, ExportIndex, ImportRef, InstanceIndex, InstantiationGraph,
    },
    config::Config,
};
use anyhow::{bail, Result};
use indexmap::{IndexMap, IndexSet};
use std::collections::{hash_map::Entry, HashMap, HashSet};
use wasm_encoder::*;
use wasmparser::types::{ComponentEntityType, Type, TypeId, TypesRef};

// Utility trait implement on component and instance types
// to abstract their encoding.
trait Encodable {
    fn type_count(&self) -> u32;
    fn ty(&mut self) -> ComponentTypeEncoder;
    fn core_type(&mut self) -> CoreTypeEncoder;
}

impl Encodable for ComponentType {
    fn type_count(&self) -> u32 {
        Self::type_count(self)
    }

    fn ty(&mut self) -> ComponentTypeEncoder {
        Self::ty(self)
    }

    fn core_type(&mut self) -> CoreTypeEncoder {
        self.core_type()
    }
}

impl Encodable for InstanceType {
    fn type_count(&self) -> u32 {
        self.type_count()
    }

    fn ty(&mut self) -> ComponentTypeEncoder {
        self.ty()
    }

    fn core_type(&mut self) -> CoreTypeEncoder {
        self.core_type()
    }
}

/// Represents a type key for type maps used in encoding.
///
/// This implementation prevents encoding of duplicate types from the same
/// validator, but not from different validators.
///
/// TODO: implement this fully in `wasmparser`?
#[derive(Copy, Clone)]
struct TypeKey<'a> {
    types: TypesRef<'a>,
    id: TypeId,
}

impl<'a> PartialEq for TypeKey<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(
            self.types.type_from_id(self.id).unwrap(),
            other.types.type_from_id(other.id).unwrap(),
        )
    }
}

impl<'a> Eq for TypeKey<'a> {}

impl std::hash::Hash for TypeKey<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        struct TypeHash<'a>(&'a Type);

        impl std::hash::Hash for TypeHash<'_> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                std::ptr::hash(self.0, state)
            }
        }

        TypeHash(self.types.type_from_id(self.id).unwrap()).hash(state);
    }
}

pub struct TypeEncoder<'a>(TypesRef<'a>);

impl<'a> TypeEncoder<'a> {
    pub fn new(types: TypesRef<'a>) -> Self {
        Self(types)
    }

    pub fn component<I, E>(&self, imports: I, exports: E) -> ComponentType
    where
        I: IntoIterator<Item = (&'a str, wasmparser::types::ComponentEntityType)>,
        E: IntoIterator<Item = (&'a str, wasmparser::types::ComponentEntityType)>,
    {
        let mut encoded = ComponentType::new();
        let mut types: HashMap<TypeKey<'a>, u32> = HashMap::new();

        for (name, ty) in imports {
            let ty = self.component_entity_type(&mut encoded, &mut types, ty);
            encoded.import(name, ty);
        }

        for (name, ty) in exports {
            let ty = self.component_entity_type(&mut encoded, &mut types, ty);
            encoded.export(name, ty);
        }

        encoded
    }

    pub fn instance<E>(&self, exports: E) -> InstanceType
    where
        E: IntoIterator<Item = (&'a str, wasmparser::types::ComponentEntityType)>,
    {
        let mut encoded = InstanceType::new();
        let mut types: HashMap<TypeKey<'a>, u32> = HashMap::new();

        for (name, ty) in exports {
            let ty = self.component_entity_type(&mut encoded, &mut types, ty);
            encoded.export(name, ty);
        }

        encoded
    }

    pub fn module<I, E>(&self, imports: I, exports: E) -> ModuleType
    where
        I: IntoIterator<Item = (&'a str, &'a str, wasmparser::types::EntityType)>,
        E: IntoIterator<Item = (&'a str, wasmparser::types::EntityType)>,
    {
        let mut encoded = ModuleType::new();
        let mut types: HashMap<TypeKey<'a>, u32> = HashMap::new();

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
        types: &mut HashMap<TypeKey<'a>, u32>,
        ty: wasmparser::types::EntityType,
    ) -> EntityType {
        match ty {
            wasmparser::types::EntityType::Func(id) => {
                let idx = match types.entry(TypeKey { types: self.0, id }) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let ty = self.0.type_from_id(id).unwrap().as_func_type().unwrap();
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
                let idx = match types.entry(TypeKey { types: self.0, id }) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let ty = self.0.type_from_id(id).unwrap().as_func_type().unwrap();
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

    fn val_type(ty: wasmparser::ValType) -> ValType {
        match ty {
            wasmparser::ValType::I32 => ValType::I32,
            wasmparser::ValType::I64 => ValType::I64,
            wasmparser::ValType::F32 => ValType::F32,
            wasmparser::ValType::F64 => ValType::F64,
            wasmparser::ValType::V128 => ValType::V128,
            wasmparser::ValType::Ref(ty) => Self::ref_type(ty),
            wasmparser::ValType::Bot => unimplemented!(),
        }
    }

    fn ref_type(ty: wasmparser::RefType) -> ValType {
        match ty {
            wasmparser::FUNC_REF => ValType::FuncRef,
            wasmparser::EXTERN_REF => ValType::FuncRef,
            _ => unimplemented!(),
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

    fn component_entity_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeKey<'a>, u32>,
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
            wasmparser::types::ComponentEntityType::Type(id) => {
                ComponentTypeRef::Type(TypeBounds::Eq, self.ty(encodable, types, id))
            }
            wasmparser::types::ComponentEntityType::Instance(id) => {
                ComponentTypeRef::Instance(self.component_instance_type(encodable, types, id))
            }
            wasmparser::types::ComponentEntityType::Component(id) => {
                ComponentTypeRef::Component(self.component_type(encodable, types, id))
            }
        }
    }

    fn module_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeKey<'a>, u32>,
        id: TypeId,
    ) -> u32 {
        match types.entry(TypeKey { types: self.0, id }) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let ty = self.0.type_from_id(id).unwrap().as_module_type().unwrap();

                let module = self.module(
                    ty.imports
                        .iter()
                        .map(|((m, n), t)| (m.as_str(), n.as_str(), *t)),
                    ty.exports.iter().map(|(n, t)| (n.as_str(), *t)),
                );

                let index = encodable.type_count();
                encodable.core_type().module(&module);
                *e.insert(index)
            }
        }
    }

    fn component_instance_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeKey<'a>, u32>,
        id: TypeId,
    ) -> u32 {
        match types.entry(TypeKey { types: self.0, id }) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let ty = self
                    .0
                    .type_from_id(id)
                    .unwrap()
                    .as_component_instance_type()
                    .unwrap();

                let instance =
                    self.instance(ty.exports(self.0).iter().map(|(n, t)| (n.as_str(), *t)));

                let index = encodable.type_count();
                encodable.ty().instance(&instance);
                *e.insert(index)
            }
        }
    }

    fn component_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeKey<'a>, u32>,
        id: TypeId,
    ) -> u32 {
        match types.entry(TypeKey { types: self.0, id }) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let ty = self
                    .0
                    .type_from_id(id)
                    .unwrap()
                    .as_component_type()
                    .unwrap();

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
        types: &mut HashMap<TypeKey<'a>, u32>,
        id: TypeId,
    ) -> u32 {
        if let Some(idx) = types.get(&TypeKey { types: self.0, id }) {
            return *idx;
        }

        let ty = self
            .0
            .type_from_id(id)
            .unwrap()
            .as_component_func_type()
            .unwrap();

        let params = ty
            .params
            .iter()
            .map(|(name, ty)| {
                (
                    name.as_deref(),
                    self.component_val_type(encodable, types, *ty),
                )
            })
            .collect::<Vec<_>>();

        let results = ty
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

        if params.len() == 1 && params[0].0.is_none() {
            f.param(params[0].1);
        } else {
            f.params(params.into_iter().map(|(name, ty)| (name.unwrap(), ty)));
        }

        if results.len() == 1 && results[0].0.is_none() {
            f.result(results[0].1);
        } else {
            f.results(results.into_iter().map(|(name, ty)| (name.unwrap(), ty)));
        }

        types.insert(TypeKey { types: self.0, id }, index);
        index
    }

    fn ty(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeKey<'a>, u32>,
        id: TypeId,
    ) -> u32 {
        let ty = self.0.type_from_id(id).unwrap();

        match ty {
            wasmparser::types::Type::Func(_) | wasmparser::types::Type::Instance(_) => {
                unreachable!()
            }
            wasmparser::types::Type::Module(_) => self.module_type(encodable, types, id),
            wasmparser::types::Type::Component(_) => self.component_type(encodable, types, id),
            wasmparser::types::Type::ComponentInstance(_) => {
                self.component_instance_type(encodable, types, id)
            }
            wasmparser::types::Type::ComponentFunc(_) => {
                self.component_func_type(encodable, types, id)
            }
            wasmparser::types::Type::Defined(_) => self.defined_type(encodable, types, id),
        }
    }

    fn component_val_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeKey<'a>, u32>,
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
        types: &mut HashMap<TypeKey<'a>, u32>,
        id: TypeId,
    ) -> u32 {
        if let Some(idx) = types.get(&TypeKey { types: self.0, id }) {
            return *idx;
        }

        let ty = self.0.type_from_id(id).unwrap().as_defined_type().unwrap();

        let index = match ty {
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
        };

        types.insert(TypeKey { types: self.0, id }, index);
        index
    }

    fn record(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeKey<'a>, u32>,
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
        types: &mut HashMap<TypeKey<'a>, u32>,
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
        types: &mut HashMap<TypeKey<'a>, u32>,
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
        types: &mut HashMap<TypeKey<'a>, u32>,
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

    fn flags(encodable: &mut impl Encodable, names: &IndexSet<String>) -> u32 {
        let index = encodable.type_count();
        encodable
            .ty()
            .defined_type()
            .flags(names.iter().map(|n| n.as_str()));
        index
    }

    fn enum_type(encodable: &mut impl Encodable, cases: &IndexSet<String>) -> u32 {
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
        types: &mut HashMap<TypeKey<'a>, u32>,
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
        types: &mut HashMap<TypeKey<'a>, u32>,
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
        types: &mut HashMap<TypeKey<'a>, u32>,
        ok: Option<wasmparser::types::ComponentValType>,
        err: Option<wasmparser::types::ComponentValType>,
    ) -> u32 {
        let ok = ok.map(|ty| self.component_val_type(encodable, types, ty));
        let err = err.map(|ty| self.component_val_type(encodable, types, ty));

        let index = encodable.type_count();
        encodable.ty().defined_type().result(ok, err);
        index
    }
}

/// Used to encode an instantiation graph.
pub(crate) struct InstantiationGraphEncoder<'a> {
    /// The associated composition configuration.
    config: &'a Config,
    /// The graph being encoded.
    graph: &'a InstantiationGraph,
    /// Map from graph component index to encoded component index.
    component_indexes: HashMap<ComponentIndex, u32>,
    /// Map from graph instance index to encoded instantiation index.
    instance_indexes: HashMap<InstanceIndex, u32>,
    /// The used import names during encoding.
    imports: HashSet<&'a str>,
    /// Map from an export on a graph instance to the aliased encoded instance index.
    aliases: HashMap<(InstanceIndex, ExportIndex), u32>,
    /// The number of modules encoded (i.e. current module index).
    modules: u32,
    /// The number of component functions encoded (i.e. current function index).
    funcs: u32,
    /// The number of values encoded (i.e. current value index).
    values: u32,
    /// The number of types encoded (i.e. current type index).
    types: u32,
    /// The number of component instances encoded (i.e. current instance index).
    instances: u32,
    /// The number of components encoded (i.e. current component index).
    components: u32,
}

impl<'a> InstantiationGraphEncoder<'a> {
    /// Create a new encoder for the given graph.
    pub(crate) fn new(config: &'a Config, graph: &'a InstantiationGraph) -> Self {
        Self {
            config,
            graph,
            component_indexes: Default::default(),
            instance_indexes: Default::default(),
            imports: Default::default(),
            aliases: Default::default(),
            modules: 0,
            funcs: 0,
            values: 0,
            types: 0,
            instances: 0,
            components: 0,
        }
    }

    /// Encodes the graph into a component.
    pub(crate) fn encode(mut self) -> Result<Vec<u8>> {
        let mut encoded = wasm_encoder::Component::new();

        // Encode the instances from the graph
        for instance in self.graph.instantiation_order()? {
            if let Some(component) = self.graph.component(instance) {
                self.encode_instantiation(instance, component, &mut encoded)?;
                continue;
            }

            if let Some(refs) = self.graph.import_refs(instance) {
                self.encode_instance_import(instance, refs, &mut encoded)?;
                continue;
            }

            unreachable!("every instance in the graph should either be instantiated or imported");
        }

        // Encode the exports from the root component
        self.encode_exports(&mut encoded)?;

        Ok(encoded.finish())
    }

    /// Encode an instance import in the given component.
    fn encode_instance_import(
        &mut self,
        instance: InstanceIndex,
        refs: &IndexSet<ImportRef>,
        encoded: &mut wasm_encoder::Component,
    ) -> Result<()> {
        // Build a map of export names to types; this will be used to encode
        // the type of the imported instance
        let mut exports: IndexMap<&String, (&crate::composer::Component, ComponentEntityType)> =
            IndexMap::new();

        let instance_name = self.graph.instance_name(instance);
        if !self.imports.insert(instance_name) {
            bail!("cannot import instance `{instance_name}` because it conflicts with an imported component of the same name");
        }

        for r in refs {
            let (component, _, ty) = self.graph.resolve_import(*r);
            let types = component.types();
            for (export_name, export_type) in ty.exports(types) {
                match exports.entry(export_name) {
                    indexmap::map::Entry::Occupied(mut e) => {
                        // Export already exists, ensure the types are compatible
                        let (existing_component, existing_entity_type) = e.get();

                        // If the existing type is still a subtype, do nothing
                        if ComponentEntityType::is_subtype_of(
                            existing_entity_type,
                            existing_component.types(),
                            export_type,
                            types,
                        ) {
                            continue;
                        }

                        // If the new type is the subtype, replace the existing type
                        if ComponentEntityType::is_subtype_of(
                            export_type,
                            types,
                            existing_entity_type,
                            existing_component.types(),
                        ) {
                            *e.get_mut() = (component, *export_type);
                            continue;
                        }

                        // Otherwise, it's a conflict error
                        bail!(
                            "cannot import instance `{instance_name}` due to conflicting types for export `{export_name}` between components `{a}` and `{b}`",
                            a = existing_component.path().display(),
                            b = component.path().display()
                        );
                    }
                    indexmap::map::Entry::Vacant(e) => {
                        e.insert((component, *export_type));
                    }
                }
            }
        }

        let mut instance_type = InstanceType::new();
        let mut types = HashMap::new();
        for (name, (component, ty)) in exports {
            let encoder = TypeEncoder::new(component.types());
            let ty = encoder.component_entity_type(&mut instance_type, &mut types, ty);
            instance_type.export(name, ty);
        }

        let mut types = ComponentTypeSection::new();
        let type_index = self.types;
        types.instance(&instance_type);
        self.types += 1;
        encoded.section(&types);

        let mut imports = ComponentImportSection::new();
        let instance_index = self.instances;
        imports.import(instance_name, ComponentTypeRef::Instance(type_index));
        self.instances += 1;
        encoded.section(&imports);

        log::debug!("importing instance `{instance_name}` (encoded index {instance_index})",);

        self.instance_indexes.insert(instance, instance_index);

        Ok(())
    }

    /// Encode a component instantiation in the given component.
    fn encode_instantiation(
        &mut self,
        instance: InstanceIndex,
        component: &'a Component,
        encoded: &mut wasm_encoder::Component,
    ) -> Result<()> {
        let instance_name = self.graph.instance_name(instance);
        let dependency = self.config.dependency_name(instance_name);

        // Encode the instance's component if it hasn't been encoded already
        let component_index = match self.component_indexes.entry(component.index()) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let index = match component.import_name() {
                    Some(name) => {
                        if !self.imports.insert(name) {
                            bail!(
                                "cannot import dependency `{dependency}` (`{path}`) with name `{name}` because it conflicts with an imported instance or component of the same name",
                                path = component.path().display(),
                            );
                        }

                        log::debug!(
                            "importing component `{dependency}` with name `{name}` (encoded index {component_index}",
                            component_index = self.components,
                        );

                        let mut types = ComponentTypeSection::new();
                        let type_index = self.types;
                        types.component(&component.ty());
                        self.types += 1;
                        encoded.section(&types);

                        let mut imports = ComponentImportSection::new();
                        let component_index = self.components;
                        imports.import(name, ComponentTypeRef::Component(type_index));
                        self.components += 1;
                        encoded.section(&imports);

                        component_index
                    }
                    None => {
                        let component_index = self.components;
                        log::debug!("defining component `{dependency}` in composed component (encoded index {component_index})");

                        encoded.section(&RawSection {
                            id: ComponentSectionId::Component.into(),
                            data: component.bytes(),
                        });

                        self.components += 1;
                        component_index
                    }
                };

                *e.insert(index)
            }
        };

        let args = self.graph.instantiation_args(instance, |instance, export| {
            let instance_index = self.instance_indexes[&instance];

            // Check if we're aliasing an export from the instance
            match export {
                Some(export) => match self.aliases.entry((instance, export)) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let name = self.graph.component(instance).unwrap().export(export).0;

                        let mut aliases = ComponentAliasSection::new();
                        let alias_index = self.instances;
                        aliases.instance_export(
                            instance_index,
                            ComponentExportKind::Instance,
                            name,
                        );
                        self.instances += 1;
                        encoded.section(&aliases);

                        log::debug!(
                            "aliasing instance export `{name}` from instance `{instance_name}` (encoded index {alias_index})",
                            instance_name = self.graph.instance_name(instance),
                        );

                        *e.insert(alias_index)
                    }
                },
                None => instance_index,
            }
        });

        log::debug!(
            "instantiating component `{dependency}` as instance `{instance_name}` (encoded index {instance_index}) with {args:?}",
            instance_index = self.instances,
        );

        let mut instances = ComponentInstanceSection::new();
        let instance_index = self.instances;
        instances.instantiate(component_index, args);
        self.instances += 1;
        encoded.section(&instances);

        self.instance_indexes.insert(instance, instance_index);

        Ok(())
    }

    /// Encode the exports of the composed component.
    ///
    /// This always exports everything from the root (index 0) component.
    fn encode_exports(&mut self, encoded: &mut wasm_encoder::Component) -> Result<()> {
        let mut exports = ComponentExportSection::new();

        // The root instance is always the first node in the graph
        let instance = InstanceIndex::new(0);
        let component = self.graph.component(instance).unwrap();
        let instance_index = self.instance_indexes[&instance];

        // Alias all exports from the root instance
        let mut aliases = ComponentAliasSection::new();
        for (name, kind, _) in component.exports() {
            self.encode_alias_and_export(instance_index, name, kind, &mut aliases, &mut exports);
        }

        if !aliases.is_empty() {
            encoded.section(&aliases);
        }

        if !exports.is_empty() {
            encoded.section(&exports);
        }

        Ok(())
    }

    /// Encode an alias for an instance export and then export the aliased item.
    fn encode_alias_and_export(
        &mut self,
        instance_index: u32,
        name: &str,
        kind: wasmparser::ComponentExternalKind,
        aliases: &mut ComponentAliasSection,
        exports: &mut ComponentExportSection,
    ) {
        let (indexes, kind) = match kind {
            wasmparser::ComponentExternalKind::Module => {
                (&mut self.modules, ComponentExportKind::Module)
            }
            wasmparser::ComponentExternalKind::Func => (&mut self.funcs, ComponentExportKind::Func),
            wasmparser::ComponentExternalKind::Value => {
                (&mut self.values, ComponentExportKind::Value)
            }
            wasmparser::ComponentExternalKind::Type => (&mut self.types, ComponentExportKind::Type),
            wasmparser::ComponentExternalKind::Instance => {
                (&mut self.instances, ComponentExportKind::Instance)
            }
            wasmparser::ComponentExternalKind::Component => {
                (&mut self.components, ComponentExportKind::Component)
            }
        };

        let index = *indexes;
        aliases.instance_export(instance_index, kind, name);
        *indexes += 1;
        exports.export(name, kind, index);
    }
}
