use indexmap::IndexSet;
use std::collections::{hash_map::Entry, HashMap};
use wasm_encoder::{
    ComponentType, ComponentTypeRef, ComponentValType, EntityType, GlobalType, InstanceType,
    MemoryType, ModuleType, PrimitiveValType, TableType, TagKind, TagType, TypeBounds, ValType,
};
use wasmparser::types::{TypeId, Types};

// Utility trait implement on component and instance types
// to abstract their encoding.
trait Encodable {
    fn type_count(&self) -> u32;
    fn ty(&mut self) -> wasm_encoder::ComponentTypeEncoder;
    fn core_type(&mut self) -> wasm_encoder::CoreTypeEncoder;
}

impl Encodable for ComponentType {
    fn type_count(&self) -> u32 {
        Self::type_count(self)
    }

    fn ty(&mut self) -> wasm_encoder::ComponentTypeEncoder {
        Self::ty(self)
    }

    fn core_type(&mut self) -> wasm_encoder::CoreTypeEncoder {
        self.core_type()
    }
}

impl Encodable for InstanceType {
    fn type_count(&self) -> u32 {
        self.type_count()
    }

    fn ty(&mut self) -> wasm_encoder::ComponentTypeEncoder {
        self.ty()
    }

    fn core_type(&mut self) -> wasm_encoder::CoreTypeEncoder {
        self.core_type()
    }
}

pub struct TypeEncoder<'a>(&'a Types);

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
        let mut types: HashMap<TypeId, u32> = HashMap::new();

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
        let mut types: HashMap<TypeId, u32> = HashMap::new();

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
        let mut types: HashMap<TypeId, u32> = HashMap::new();

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
                let idx = match types.entry(id) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let ty = self.0.type_from_id(id).unwrap().as_func_type().unwrap();
                        let index = encodable.type_count();
                        encodable.ty().function(
                            ty.params.iter().copied().map(Self::val_type),
                            ty.returns.iter().copied().map(Self::val_type),
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
                let idx = match types.entry(id) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let ty = self.0.type_from_id(id).unwrap().as_func_type().unwrap();
                        let index = encodable.type_count();
                        encodable.ty().function(
                            ty.params.iter().copied().map(Self::val_type),
                            ty.returns.iter().copied().map(Self::val_type),
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
            wasmparser::ValType::FuncRef => ValType::FuncRef,
            wasmparser::ValType::ExternRef => ValType::ExternRef,
        }
    }

    fn table_type(ty: wasmparser::TableType) -> TableType {
        TableType {
            element_type: Self::val_type(ty.element_type),
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
        types: &mut HashMap<TypeId, u32>,
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
        types: &mut HashMap<TypeId, u32>,
        id: TypeId,
    ) -> u32 {
        match types.entry(id) {
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
        types: &mut HashMap<TypeId, u32>,
        id: TypeId,
    ) -> u32 {
        match types.entry(id) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let ty = self
                    .0
                    .type_from_id(id)
                    .unwrap()
                    .as_component_instance_type()
                    .unwrap();

                let instance = self.instance(
                    ty.exports(self.0.as_ref())
                        .iter()
                        .map(|(n, t)| (n.as_str(), *t)),
                );

                let index = encodable.type_count();
                encodable.ty().instance(&instance);
                *e.insert(index)
            }
        }
    }

    fn component_type(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeId, u32>,
        id: TypeId,
    ) -> u32 {
        match types.entry(id) {
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
        types: &mut HashMap<TypeId, u32>,
        id: TypeId,
    ) -> u32 {
        if let Some(idx) = types.get(&id) {
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

        types.insert(id, index);
        index
    }

    fn ty(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeId, u32>,
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
        types: &mut HashMap<TypeId, u32>,
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
        types: &mut HashMap<TypeId, u32>,
        id: TypeId,
    ) -> u32 {
        if let Some(idx) = types.get(&id) {
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

        types.insert(id, index);
        index
    }

    fn record(
        &self,
        encodable: &mut impl Encodable,
        types: &mut HashMap<TypeId, u32>,
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
        types: &mut HashMap<TypeId, u32>,
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
        types: &mut HashMap<TypeId, u32>,
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
        types: &mut HashMap<TypeId, u32>,
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
        types: &mut HashMap<TypeId, u32>,
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
        types: &mut HashMap<TypeId, u32>,
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
        types: &mut HashMap<TypeId, u32>,
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
