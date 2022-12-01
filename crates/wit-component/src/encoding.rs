//! Support for encoding a core wasm module into a component.
//!
//! This module, at a high level, is tasked with transforming a core wasm
//! module into a component. This will process the imports/exports of the core
//! wasm module and translate between the `wit-parser` AST and the component
//! model binary format, producing a final component which sill import
//! `*.wit` defined interfaces and export `*.wit` defined interfaces as well
//! with everything wired up internally according to the canonical ABI and such.
//!
//! This doc block here is not currently 100% complete and doesn't cover the
//! full functionality of this module.
//!
//! # Adapter Modules
//!
//! One feature of this encoding process which is non-obvious is the support for
//! "adapter modules". The general idea here is that historical host API
//! definitions have been around for quite some time, such as
//! `wasi_snapshot_preview1`, but these host API definitions are not compatible
//! with the canonical ABI or component model exactly. These APIs, however, can
//! in most situations be roughly adapted to component-model equivalents. This
//! is where adapter modules come into play, they're converting from some
//! arbitrary API/ABI into a component-model using API.
//!
//! An adapter module is a separately compiled `*.wasm` blob which will export
//! functions matching the desired ABI (e.g. exporting functions matching the
//! `wasi_snapshot_preview1` ABI). The `*.wasm` blob will then import functions
//! in the canonical ABI and internally adapt the exported functions to the
//! imported functions. The encoding support in this module is what wires
//! everything up and makes sure that everything is imported and exported to the
//! right place. Adapter modules currently always use "indirect lowerings"
//! meaning that a shim module is created and provided as the imports to the
//! main core wasm module, and the shim module is "filled in" at a later time
//! during the instantiation process.
//!
//! Adapter modules are not intended to be general purpose and are currently
//! very restrictive, namely:
//!
//! * They must import a linear memory and not define their own linear memory
//!   otherwise. In other words they import memory and cannot use multi-memory.
//! * They cannot define any `elem` or `data` segments since otherwise there's
//!   no knowledge ahead-of-time of where their data or element segments could
//!   go. This means things like no panics, no indirect calls, etc.
//! * Only one mutable global is allowed and it's assumed to be the stack
//!   pointer. This stack pointer is automatically configured with an injected
//!   `start` function that is allocated with `memory.grow (i32.const 1)`,
//!   meaning that the shim module has 64k of stack space and no protection if
//!   that overflows.
//!
//! This means that adapter modules are not meant to be written by everyone.
//! It's assumed that these will be relatively few and far between yet still a
//! crucial part of the transition process from to the component model since
//! otherwise there's no way to run a `wasi_snapshot_preview1` module within the
//! component model.

use crate::metadata::{self, BindgenMetadata};
use crate::{
    validation::{
        validate_adapter_module, validate_module, ValidatedAdapter, ValidatedModule,
        MAIN_MODULE_IMPORT_NAME,
    },
    StringEncoding,
};
use anyhow::{anyhow, bail, Context, Result};
use indexmap::{map::Entry, IndexMap, IndexSet};
use std::hash::{Hash, Hasher};
use std::mem;
use wasm_encoder::*;
use wasmparser::{FuncType, Validator, WasmFeatures};
use wit_parser::{
    abi::{AbiVariant, WasmSignature, WasmType},
    Enum, Flags, Function, FunctionKind, Interface, Params, Record, Result_, Results, Tuple, Type,
    TypeDef, TypeDefKind, Union, Variant, World,
};

const INDIRECT_TABLE_NAME: &str = "$imports";

fn to_val_type(ty: &WasmType) -> ValType {
    match ty {
        WasmType::I32 => ValType::I32,
        WasmType::I64 => ValType::I64,
        WasmType::F32 => ValType::F32,
        WasmType::F64 => ValType::F64,
    }
}

struct TypeKey<'a> {
    interface: &'a Interface,
    ty: Type,
}

impl Hash for TypeKey<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.ty {
            Type::Id(id) => TypeDefKey::new(self.interface, &self.interface.types[id]).hash(state),
            _ => self.ty.hash(state),
        }
    }
}

impl PartialEq for TypeKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self.ty, other.ty) {
            (Type::Id(id), Type::Id(other_id)) => {
                TypeDefKey::new(self.interface, &self.interface.types[id])
                    == TypeDefKey::new(other.interface, &other.interface.types[other_id])
            }
            _ => self.ty.eq(&other.ty),
        }
    }
}

impl Eq for TypeKey<'_> {}

/// Represents a key type for interface type definitions.
pub struct TypeDefKey<'a> {
    interface: &'a Interface,
    def: &'a TypeDef,
}

impl<'a> TypeDefKey<'a> {
    fn new(interface: &'a Interface, def: &'a TypeDef) -> Self {
        Self { interface, def }
    }
}

impl PartialEq for TypeDefKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        let def = self.def;
        let other_def = other.def;
        def.name == other_def.name
            && match (&def.kind, &other_def.kind) {
                (TypeDefKind::Record(r1), TypeDefKind::Record(r2)) => {
                    if r1.fields.len() != r2.fields.len() {
                        return false;
                    }

                    r1.fields.iter().zip(r2.fields.iter()).all(|(f1, f2)| {
                        f1.name == f2.name
                            && TypeKey {
                                interface: self.interface,
                                ty: f1.ty,
                            }
                            .eq(&TypeKey {
                                interface: other.interface,
                                ty: f2.ty,
                            })
                    })
                }
                (TypeDefKind::Tuple(t1), TypeDefKind::Tuple(t2)) => {
                    if t1.types.len() != t2.types.len() {
                        return false;
                    }

                    t1.types.iter().zip(t2.types.iter()).all(|(t1, t2)| {
                        TypeKey {
                            interface: self.interface,
                            ty: *t1,
                        }
                        .eq(&TypeKey {
                            interface: other.interface,
                            ty: *t2,
                        })
                    })
                }
                (TypeDefKind::Flags(f1), TypeDefKind::Flags(f2)) => {
                    if f1.flags.len() != f2.flags.len() {
                        return false;
                    }

                    f1.flags
                        .iter()
                        .zip(f2.flags.iter())
                        .all(|(f1, f2)| f1.name == f2.name)
                }
                (TypeDefKind::Variant(v1), TypeDefKind::Variant(v2)) => {
                    if v1.cases.len() != v2.cases.len() {
                        return false;
                    }

                    v1.cases.iter().zip(v2.cases.iter()).all(|(c1, c2)| {
                        c1.name == c2.name
                            && match (c1.ty, c2.ty) {
                                (Some(ty1), Some(ty2)) => {
                                    TypeKey {
                                        interface: self.interface,
                                        ty: ty1,
                                    } == TypeKey {
                                        interface: other.interface,
                                        ty: ty2,
                                    }
                                }
                                (None, None) => true,
                                _ => false,
                            }
                    })
                }
                (TypeDefKind::Union(v1), TypeDefKind::Union(v2)) => {
                    if v1.cases.len() != v2.cases.len() {
                        return false;
                    }

                    v1.cases.iter().zip(v2.cases.iter()).all(|(c1, c2)| {
                        TypeKey {
                            interface: self.interface,
                            ty: c1.ty,
                        } == TypeKey {
                            interface: other.interface,
                            ty: c2.ty,
                        }
                    })
                }
                (TypeDefKind::Enum(e1), TypeDefKind::Enum(e2)) => {
                    if e1.cases.len() != e2.cases.len() {
                        return false;
                    }

                    e1.cases
                        .iter()
                        .zip(e2.cases.iter())
                        .all(|(c1, c2)| c1.name == c2.name)
                }
                (TypeDefKind::List(t1), TypeDefKind::List(t2))
                | (TypeDefKind::Type(t1), TypeDefKind::Type(t2))
                | (TypeDefKind::Option(t1), TypeDefKind::Option(t2)) => TypeKey {
                    interface: self.interface,
                    ty: *t1,
                }
                .eq(&TypeKey {
                    interface: other.interface,
                    ty: *t2,
                }),
                (TypeDefKind::Result(r1), TypeDefKind::Result(r2)) => {
                    let ok_eq = match (r1.ok, r2.ok) {
                        (Some(ok1), Some(ok2)) => {
                            TypeKey {
                                interface: self.interface,
                                ty: ok1,
                            } == TypeKey {
                                interface: other.interface,
                                ty: ok2,
                            }
                        }
                        (None, None) => true,
                        _ => false,
                    };
                    let err_eq = match (r1.err, r2.err) {
                        (Some(err1), Some(err2)) => {
                            TypeKey {
                                interface: self.interface,
                                ty: err1,
                            } == TypeKey {
                                interface: other.interface,
                                ty: err2,
                            }
                        }
                        (None, None) => true,
                        _ => false,
                    };
                    ok_eq && err_eq
                }
                _ => false,
            }
    }
}

impl Eq for TypeDefKey<'_> {}

impl Hash for TypeDefKey<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let def = self.def;
        def.name.hash(state);
        match &def.kind {
            TypeDefKind::Record(r) => {
                state.write_u8(0);
                r.fields.len().hash(state);
                for f in &r.fields {
                    f.name.hash(state);
                    TypeKey {
                        interface: self.interface,
                        ty: f.ty,
                    }
                    .hash(state);
                }
            }
            TypeDefKind::Tuple(t) => {
                state.write_u8(1);
                t.types.len().hash(state);
                for ty in &t.types {
                    TypeKey {
                        interface: self.interface,
                        ty: *ty,
                    }
                    .hash(state);
                }
            }
            TypeDefKind::Flags(r) => {
                state.write_u8(2);
                r.flags.len().hash(state);
                for f in &r.flags {
                    f.name.hash(state);
                }
            }
            TypeDefKind::Variant(v) => {
                state.write_u8(3);
                v.cases.len().hash(state);
                for c in &v.cases {
                    c.name.hash(state);
                    c.ty.map(|ty| TypeKey {
                        interface: self.interface,
                        ty,
                    })
                    .hash(state);
                }
            }
            TypeDefKind::Enum(e) => {
                state.write_u8(4);
                e.cases.len().hash(state);
                for c in &e.cases {
                    c.name.hash(state);
                }
            }
            TypeDefKind::List(ty) => {
                state.write_u8(5);
                TypeKey {
                    interface: self.interface,
                    ty: *ty,
                }
                .hash(state);
            }
            TypeDefKind::Type(ty) => {
                state.write_u8(6);
                TypeKey {
                    interface: self.interface,
                    ty: *ty,
                }
                .hash(state);
            }
            TypeDefKind::Option(ty) => {
                state.write_u8(7);
                TypeKey {
                    interface: self.interface,
                    ty: *ty,
                }
                .hash(state);
            }
            TypeDefKind::Result(r) => {
                state.write_u8(8);
                r.ok.map(|ty| TypeKey {
                    interface: self.interface,
                    ty,
                })
                .hash(state);
                r.err
                    .map(|ty| TypeKey {
                        interface: self.interface,
                        ty,
                    })
                    .hash(state);
            }
            TypeDefKind::Union(u) => {
                state.write_u8(9);
                u.cases.len().hash(state);
                for case in u.cases.iter() {
                    TypeKey {
                        interface: self.interface,
                        ty: case.ty,
                    }
                    .hash(state);
                }
            }
            TypeDefKind::Future(_) => todo!("hash for future"),
            TypeDefKind::Stream(_) => todo!("hash for stream"),
        }
    }
}

/// Represents a key type for interface function definitions.
pub struct FunctionKey<'a> {
    interface: &'a Interface,
    func: &'a Function,
}

impl PartialEq for FunctionKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.func.params.len() != other.func.params.len() {
            return false;
        }

        let key_equal = |t1, t2| {
            TypeKey {
                interface: self.interface,
                ty: t1,
            }
            .eq(&TypeKey {
                interface: other.interface,
                ty: t2,
            })
        };

        let params_equal = |ps: &Params, ops: &Params| {
            ps.len() == ops.len()
                && ps
                    .iter()
                    .zip(ops.iter())
                    .all(|((n1, t1), (n2, t2))| n1 == n2 && key_equal(*t1, *t2))
        };

        let results_equal = |rs: &Results, ors: &Results| match (rs, ors) {
            (Results::Named(rs), Results::Named(ors)) => params_equal(rs, ors),
            (Results::Anon(ty), Results::Anon(oty)) => key_equal(*ty, *oty),
            _ => false,
        };

        params_equal(&self.func.params, &other.func.params)
            && results_equal(&self.func.results, &other.func.results)
    }
}

impl Eq for FunctionKey<'_> {}

impl Hash for FunctionKey<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.func.params.len().hash(state);
        for (name, ty) in self.func.params.iter() {
            name.hash(state);
            TypeKey {
                interface: self.interface,
                ty: *ty,
            }
            .hash(state);
        }
        match &self.func.results {
            Results::Named(rs) => {
                state.write_u8(0);
                rs.len().hash(state);
                for (name, ty) in rs.iter() {
                    name.hash(state);
                    TypeKey {
                        interface: self.interface,
                        ty: *ty,
                    }
                    .hash(state);
                }
            }
            Results::Anon(ty) => {
                state.write_u8(1);
                TypeKey {
                    interface: self.interface,
                    ty: *ty,
                }
                .hash(state);
            }
        }
    }
}

#[derive(Default)]
struct InstanceTypeEncoder<'a> {
    ty: InstanceType,
    aliased_types: IndexMap<ComponentTypeRef, ComponentTypeRef>,
    exported_types: IndexMap<&'a str, (&'a str, ComponentTypeRef)>,
}

impl<'a> InstanceTypeEncoder<'a> {
    fn export(&mut self, name: &'a str, url: &'a str, type_ref: ComponentTypeRef) -> Result<()> {
        match self.exported_types.entry(name) {
            Entry::Occupied(e) => {
                if e.get().1 != type_ref {
                    bail!("duplicate export `{}`", name)
                }
            }
            Entry::Vacant(entry) => {
                entry.insert((url, type_ref));
                let alias = self.alias_type(type_ref);
                self.ty.export(name, url, alias);
            }
        }

        Ok(())
    }

    fn alias_type(&mut self, type_ref: ComponentTypeRef) -> ComponentTypeRef {
        match self.aliased_types.entry(type_ref) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let index = self.ty.type_count();
                let (alias, outer_index) = match type_ref {
                    ComponentTypeRef::Module(outer) => (ComponentTypeRef::Module(index), outer),
                    ComponentTypeRef::Func(outer) => (ComponentTypeRef::Func(index), outer),
                    ComponentTypeRef::Value(ComponentValType::Primitive(_)) => unreachable!(),
                    ComponentTypeRef::Value(ComponentValType::Type(outer)) => (
                        ComponentTypeRef::Value(ComponentValType::Type(index)),
                        outer,
                    ),
                    ComponentTypeRef::Type(bounds, outer) => {
                        (ComponentTypeRef::Type(bounds, index), outer)
                    }
                    ComponentTypeRef::Instance(outer) => (ComponentTypeRef::Instance(index), outer),
                    ComponentTypeRef::Component(outer) => {
                        (ComponentTypeRef::Component(index), outer)
                    }
                };

                self.ty.alias_outer_type(1, outer_index);
                e.insert(alias);
                alias
            }
        }
    }
}

#[derive(Default)]
struct TypeEncoder<'a> {
    types: ComponentTypeSection,
    type_map: IndexMap<TypeDefKey<'a>, u32>,
    func_type_map: IndexMap<FunctionKey<'a>, u32>,
    exports: ComponentExportSection,
}

impl<'a> TypeEncoder<'a> {
    fn finish(&self, component: &mut ComponentEncoding) {
        if !self.types.is_empty() {
            component.flush();
            component.component.section(&self.types);
        }

        if !self.exports.is_empty() {
            component.flush();
            component.component.section(&self.exports);
        }
    }

    fn encode_instance_imports(
        &mut self,
        interfaces: &'a IndexMap<String, Interface>,
        info: Option<&ValidatedModule<'a>>,
        imports: &mut ImportEncoder<'a>,
    ) -> Result<()> {
        for (name, import) in interfaces {
            let required_funcs = match info {
                Some(info) => match info.required_imports.get(name.as_str()) {
                    Some(required) => Some(required),
                    None => continue,
                },
                None => None,
            };
            self.encode_instance_import(name, import, required_funcs, imports)?;
        }

        Ok(())
    }

    /// Encodes an import of the `import` interface specified which will be
    /// slimmed down to the `required_funcs` set, if specified.
    ///
    /// The imported instance, if any, is placed within `imports`.
    fn encode_instance_import(
        &mut self,
        name: &'a str,
        import: &'a Interface,
        required_funcs: Option<&IndexSet<&'a str>>,
        imports: &mut ImportEncoder<'a>,
    ) -> Result<()> {
        if let Some(index) = self.encode_interface_as_instance_type(import, required_funcs)? {
            imports.import(
                name,
                import,
                ComponentTypeRef::Instance(index),
                required_funcs,
            )?;
        }

        Ok(())
    }

    /// Generates an instance type index representing `import` slimmed down to
    /// `required_funcs`, if specified.
    fn encode_interface_as_instance_type(
        &mut self,
        import: &'a Interface,
        required_funcs: Option<&IndexSet<&'a str>>,
    ) -> Result<Option<u32>> {
        let exports =
            match self.encode_interface_as_instance_type_exports(import, required_funcs)? {
                Some(exports) => exports,
                None => return Ok(None),
            };
        let mut instance = InstanceTypeEncoder::default();
        for (name, ty) in exports {
            instance.export(name, "", ty)?;
        }

        Ok(Some(self.encode_instance_type(&instance.ty)))
    }

    /// Generates a list of items that make up `import`.
    fn encode_interface_as_instance_type_exports(
        &mut self,
        import: &'a Interface,
        required_funcs: Option<&IndexSet<&'a str>>,
    ) -> Result<Option<Vec<(&'a str, ComponentTypeRef)>>> {
        // Don't import empty instances if no functions are actually required
        // from this interface.
        match required_funcs {
            Some(funcs) if funcs.is_empty() => return Ok(None),
            _ => {}
        }

        let mut exports = self.encode_interface_named_types(import)?;

        for func in &import.functions {
            if let Some(required_funcs) = required_funcs {
                if !required_funcs.contains(func.name.as_str()) {
                    continue;
                }
            }
            Self::validate_function(func)?;

            let index = self.encode_func_type(import, func)?;
            exports.push((func.name.as_str(), ComponentTypeRef::Func(index)));
        }

        Ok(Some(exports))
    }

    fn encode_interface_named_types(
        &mut self,
        interface: &'a Interface,
    ) -> Result<Vec<(&'a str, ComponentTypeRef)>> {
        let mut exports = Vec::new();

        for (id, def) in &interface.types {
            let name = match &def.name {
                Some(name) => name,
                None => continue,
            };
            let idx = match self.encode_valtype(interface, &Type::Id(id))? {
                ComponentValType::Type(idx) => idx,
                // With a name this type should be converted to an indexed type
                // automatically and this shouldn't be possible.
                ComponentValType::Primitive(_) => unreachable!(),
            };
            exports.push((name.as_str(), ComponentTypeRef::Type(TypeBounds::Eq, idx)));
        }

        Ok(exports)
    }

    fn encode_func_types(&mut self, interfaces: impl Iterator<Item = &'a Interface>) -> Result<()> {
        for export in interfaces {
            // TODO: stick interface documentation in a custom section?

            for func in &export.functions {
                Self::validate_function(func)?;

                self.encode_func_type(export, func)?;
            }
        }

        Ok(())
    }

    fn encode_instance_type(&mut self, ty: &InstanceType) -> u32 {
        let index = self.types.len();
        self.types.instance(ty);
        index
    }

    fn encode_params(
        &mut self,
        interface: &'a Interface,
        params: &'a Params,
    ) -> Result<Vec<(&'a str, ComponentValType)>> {
        params
            .iter()
            .map(|(name, ty)| Ok((name.as_str(), self.encode_valtype(interface, ty)?)))
            .collect::<Result<_>>()
    }

    fn encode_func_type(&mut self, interface: &'a Interface, func: &'a Function) -> Result<u32> {
        let key = FunctionKey { interface, func };
        if let Some(index) = self.func_type_map.get(&key) {
            return Ok(*index);
        }

        // Encode all referenced parameter types from this function.
        let params: Vec<_> = self.encode_params(interface, &func.params)?;

        enum EncodedResults<'a> {
            Named(Vec<(&'a str, ComponentValType)>),
            Anon(ComponentValType),
        }

        let results = match &func.results {
            Results::Named(rs) => EncodedResults::Named(self.encode_params(interface, rs)?),
            Results::Anon(ty) => EncodedResults::Anon(self.encode_valtype(interface, ty)?),
        };

        // Encode the function type
        let index = self.types.len();
        let mut f = self.types.function();
        f.params(params);
        match results {
            EncodedResults::Named(rs) => f.results(rs),
            EncodedResults::Anon(ty) => f.result(ty),
        };
        self.func_type_map.insert(key, index);
        Ok(index)
    }

    fn encode_valtype(&mut self, interface: &'a Interface, ty: &Type) -> Result<ComponentValType> {
        Ok(match ty {
            Type::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
            Type::U8 => ComponentValType::Primitive(PrimitiveValType::U8),
            Type::U16 => ComponentValType::Primitive(PrimitiveValType::U16),
            Type::U32 => ComponentValType::Primitive(PrimitiveValType::U32),
            Type::U64 => ComponentValType::Primitive(PrimitiveValType::U64),
            Type::S8 => ComponentValType::Primitive(PrimitiveValType::S8),
            Type::S16 => ComponentValType::Primitive(PrimitiveValType::S16),
            Type::S32 => ComponentValType::Primitive(PrimitiveValType::S32),
            Type::S64 => ComponentValType::Primitive(PrimitiveValType::S64),
            Type::Float32 => ComponentValType::Primitive(PrimitiveValType::Float32),
            Type::Float64 => ComponentValType::Primitive(PrimitiveValType::Float64),
            Type::Char => ComponentValType::Primitive(PrimitiveValType::Char),
            Type::String => ComponentValType::Primitive(PrimitiveValType::String),
            Type::Id(id) => {
                let ty = &interface.types[*id];
                let key = TypeDefKey::new(interface, &interface.types[*id]);
                let encoded = if let Some(index) = self.type_map.get(&key) {
                    ComponentValType::Type(*index)
                } else {
                    let mut encoded = match &ty.kind {
                        TypeDefKind::Record(r) => self.encode_record(interface, r)?,
                        TypeDefKind::Tuple(t) => self.encode_tuple(interface, t)?,
                        TypeDefKind::Flags(r) => self.encode_flags(r)?,
                        TypeDefKind::Variant(v) => self.encode_variant(interface, v)?,
                        TypeDefKind::Union(u) => self.encode_union(interface, u)?,
                        TypeDefKind::Option(t) => self.encode_option(interface, t)?,
                        TypeDefKind::Result(r) => self.encode_result(interface, r)?,
                        TypeDefKind::Enum(e) => self.encode_enum(e)?,
                        TypeDefKind::List(ty) => {
                            let ty = self.encode_valtype(interface, ty)?;
                            let index = self.types.len();
                            let encoder = self.types.defined_type();
                            encoder.list(ty);
                            ComponentValType::Type(index)
                        }
                        TypeDefKind::Type(ty) => self.encode_valtype(interface, ty)?,
                        TypeDefKind::Future(_) => todo!("encoding for future type"),
                        TypeDefKind::Stream(_) => todo!("encoding for stream type"),
                    };

                    if ty.name.is_some() {
                        if let ComponentValType::Primitive(ty) = encoded {
                            // Named primitive types need entries in the type
                            // section, so convert this to a type reference
                            let index = self.types.len();
                            self.types.defined_type().primitive(ty);
                            encoded = ComponentValType::Type(index);
                        }
                    }

                    if let ComponentValType::Type(index) = encoded {
                        self.type_map.insert(key, index);
                    }

                    encoded
                };

                encoded
            }
        })
    }

    fn encode_optional_valtype(
        &mut self,
        interface: &'a Interface,
        ty: Option<&Type>,
    ) -> Result<Option<ComponentValType>> {
        match ty {
            Some(ty) => self.encode_valtype(interface, ty).map(Some),
            None => Ok(None),
        }
    }

    fn encode_record(
        &mut self,
        interface: &'a Interface,
        record: &Record,
    ) -> Result<ComponentValType> {
        let fields = record
            .fields
            .iter()
            .map(|f| Ok((f.name.as_str(), self.encode_valtype(interface, &f.ty)?)))
            .collect::<Result<Vec<_>>>()?;

        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.record(fields);
        Ok(ComponentValType::Type(index))
    }

    fn encode_tuple(
        &mut self,
        interface: &'a Interface,
        tuple: &Tuple,
    ) -> Result<ComponentValType> {
        let tys = tuple
            .types
            .iter()
            .map(|ty| self.encode_valtype(interface, ty))
            .collect::<Result<Vec<_>>>()?;
        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.tuple(tys);
        Ok(ComponentValType::Type(index))
    }

    fn encode_flags(&mut self, flags: &Flags) -> Result<ComponentValType> {
        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.flags(flags.flags.iter().map(|f| f.name.as_str()));
        Ok(ComponentValType::Type(index))
    }

    fn encode_variant(
        &mut self,
        interface: &'a Interface,
        variant: &Variant,
    ) -> Result<ComponentValType> {
        let cases = variant
            .cases
            .iter()
            .map(|c| {
                Ok((
                    c.name.as_str(),
                    self.encode_optional_valtype(interface, c.ty.as_ref())?,
                    None, // TODO: support defaulting case values in the future
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.variant(cases);
        Ok(ComponentValType::Type(index))
    }

    fn encode_union(
        &mut self,
        interface: &'a Interface,
        union: &Union,
    ) -> Result<ComponentValType> {
        let tys = union
            .cases
            .iter()
            .map(|c| self.encode_valtype(interface, &c.ty))
            .collect::<Result<Vec<_>>>()?;

        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.union(tys);
        Ok(ComponentValType::Type(index))
    }

    fn encode_option(
        &mut self,
        interface: &'a Interface,
        payload: &Type,
    ) -> Result<ComponentValType> {
        let ty = self.encode_valtype(interface, payload)?;
        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.option(ty);
        Ok(ComponentValType::Type(index))
    }

    fn encode_result(
        &mut self,
        interface: &'a Interface,
        result: &Result_,
    ) -> Result<ComponentValType> {
        let ok = self.encode_optional_valtype(interface, result.ok.as_ref())?;
        let error = self.encode_optional_valtype(interface, result.err.as_ref())?;
        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.result(ok, error);
        Ok(ComponentValType::Type(index))
    }

    fn encode_enum(&mut self, enum_: &Enum) -> Result<ComponentValType> {
        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.enum_type(enum_.cases.iter().map(|c| c.name.as_str()));
        Ok(ComponentValType::Type(index))
    }

    fn validate_function(function: &Function) -> Result<()> {
        if function.name.is_empty() {
            bail!("interface has an unnamed function");
        }

        if !matches!(function.kind, FunctionKind::Freestanding) {
            bail!(
                "unsupported function `{}`: only free-standing functions are currently supported",
                function.name
            );
        }

        Ok(())
    }
}

bitflags::bitflags! {
    /// Options in the `canon lower` or `canon lift` required for a particular
    /// function.
    struct RequiredOptions: u8 {
        /// A memory must be specified, typically the "main module"'s memory
        /// export.
        const MEMORY = 1 << 0;
        /// A `realloc` function must be specified, typically named
        /// `cabi_realloc`.
        const REALLOC = 1 << 1;
        /// A string encoding must be specified, which is always utf-8 for now
        /// today.
        const STRING_ENCODING = 1 << 2;
    }
}

impl RequiredOptions {
    fn for_import(interface: &Interface, func: &Function) -> RequiredOptions {
        let sig = interface.wasm_signature(AbiVariant::GuestImport, func);
        let mut ret = RequiredOptions::empty();
        // Lift the params and lower the results for imports
        ret.add_lift(TypeContents::for_types(
            interface,
            func.params.iter().map(|(_, t)| t),
        ));
        ret.add_lower(TypeContents::for_types(
            interface,
            func.results.iter_types(),
        ));

        // If anything is indirect then `memory` will be required to read the
        // indirect values.
        if sig.retptr || sig.indirect_params {
            ret |= RequiredOptions::MEMORY;
        }
        ret
    }

    fn for_export(interface: &Interface, func: &Function) -> RequiredOptions {
        let sig = interface.wasm_signature(AbiVariant::GuestExport, func);
        let mut ret = RequiredOptions::empty();
        // Lower the params and lift the results for exports
        ret.add_lower(TypeContents::for_types(
            interface,
            func.params.iter().map(|(_, t)| t),
        ));
        ret.add_lift(TypeContents::for_types(
            interface,
            func.results.iter_types(),
        ));

        // If anything is indirect then `memory` will be required to read the
        // indirect values, but if the arguments are indirect then `realloc` is
        // additionally required to allocate space for the parameters.
        if sig.retptr || sig.indirect_params {
            ret |= RequiredOptions::MEMORY;
            if sig.indirect_params {
                ret |= RequiredOptions::REALLOC;
            }
        }
        ret
    }

    fn add_lower(&mut self, types: TypeContents) {
        // If lists/strings are lowered into wasm then memory is required as
        // usual but `realloc` is also required to allow the external caller to
        // allocate space in the destination for the list/string.
        if types.contains(TypeContents::LIST) {
            *self |= RequiredOptions::MEMORY | RequiredOptions::REALLOC;
        }
        if types.contains(TypeContents::STRING) {
            *self |= RequiredOptions::MEMORY
                | RequiredOptions::STRING_ENCODING
                | RequiredOptions::REALLOC;
        }
    }

    fn add_lift(&mut self, types: TypeContents) {
        // Unlike for `lower` when lifting a string/list all that's needed is
        // memory, since the string/list already resides in memory `realloc`
        // isn't needed.
        if types.contains(TypeContents::LIST) {
            *self |= RequiredOptions::MEMORY;
        }
        if types.contains(TypeContents::STRING) {
            *self |= RequiredOptions::MEMORY | RequiredOptions::STRING_ENCODING;
        }
    }

    fn into_iter(
        self,
        encoding: StringEncoding,
        memory_index: Option<u32>,
        realloc_index: Option<u32>,
    ) -> Result<impl ExactSizeIterator<Item = CanonicalOption>> {
        #[derive(Default)]
        struct Iter {
            options: [Option<CanonicalOption>; 3],
            current: usize,
            count: usize,
        }

        impl Iter {
            fn push(&mut self, option: CanonicalOption) {
                assert!(self.count < self.options.len());
                self.options[self.count] = Some(option);
                self.count += 1;
            }
        }

        impl Iterator for Iter {
            type Item = CanonicalOption;

            fn next(&mut self) -> Option<Self::Item> {
                if self.current == self.count {
                    return None;
                }
                let option = self.options[self.current];
                self.current += 1;
                option
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.count - self.current, Some(self.count - self.current))
            }
        }

        impl ExactSizeIterator for Iter {}

        let mut iter = Iter::default();

        if self.contains(RequiredOptions::MEMORY) {
            iter.push(CanonicalOption::Memory(memory_index.ok_or_else(|| {
                anyhow!("module does not export a memory named `memory`")
            })?));
        }

        if self.contains(RequiredOptions::REALLOC) {
            iter.push(CanonicalOption::Realloc(realloc_index.ok_or_else(
                || anyhow!("module does not export a function named `cabi_realloc`"),
            )?));
        }

        if self.contains(RequiredOptions::STRING_ENCODING) {
            iter.push(encoding.into());
        }

        Ok(iter)
    }
}

bitflags::bitflags! {
    /// Flags about what kinds of types are present within the recursive
    /// structure of a type.
    struct TypeContents: u8 {
        const STRING = 1 << 0;
        const LIST = 1 << 1;
    }
}

impl TypeContents {
    fn for_types<'a>(interface: &Interface, types: impl Iterator<Item = &'a Type>) -> Self {
        let mut cur = TypeContents::empty();
        for ty in types {
            cur |= Self::for_type(interface, ty);
        }
        cur
    }

    fn for_optional_types<'a>(
        interface: &Interface,
        types: impl Iterator<Item = Option<&'a Type>>,
    ) -> Self {
        Self::for_types(interface, types.flatten())
    }

    fn for_optional_type(interface: &Interface, ty: Option<&Type>) -> Self {
        match ty {
            Some(ty) => Self::for_type(interface, ty),
            None => Self::empty(),
        }
    }

    fn for_type(interface: &Interface, ty: &Type) -> Self {
        match ty {
            Type::Id(id) => match &interface.types[*id].kind {
                TypeDefKind::Record(r) => {
                    Self::for_types(interface, r.fields.iter().map(|f| &f.ty))
                }
                TypeDefKind::Tuple(t) => Self::for_types(interface, t.types.iter()),
                TypeDefKind::Flags(_) => Self::empty(),
                TypeDefKind::Option(t) => Self::for_type(interface, t),
                TypeDefKind::Result(r) => {
                    Self::for_optional_type(interface, r.ok.as_ref())
                        | Self::for_optional_type(interface, r.err.as_ref())
                }
                TypeDefKind::Variant(v) => {
                    Self::for_optional_types(interface, v.cases.iter().map(|c| c.ty.as_ref()))
                }
                TypeDefKind::Union(v) => Self::for_types(interface, v.cases.iter().map(|c| &c.ty)),
                TypeDefKind::Enum(_) => Self::empty(),
                TypeDefKind::List(t) => Self::for_type(interface, t) | Self::LIST,
                TypeDefKind::Type(t) => Self::for_type(interface, t),
                TypeDefKind::Future(_) => todo!("encoding for future"),
                TypeDefKind::Stream(_) => todo!("encoding for stream"),
            },
            Type::String => Self::STRING,
            _ => Self::empty(),
        }
    }
}

/// State relating to encoding a component.
#[derive(Default)]
struct EncodingState<'a> {
    /// The component being encoded.
    component: ComponentEncoding,
    /// The index into the core module index space for the inner core module.
    ///
    /// If `None`, the core module has not been encoded.
    module_index: Option<u32>,
    /// The index into the core instance index space for the inner core module.
    ///
    /// If `None`, the core module has not been instantiated.
    instance_index: Option<u32>,
    /// The index in the core memory index space for the exported memory.
    ///
    /// If `None`, then the memory has not yet been aliased.
    memory_index: Option<u32>,
    /// The index in the core function index space for the realloc function.
    ///
    /// If `None`, then the realloc function has not yet been aliased.
    realloc_index: Option<u32>,
    /// The index of the shim instance used for lowering imports into the core instance.
    ///
    /// If `None`, then the shim instance how not yet been encoded.
    shim_instance_index: Option<u32>,
    /// The index of the fixups module to instantiate to fill in the lowered imports.
    ///
    /// If `None`, then a fixup module has not yet been encoded.
    fixups_module_index: Option<u32>,

    /// A map of named adapter modules and the index that the module was defined
    /// at.
    adapter_modules: IndexMap<&'a str, u32>,
    /// A map of adapter module instances and the index of their instance.
    adapter_instances: IndexMap<&'a str, u32>,
    /// A map of the index of the aliased realloc function for each adapter
    /// module. Note that adapters have two realloc functions, one for imports
    /// and one for exports.
    adapter_import_reallocs: IndexMap<&'a str, Option<u32>>,
    adapter_export_reallocs: IndexMap<&'a str, Option<u32>>,
}

impl<'a> EncodingState<'a> {
    fn encode_core_module(&mut self, module: &[u8]) -> u32 {
        assert!(self.module_index.is_none());
        let ret = self.component.core_module_raw(module);
        self.module_index = Some(ret);
        ret
    }

    fn encode_core_adapter_module(&mut self, name: &'a str, module: &[u8]) -> u32 {
        let index = self.component.core_module_raw(module);
        assert!(self.adapter_modules.insert(name, index).is_none());
        index
    }

    fn encode_core_instantiation(
        &mut self,
        imports: &ImportEncoder<'a>,
        info: &ValidatedModule<'a>,
    ) -> Result<()> {
        // Encode a shim instantiation if needed
        let shims = self.encode_shim_instantiation(imports, info);

        // For each instance import into the main module create a
        // pseudo-core-wasm-module via a bag-of-exports.
        let mut args = Vec::new();
        for name in info.required_imports.keys() {
            let index = self.import_instance_to_lowered_core_instance(
                CustomModule::Main,
                name,
                imports,
                &shims,
                info.metadata,
            );
            args.push((*name, ModuleArg::Instance(index)));
        }

        // For each adapter module instance imported into the core wasm module
        // the appropriate shim is packaged up into a bag-of-exports instance.
        // Note that adapter modules currently don't deal with
        // indirect-vs-direct lowerings, everything is indirect.
        for (adapter, funcs) in info.adapters_required.iter() {
            let shim_instance = self
                .shim_instance_index
                .expect("shim should be instantiated");
            let mut exports = Vec::new();

            for (func, _ty) in funcs {
                let index = self.component.alias_core_item(
                    shim_instance,
                    ExportKind::Func,
                    &shims.shim_names[&ShimKind::Adapter { adapter, func }],
                );
                exports.push((*func, ExportKind::Func, index));
            }

            let index = self.component.instantiate_core_exports(exports);
            args.push((*adapter, ModuleArg::Instance(index)));
        }

        // Instantiate the main module now that all of its arguments have been
        // prepared. With this we know have the main linear memory for
        // liftings/lowerings later on as well as the adapter modules, if any,
        // instantiated after the core wasm module.
        self.instantiate_core_module(args, info);
        self.instantiate_adapter_modules(imports, &shims);

        // With all the core wasm instances in play now the original shim
        // module, if present, can be filled in with lowerings/adapters/etc.
        self.encode_indirect_lowerings(imports, shims)
    }

    /// Lowers a named imported interface a core wasm instances suitable to
    /// provide as an instantiation argument to another core wasm module.
    ///
    /// * `for_module` the module that this instance is being created for, or
    ///   otherwise which `realloc` option is used for the lowerings.
    /// * `name` - the name of the imported interface that's being lowered.
    /// * `imports` - the list of all imports known for this encoding.
    /// * `shims` - the indirect/adapter shims created prior, if any.
    fn import_instance_to_lowered_core_instance(
        &mut self,
        for_module: CustomModule<'_>,
        name: &str,
        imports: &ImportEncoder<'_>,
        shims: &Shims<'_>,
        metadata: &BindgenMetadata,
    ) -> u32 {
        let (instance_index, _, import) = imports.map.get_full(name).unwrap();
        let mut exports = Vec::with_capacity(import.direct.len() + import.indirect.len());

        // Add an entry for all indirect lowerings which come as an export of
        // the shim module.
        for (i, lowering) in import.indirect.iter().enumerate() {
            let encoding =
                metadata.import_encodings[&(name.to_string(), lowering.name.to_string())];
            let index = self.component.alias_core_item(
                self.shim_instance_index
                    .expect("shim should be instantiated"),
                ExportKind::Func,
                &shims.shim_names[&ShimKind::IndirectLowering {
                    interface: name,
                    indirect_index: i,
                    realloc: for_module,
                    encoding,
                }],
            );
            exports.push((lowering.name, ExportKind::Func, index));
        }

        // All direct lowerings can be `canon lower`'d here immediately and
        // passed as arguments.
        for lowering in &import.direct {
            let func_index = self
                .component
                .alias_func(instance_index as u32, lowering.name);
            let core_func_index = self.component.lower_func(func_index, []);
            exports.push((lowering.name, ExportKind::Func, core_func_index));
        }

        self.component.instantiate_core_exports(exports)
    }

    fn encode_imports(&mut self, imports: &ImportEncoder) {
        for (name, import) in &imports.map {
            self.component.import(name, import.url, import.ty);
        }
    }

    fn encode_exports<'b>(
        &mut self,
        types: &TypeEncoder<'b>,
        metadata: &BindgenMetadata,
        instance_index: u32,
        realloc_index: Option<u32>,
    ) -> Result<()> {
        for (export, export_name) in metadata.world.exports() {
            let mut interface_exports = Vec::new();

            // Make sure all named types are present in the exported instance
            for (_id, def) in export.types.iter() {
                let name = match &def.name {
                    Some(name) => name,
                    None => continue,
                };
                let ty = *types
                    .type_map
                    .get(&TypeDefKey {
                        interface: export,
                        def,
                    })
                    .expect("the type should be encoded");
                interface_exports.push((name.as_str(), ComponentExportKind::Type, ty));
            }

            // Alias the exports from the core module
            for func in &export.functions {
                let name = func.core_export_name(export_name);
                let core_func_index =
                    self.component
                        .alias_core_item(instance_index, ExportKind::Func, name.as_ref());

                let ty = *types
                    .func_type_map
                    .get(&FunctionKey {
                        interface: export,
                        func,
                    })
                    .expect("the type should be encoded");

                let options = RequiredOptions::for_export(export, func);

                let encoding = metadata.export_encodings[&name[..]];
                let mut options = options
                    .into_iter(encoding, self.memory_index, realloc_index)?
                    .collect::<Vec<_>>();
                if export.guest_export_needs_post_return(func) {
                    let post_return = self.component.alias_core_item(
                        instance_index,
                        ExportKind::Func,
                        &format!("cabi_post_{name}"),
                    );
                    options.push(CanonicalOption::PostReturn(post_return));
                }
                let func_index = self.component.lift_func(core_func_index, ty, options);

                interface_exports.push((func.name.as_str(), ComponentExportKind::Func, func_index));
            }

            if interface_exports.is_empty() {
                continue;
            }

            // The default exported interface has all of its items exported
            // directly but otherwise an instance type is created and then
            // exported.
            match export_name {
                Some(export_name) => {
                    if export_name.is_empty() {
                        bail!("cannot export an unnamed interface");
                    }

                    let instance_index = self.component.instantiate_exports(interface_exports);
                    self.component.export(
                        export_name,
                        export.url.as_deref().unwrap_or(""),
                        ComponentExportKind::Instance,
                        instance_index,
                    );
                }
                None => {
                    for (name, kind, idx) in interface_exports {
                        self.component.export(name, "", kind, idx);
                    }
                }
            }
        }

        Ok(())
    }

    fn encode_shim_instantiation(
        &mut self,
        imports: &ImportEncoder<'a>,
        info: &ValidatedModule<'a>,
    ) -> Shims<'a> {
        let mut signatures = Vec::new();
        let mut ret = Shims::default();

        // For all interfaces imported into the main module record all of their
        // indirect lowerings into `Shims`.
        for name in info.required_imports.keys() {
            let import = &imports.map[name];
            ret.append_indirect(
                name,
                CustomModule::Main,
                import,
                info.metadata,
                &mut signatures,
            );
        }

        // For all required adapter modules a shim is created for each required
        // function and additionally a set of shims are created for the
        // interface imported into the shim module itself.
        for (adapter, funcs) in info.adapters_required.iter() {
            let info = &imports.adapters[adapter];
            for (name, _) in info.required_imports.iter() {
                let import = &imports.map[name];
                ret.append_indirect(
                    name,
                    CustomModule::Adapter(adapter),
                    import,
                    info.metadata,
                    &mut signatures,
                );
            }
            for (func, ty) in funcs {
                let name = ret.list.len().to_string();
                log::debug!("shim {name} is adapter `{adapter}::{func}`");
                signatures.push(WasmSignature {
                    params: ty.params().iter().map(to_wasm_type).collect(),
                    results: ty.results().iter().map(to_wasm_type).collect(),
                    indirect_params: false,
                    retptr: false,
                });
                ret.list.push(Shim {
                    name,
                    debug_name: format!("adapt-{adapter}-{func}"),
                    // Pessimistically assume that all adapters require memory
                    // in one form or another. While this isn't technically true
                    // it's true enough for WASI.
                    options: RequiredOptions::MEMORY,
                    kind: ShimKind::Adapter { adapter, func },
                });
            }
        }
        if ret.list.is_empty() {
            return ret;
        }

        for shim in ret.list.iter() {
            ret.shim_names.insert(shim.kind, shim.name.clone());
        }

        assert!(self.shim_instance_index.is_none());
        assert!(self.fixups_module_index.is_none());

        // This function encodes two modules:
        // - A shim module that defines a table and exports functions
        //   that indirectly call through the table.
        // - A fixup module that imports that table and a set of functions
        //   and populates the imported table via active element segments. The
        //   fixup module is used to populate the shim's table once the
        //   imported functions have been lowered.

        let mut types = TypeSection::new();
        let mut tables = TableSection::new();
        let mut functions = FunctionSection::new();
        let mut exports = ExportSection::new();
        let mut code = CodeSection::new();
        let mut sigs = IndexMap::new();
        let mut imports_section = ImportSection::new();
        let mut elements = ElementSection::new();
        let mut func_indexes = Vec::new();
        let mut func_names = NameMap::new();

        for (i, (sig, shim)) in signatures.iter().zip(&ret.list).enumerate() {
            let i = i as u32;
            let type_index = *sigs.entry(sig).or_insert_with(|| {
                let index = types.len();
                types.function(
                    sig.params.iter().map(to_val_type),
                    sig.results.iter().map(to_val_type),
                );
                index
            });

            functions.function(type_index);
            Self::encode_shim_function(type_index, i, &mut code, sig.params.len() as u32);
            exports.export(&shim.name, ExportKind::Func, i);

            imports_section.import("", &shim.name, EntityType::Function(type_index));
            func_indexes.push(i);
            func_names.append(i, &shim.debug_name);
        }
        let mut names = NameSection::new();
        names.functions(&func_names);

        let table_type = TableType {
            element_type: ValType::FuncRef,
            minimum: signatures.len() as u32,
            maximum: Some(signatures.len() as u32),
        };

        tables.table(table_type);

        exports.export(INDIRECT_TABLE_NAME, ExportKind::Table, 0);
        imports_section.import("", INDIRECT_TABLE_NAME, table_type);

        elements.active(
            None,
            &ConstExpr::i32_const(0),
            ValType::FuncRef,
            Elements::Functions(&func_indexes),
        );

        let mut shim = Module::new();
        shim.section(&types);
        shim.section(&functions);
        shim.section(&tables);
        shim.section(&exports);
        shim.section(&code);
        shim.section(&names);

        let mut fixups = Module::default();
        fixups.section(&types);
        fixups.section(&imports_section);
        fixups.section(&elements);

        let shim_module_index = self.component.core_module(&shim);
        self.fixups_module_index = Some(self.component.core_module(&fixups));
        self.shim_instance_index = Some(self.component.instantiate(shim_module_index, []));

        return ret;

        fn to_wasm_type(ty: &wasmparser::ValType) -> WasmType {
            match ty {
                wasmparser::ValType::I32 => WasmType::I32,
                wasmparser::ValType::I64 => WasmType::I64,
                wasmparser::ValType::F32 => WasmType::F32,
                wasmparser::ValType::F64 => WasmType::F64,
                _ => unreachable!(),
            }
        }
    }

    fn encode_shim_function(
        type_index: u32,
        func_index: u32,
        code: &mut CodeSection,
        param_count: u32,
    ) {
        let mut func = wasm_encoder::Function::new(std::iter::empty());
        for i in 0..param_count {
            func.instruction(&Instruction::LocalGet(i));
        }
        func.instruction(&Instruction::I32Const(func_index as i32));
        func.instruction(&Instruction::CallIndirect {
            ty: type_index,
            table: 0,
        });
        func.instruction(&Instruction::End);
        code.function(&func);
    }

    fn encode_indirect_lowerings(
        &mut self,
        imports: &ImportEncoder,
        shims: Shims<'_>,
    ) -> Result<()> {
        if shims.list.is_empty() {
            return Ok(());
        }

        let shim_instance_index = self
            .shim_instance_index
            .expect("must have an instantiated shim");

        let table_index = self.component.alias_core_item(
            shim_instance_index,
            ExportKind::Table,
            INDIRECT_TABLE_NAME,
        );

        let mut exports = Vec::with_capacity(imports.indirect_count as usize);
        exports.push((INDIRECT_TABLE_NAME, ExportKind::Table, table_index));

        for shim in shims.list.iter() {
            let core_func_index = match &shim.kind {
                // Indirect lowerings are a `canon lower`'d function with
                // options specified from a previously instantiated instance.
                // This previous instance could either be the main module or an
                // adapter module, which affects the `realloc` option here.
                // Currently only one linear memory is supported so the linear
                // memory always comes from the main module.
                ShimKind::IndirectLowering {
                    interface,
                    indirect_index,
                    realloc,
                    encoding,
                } => {
                    let (instance_index, _, interface) = imports.map.get_full(interface).unwrap();
                    let func_index = self.component.alias_func(
                        instance_index as u32,
                        interface.indirect[*indirect_index].name,
                    );

                    let realloc = match realloc {
                        CustomModule::Main => self.realloc_index,
                        CustomModule::Adapter(name) => self.adapter_import_reallocs[name],
                    };

                    self.component.lower_func(
                        func_index,
                        shim.options
                            .into_iter(*encoding, self.memory_index, realloc)?,
                    )
                }

                // Adapter shims are defined by an export from and adapter
                // instance, so use the specified name here and the previously
                // created instances to get the core item that represents the
                // shim.
                ShimKind::Adapter { adapter, func } => self.component.alias_core_item(
                    self.adapter_instances[adapter],
                    ExportKind::Func,
                    func,
                ),
            };

            exports.push((shim.name.as_str(), ExportKind::Func, core_func_index));
        }

        let instance_index = self.component.instantiate_core_exports(exports);
        self.component.instantiate(
            self.fixups_module_index.expect("must have fixup module"),
            [("", ModuleArg::Instance(instance_index))],
        );
        Ok(())
    }

    fn instantiate_core_module<'b, A>(&mut self, args: A, info: &ValidatedModule<'_>)
    where
        A: IntoIterator<Item = (&'b str, ModuleArg)>,
        A::IntoIter: ExactSizeIterator,
    {
        assert!(self.instance_index.is_none());

        let instance_index = self
            .component
            .instantiate(self.module_index.expect("core module encoded"), args);

        if info.has_memory {
            self.memory_index = Some(self.component.alias_core_item(
                instance_index,
                ExportKind::Memory,
                "memory",
            ));
        }

        if let Some(name) = &info.realloc {
            self.realloc_index = Some(self.component.alias_core_item(
                instance_index,
                ExportKind::Func,
                name,
            ));
        }

        self.instance_index = Some(instance_index);
    }

    /// This function will instantiate all required adapter modules required by
    /// the main module (specified by `info`).
    ///
    /// Each adapter here is instantiated with its required imported interface,
    /// if any.
    fn instantiate_adapter_modules(&mut self, imports: &ImportEncoder<'a>, shims: &Shims<'_>) {
        for (name, info) in imports.adapters.iter() {
            let mut args = Vec::new();

            let mut core_exports = Vec::new();
            for export_name in info.needs_core_exports.iter() {
                let index = self.component.alias_core_item(
                    self.instance_index
                        .expect("adaptee index set at this point"),
                    ExportKind::Func,
                    export_name,
                );
                core_exports.push((export_name.as_str(), ExportKind::Func, index));
            }
            if !core_exports.is_empty() {
                let instance = self.component.instantiate_core_exports(core_exports);
                args.push((MAIN_MODULE_IMPORT_NAME, ModuleArg::Instance(instance)));
            }
            // If the adapter module requires a `memory` import then specify
            // that here. For now assume that the module name of the memory is
            // different from the imported interface. That's true enough for now
            // since it's `env::memory`.
            if let Some((module, name)) = &info.needs_memory {
                for (import_name, _) in info.required_imports.iter() {
                    assert!(module != import_name);
                }
                assert!(module != name);
                let memory = self.memory_index.unwrap();
                let instance = self.component.instantiate_core_exports([(
                    name.as_str(),
                    ExportKind::Memory,
                    memory,
                )]);
                args.push((module.as_str(), ModuleArg::Instance(instance)));
            }
            for (import_name, _) in info.required_imports.iter() {
                let instance = self.import_instance_to_lowered_core_instance(
                    CustomModule::Adapter(name),
                    import_name,
                    imports,
                    shims,
                    info.metadata,
                );
                args.push((import_name, ModuleArg::Instance(instance)));
            }
            let instance = self.component.instantiate(self.adapter_modules[name], args);
            self.adapter_instances.insert(name, instance);

            let realloc = info.export_realloc.as_ref().map(|name| {
                self.component
                    .alias_core_item(instance, ExportKind::Func, name)
            });
            self.adapter_export_reallocs.insert(name, realloc);
            let realloc = info.import_realloc.as_ref().map(|name| {
                self.component
                    .alias_core_item(instance, ExportKind::Func, name)
            });
            self.adapter_import_reallocs.insert(name, realloc);
        }
    }
}

/// A list of "shims" which start out during the component instantiation process
/// as functions which immediately trap due to a `call_indirect`-to-`null` but
/// will get filled in by the time the component instantiation process
/// completes.
///
/// Shims currently include:
///
/// * "Indirect functions" lowered from imported instances where the lowering
///   requires an item exported from the main module. These are indirect due to
///   the circular dependency between the module needing an import and the
///   import needing the module.
///
/// * Adapter modules which convert from a historical ABI to the component
///   model's ABI (e.g. wasi preview1 to preview2) get a shim since the adapters
///   are currently indicated as always requiring the memory of the main module.
///
/// This structure is created by `encode_shim_instantiation`.
#[derive(Default)]
struct Shims<'a> {
    /// The list of all shims that a module will require.
    list: Vec<Shim<'a>>,

    /// A map from a shim to the name of the shim in the shim instance.
    shim_names: IndexMap<ShimKind<'a>, String>,
}

struct Shim<'a> {
    /// Canonical ABI options required by this shim, used during `canon lower`
    /// operations.
    options: RequiredOptions,

    /// The name, in the shim instance, of this shim.
    ///
    /// Currently this is `"0"`, `"1"`, ...
    name: String,

    /// A human-readable debugging name for this shim, used in a core wasm
    /// `name` section.
    debug_name: String,

    /// Precise information about what this shim is a lowering of.
    kind: ShimKind<'a>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum ShimKind<'a> {
    /// This shim is a late indirect lowering of an imported function in a
    /// component which is only possible after prior core wasm modules are
    /// instantiated so their memories and functions are available.
    IndirectLowering {
        /// The name of the interface that's being lowered.
        interface: &'a str,
        /// The index within the `indirect` array of the function being lowered.
        indirect_index: usize,
        /// Which instance to pull the `realloc` function from, if necessary.
        realloc: CustomModule<'a>,
        /// The string encoding that this lowering is going to use.
        encoding: StringEncoding,
    },
    /// This shim is a core wasm function defined in an adapter module but isn't
    /// available until the adapter module is itself instantiated.
    Adapter {
        /// The name of the adapter module this shim comes from.
        adapter: &'a str,
        /// The name of the export in the adapter module this shim points to.
        func: &'a str,
    },
}

/// Indicator for which module is being used for a lowering or where options
/// like `realloc` are drawn from.
///
/// This is necessary for situations such as an imported function being lowered
/// into the main module and additionally into an adapter module. For example an
/// adapter might adapt from preview1 to preview2 for the standard library of a
/// programming language but the main module's custom application code may also
/// explicitly import from preview2. These two different lowerings of a preview2
/// function are parameterized by this enumeration.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum CustomModule<'a> {
    /// This points to the "main module" which is generally the "output of LLVM"
    /// or what a user wrote.
    Main,
    /// This is selecting an adapter module, identified by name here, where
    /// something is being lowered into.
    Adapter(&'a str),
}

impl<'a> Shims<'a> {
    /// Adds all shims necessary for the `import` provided, namely iterating
    /// over its indirect lowerings and appending a shim per lowering.
    fn append_indirect(
        &mut self,
        name: &'a str,
        for_module: CustomModule<'a>,
        import: &ImportedInterface<'a>,
        metadata: &BindgenMetadata,
        sigs: &mut Vec<WasmSignature>,
    ) {
        for (indirect_index, lowering) in import.indirect.iter().enumerate() {
            let shim_name = self.list.len().to_string();
            log::debug!(
                "shim {shim_name} is import `{name}` lowering {indirect_index} `{}`",
                lowering.name
            );
            sigs.push(lowering.sig.clone());
            let encoding =
                metadata.import_encodings[&(name.to_string(), lowering.name.to_string())];
            self.list.push(Shim {
                name: shim_name,
                debug_name: format!("indirect-{name}-{}", lowering.name),
                options: lowering.options,
                kind: ShimKind::IndirectLowering {
                    interface: name,
                    indirect_index,
                    realloc: for_module,
                    encoding,
                },
            });
        }
    }
}

#[derive(Debug)]
struct DirectLowering<'a> {
    name: &'a str,
}

#[derive(Debug)]
struct IndirectLowering<'a> {
    name: &'a str,
    sig: WasmSignature,
    options: RequiredOptions,
}

#[derive(Debug)]
struct ImportedInterface<'a> {
    ty: ComponentTypeRef,
    url: &'a str,
    direct: Vec<DirectLowering<'a>>,
    indirect: Vec<IndirectLowering<'a>>,
}

/// Helper type used when encoding a component to have helpers that
/// simultaneously encode an item while returning its corresponding index in the
/// generated index spaces as well.
#[derive(Default)]
struct ComponentEncoding {
    /// The binary component as created by `wasm-encoder`.
    component: Component,

    /// The last section which was appended to during encoding. This type is
    /// generated by the `section_accessors` macro below.
    ///
    /// When something is encoded this is used if it matches the kind of item
    /// being encoded, otherwise it's "flushed" to the output component and a
    /// new section is started.
    last_section: LastSection,

    // Core index spaces
    core_modules: u32,
    core_funcs: u32,
    core_memories: u32,
    core_tables: u32,
    core_instances: u32,

    // Component index spaces
    funcs: u32,
    instances: u32,
}

impl ComponentEncoding {
    fn finish(mut self) -> Vec<u8> {
        self.flush();
        self.component.finish()
    }

    fn instantiate<'a, A>(&mut self, module_index: u32, args: A) -> u32
    where
        A: IntoIterator<Item = (&'a str, ModuleArg)>,
        A::IntoIter: ExactSizeIterator,
    {
        self.instances().instantiate(module_index, args);
        inc(&mut self.core_instances)
    }

    fn alias_func(&mut self, instance: u32, name: &str) -> u32 {
        self.aliases()
            .instance_export(instance, ComponentExportKind::Func, name);
        inc(&mut self.funcs)
    }

    fn lower_func<O>(&mut self, func_index: u32, options: O) -> u32
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        self.canonical_functions().lower(func_index, options);
        inc(&mut self.core_funcs)
    }

    fn lift_func<O>(&mut self, core_func_index: u32, type_index: u32, options: O) -> u32
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        self.canonical_functions()
            .lift(core_func_index, type_index, options);
        inc(&mut self.funcs)
    }

    fn instantiate_core_exports<'a, E>(&mut self, exports: E) -> u32
    where
        E: IntoIterator<Item = (&'a str, ExportKind, u32)>,
        E::IntoIter: ExactSizeIterator,
    {
        self.instances().export_items(exports);
        inc(&mut self.core_instances)
    }

    fn instantiate_exports<'a, E>(&mut self, exports: E) -> u32
    where
        E: IntoIterator<Item = (&'a str, ComponentExportKind, u32)>,
        E::IntoIter: ExactSizeIterator,
    {
        self.component_instances().export_items(exports);
        inc(&mut self.instances)
    }

    fn core_module(&mut self, module: &Module) -> u32 {
        self.flush();
        self.component.section(&ModuleSection(module));
        inc(&mut self.core_modules)
    }

    fn core_module_raw(&mut self, module: &[u8]) -> u32 {
        self.flush();
        self.component.section(&wasm_encoder::RawSection {
            id: ComponentSectionId::CoreModule.into(),
            data: module,
        });
        inc(&mut self.core_modules)
    }

    fn alias_core_item(&mut self, instance: u32, kind: ExportKind, name: &str) -> u32 {
        self.aliases().core_instance_export(instance, kind, name);
        match kind {
            ExportKind::Func => inc(&mut self.core_funcs),
            ExportKind::Table => inc(&mut self.core_tables),
            ExportKind::Memory => inc(&mut self.core_memories),
            ExportKind::Global | ExportKind::Tag => unreachable!(),
        }
    }

    fn export(&mut self, name: &str, url: &str, kind: ComponentExportKind, idx: u32) {
        self.exports().export(name, url, kind, idx);
    }

    fn import(&mut self, name: &str, url: &str, ty: ComponentTypeRef) -> u32 {
        let ret = match &ty {
            ComponentTypeRef::Instance(_) => inc(&mut self.instances),
            ComponentTypeRef::Func(_) => inc(&mut self.funcs),
            _ => unimplemented!(),
        };
        self.imports().import(name, url, ty);
        ret
    }
}

// Helper macro to generate methods on `ComponentEncoding` to get specific
// section encoders that automatically flush and write out prior sections as
// necessary.
macro_rules! section_accessors {
    ($($method:ident => $section:ident)*) => (
        #[derive(Default)]
        enum LastSection {
            #[default]
            None,
            $($section($section),)*
        }

        impl ComponentEncoding {
            $(
                fn $method(&mut self) -> &mut $section {
                    match &self.last_section {
                        // The last encoded section matches the section that's
                        // being requested, so no change is necessary.
                        LastSection::$section(_) => {}

                        // Otherwise the last section didn't match this section,
                        // so flush any prior section if needed and start
                        // encoding the desired section of this method.
                        _ => {
                            self.flush();
                            self.last_section = LastSection::$section($section::new());
                        }
                    }
                    match &mut self.last_section {
                        LastSection::$section(ret) => ret,
                        _ => unreachable!()
                    }
                }
            )*

            /// Writes out the last section into the final component binary if
            /// there is a section specified, otherwise does nothing.
            fn flush(&mut self) {
                match mem::take(&mut self.last_section) {
                    LastSection::None => {}
                    $(
                        LastSection::$section(section) => {
                            self.component.section(&section);
                        }
                    )*
                }
            }

        }
    )
}

section_accessors! {
    component_instances => ComponentInstanceSection
    instances => InstanceSection
    canonical_functions => CanonicalFunctionSection
    aliases => ComponentAliasSection
    exports => ComponentExportSection
    imports => ComponentImportSection
}

fn inc(idx: &mut u32) -> u32 {
    let ret = *idx;
    *idx += 1;
    ret
}

/// The import encoder handles indirect lowering of any imports
/// that require canonical options to be specified.
///
/// Lowering of such imports is done through a shim module that
/// defines a table of functions and exports functions that indirectly
/// call through the table.
///
/// Another module is responsible for "fixing-up" the table of functions
/// once the functions have been lowered, after the core module is instantiated.
///
/// If a lowering does not require canonical options, the import is lowered before
/// the core module is instantiated and passed directly as an instantiation argument.
#[derive(Default)]
struct ImportEncoder<'a> {
    map: IndexMap<&'a str, ImportedInterface<'a>>,
    adapters: IndexMap<&'a str, ValidatedAdapter<'a>>,
    direct_count: u32,
    indirect_count: u32,
}

impl<'a> ImportEncoder<'a> {
    fn import(
        &mut self,
        name: &'a str,
        interface: &'a Interface,
        ty: ComponentTypeRef,
        funcs: Option<&IndexSet<&'a str>>,
    ) -> Result<()> {
        match self.map.entry(name) {
            indexmap::map::Entry::Occupied(e) => {
                if e.get().ty != ty {
                    bail!("duplicate import `{name}`")
                }
            }
            indexmap::map::Entry::Vacant(e) => {
                let mut direct = Vec::new();
                let mut indirect = Vec::new();
                for f in &interface.functions {
                    if let Some(funcs) = funcs {
                        if !funcs.contains(f.name.as_str()) {
                            continue;
                        }
                    }
                    let options = RequiredOptions::for_import(interface, f);

                    if options.is_empty() {
                        self.direct_count += 1;
                        direct.push(DirectLowering { name: &f.name });
                    } else {
                        let sig = interface.wasm_signature(AbiVariant::GuestImport, f);
                        self.indirect_count += 1;
                        indirect.push(IndirectLowering {
                            name: &f.name,
                            sig,
                            options,
                        });
                    }
                }

                e.insert(ImportedInterface {
                    ty,
                    url: interface.url.as_deref().unwrap_or(""),
                    direct,
                    indirect,
                });
            }
        }

        Ok(())
    }
}

/// An encoder of components based on `wit` interface definitions.
#[derive(Default)]
pub struct ComponentEncoder {
    module: Vec<u8>,
    metadata: BindgenMetadata,
    validate: bool,
    types_only: bool,

    // This is a map from the name of the adapter to a pair of:
    //
    // * The wasm of the adapter itself, with `component-type` sections
    //   stripped.
    // * the metadata for the adapter, verified to have no exports and only
    //   imports.
    adapters: IndexMap<String, (Vec<u8>, BindgenMetadata)>,
}

impl ComponentEncoder {
    /// Set the core module to encode as a component.
    /// This method will also parse any component type information stored in custom sections
    /// inside the module, and add them as the interface, imports, and exports.
    pub fn module(mut self, module: &[u8]) -> Result<Self> {
        let (wasm, metadata) = metadata::decode(module)?;
        self.module = wasm;
        self.metadata.merge(metadata)?;
        Ok(self)
    }

    /// Sets whether or not the encoder will validate its output.
    pub fn validate(mut self, validate: bool) -> Self {
        self.validate = validate;
        self
    }

    /// Add a "world" of interfaces (exports/imports/default) to this encoder
    /// to configure what's being imported/exported.
    ///
    /// The string encoding of the specified world is supplied here as
    /// well.
    pub fn world(mut self, world: World, encoding: StringEncoding) -> Result<Self> {
        self.metadata.merge(BindgenMetadata::new(world, encoding))?;
        Ok(self)
    }

    /// Specifies a new adapter which is used to translate from a historical
    /// wasm ABI to the canonical ABI and the `interface` provided.
    ///
    /// This is primarily used to polyfill, for example,
    /// `wasi_snapshot_preview1` with a component-model using interface. The
    /// `name` provided is the module name of the adapter that is being
    /// polyfilled, for example `"wasi_snapshot_preview1"`.
    ///
    /// The `bytes` provided is a core wasm module which implements the `name`
    /// interface in terms of the `interface` interface. This core wasm module
    /// is severely restricted in its shape, for example it cannot have any data
    /// segments or element segments.
    ///
    /// The `interface` provided is the component-model-using-interface that the
    /// wasm module specified by `bytes` imports. The `bytes` will then import
    /// `interface` and export functions to get imported from the module `name`
    /// in the core wasm that's being wrapped.
    pub fn adapter(mut self, name: &str, bytes: &[u8]) -> Result<Self> {
        let (wasm, metadata) = metadata::decode(bytes)?;
        self.adapters.insert(name.to_string(), (wasm, metadata));
        Ok(self)
    }

    /// Indicates whether this encoder is only encoding types and does not
    /// require a `module` as input.
    pub fn types_only(mut self, only: bool) -> Self {
        self.types_only = only;
        self
    }

    /// Encode the component and return the bytes.
    pub fn encode(&self) -> Result<Vec<u8>> {
        let info = if !self.module.is_empty() {
            let adapters = self
                .adapters
                .keys()
                .map(|s| s.as_str())
                .collect::<IndexSet<_>>();
            Some(validate_module(&self.module, &self.metadata, &adapters)?)
        } else {
            None
        };

        let mut state = EncodingState::default();
        let mut types = TypeEncoder::default();
        let mut imports = ImportEncoder::default();
        types.encode_func_types(self.metadata.world.exports().map(|p| p.0))?;
        types.encode_instance_imports(&self.metadata.world.imports, info.as_ref(), &mut imports)?;

        for (_name, (_, metadata)) in self.adapters.iter() {
            types.encode_func_types(metadata.world.exports().map(|p| p.0))?;
        }

        if self.types_only {
            if !self.module.is_empty() {
                bail!("a module cannot be specified for a types-only encoding");
            }

            // In "types only" mode the main difference is how exports are
            // encoded, imports are the same as usual. Exported interfaces are
            // represented with an exported instance type, and exported default
            // interfaces are represented as direct exports from the component
            // itself.
            let mut raw_exports = Vec::new();
            let mut default_exports = Vec::new();
            for (interface, name) in self.metadata.world.exports() {
                match name {
                    Some(name) => {
                        let index = types
                            .encode_interface_as_instance_type(interface, None)?
                            .unwrap();
                        raw_exports.push((
                            name,
                            interface.url.as_deref().unwrap_or(""),
                            ComponentExportKind::Type,
                            index,
                        ));
                    }
                    None => {
                        default_exports = types
                            .encode_interface_as_instance_type_exports(interface, None)?
                            .unwrap();
                    }
                }
            }

            types.finish(&mut state.component);
            state.encode_imports(&imports);

            for (name, url, ty, index) in raw_exports {
                state.component.export(name, url, ty, index);
            }

            // Default exports are encoded without a URL since all of the
            // items being exported come from the same interface
            for (name, ty) in default_exports {
                let index = match ty {
                    ComponentTypeRef::Type(_, idx) => idx,
                    ComponentTypeRef::Func(idx) => idx,
                    _ => unimplemented!(),
                };
                state
                    .component
                    .export(name, "", ComponentExportKind::Type, index);
            }
        } else {
            if self.module.is_empty() {
                bail!("a module is required when encoding a component");
            }
            let info = info.as_ref().unwrap();

            // Process adapters which are required here. Iterate over all
            // adapters and figure out what functions are required from the
            // adapter itself, either because the functions are imported by the
            // main module or they're part of the adapter's exports.
            for (name, (wasm, metadata)) in self.adapters.iter() {
                let required =
                    required_adapter_exports(info.adapters_required.get(name.as_str()), metadata);
                if required.is_empty() {
                    continue;
                }
                let wasm = crate::gc::run(wasm, &required)
                    .context("failed to reduce input adapter module to its minimal size")?;
                let info = validate_adapter_module(&wasm, metadata, &required)
                    .context("failed to validate the imports of the minimized adapter module")?;
                state.encode_core_adapter_module(name, &wasm);
                for (name, required) in info.required_imports.iter() {
                    let interface = &metadata.world.imports[*name];
                    types.encode_instance_import(name, interface, Some(required), &mut imports)?;
                }
                imports.adapters.insert(name, info);
            }

            for (interface, _default) in self.metadata.world.exports() {
                types.encode_interface_named_types(interface)?;
            }

            types.finish(&mut state.component);

            state.encode_imports(&imports);
            state.encode_core_module(&self.module);
            state.encode_core_instantiation(&imports, info)?;
            state.encode_exports(
                &types,
                &self.metadata,
                state.instance_index.expect("instantiated by now"),
                state.realloc_index,
            )?;
            for (name, (_, metadata)) in self.adapters.iter() {
                if metadata.world.exports().count() == 0 {
                    continue;
                }
                state.encode_exports(
                    &types,
                    metadata,
                    state.adapter_instances[name.as_str()],
                    state.adapter_export_reallocs[name.as_str()],
                )?;
            }
        }

        let bytes = state.component.finish();

        if self.validate {
            let mut validator = Validator::new_with_features(WasmFeatures {
                component_model: true,
                ..Default::default()
            });

            validator
                .validate_all(&bytes)
                .context("failed to validate component output")?;
        }

        Ok(bytes)
    }
}

fn required_adapter_exports(
    required_by_import: Option<&IndexMap<&str, FuncType>>,
    metadata: &BindgenMetadata,
) -> IndexMap<String, FuncType> {
    use wasmparser::ValType;

    let mut required = IndexMap::new();
    if let Some(imports) = required_by_import {
        for (name, ty) in imports {
            required.insert(name.to_string(), ty.clone());
        }
    }
    for (interface, name) in metadata.world.exports() {
        for func in interface.functions.iter() {
            let name = func.core_export_name(name);
            let ty = interface.wasm_signature(AbiVariant::GuestExport, func);
            let prev = required.insert(
                name.into_owned(),
                wasmparser::FuncType::new(
                    ty.params.iter().map(to_valty),
                    ty.results.iter().map(to_valty),
                ),
            );
            assert!(prev.is_none());
        }
    }
    return required;

    fn to_valty(ty: &WasmType) -> ValType {
        match ty {
            WasmType::I32 => ValType::I32,
            WasmType::I64 => ValType::I64,
            WasmType::F32 => ValType::F32,
            WasmType::F64 => ValType::F64,
        }
    }
}
