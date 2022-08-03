use crate::{
    validation::{expected_export_name, validate_module},
    StringEncoding,
};
use anyhow::{anyhow, bail, Context, Result};
use indexmap::{map::Entry, IndexMap, IndexSet};
use std::{
    hash::{Hash, Hasher},
    ops::{BitOr, BitOrAssign},
};
use wasm_encoder::*;
use wasmparser::{Validator, WasmFeatures};
use wit_parser::{
    abi::{AbiVariant, WasmSignature, WasmType},
    Enum, Expected, Flags, Function, FunctionKind, Interface, Record, Tuple, Type, TypeDef,
    TypeDefKind, Union, Variant,
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
                            && TypeKey {
                                interface: self.interface,
                                ty: c1.ty,
                            } == TypeKey {
                                interface: other.interface,
                                ty: c2.ty,
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
                (TypeDefKind::Expected(e1), TypeDefKind::Expected(e2)) => {
                    TypeKey {
                        interface: self.interface,
                        ty: e1.ok,
                    } == TypeKey {
                        interface: other.interface,
                        ty: e2.ok,
                    } && TypeKey {
                        interface: self.interface,
                        ty: e1.err,
                    } == TypeKey {
                        interface: other.interface,
                        ty: e2.err,
                    }
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
                for f in &r.flags {
                    f.name.hash(state);
                }
            }
            TypeDefKind::Variant(v) => {
                state.write_u8(3);
                for c in &v.cases {
                    c.name.hash(state);
                    TypeKey {
                        interface: self.interface,
                        ty: c.ty,
                    }
                    .hash(state);
                }
            }
            TypeDefKind::Enum(e) => {
                state.write_u8(4);
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
            TypeDefKind::Expected(e) => {
                state.write_u8(8);
                TypeKey {
                    interface: self.interface,
                    ty: e.ok,
                }
                .hash(state);
                TypeKey {
                    interface: self.interface,
                    ty: e.err,
                }
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

        self.func
            .params
            .iter()
            .zip(other.func.params.iter())
            .all(|((n1, t1), (n2, t2))| {
                n1 == n2
                    && TypeKey {
                        interface: self.interface,
                        ty: *t1,
                    }
                    .eq(&TypeKey {
                        interface: other.interface,
                        ty: *t2,
                    })
            })
            && TypeKey {
                interface: self.interface,
                ty: self.func.result,
            }
            .eq(&TypeKey {
                interface: other.interface,
                ty: other.func.result,
            })
    }
}

impl Eq for FunctionKey<'_> {}

impl Hash for FunctionKey<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.func.params.len().hash(state);
        for (name, ty) in &self.func.params {
            name.hash(state);
            TypeKey {
                interface: self.interface,
                ty: *ty,
            }
            .hash(state);
        }
        TypeKey {
            interface: self.interface,
            ty: self.func.result,
        }
        .hash(state);
    }
}

#[derive(Default)]
struct InstanceTypeEncoder<'a> {
    ty: InstanceType,
    aliased_types: IndexMap<ComponentTypeRef, ComponentTypeRef>,
    exported_types: IndexMap<&'a str, ComponentTypeRef>,
}

impl<'a> InstanceTypeEncoder<'a> {
    fn export(&mut self, name: &'a str, type_ref: ComponentTypeRef) -> Result<()> {
        match self.exported_types.entry(name) {
            Entry::Occupied(e) => {
                if *e.get() != type_ref {
                    bail!("duplicate export `{}`", name)
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(type_ref);
                let alias = self.alias_type(type_ref);
                self.ty.export(name, alias);
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
    exported_types: IndexMap<&'a str, ComponentTypeRef>,
}

impl<'a> TypeEncoder<'a> {
    fn finish(&self, component: &mut Component) {
        if !self.types.is_empty() {
            component.section(&self.types);
        }

        if !self.exports.is_empty() {
            component.section(&self.exports);
        }
    }

    fn encode_instance_imports(
        &mut self,
        interfaces: &'a [Interface],
        required_imports: &IndexSet<&'a str>,
        imports: &mut ImportEncoder<'a>,
    ) -> Result<()> {
        for import in interfaces {
            if !required_imports.contains(import.name.as_str()) {
                continue;
            }

            Self::validate_interface(import)?;

            let mut instance = InstanceTypeEncoder::default();

            for func in &import.functions {
                Self::validate_function(func)?;

                let index = self.encode_func_type(import, func, false)?;
                instance.export(&func.name, ComponentTypeRef::Func(index))?;
            }

            let index = self.encode_instance_type(&instance.ty);
            imports.import(import, ComponentTypeRef::Instance(index))?;
        }

        Ok(())
    }

    fn encode_func_types(
        &mut self,
        interfaces: impl Iterator<Item = (&'a Interface, bool)>,
        export_func_types: bool,
    ) -> Result<()> {
        for (export, is_default) in interfaces {
            Self::validate_interface(export)?;

            // TODO: stick interface documentation in a custom section?

            for func in &export.functions {
                Self::validate_function(func)?;

                let index = self.encode_func_type(export, func, is_default)?;

                if export_func_types {
                    self.export_type(&func.name, ComponentTypeRef::Func(index))?;
                }
            }
        }

        Ok(())
    }

    fn encode_instance_type(&mut self, ty: &InstanceType) -> u32 {
        let index = self.types.len();
        self.types.instance(ty);
        index
    }

    fn encode_func_type(
        &mut self,
        interface: &'a Interface,
        func: &'a Function,
        export_named_types: bool,
    ) -> Result<u32> {
        let key = FunctionKey { interface, func };
        if let Some(index) = self.func_type_map.get(&key) {
            return Ok(*index);
        }

        // Encode all referenced parameter types from this function.
        let params: Vec<_> = func
            .params
            .iter()
            .map(|(name, ty)| {
                Ok((
                    Some(name.as_str()),
                    self.encode_valtype(interface, ty, export_named_types)?,
                ))
            })
            .collect::<Result<_>>()?;
        let result = self.encode_valtype(interface, &func.result, export_named_types)?;

        // Encode the function type
        let index = self.types.len();
        self.types.function(params, result);
        self.func_type_map.insert(key, index);
        Ok(index)
    }

    fn encode_valtype(
        &mut self,
        interface: &'a Interface,
        ty: &Type,
        export_named_types: bool,
    ) -> Result<ComponentValType> {
        Ok(match ty {
            Type::Unit => ComponentValType::Primitive(PrimitiveValType::Unit),
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
                        TypeDefKind::Record(r) => {
                            self.encode_record(interface, r, export_named_types)?
                        }
                        TypeDefKind::Tuple(t) => {
                            self.encode_tuple(interface, t, export_named_types)?
                        }
                        TypeDefKind::Flags(r) => self.encode_flags(r)?,
                        TypeDefKind::Variant(v) => {
                            self.encode_variant(interface, v, export_named_types)?
                        }
                        TypeDefKind::Union(u) => {
                            self.encode_union(interface, u, export_named_types)?
                        }
                        TypeDefKind::Option(t) => {
                            self.encode_option(interface, t, export_named_types)?
                        }
                        TypeDefKind::Expected(e) => {
                            self.encode_expected(interface, e, export_named_types)?
                        }
                        TypeDefKind::Enum(e) => self.encode_enum(e)?,
                        TypeDefKind::List(ty) => {
                            let ty = self.encode_valtype(interface, ty, export_named_types)?;
                            let index = self.types.len();
                            let encoder = self.types.defined_type();
                            encoder.list(ty);
                            ComponentValType::Type(index)
                        }
                        TypeDefKind::Type(ty) => {
                            self.encode_valtype(interface, ty, export_named_types)?
                        }
                        TypeDefKind::Future(_) => todo!("encoding for future type"),
                        TypeDefKind::Stream(_) => todo!("encoding for stream type"),
                    };

                    if ty.name.is_some() {
                        if let ComponentValType::Primitive(ty) = encoded {
                            // Named primitive types need entries in the type section, so
                            // convert this to a type reference
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

                if export_named_types {
                    // Named types need to be exported
                    if let Some(name) = ty.name.as_deref() {
                        if let ComponentValType::Type(index) = encoded {
                            self.export_type(name, ComponentTypeRef::Type(TypeBounds::Eq, index))?;
                        }
                    }
                }

                encoded
            }
            Type::Handle(_) => {
                bail!("the use of handle types in interfaces is not currently supported")
            }
        })
    }

    fn encode_record(
        &mut self,
        interface: &'a Interface,
        record: &Record,
        export_named_types: bool,
    ) -> Result<ComponentValType> {
        let fields = record
            .fields
            .iter()
            .map(|f| {
                Ok((
                    f.name.as_str(),
                    self.encode_valtype(interface, &f.ty, export_named_types)?,
                ))
            })
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
        export_named_types: bool,
    ) -> Result<ComponentValType> {
        let tys = tuple
            .types
            .iter()
            .map(|ty| self.encode_valtype(interface, ty, export_named_types))
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
        export_named_types: bool,
    ) -> Result<ComponentValType> {
        let cases = variant
            .cases
            .iter()
            .map(|c| {
                Ok((
                    c.name.as_str(),
                    self.encode_valtype(interface, &c.ty, export_named_types)?,
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
        export_named_types: bool,
    ) -> Result<ComponentValType> {
        let tys = union
            .cases
            .iter()
            .map(|c| self.encode_valtype(interface, &c.ty, export_named_types))
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
        export_named_types: bool,
    ) -> Result<ComponentValType> {
        let ty = self.encode_valtype(interface, payload, export_named_types)?;
        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.option(ty);
        Ok(ComponentValType::Type(index))
    }

    fn encode_expected(
        &mut self,
        interface: &'a Interface,
        expected: &Expected,
        export_named_types: bool,
    ) -> Result<ComponentValType> {
        let ok = self.encode_valtype(interface, &expected.ok, export_named_types)?;
        let error = self.encode_valtype(interface, &expected.err, export_named_types)?;
        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.expected(ok, error);
        Ok(ComponentValType::Type(index))
    }

    fn encode_enum(&mut self, enum_: &Enum) -> Result<ComponentValType> {
        let index = self.types.len();
        let encoder = self.types.defined_type();
        encoder.enum_type(enum_.cases.iter().map(|c| c.name.as_str()));
        Ok(ComponentValType::Type(index))
    }

    fn export_type(&mut self, name: &'a str, type_ref: ComponentTypeRef) -> Result<()> {
        match self.exported_types.entry(name) {
            Entry::Occupied(e) => {
                if *e.get() != type_ref {
                    bail!("duplicate export `{}`", name)
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(type_ref);

                let index = match type_ref {
                    ComponentTypeRef::Module(index) => index,
                    ComponentTypeRef::Func(index) => index,
                    ComponentTypeRef::Value(ComponentValType::Primitive(_)) => unreachable!(),
                    ComponentTypeRef::Value(ComponentValType::Type(index)) => index,
                    ComponentTypeRef::Type(_, index) => index,
                    ComponentTypeRef::Instance(index) => index,
                    ComponentTypeRef::Component(index) => index,
                };

                self.exports.export(name, ComponentExportKind::Type, index);
            }
        }

        Ok(())
    }

    fn validate_interface(interface: &Interface) -> Result<()> {
        if interface.resources.len() != 0 {
            bail!("the use of resources in interfaces is not currently not supported");
        }

        Ok(())
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

        if function.is_async {
            bail!(
                "unsupported function `{}`: only synchronous functions are currently supported",
                function.name
            );
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RequiredOptions {
    // No required options.
    None,
    // Only the memory option is required.
    Memory,
    // The realloc option (and, by extension, the memory option) is required.
    Realloc,
    // The encoding, realloc, and memory options are required.
    All,
}

impl RequiredOptions {
    fn for_types<'a>(interface: &Interface, mut types: impl Iterator<Item = &'a Type>) -> Self {
        match types.try_fold(Self::None, |mut acc, ty| {
            acc |= Self::for_type(interface, ty);
            if acc == Self::All {
                // If something requires all the options, then we're done searching.
                // Returning an error here so that the operation terminates early.
                Err(acc)
            } else {
                Ok(acc)
            }
        }) {
            Ok(o) | Err(o) => o,
        }
    }

    fn for_type(interface: &Interface, ty: &Type) -> Self {
        match ty {
            Type::Id(id) => match &interface.types[*id].kind {
                TypeDefKind::Record(r) => {
                    Self::for_types(interface, r.fields.iter().map(|f| &f.ty))
                }
                TypeDefKind::Tuple(t) => Self::for_types(interface, t.types.iter()),
                TypeDefKind::Flags(_) => Self::None,
                TypeDefKind::Option(t) => Self::for_type(interface, t),
                TypeDefKind::Expected(e) => {
                    Self::for_type(interface, &e.ok) | Self::for_type(interface, &e.err)
                }
                TypeDefKind::Variant(v) => {
                    Self::for_types(interface, v.cases.iter().map(|c| &c.ty))
                }
                TypeDefKind::Union(v) => Self::for_types(interface, v.cases.iter().map(|c| &c.ty)),
                TypeDefKind::Enum(_) => Self::None,
                TypeDefKind::List(t) => {
                    // Lists need at least the `realloc` option, but may require
                    // the encoding option if there's a string somewhere in the
                    // type.
                    Self::for_type(interface, t) | Self::Realloc
                }
                TypeDefKind::Type(t) => Self::for_type(interface, t),
                TypeDefKind::Future(_) => todo!("encoding for future"),
                TypeDefKind::Stream(_) => todo!("encoding for stream"),
            },
            Type::String => Self::All,
            _ => Self::None,
        }
    }

    fn for_function(interface: &Interface, function: &Function) -> Self {
        Self::for_types(
            interface,
            function
                .params
                .iter()
                .map(|(_, ty)| ty)
                .chain([&function.result]),
        )
    }

    fn into_iter(
        self,
        encoding: StringEncoding,
        memory_index: Option<u32>,
        realloc_index: Option<u32>,
    ) -> Result<impl Iterator<Item = CanonicalOption> + ExactSizeIterator> {
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

        if self == RequiredOptions::None {
            return Ok(iter);
        }

        iter.push(CanonicalOption::Memory(memory_index.ok_or_else(|| {
            anyhow!("module does not export a memory named `memory`")
        })?));

        if self == RequiredOptions::Memory {
            return Ok(iter);
        }

        iter.push(CanonicalOption::Realloc(realloc_index.ok_or_else(
            || anyhow!("module does not export a function named `canonical_abi_realloc`"),
        )?));

        if self == RequiredOptions::Realloc {
            return Ok(iter);
        }

        assert_eq!(self, RequiredOptions::All);

        iter.push(encoding.into());
        Ok(iter)
    }
}

impl BitOrAssign for RequiredOptions {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitOr for RequiredOptions {
    type Output = RequiredOptions;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::All, _) | (_, Self::All) => Self::All,
            (Self::Realloc, _) | (_, Self::Realloc) => Self::Realloc,
            (Self::Memory, _) | (_, Self::Memory) => Self::Memory,
            (Self::None, Self::None) => Self::None,
        }
    }
}

/// State relating to encoding a component.
#[derive(Default)]
struct EncodingState {
    /// The component being encoded.
    component: Component,
    /// The various index spaces used in the encoding.
    indexes: Indexes,
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
}

impl EncodingState {
    fn encode_core_module(&mut self, module: &[u8]) -> u32 {
        *self.module_index.get_or_insert_with(|| {
            self.component.section(&wasm_encoder::RawSection {
                id: ComponentSectionId::CoreModule.into(),
                data: module,
            });

            self.indexes.alloc_core_module()
        })
    }

    fn encode_core_instantiation(
        &mut self,
        encoding: StringEncoding,
        imports: &ImportEncoder,
        has_memory: bool,
        has_realloc: bool,
    ) -> Result<()> {
        if imports.map.is_empty() {
            self.instantiate_core_module([], has_memory, has_realloc);
            return Ok(());
        }

        // Encode a shim instantiation if needed
        self.encode_shim_instantiation(imports);

        let mut instances = InstanceSection::new();
        let args: Vec<_> = imports
            .map
            .iter()
            .enumerate()
            .map(|(instance_index, (name, import))| {
                let mut exports = Vec::with_capacity(import.direct.len() + import.indirect.len());

                let mut core_aliases = AliasSection::new();
                for lowering in &import.indirect {
                    let index = self.alias_core_item(
                        &mut core_aliases,
                        self.shim_instance_index
                            .expect("shim should be instantiated"),
                        ExportKind::Func,
                        &lowering.export_name,
                    );
                    exports.push((lowering.name, ExportKind::Func, index));
                }
                self.component.section(&core_aliases);

                let mut aliases = ComponentAliasSection::new();
                let mut functions = CanonicalFunctionSection::new();

                for lowering in &import.direct {
                    let func_index =
                        self.alias_func(&mut aliases, instance_index as u32, lowering.name);
                    let core_func_index = self.lower_func(&mut functions, func_index, []);
                    exports.push((lowering.name, ExportKind::Func, core_func_index));
                }

                self.component.section(&aliases);
                self.component.section(&functions);

                let index = self.instantiate_core_exports(&mut instances, exports);
                (*name, ModuleArg::Instance(index))
            })
            .collect();

        self.component.section(&instances);

        self.instantiate_core_module(args, has_memory, has_realloc);
        self.encode_indirect_lowerings(encoding, imports)
    }

    fn encode_imports(&mut self, imports: &ImportEncoder) {
        let mut section = ComponentImportSection::default();

        for (name, import) in &imports.map {
            section.import(name, import.ty);
            self.indexes.alloc_instance();
        }

        if !section.is_empty() {
            self.component.section(&section);
        }
    }

    fn encode_exports<'a>(
        &mut self,
        encoding: StringEncoding,
        exports: impl Iterator<Item = (&'a Interface, bool)>,
        func_types: &IndexMap<FunctionKey<'a>, u32>,
    ) -> Result<()> {
        let core_instance_index = self.instance_index.expect("must be instantiated");

        let mut section = ComponentExportSection::default();
        let mut instances = ComponentInstanceSection::new();

        for (export, is_default) in exports {
            // Alias the exports from the core module
            let mut aliases = AliasSection::new();
            let mut functions = CanonicalFunctionSection::new();
            let mut interface_exports = Vec::new();
            for func in &export.functions {
                let name =
                    expected_export_name((!is_default).then(|| export.name.as_str()), &func.name);

                let core_func_index = self.alias_core_item(
                    &mut aliases,
                    core_instance_index,
                    ExportKind::Func,
                    name.as_ref(),
                );

                let ty = *func_types
                    .get(&FunctionKey {
                        interface: export,
                        func,
                    })
                    .expect("the type should be encoded");

                let sig = export.wasm_signature(AbiVariant::GuestExport, func);
                let options = RequiredOptions::for_function(export, func)
                    | (if sig.retptr || sig.indirect_params {
                        RequiredOptions::Memory
                    } else {
                        RequiredOptions::None
                    });

                // TODO: support the post-return option somehow (not yet supported in wit-bindgen)
                let func_index = self.lift_func(
                    &mut functions,
                    core_func_index,
                    ty,
                    options.into_iter(encoding, self.memory_index, self.realloc_index)?,
                );

                if is_default {
                    // Directly export the lifted function
                    section.export(&func.name, ComponentExportKind::Func, func_index);
                } else {
                    // Otherwise, add it to the list for later instantiation
                    interface_exports.push((
                        func.name.as_str(),
                        ComponentExportKind::Func,
                        func_index,
                    ));
                }
            }

            self.component.section(&aliases);
            self.component.section(&functions);

            if !interface_exports.is_empty() {
                if export.name.is_empty() {
                    bail!("cannot export an unnamed interface");
                }

                let instance_index = self.instantiate_exports(&mut instances, interface_exports);
                section.export(&export.name, ComponentExportKind::Instance, instance_index);
            }
        }

        if !instances.is_empty() {
            self.component.section(&instances);
        }

        if !section.is_empty() {
            self.component.section(&section);
        }

        Ok(())
    }

    fn encode_shim_instantiation(&mut self, imports: &ImportEncoder) {
        if imports.indirect_count == 0 {
            return;
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

        let mut func_index = 0;
        for import in imports.map.values() {
            for lowering in &import.indirect {
                let type_index = *sigs.entry(&lowering.sig).or_insert_with(|| {
                    let index = types.len();
                    types.function(
                        lowering.sig.params.iter().map(to_val_type),
                        lowering.sig.results.iter().map(to_val_type),
                    );
                    index
                });

                functions.function(type_index);
                Self::encode_shim_function(
                    type_index,
                    func_index,
                    &mut code,
                    lowering.sig.params.len() as u32,
                );
                exports.export(&lowering.export_name, ExportKind::Func, func_index);

                imports_section.import("", &lowering.export_name, EntityType::Function(type_index));
                func_indexes.push(func_index);

                func_index += 1;
            }
        }

        let table_type = TableType {
            element_type: ValType::FuncRef,
            minimum: func_index,
            maximum: Some(func_index),
        };

        tables.table(table_type);

        exports.export(INDIRECT_TABLE_NAME, ExportKind::Table, 0);
        imports_section.import("", INDIRECT_TABLE_NAME, table_type);

        elements.active(
            None,
            &Instruction::I32Const(0),
            ValType::FuncRef,
            Elements::Functions(&func_indexes),
        );

        let mut shim = Module::new();
        shim.section(&types);
        shim.section(&functions);
        shim.section(&tables);
        shim.section(&exports);
        shim.section(&code);

        let mut fixups = Module::default();
        fixups.section(&types);
        fixups.section(&imports_section);
        fixups.section(&elements);

        let shim_module_index = self.indexes.alloc_core_module();
        self.component.section(&ModuleSection(&shim));

        self.fixups_module_index = Some(self.indexes.alloc_core_module());
        self.component.section(&ModuleSection(&fixups));

        let mut instances = InstanceSection::default();
        self.shim_instance_index = Some(self.instantiate(&mut instances, shim_module_index, []));
        self.component.section(&instances);
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
        encoding: StringEncoding,
        imports: &ImportEncoder,
    ) -> Result<()> {
        if imports.indirect_count == 0 {
            return Ok(());
        }

        let shim_instance_index = self
            .shim_instance_index
            .expect("must have an instantiated shim");

        let mut core_aliases = AliasSection::new();
        let table_index = self.alias_core_item(
            &mut core_aliases,
            shim_instance_index,
            ExportKind::Table,
            INDIRECT_TABLE_NAME,
        );
        self.component.section(&core_aliases);

        let mut exports = Vec::with_capacity(imports.indirect_count as usize);
        exports.push((INDIRECT_TABLE_NAME, ExportKind::Table, table_index));

        let mut aliases = ComponentAliasSection::new();
        let mut functions = CanonicalFunctionSection::new();
        for (instance_index, import) in imports.map.values().enumerate() {
            for lowering in &import.indirect {
                let func_index =
                    self.alias_func(&mut aliases, instance_index as u32, lowering.name);

                let core_func_index = self.lower_func(
                    &mut functions,
                    func_index,
                    lowering
                        .options
                        .into_iter(encoding, self.memory_index, self.realloc_index)?,
                );

                exports.push((
                    lowering.export_name.as_str(),
                    ExportKind::Func,
                    core_func_index,
                ));
            }
        }

        self.component.section(&aliases);
        self.component.section(&functions);

        let mut instances = InstanceSection::new();
        let instance_index = self.instantiate_core_exports(&mut instances, exports);
        self.instantiate(
            &mut instances,
            self.fixups_module_index.expect("must have fixup module"),
            [("", ModuleArg::Instance(instance_index))],
        );
        self.component.section(&instances);
        Ok(())
    }

    fn instantiate_core_module<'a, A>(&mut self, args: A, has_memory: bool, has_realloc: bool)
    where
        A: IntoIterator<Item = (&'a str, ModuleArg)>,
        A::IntoIter: ExactSizeIterator,
    {
        assert!(self.instance_index.is_none());

        let mut instances = InstanceSection::new();
        let mut aliases = AliasSection::new();

        let instance_index = self.instantiate(
            &mut instances,
            self.module_index.expect("core module encoded"),
            args,
        );

        if has_memory {
            self.memory_index = Some(self.alias_core_item(
                &mut aliases,
                instance_index,
                ExportKind::Memory,
                "memory",
            ));
        }

        if has_realloc {
            self.realloc_index = Some(self.alias_core_item(
                &mut aliases,
                instance_index,
                ExportKind::Func,
                "canonical_abi_realloc",
            ));
        }

        self.component.section(&instances);
        self.component.section(&aliases);

        self.instance_index = Some(instance_index);
    }

    fn instantiate<'a, A>(
        &mut self,
        instances: &mut InstanceSection,
        module_index: u32,
        args: A,
    ) -> u32
    where
        A: IntoIterator<Item = (&'a str, ModuleArg)>,
        A::IntoIter: ExactSizeIterator,
    {
        instances.instantiate(module_index, args);
        self.indexes.alloc_core_instance()
    }

    fn alias_core_item(
        &mut self,
        aliases: &mut AliasSection,
        instance: u32,
        kind: ExportKind,
        name: &str,
    ) -> u32 {
        aliases.instance_export(instance, kind, name);
        match kind {
            ExportKind::Func => self.indexes.alloc_core_func(),
            ExportKind::Table => self.indexes.alloc_core_table(),
            ExportKind::Memory => self.indexes.alloc_core_memory(),
            ExportKind::Global | ExportKind::Tag => unreachable!(),
        }
    }

    fn alias_func(
        &mut self,
        aliases: &mut ComponentAliasSection,
        instance: u32,
        name: &str,
    ) -> u32 {
        aliases.instance_export(instance, ComponentExportKind::Func, name);
        self.indexes.alloc_func()
    }

    fn lower_func<O>(
        &mut self,
        functions: &mut CanonicalFunctionSection,
        func_index: u32,
        options: O,
    ) -> u32
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        functions.lower(func_index, options);
        self.indexes.alloc_core_func()
    }

    fn lift_func<O>(
        &mut self,
        functions: &mut CanonicalFunctionSection,
        core_func_index: u32,
        type_index: u32,
        options: O,
    ) -> u32
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        functions.lift(core_func_index, type_index, options);
        self.indexes.alloc_func()
    }

    fn instantiate_core_exports<'a, E>(
        &mut self,
        instances: &mut InstanceSection,
        exports: E,
    ) -> u32
    where
        E: IntoIterator<Item = (&'a str, ExportKind, u32)>,
        E::IntoIter: ExactSizeIterator,
    {
        instances.export_items(exports);
        self.indexes.alloc_core_instance()
    }

    fn instantiate_exports<'a, E>(
        &mut self,
        instances: &mut ComponentInstanceSection,
        exports: E,
    ) -> u32
    where
        E: IntoIterator<Item = (&'a str, ComponentExportKind, u32)>,
        E::IntoIter: ExactSizeIterator,
    {
        instances.export_items(exports);
        self.indexes.alloc_instance()
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
    export_name: String,
}

#[derive(Debug)]
struct ImportedInterface<'a> {
    ty: ComponentTypeRef,
    direct: Vec<DirectLowering<'a>>,
    indirect: Vec<IndirectLowering<'a>>,
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
#[derive(Debug, Default)]
struct ImportEncoder<'a> {
    map: IndexMap<&'a str, ImportedInterface<'a>>,
    direct_count: u32,
    indirect_count: u32,
}

impl<'a> ImportEncoder<'a> {
    fn import(&mut self, interface: &'a Interface, ty: ComponentTypeRef) -> Result<()> {
        match self.map.entry(&interface.name) {
            indexmap::map::Entry::Occupied(e) => {
                if e.get().ty != ty {
                    bail!("duplicate import `{}`", interface.name)
                }
            }
            indexmap::map::Entry::Vacant(e) => {
                let mut direct = Vec::new();
                let mut indirect = Vec::new();
                for f in &interface.functions {
                    let sig = interface.wasm_signature(AbiVariant::GuestImport, f);
                    let options = RequiredOptions::for_function(interface, f)
                        | (if sig.retptr || sig.indirect_params {
                            RequiredOptions::Memory
                        } else {
                            RequiredOptions::None
                        });

                    match options {
                        RequiredOptions::All
                        | RequiredOptions::Realloc
                        | RequiredOptions::Memory => {
                            let element_index = self.indirect_count;
                            self.indirect_count += 1;
                            indirect.push(IndirectLowering {
                                name: &f.name,
                                sig,
                                options,
                                export_name: element_index.to_string(),
                            });
                        }
                        RequiredOptions::None => {
                            self.direct_count += 1;
                            direct.push(DirectLowering { name: &f.name });
                        }
                    }
                }

                e.insert(ImportedInterface {
                    ty,
                    direct,
                    indirect,
                });
            }
        }

        Ok(())
    }
}

#[derive(Default)]
struct Indexes {
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

impl Indexes {
    fn alloc_core_module(&mut self) -> u32 {
        let index = self.core_modules;
        self.core_modules += 1;
        index
    }

    fn alloc_core_func(&mut self) -> u32 {
        let index = self.core_funcs;
        self.core_funcs += 1;
        index
    }

    fn alloc_core_memory(&mut self) -> u32 {
        let index = self.core_memories;
        self.core_memories += 1;
        index
    }

    fn alloc_core_table(&mut self) -> u32 {
        let index = self.core_tables;
        self.core_tables += 1;
        index
    }

    fn alloc_core_instance(&mut self) -> u32 {
        let index = self.core_instances;
        self.core_instances += 1;
        index
    }

    fn alloc_func(&mut self) -> u32 {
        let index = self.funcs;
        self.funcs += 1;
        index
    }

    fn alloc_instance(&mut self) -> u32 {
        let index = self.instances;
        self.instances += 1;
        index
    }
}

/// An encoder of components based on `wit` interface definitions.
#[derive(Default)]
pub struct ComponentEncoder<'a> {
    module: &'a [u8],
    encoding: StringEncoding,
    interface: Option<&'a Interface>,
    imports: &'a [Interface],
    exports: &'a [Interface],
    validate: bool,
    types_only: bool,
}

impl<'a> ComponentEncoder<'a> {
    /// Set the core module to encode as a component.
    pub fn module(mut self, module: &'a [u8]) -> Self {
        self.module = module;
        self
    }

    /// Set the string encoding expected by the core module.
    pub fn encoding(mut self, encoding: StringEncoding) -> Self {
        self.encoding = encoding;
        self
    }

    /// Sets whether or not the encoder will validate its output.
    pub fn validate(mut self, validate: bool) -> Self {
        self.validate = validate;
        self
    }

    /// Set the default interface exported by the component.
    pub fn interface(mut self, interface: &'a Interface) -> Self {
        self.interface = Some(interface);
        self
    }

    /// Set the interfaces the component imports.
    pub fn imports(mut self, imports: &'a [Interface]) -> Self {
        self.imports = imports;
        self
    }

    /// Set the interfaces the component exports.
    pub fn exports(mut self, exports: &'a [Interface]) -> Self {
        self.exports = exports;
        self
    }

    /// Encode the component and return the bytes.
    pub fn encode(&self) -> Result<Vec<u8>> {
        let (required_imports, has_memory, has_realloc) = if !self.module.is_empty() {
            validate_module(self.module, &self.interface, self.imports, self.exports)?
        } else {
            (Default::default(), false, false)
        };

        let exports = self
            .interface
            .iter()
            .copied()
            .map(|i| (i, true))
            .chain(self.exports.iter().map(|i| (i, false)));

        let mut state = EncodingState::default();
        let mut types = TypeEncoder::default();
        let mut imports = ImportEncoder::default();
        types.encode_func_types(exports.clone(), false)?;
        types.encode_instance_imports(self.imports, &required_imports, &mut imports)?;
        types.finish(&mut state.component);

        if self.types_only {
            if !self.module.is_empty() {
                bail!("a module cannot be specified for a types-only encoding");
            }
        } else {
            if self.module.is_empty() {
                bail!("a module is required when encoding a component");
            }

            state.encode_imports(&imports);
            state.encode_core_module(self.module);
            state.encode_core_instantiation(self.encoding, &imports, has_memory, has_realloc)?;
            state.encode_exports(self.encoding, exports, &types.func_type_map)?;
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

/// An encoder for a single interface definition.
///
/// The resulting component will only encode the type information
/// of the interface.
pub struct InterfaceEncoder<'a> {
    interface: &'a Interface,
    validate: bool,
}

impl<'a> InterfaceEncoder<'a> {
    /// Create a new encoder for the given interface.
    pub fn new(interface: &'a Interface) -> Self {
        Self {
            interface,
            validate: false,
        }
    }

    /// Sets whether or not the encoder will validate its output.
    pub fn validate(mut self, validate: bool) -> Self {
        self.validate = validate;
        self
    }

    /// Encode the interface as a component and return the bytes.
    pub fn encode(&self) -> Result<Vec<u8>> {
        let mut component = Component::default();

        let mut types = TypeEncoder::default();
        types.encode_func_types([(self.interface, true)].into_iter(), true)?;
        types.finish(&mut component);

        let bytes = component.finish();

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
