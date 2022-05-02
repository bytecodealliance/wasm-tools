use crate::{
    validation::{expected_export_name, validate_module},
    StringEncoding,
};
use anyhow::{bail, Context, Result};
use indexmap::IndexMap;
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::{Hash, Hasher},
    ops::BitOr,
};
use wasm_encoder::*;
use wasmparser::{Validator, WasmFeatures};
use wit_parser::{
    abi::{AbiVariant, WasmSignature, WasmType},
    Flags, Function, FunctionKind, Interface, Record, Tuple, Type, TypeDef, TypeDefKind, Variant,
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
                            && c1
                                .ty
                                .map(|ty| TypeKey {
                                    interface: self.interface,
                                    ty,
                                })
                                .eq(&c2.ty.map(|ty| TypeKey {
                                    interface: other.interface,
                                    ty,
                                }))
                    })
                }
                (TypeDefKind::List(t1), TypeDefKind::List(t2))
                | (TypeDefKind::Type(t1), TypeDefKind::Type(t2)) => TypeKey {
                    interface: self.interface,
                    ty: *t1,
                }
                .eq(&TypeKey {
                    interface: other.interface,
                    ty: *t2,
                }),
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
                    c.ty.map(|ty| TypeKey {
                        interface: self.interface,
                        ty,
                    })
                    .hash(state);
                }
            }
            TypeDefKind::List(ty) => {
                state.write_u8(4);
                TypeKey {
                    interface: self.interface,
                    ty: *ty,
                }
                .hash(state);
            }
            TypeDefKind::Type(ty) => {
                state.write_u8(5);
                TypeKey {
                    interface: self.interface,
                    ty: *ty,
                }
                .hash(state);
            }
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
    aliases: HashMap<u32, u32>,
    exports: HashMap<&'a str, u32>,
}

impl<'a> InstanceTypeEncoder<'a> {
    fn export_type(&mut self, name: &'a str, index: u32) -> Result<()> {
        match self.exports.entry(name) {
            Entry::Occupied(e) => {
                if *e.get() != index {
                    bail!("duplicate export `{}`", name)
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(index);
                let index = self.alias_type(index);
                self.ty.export(name, index);
            }
        }

        Ok(())
    }

    fn alias_type(&mut self, index: u32) -> u32 {
        match self.aliases.entry(index) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let alias = self.ty.type_count();
                e.insert(alias);
                self.ty.alias_outer_type(1, index);
                alias
            }
        }
    }
}

#[derive(Default)]
struct TypeEncoder<'a> {
    types: ComponentTypeSection,
    type_map: HashMap<TypeDefKey<'a>, u32>,
    func_type_map: HashMap<FunctionKey<'a>, u32>,
    exports: ComponentExportSection,
    exported: HashMap<&'a str, u32>,
}

impl<'a> TypeEncoder<'a> {
    fn encode_instance_type(&mut self, ty: &InstanceType) -> u32 {
        let index = self.types.len();
        self.types.instance(ty);
        index
    }

    fn encode_func_type(
        &mut self,
        interface: &'a Interface,
        func: &'a Function,
        instance: &mut Option<InstanceTypeEncoder<'a>>,
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
                    self.encode_type(interface, instance, ty)?,
                ))
            })
            .collect::<Result<_>>()?;
        let result = self.encode_type(interface, instance, &func.result)?;

        // Encode the function type
        let index = self.types.len();
        self.types.function(params, result);
        self.func_type_map.insert(key, index);
        Ok(index)
    }

    fn encode_type(
        &mut self,
        interface: &'a Interface,
        instance: &mut Option<InstanceTypeEncoder<'a>>,
        ty: &Type,
    ) -> Result<InterfaceTypeRef> {
        Ok(match ty {
            Type::Unit => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit),
            Type::Bool => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Bool),
            Type::U8 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U8),
            Type::U16 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U16),
            Type::U32 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U32),
            Type::U64 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U64),
            Type::S8 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S8),
            Type::S16 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S16),
            Type::S32 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S32),
            Type::S64 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S64),
            Type::Float32 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Float32),
            Type::Float64 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Float64),
            Type::Char => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Char),
            Type::String => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::String),
            Type::Id(id) => {
                let ty = &interface.types[*id];
                let key = TypeDefKey::new(interface, &interface.types[*id]);
                let encoded = if let Some(index) = self.type_map.get(&key) {
                    InterfaceTypeRef::Type(*index)
                } else {
                    let mut encoded = match &ty.kind {
                        TypeDefKind::Record(r) => self.encode_record(interface, instance, r)?,
                        TypeDefKind::Tuple(t) => self.encode_tuple(interface, instance, t)?,
                        TypeDefKind::Flags(r) => self.encode_flags(r)?,
                        TypeDefKind::Variant(v) => self.encode_variant(interface, instance, v)?,
                        TypeDefKind::List(ty) => {
                            let ty = self.encode_type(interface, instance, ty)?;
                            let index = self.types.len();
                            let encoder = self.types.interface_type();
                            encoder.list(ty);
                            InterfaceTypeRef::Type(index)
                        }
                        TypeDefKind::Type(ty) => self.encode_type(interface, instance, ty)?,
                    };

                    if ty.name.is_some() {
                        if let InterfaceTypeRef::Primitive(ty) = encoded {
                            // Named primitive types need entries in the type section
                            let index = self.types.len();
                            self.types.interface_type().primitive(ty);
                            encoded = InterfaceTypeRef::Type(index);
                        }
                    }

                    if let InterfaceTypeRef::Type(index) = encoded {
                        self.type_map.insert(key, index);
                    }

                    encoded
                };

                if let Some(name) = ty.name.as_deref() {
                    if let InterfaceTypeRef::Type(index) = encoded {
                        self.export_type(instance, name, index)?;
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
        instance: &mut Option<InstanceTypeEncoder<'a>>,
        record: &Record,
    ) -> Result<InterfaceTypeRef> {
        let fields = record
            .fields
            .iter()
            .map(|f| {
                Ok((
                    f.name.as_str(),
                    self.encode_type(interface, instance, &f.ty)?,
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        let index = self.types.len();
        let encoder = self.types.interface_type();
        encoder.record(fields);
        Ok(InterfaceTypeRef::Type(index))
    }

    fn encode_tuple(
        &mut self,
        interface: &'a Interface,
        instance: &mut Option<InstanceTypeEncoder<'a>>,
        tuple: &Tuple,
    ) -> Result<InterfaceTypeRef> {
        let tys = tuple
            .types
            .iter()
            .map(|ty| self.encode_type(interface, instance, ty))
            .collect::<Result<Vec<_>>>()?;
        let index = self.types.len();
        let encoder = self.types.interface_type();
        encoder.tuple(tys);
        Ok(InterfaceTypeRef::Type(index))
    }

    fn encode_flags(&mut self, flags: &Flags) -> Result<InterfaceTypeRef> {
        let index = self.types.len();
        let encoder = self.types.interface_type();
        encoder.flags(flags.flags.iter().map(|f| f.name.as_str()));
        Ok(InterfaceTypeRef::Type(index))
    }

    fn encode_variant(
        &mut self,
        interface: &'a Interface,
        instance: &mut Option<InstanceTypeEncoder<'a>>,
        variant: &Variant,
    ) -> Result<InterfaceTypeRef> {
        if let Some(ty) = variant.as_option() {
            let ty = self.encode_type(interface, instance, ty)?;
            let index = self.types.len();
            let encoder = self.types.interface_type();
            encoder.option(ty);
            return Ok(InterfaceTypeRef::Type(index));
        }

        if let Some((ok, error)) = variant.as_expected() {
            let ok = ok
                .map(|ty| self.encode_type(interface, instance, ty))
                .transpose()?
                .unwrap_or(InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit));
            let error = error
                .map(|ty| self.encode_type(interface, instance, ty))
                .transpose()?
                .unwrap_or(InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit));
            let index = self.types.len();
            let encoder = self.types.interface_type();
            encoder.expected(ok, error);
            return Ok(InterfaceTypeRef::Type(index));
        }

        if variant.is_enum() {
            let index = self.types.len();
            let encoder = self.types.interface_type();
            encoder.enum_type(variant.cases.iter().map(|c| c.name.as_str()));
            return Ok(InterfaceTypeRef::Type(index));
        }

        if variant.is_union() {
            let tys = variant
                .cases
                .iter()
                .map(|c| self.encode_type(interface, instance, c.ty.as_ref().unwrap()))
                .collect::<Result<Vec<_>>>()?;

            let index = self.types.len();
            let encoder = self.types.interface_type();
            encoder.union(tys);
            return Ok(InterfaceTypeRef::Type(index));
        }

        let cases = variant
            .cases
            .iter()
            .map(|c| {
                Ok((
                    c.name.as_str(),
                    match c.ty.as_ref() {
                        Some(ty) => self.encode_type(interface, instance, ty)?,
                        None => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit),
                    },
                    None, // TODO: support defaulting case values in the future
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        let index = self.types.len();
        let encoder = self.types.interface_type();
        encoder.variant(cases);
        Ok(InterfaceTypeRef::Type(index))
    }

    fn export_type(
        &mut self,
        instance_ty: &mut Option<InstanceTypeEncoder<'a>>,
        name: &'a str,
        index: u32,
    ) -> Result<()> {
        if let Some(instance) = instance_ty.as_mut() {
            return instance.export_type(name, index);
        }

        match self.exported.entry(name) {
            Entry::Occupied(e) => {
                if *e.get() != index {
                    bail!("duplicate export `{}`", name)
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(index);
                self.exports.export(name, ComponentExport::Type(index));
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RequiredOptions {
    // No required options.
    None,
    // The encoding and into options are required.
    Encoding,
    // Only the into option is required.
    Into,
}

impl RequiredOptions {
    fn for_types<'a>(interface: &Interface, mut types: impl Iterator<Item = &'a Type>) -> Self {
        match types.try_fold(Self::None, |acc, ty| {
            match Self::for_type(interface, ty) {
                Self::None => {
                    // Don't update the accumulator
                    Ok(acc)
                }
                Self::Encoding => {
                    // If something requires the encoding option, we're done searching
                    // Returning an error here so that the operation terminates early.
                    Err(Self::Encoding)
                }
                Self::Into => {
                    // Otherwise, update the accumulator (keep looking in case encoding is required)
                    Ok(Self::Into)
                }
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
                TypeDefKind::Variant(v) => {
                    Self::for_types(interface, v.cases.iter().filter_map(|c| c.ty.as_ref()))
                }
                TypeDefKind::List(t) => {
                    // Lists need at least the `into` option, but may require
                    // the encoding option if there's a string somewhere in the
                    // type.
                    Self::for_type(interface, t) | Self::Into
                }
                TypeDefKind::Type(t) => Self::for_type(interface, t),
            },
            Type::String => Self::Encoding,
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
}

impl BitOr for RequiredOptions {
    type Output = RequiredOptions;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Encoding, _) | (_, Self::Encoding) => Self::Encoding,
            (Self::Into, _) | (_, Self::Into) => Self::Into,
            _ => Self::None,
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
    export_name: String,
}

#[derive(Debug)]
enum Lowering<'a> {
    Direct(DirectLowering<'a>),
    Indirect(IndirectLowering<'a>),
}

#[derive(Debug)]
struct ImportedInterface<'a> {
    ty: u32,
    lowerings: Vec<Lowering<'a>>,
    // Stores indexes into `lowerings` that are indirect.
    indirect_lowerings: Vec<usize>,
}

/// The import encoder handles indirect lowering of any imports
/// that require the `into` option specified.
///
/// Lowering of such imports is done through a shim module that
/// defines a table of functions and exports functions that indirectly
/// call through the table.
///
/// Another module is responsible for "fixing-up" the table of functions
/// once the functions have been lowered (after the core module is instantiated)
///
/// If a lowering does not require the `into` option, the import is lowered before
/// the core module is instantiated and passed directly as an instantiation argument.
#[derive(Debug, Default)]
struct ImportEncoder<'a> {
    imports: IndexMap<&'a str, ImportedInterface<'a>>,
    direct_count: u32,
    indirect_count: u32,
}

impl<'a> ImportEncoder<'a> {
    fn import(&mut self, interface: &'a Interface, ty: u32) -> Result<()> {
        match self.imports.entry(&interface.name) {
            indexmap::map::Entry::Occupied(e) => {
                if e.get().ty != ty {
                    bail!("duplicate import `{}`", interface.name)
                }
            }
            indexmap::map::Entry::Vacant(e) => {
                let mut indirect_lowerings = Vec::new();
                let lowerings = interface
                    .functions
                    .iter()
                    .enumerate()
                    .map(|(i, f)| {
                        let sig = interface.wasm_signature(AbiVariant::GuestImport, f);
                        let options = RequiredOptions::for_function(interface, f)
                            | if sig.retptr {
                                RequiredOptions::Into
                            } else {
                                RequiredOptions::None
                            };

                        match options {
                            RequiredOptions::Encoding | RequiredOptions::Into => {
                                let table_index = self.indirect_count;
                                self.indirect_count += 1;
                                indirect_lowerings.push(i);
                                Lowering::Indirect(IndirectLowering {
                                    name: &f.name,
                                    sig,
                                    options,
                                    export_name: table_index.to_string(),
                                })
                            }
                            RequiredOptions::None => {
                                self.direct_count += 1;
                                Lowering::Direct(DirectLowering { name: &f.name })
                            }
                        }
                    })
                    .collect();

                e.insert(ImportedInterface {
                    ty,
                    lowerings,
                    indirect_lowerings,
                });
            }
        }

        Ok(())
    }

    fn len(&self) -> u32 {
        self.imports.len() as u32
    }

    fn is_empty(&self) -> bool {
        self.imports.is_empty()
    }

    fn encode_imports(&self, component: &mut Component) {
        let mut imports = ComponentImportSection::default();

        for (name, import) in &self.imports {
            imports.import(name, import.ty);
        }

        component.section(&imports);
    }

    fn create_shim_modules(&self) -> Option<(Module, Module)> {
        if self.indirect_count == 0 {
            return None;
        }

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
        let mut sigs = HashMap::new();
        let mut imports = ImportSection::new();
        let mut elements = ElementSection::new();
        let mut func_indexes = Vec::new();

        let mut func_index = 0;
        for import in self.imports.values() {
            for i in &import.indirect_lowerings {
                let lowering = match &import.lowerings[*i] {
                    Lowering::Indirect(lowering) => lowering,
                    Lowering::Direct(_) => unreachable!(),
                };

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
                exports.export(&lowering.export_name, Export::Function(func_index));

                imports.import("", &lowering.export_name, EntityType::Function(type_index));
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

        exports.export(INDIRECT_TABLE_NAME, Export::Table(0));
        imports.import("", INDIRECT_TABLE_NAME, table_type);

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
        fixups.section(&imports);
        fixups.section(&elements);

        Some((shim, fixups))
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

    fn encode_core_instantiation(
        &self,
        component: &mut Component,
        shim_instance_index: Option<u32>,
        core_module_index: u32,
        instance_count: &mut u32,
        function_count: &mut u32,
    ) -> u32 {
        let mut aliases = AliasSection::new();
        let mut functions = ComponentFunctionSection::new();
        let mut instances = InstanceSection::new();

        let mut direct_count = 0;
        let direct_start_index = *function_count + self.direct_count + self.indirect_count;

        let args: Vec<_> = self
            .imports
            .iter()
            .enumerate()
            .map(|(instance_index, (name, import))| {
                let alias_start_index = *function_count + aliases.len();

                let exports = import.lowerings.iter().enumerate().map(|(i, lowering)| {
                    let (name, func_index) = match lowering {
                        Lowering::Direct(lowering) => {
                            aliases.instance_export(
                                instance_index as u32,
                                AliasExportKind::ComponentFunction,
                                lowering.name,
                            );

                            functions.lower(alias_start_index + i as u32, []);

                            let i = direct_count;
                            direct_count += 1;

                            (lowering.name, direct_start_index + i as u32)
                        }
                        Lowering::Indirect(lowering) => {
                            aliases.instance_export(
                                shim_instance_index.unwrap(),
                                AliasExportKind::Function,
                                &lowering.export_name,
                            );

                            (lowering.name, alias_start_index + i as u32)
                        }
                    };

                    (name, Export::Function(func_index))
                });

                instances.export_core_items(exports);
                (
                    *name,
                    ModuleArg::Instance(*instance_count + instance_index as u32),
                )
            })
            .collect();

        let core_instance_index = *instance_count + instances.len();
        instances.instantiate_module(core_module_index, args);

        component.section(&aliases);
        *function_count += aliases.len();

        component.section(&functions);
        *function_count += functions.len();

        component.section(&instances);
        *instance_count += instances.len();

        core_instance_index
    }

    #[allow(clippy::too_many_arguments)]
    fn encode_indirect_lowerings(
        &self,
        component: &mut Component,
        encoding: StringEncoding,
        instance_count: &mut u32,
        function_count: &mut u32,
        core_instance_index: u32,
        shim_instance_index: u32,
        fixup_module_index: u32,
    ) {
        if self.indirect_count == 0 {
            return;
        }

        let mut aliases = AliasSection::new();
        let mut functions = ComponentFunctionSection::new();
        let mut instances = InstanceSection::new();

        let encoding_options = [encoding.into(), CanonicalOption::Into(core_instance_index)];
        let into_options = [CanonicalOption::Into(core_instance_index)];

        let mut indirect_count = 0;
        let indirect_start_index = *function_count + self.indirect_count;

        aliases.instance_export(
            shim_instance_index,
            AliasExportKind::Table,
            INDIRECT_TABLE_NAME,
        );

        let mut exports = Vec::with_capacity(1 + self.indirect_count as usize);
        exports.push((INDIRECT_TABLE_NAME, Export::Table(0)));

        for (instance_index, import) in self.imports.values().enumerate() {
            for i in &import.indirect_lowerings {
                let lowering = match &import.lowerings[*i] {
                    Lowering::Indirect(lowering) => lowering,
                    Lowering::Direct(_) => unreachable!(),
                };

                aliases.instance_export(
                    instance_index as u32,
                    AliasExportKind::ComponentFunction,
                    lowering.name,
                );

                let options: &[CanonicalOption] = match lowering.options {
                    RequiredOptions::None => &[],
                    RequiredOptions::Encoding => &encoding_options,
                    RequiredOptions::Into => &into_options,
                };

                functions.lower(*function_count + indirect_count, options.iter().copied());

                exports.push((
                    lowering.export_name.as_str(),
                    Export::Function(indirect_start_index + indirect_count),
                ));

                indirect_count += 1;
            }
        }

        instances.export_core_items(exports);
        instances.instantiate_module(
            fixup_module_index,
            [("", ModuleArg::Instance(*instance_count))],
        );

        component.section(&aliases);
        *function_count += aliases.len() - 1 /* don't include the table */;

        component.section(&functions);
        *function_count += functions.len();

        component.section(&instances);
        *instance_count += instances.len();
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
    module_count: u32,
    instance_count: u32,
    function_count: u32,
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

    /// Sets whether or not the encoder will only encode types.
    ///
    /// A types-only encoding is sufficient to describe a component's
    /// interface, but does not contain an implementation.
    pub fn types_only(mut self, types_only: bool) -> Self {
        self.types_only = types_only;
        self
    }

    /// Encode the component and return the bytes.
    pub fn encode(mut self) -> Result<Vec<u8>> {
        let required_imports = if !self.module.is_empty() {
            validate_module(self.module, &self.interface, self.imports, self.exports)?
        } else {
            HashSet::new()
        };

        let mut component = Component::default();
        let mut types = TypeEncoder::default();
        let mut imports = ImportEncoder::default();

        self.encode_import_types(&mut types, &mut imports, &required_imports)?;
        self.encode_export_types(&mut types)?;
        component.section(&types.types);

        if self.types_only {
            if !self.module.is_empty() {
                bail!("a module cannot be specified for a types-only encoding");
            }
        } else {
            let core_module_index = self.module_count;
            self.encode_core_module(&mut component)?;

            let core_instance_index =
                self.encode_core_instantiation(&mut component, &imports, core_module_index);

            self.encode_exports(&mut component, &mut types, core_instance_index);
        }

        component.section(&types.exports);

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

    fn encode_core_module(&mut self, component: &mut Component) -> Result<()> {
        if self.module.is_empty() {
            bail!("a module is required when encoding a component");
        }

        component.section(&wasm_encoder::RawSection {
            id: ComponentSectionId::Module.into(),
            data: self.module,
        });

        self.module_count += 1;
        Ok(())
    }

    fn encode_import_types(
        &mut self,
        types: &mut TypeEncoder<'a>,
        imports: &mut ImportEncoder<'a>,
        required_imports: &HashSet<&'a str>,
    ) -> Result<()> {
        for import in self.imports {
            if !required_imports.contains(import.name.as_str()) {
                continue;
            }

            Self::validate_interface(import)?;

            let mut instance = Some(InstanceTypeEncoder::default());

            for func in &import.functions {
                Self::validate_function(func)?;

                let index = types.encode_func_type(import, func, &mut instance)?;
                types.export_type(&mut instance, &func.name, index)?;
            }

            let index = types.encode_instance_type(&instance.as_ref().unwrap().ty);
            imports.import(import, index)?;
        }

        Ok(())
    }

    fn encode_export_types(&self, types: &mut TypeEncoder<'a>) -> Result<()> {
        for (export, is_default) in self
            .interface
            .iter()
            .copied()
            .map(|i| (i, true))
            .chain(self.exports.iter().map(|i| (i, false)))
        {
            Self::validate_interface(export)?;

            let mut instance = if is_default {
                None
            } else {
                if export.name.is_empty() {
                    bail!("cannot export an unnamed interface");
                }
                Some(InstanceTypeEncoder::default())
            };

            // TODO: stick interface documentation in a custom section?

            for func in &export.functions {
                Self::validate_function(func)?;

                let index = types.encode_func_type(export, func, &mut instance)?;

                if self.types_only {
                    types.export_type(&mut instance, &func.name, index)?;
                }
            }

            match instance {
                Some(instance) if self.types_only => {
                    let index = types.encode_instance_type(&instance.ty);
                    types.export_type(&mut None, &export.name, index)?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn encode_core_instantiation(
        &mut self,
        component: &mut Component,
        imports: &ImportEncoder,
        core_module_index: u32,
    ) -> u32 {
        // If there's no imports, directly instantiate the core module.
        if imports.is_empty() {
            let mut instances = InstanceSection::new();
            let core_instance_index = self.instance_count;
            instances.instantiate_module::<_, ModuleArg>(core_module_index, []);
            self.instance_count += 1;
            component.section(&instances);
            return core_instance_index;
        }

        // Otherwise, we need to encode the imports, lower the direct lowerings,
        // and pass the imports through to the core module instantiation.
        imports.encode_imports(component);
        self.instance_count += imports.len();

        let (fixup_module_index, shim_instance_index) =
            if let Some((shim, fixups)) = imports.create_shim_modules() {
                // Shim modules were encoded, so add them to the component
                let shim_module_index = self.module_count;
                component.section(&ModuleSection(shim));
                self.module_count += 1;

                let fixup_module_index = self.module_count;
                component.section(&ModuleSection(fixups));
                self.module_count += 1;

                // We must instantiate the shim module before any aliasing below
                let shim_instance_index = self.instance_count;
                let mut instances = InstanceSection::new();
                instances.instantiate_module::<_, ModuleArg>(shim_module_index, []);
                self.instance_count += 1;

                component.section(&instances);

                (Some(fixup_module_index), Some(shim_instance_index))
            } else {
                // No shim instance needed
                (None, None)
            };

        let core_instance_index = imports.encode_core_instantiation(
            component,
            shim_instance_index,
            core_module_index,
            &mut self.instance_count,
            &mut self.function_count,
        );

        // Finally, lower the indirect imports
        if let (Some(shim_instance_index), Some(fixup_module_index)) =
            (shim_instance_index, fixup_module_index)
        {
            imports.encode_indirect_lowerings(
                component,
                self.encoding,
                &mut self.instance_count,
                &mut self.function_count,
                core_instance_index,
                shim_instance_index,
                fixup_module_index,
            );
        }

        core_instance_index
    }

    fn encode_exports(
        &mut self,
        component: &mut Component,
        types: &mut TypeEncoder<'a>,
        core_instance_index: u32,
    ) {
        let encoding_options = [
            self.encoding.into(),
            CanonicalOption::Into(core_instance_index),
        ];
        let into_options = [CanonicalOption::Into(core_instance_index)];

        for (export, is_default) in self
            .interface
            .iter()
            .copied()
            .map(|i| (i, true))
            .chain(self.exports.iter().map(|i| (i, false)))
        {
            let alias_start_index = self.function_count;

            // Alias the exports from the core module
            let mut aliases = AliasSection::new();
            for func in &export.functions {
                let name =
                    expected_export_name((!is_default).then(|| export.name.as_str()), &func.name);

                aliases.instance_export(
                    core_instance_index,
                    AliasExportKind::Function,
                    name.as_ref(),
                );

                self.function_count += 1;
            }

            component.section(&aliases);

            let mut functions = ComponentFunctionSection::new();
            let mut exports = Vec::new();
            for (i, func) in export.functions.iter().enumerate() {
                // The type should already have been encoded
                let ty = *types
                    .func_type_map
                    .get(&FunctionKey {
                        interface: export,
                        func,
                    })
                    .expect("the type should be encoded");

                let options = match RequiredOptions::for_function(export, func) {
                    RequiredOptions::None => &[] as &[CanonicalOption],
                    RequiredOptions::Encoding => &encoding_options,
                    RequiredOptions::Into => &into_options,
                };

                functions.lift(ty, alias_start_index + i as u32, options.iter().copied());

                if is_default {
                    // Directly export the lifted function
                    types
                        .exports
                        .export(&func.name, ComponentExport::Function(self.function_count));
                } else {
                    // Otherwise, add it to the list for later instantiation
                    exports.push((
                        func.name.as_str(),
                        ComponentExport::Function(self.function_count),
                    ));
                }

                self.function_count += 1;
            }

            component.section(&functions);

            if !exports.is_empty() {
                let mut instances = InstanceSection::new();

                instances.export_items(exports);
                types
                    .exports
                    .export(&export.name, ComponentExport::Instance(self.instance_count));
                self.instance_count += 1;

                component.section(&instances);
            }
        }
    }
}
