//! State relating to validating a WebAssembly component.

use super::{check_max, core::Module, SnapshotList, TypeDef};
use crate::{
    limits::*, validator::core::EntityType, BinaryReaderError, CanonicalOption, FuncType,
    GlobalType, MemoryType, PrimitiveInterfaceType, Result, TableType, Type, WasmFeatures,
};
use std::collections::{HashMap, HashSet};

fn push_primitive_wasm_types(ty: &PrimitiveInterfaceType, wasm_types: &mut Vec<Type>) {
    match ty {
        PrimitiveInterfaceType::Unit => {}
        PrimitiveInterfaceType::Bool
        | PrimitiveInterfaceType::S8
        | PrimitiveInterfaceType::U8
        | PrimitiveInterfaceType::S16
        | PrimitiveInterfaceType::U16
        | PrimitiveInterfaceType::S32
        | PrimitiveInterfaceType::U32
        | PrimitiveInterfaceType::Char => {
            wasm_types.push(Type::I32);
        }
        PrimitiveInterfaceType::S64 | PrimitiveInterfaceType::U64 => {
            wasm_types.push(Type::I64);
        }
        PrimitiveInterfaceType::F32 => wasm_types.push(Type::F32),
        PrimitiveInterfaceType::F64 => wasm_types.push(Type::F64),
        PrimitiveInterfaceType::String => wasm_types.extend([Type::I32, Type::I32]),
    }
}

#[derive(Clone)]
pub struct ModuleType {
    imports: HashMap<(String, String), Vec<EntityType>>,
    exports: HashMap<String, EntityType>,
}

impl ModuleType {
    fn is_subtype_of(&self, other: &Self) -> bool {
        // For module type subtyping, all exports in the other module type
        // must be present in this module type's exports (i.e. it can export
        // *more* than what this module type needs).
        // However, for imports, the check is reversed (i.e. it is okay
        // to import *less* than what this module type needs).
        self.imports
            .iter()
            .all(|(k, ty)| match other.imports.get(k) {
                Some(other) => other[0].is_subtype_of(&ty[0]),
                None => false,
            })
            && other
                .exports
                .iter()
                .all(|(k, other)| match self.exports.get(k) {
                    Some(ty) => ty.is_subtype_of(other),
                    None => false,
                })
    }
}

#[derive(Clone)]
pub struct ComponentType {
    imports: HashMap<String, ComponentEntityType>,
    exports: HashMap<String, ComponentEntityType>,
}

impl ComponentType {
    fn is_subtype_of(&self, other: &Self, types: &SnapshotList<TypeDef>) -> bool {
        // For component type subtyping, all exports in the other component type
        // must be present in this component type's exports (i.e. it can export
        // *more* than what this component type needs).
        // However, for imports, the check is reversed (i.e. it is okay
        // to import *less* than what this component type needs).
        self.imports
            .iter()
            .all(|(k, ty)| match other.imports.get(k) {
                Some(other) => other.is_subtype_of(ty, types),
                None => false,
            })
            && other
                .exports
                .iter()
                .all(|(k, other)| match self.exports.get(k) {
                    Some(ty) => ty.is_subtype_of(other, types),
                    None => false,
                })
    }
}

#[derive(Clone)]
pub struct InstanceType {
    exports: HashMap<String, ComponentEntityType>,
}

impl InstanceType {
    fn is_subtype_of(&self, other: &Self, types: &SnapshotList<TypeDef>) -> bool {
        // For instance type subtyping, all exports in the other instance type
        // must be present in this instance type's exports (i.e. it can export
        // *more* than what this instance type needs).
        other
            .exports
            .iter()
            .all(|(k, other)| match self.exports.get(k) {
                Some(ty) => ty.is_subtype_of(other, types),
                None => false,
            })
    }
}

#[derive(Clone)]
pub struct ComponentFuncType {
    params: Box<[(String, InterfaceTypeRef)]>,
    result: InterfaceTypeRef,
    core_type: FuncType,
}

impl ComponentFuncType {
    fn requires_into_option(&self, types: &SnapshotList<TypeDef>) -> bool {
        self.result.requires_into_option(types)
            || self
                .params
                .iter()
                .any(|(_, ty)| ty.requires_into_option(types))
    }

    fn is_subtype_of(&self, other: &Self, types: &SnapshotList<TypeDef>) -> bool {
        // Subtyping rules:
        // https://github.com/WebAssembly/component-model/blob/17f94ed1270a98218e0e796ca1dad1feb7e5c507/design/mvp/Subtyping.md

        // Contravariant on return type
        if !other.result.is_subtype_of(&self.result, types) {
            return false;
        }

        // All overlapping parameters must have the same name and are subtypes
        for ((name, ty), (other_name, other_ty)) in self.params.iter().zip(other.params.iter()) {
            if name != other_name {
                return false;
            }

            if !ty.is_subtype_of(other_ty, types) {
                return false;
            }
        }

        // All remaining parameters in the supertype must be optional
        // All superfluous parameters in the subtype are ignored
        other
            .params
            .iter()
            .skip(self.params.len())
            .all(|(_, ty)| ty.is_optional(types))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterfaceTypeRef {
    Primitive(PrimitiveInterfaceType),
    Type(usize),
}

impl InterfaceTypeRef {
    fn requires_into_option(&self, types: &SnapshotList<TypeDef>) -> bool {
        match self {
            InterfaceTypeRef::Primitive(ty) => ty.requires_into_option(),
            InterfaceTypeRef::Type(ty) => types[*ty]
                .unwrap_interface_type()
                .requires_into_option(types),
        }
    }

    fn is_optional(&self, types: &SnapshotList<TypeDef>) -> bool {
        match self {
            InterfaceTypeRef::Primitive(_) => false,
            InterfaceTypeRef::Type(ty) => {
                matches!(
                    types[*ty].unwrap_interface_type(),
                    InterfaceType::Optional(_)
                )
            }
        }
    }

    fn is_subtype_of(&self, other: &Self, types: &SnapshotList<TypeDef>) -> bool {
        match (self, other) {
            (InterfaceTypeRef::Primitive(ty), InterfaceTypeRef::Primitive(other_ty)) => {
                ty.is_subtype_of(other_ty)
            }
            (InterfaceTypeRef::Type(ty), InterfaceTypeRef::Type(other_ty)) => types[*ty]
                .unwrap_interface_type()
                .is_subtype_of(types[*other_ty].unwrap_interface_type(), types),
            _ => false,
        }
    }
}

impl InterfaceTypeRef {
    fn push_wasm_types(
        &self,
        types: &SnapshotList<TypeDef>,
        offset: usize,
        wasm_types: &mut Vec<Type>,
    ) -> Result<()> {
        match self {
            Self::Primitive(ty) => push_primitive_wasm_types(ty, wasm_types),
            Self::Type(idx) => types[*idx]
                .unwrap_interface_type()
                .push_wasm_types(types, offset, wasm_types)?,
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct VariantCase {
    ty: InterfaceTypeRef,
    default_to: Option<String>,
}

#[derive(Debug, Clone)]
pub enum InterfaceType {
    Primitive(PrimitiveInterfaceType),
    Record(HashMap<String, InterfaceTypeRef>),
    Variant(HashMap<String, VariantCase>),
    List(InterfaceTypeRef),
    Tuple(Box<[InterfaceTypeRef]>),
    Flags(HashSet<String>),
    Enum(HashSet<String>),
    Union(Box<[InterfaceTypeRef]>),
    Optional(InterfaceTypeRef),
    Expected(InterfaceTypeRef, InterfaceTypeRef),
}

impl InterfaceType {
    fn requires_into_option(&self, types: &SnapshotList<TypeDef>) -> bool {
        match self {
            InterfaceType::Primitive(ty) => ty.requires_into_option(),
            InterfaceType::Record(fields) => {
                fields.values().any(|ty| ty.requires_into_option(types))
            }
            InterfaceType::Variant(cases) => cases
                .values()
                .any(|case| case.ty.requires_into_option(types)),
            InterfaceType::List(_) => true,
            InterfaceType::Tuple(tys) | InterfaceType::Union(tys) => {
                tys.iter().any(|ty| ty.requires_into_option(types))
            }
            InterfaceType::Flags(_) | InterfaceType::Enum(_) => false,
            InterfaceType::Optional(ty) => ty.requires_into_option(types),
            InterfaceType::Expected(ok, error) => {
                ok.requires_into_option(types) || error.requires_into_option(types)
            }
        }
    }

    fn is_subtype_of(&self, other: &Self, types: &SnapshotList<TypeDef>) -> bool {
        // Subtyping rules according to
        // https://github.com/WebAssembly/component-model/blob/17f94ed1270a98218e0e796ca1dad1feb7e5c507/design/mvp/Subtyping.md
        match (self, other) {
            (InterfaceType::Primitive(ty), InterfaceType::Primitive(other_ty)) => {
                ty.is_subtype_of(other_ty)
            }
            (InterfaceType::Record(fields), InterfaceType::Record(other_fields)) => {
                for (name, ty) in fields.iter() {
                    if let Some(other_ty) = other_fields.get(name) {
                        if !ty.is_subtype_of(other_ty, types) {
                            return false;
                        }
                    } else {
                        // Superfluous fields can be ignored in the subtype
                    }
                }
                // Check for missing required fields in the supertype
                for (other_name, other_ty) in other_fields.iter() {
                    if !other_ty.is_optional(types) && !fields.contains_key(other_name) {
                        return false;
                    }
                }
                true
            }
            (InterfaceType::Variant(cases), InterfaceType::Variant(other_cases)) => {
                for (name, case) in cases.iter() {
                    if let Some(other_case) = other_cases.get(name) {
                        // Contravariant subtype on the case type
                        if !other_case.ty.is_subtype_of(&case.ty, types) {
                            return false;
                        }
                    } else if let Some(default) = &case.default_to {
                        if !other_cases.contains_key(default) {
                            // The default is not in the supertype
                            return false;
                        }
                    } else {
                        // This case is not in the supertype and there is no
                        // default
                        return false;
                    }
                }
                true
            }
            (InterfaceType::List(ty), InterfaceType::List(other_ty))
            | (InterfaceType::Optional(ty), InterfaceType::Optional(other_ty)) => {
                ty.is_subtype_of(other_ty, types)
            }
            (InterfaceType::Tuple(tys), InterfaceType::Tuple(other_tys))
            | (InterfaceType::Union(tys), InterfaceType::Union(other_tys)) => {
                if tys.len() != other_tys.len() {
                    return false;
                }
                tys.iter()
                    .zip(other_tys.iter())
                    .all(|(ty, other_ty)| ty.is_subtype_of(other_ty, types))
            }
            (InterfaceType::Flags(set), InterfaceType::Flags(other_set))
            | (InterfaceType::Enum(set), InterfaceType::Enum(other_set)) => {
                set.is_subset(other_set)
            }
            (
                InterfaceType::Expected(ok, error),
                InterfaceType::Expected(other_ok, other_error),
            ) => ok.is_subtype_of(other_ok, types) && error.is_subtype_of(other_error, types),
            _ => false,
        }
    }

    fn push_wasm_types(
        &self,
        types: &SnapshotList<TypeDef>,
        offset: usize,
        wasm_types: &mut Vec<Type>,
    ) -> Result<()> {
        match self {
            Self::Primitive(ty) => push_primitive_wasm_types(ty, wasm_types),
            Self::Record(fields) => {
                for (_, ty) in fields.iter() {
                    ty.push_wasm_types(types, offset, wasm_types)?;
                }
            }
            Self::Variant(cases) => {
                Self::push_variant_types(
                    cases.iter().map(|(_, case)| &case.ty),
                    types,
                    offset,
                    wasm_types,
                )?;
            }
            Self::List(_) => {
                wasm_types.extend([Type::I32, Type::I32]);
            }
            Self::Tuple(tys) => {
                for ty in tys.iter() {
                    ty.push_wasm_types(types, offset, wasm_types)?;
                }
            }
            Self::Flags(names) => {
                if names.len() <= 32 {
                    wasm_types.push(Type::I32);
                } else if names.len() <= 64 {
                    wasm_types.push(Type::I64);
                } else {
                    for _ in 0..(names.len() + 31) / 32 {
                        wasm_types.push(Type::I32);
                    }
                }
            }
            Self::Enum(names) => {
                if names.len() < u32::max_value() as usize {
                    wasm_types.push(Type::I32);
                } else {
                    wasm_types.push(Type::I64);
                }
            }
            Self::Union(tys) => {
                Self::push_variant_types(tys.iter(), types, offset, wasm_types)?;
            }
            Self::Optional(ty) => {
                Self::push_variant_types([ty].into_iter(), types, offset, wasm_types)?;
            }
            Self::Expected(ok, error) => {
                Self::push_variant_types([ok, error].into_iter(), types, offset, wasm_types)?;
            }
        }

        Ok(())
    }

    fn push_variant_types<'a>(
        cases: impl ExactSizeIterator<Item = &'a InterfaceTypeRef>,
        types: &SnapshotList<TypeDef>,
        offset: usize,
        wasm_types: &mut Vec<Type>,
    ) -> Result<()> {
        if cases.len() < u32::max_value() as usize {
            wasm_types.push(Type::I32);
        } else {
            wasm_types.push(Type::I64);
        }

        let start = wasm_types.len();
        let mut temp = Vec::new();

        for ty in cases {
            ty.push_wasm_types(types, offset, &mut temp)?;

            for (i, ty) in temp.drain(..).enumerate() {
                match wasm_types.get_mut(start + i) {
                    Some(prev) => *prev = Self::unify_wasm_type(*prev, ty),
                    None => wasm_types.push(ty),
                }
            }
        }

        Ok(())
    }

    fn unify_wasm_type(a: Type, b: Type) -> Type {
        use Type::*;

        match (a, b) {
            (I64, _) | (_, I64) | (I32, F64) | (F64, I32) => I64,
            (I32, I32) | (I32, F32) | (F32, I32) => I32,
            (F32, F32) => F32,
            (F64, F64) | (F32, F64) | (F64, F32) => F64,
            _ => panic!("unexpected wasm type for canonical ABI"),
        }
    }
}

#[derive(Default)]
pub struct ComponentState {
    // The various lists here store indexes into the validator's type list.
    pub types: Vec<usize>,
    pub modules: Vec<usize>,
    pub components: Vec<usize>,
    pub instances: Vec<usize>,
    pub functions: Vec<usize>,
    pub values: Vec<(InterfaceTypeRef, bool)>,
    pub memories: Vec<MemoryType>,
    pub tables: Vec<TableType>,
    pub globals: Vec<GlobalType>,
    tags: Vec<usize>,
    has_start: bool,
    imports: HashMap<String, ComponentEntityType>,
    pub exports: HashMap<String, ComponentEntityType>,
}

impl ComponentState {
    pub(super) fn add_type(
        components: &mut Vec<Self>,
        def: crate::ComponentTypeDef,
        features: &WasmFeatures,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        assert!(!components.is_empty());
        let def = match def {
            crate::ComponentTypeDef::Module(defs) => {
                TypeDef::Module(components.last_mut().unwrap().create_module_type(
                    defs.into_vec(),
                    features,
                    types,
                    offset,
                )?)
            }
            crate::ComponentTypeDef::Component(defs) => TypeDef::Component(
                Self::create_component_type(components, defs.into_vec(), features, types, offset)?,
            ),
            crate::ComponentTypeDef::Instance(defs) => TypeDef::Instance(
                Self::create_instance_type(components, defs.into_vec(), features, types, offset)?,
            ),
            crate::ComponentTypeDef::Function(ty) => TypeDef::ComponentFunc(
                components
                    .last_mut()
                    .unwrap()
                    .create_function_type(ty, types, offset)?,
            ),
            crate::ComponentTypeDef::Value(ty) => TypeDef::Value(
                components
                    .last_mut()
                    .unwrap()
                    .create_interface_type_ref(ty, types, offset)?,
            ),
            crate::ComponentTypeDef::Interface(ty) => TypeDef::Interface(
                components
                    .last_mut()
                    .unwrap()
                    .create_interface_type(ty, types, offset)?,
            ),
        };

        components.last_mut().unwrap().types.push(types.len());
        types.push(def);

        Ok(())
    }

    pub(super) fn add_import(
        &mut self,
        import: crate::ComponentImport,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        let ty = self.type_index_to_entity_type(import.ty, types, "imported", offset)?;
        let (len, max, desc) = match ty {
            ComponentEntityType::Module(_) => {
                self.modules.push(self.types[import.ty as usize]);
                (self.modules.len(), MAX_WASM_MODULES, "modules")
            }
            ComponentEntityType::Component(_) => {
                self.components.push(self.types[import.ty as usize]);
                (self.components.len(), MAX_WASM_COMPONENTS, "components")
            }
            ComponentEntityType::Instance(_) => {
                self.instances.push(self.types[import.ty as usize]);
                (self.instances.len(), MAX_WASM_INSTANCES, "instances")
            }
            ComponentEntityType::Func(_) => {
                self.functions.push(self.types[import.ty as usize]);
                (self.functions.len(), MAX_WASM_FUNCTIONS, "functions")
            }
            ComponentEntityType::Value(it) => {
                self.values.push((it, false));
                (self.values.len(), MAX_WASM_VALUES, "values")
            }
            ComponentEntityType::Type(_) => {
                return Err(BinaryReaderError::new(
                    "component types cannot be imported",
                    offset,
                ));
            }
        };

        check_max(len, 0, max, desc, offset)?;

        if self.imports.insert(import.name.to_string(), ty).is_some() {
            return Err(BinaryReaderError::new(
                format!("duplicate import name `{}` already defined", import.name),
                offset,
            ));
        }

        Ok(())
    }

    pub(super) fn add_export(
        &mut self,
        name: &str,
        ty: ComponentEntityType,
        offset: usize,
    ) -> Result<()> {
        if self.exports.insert(name.to_string(), ty).is_some() {
            return Err(BinaryReaderError::new(
                format!("duplicate export name `{}` already defined", name),
                offset,
            ));
        }

        Ok(())
    }

    pub(super) fn lift_function(
        &mut self,
        type_index: u32,
        func_index: u32,
        options: Vec<CanonicalOption>,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        let ty = self.function_type_at(type_index, types, offset)?;
        let core_ty = types[self.core_function_at(func_index, types, offset)?].unwrap_func_type();

        // Lifting a function is for an export, so match the expected canonical ABI
        // export signature
        let mismatch = if ty.core_type.returns.len() > 1 {
            ty.core_type.params != core_ty.params
                || core_ty.returns.len() != 1
                || core_ty.returns[0] != Type::I32
        } else {
            ty.core_type != *core_ty
        };

        if mismatch {
            return Err(BinaryReaderError::new(
                "lowered function type does not match core function type",
                offset,
            ));
        }

        self.check_options(&options, ty, types, offset)?;
        self.functions.push(self.types[type_index as usize]);

        Ok(())
    }

    pub(super) fn lower_function(
        &mut self,
        func_index: u32,
        options: Vec<CanonicalOption>,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        let ty = self.function_type_at(func_index, types, offset)?;

        self.check_options(&options, ty, types, offset)?;

        // Lowering a function is for an import, so use a function type that matches
        // the expected canonical ABI import signature.
        let core_ty = if ty.core_type.returns.len() > 1 {
            FuncType {
                params: ty
                    .core_type
                    .params
                    .iter()
                    .chain(&[Type::I32])
                    .copied()
                    .collect(),
                returns: [].into(),
            }
        } else {
            ty.core_type.clone()
        };

        self.functions.push(types.len());
        types.push(TypeDef::Func(core_ty));

        Ok(())
    }

    pub(super) fn add_module(
        &mut self,
        module: &Module,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        module.validate_for_module_type(offset)?;

        self.modules.push(types.len());

        // We have to clone the module's imports and exports here
        // because we cannot take the data out of the `MaybeOwned`
        // as it might be shared with a function validator.
        types.push(TypeDef::Module(ModuleType {
            imports: module.imports.clone(),
            exports: module.exports.clone(),
        }));

        Ok(())
    }

    pub(super) fn add_component(&mut self, component: Self, types: &mut SnapshotList<TypeDef>) {
        self.components.push(types.len());
        types.push(TypeDef::Component(ComponentType {
            imports: component.imports,
            exports: component.exports,
        }));
    }

    pub(super) fn add_instance(
        &mut self,
        instance: crate::Instance,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        let instance = match instance {
            crate::Instance::Module { index, args } => {
                self.instantiate_module(index, args.into_vec(), types, offset)?
            }
            crate::Instance::Component { index, args } => {
                self.instantiate_component(index, args.into_vec(), types, offset)?
            }
            crate::Instance::ComponentFromExports(exports) => {
                self.instantiate_exports(exports.into_vec(), types, offset)?
            }
            crate::Instance::ModuleFromExports(exports) => {
                self.instantiate_core_exports(exports.into_vec(), types, offset)?
            }
        };

        self.instances.push(instance);

        Ok(())
    }

    pub(super) fn add_start(
        &mut self,
        func_index: u32,
        args: &[u32],
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        if self.has_start {
            return Err(BinaryReaderError::new(
                "component cannot have more than one start function",
                offset,
            ));
        }

        let ft = types[self.component_function_at(func_index, types, offset)?]
            .unwrap_component_func_type();

        if ft.params.len() != args.len() {
            return Err(BinaryReaderError::new(
                format!(
                    "component start function requires {} arguments but was given {}",
                    ft.params.len(),
                    args.len()
                ),
                offset,
            ));
        }

        for (i, ((_, ty), arg)) in ft.params.iter().zip(args).enumerate() {
            if ty != self.value_at(*arg, offset)? {
                return Err(BinaryReaderError::new(
                    format!(
                        "value type mismatch for component start function argument {}",
                        i
                    ),
                    offset,
                ));
            }
        }

        match ft.result {
            InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit) => {}
            ty => {
                self.values.push((ty, false));
            }
        }

        self.has_start = true;

        Ok(())
    }

    pub(super) fn add_alias(
        components: &mut Vec<Self>,
        alias: crate::Alias,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        assert!(!components.is_empty());
        match alias {
            crate::Alias::InstanceExport {
                kind,
                instance,
                name,
            } => components
                .last_mut()
                .unwrap()
                .alias_instance_export(kind, instance, name, types, offset),
            crate::Alias::OuterModule { count, index } => {
                check_max(
                    components.last().unwrap().modules.len(),
                    1,
                    MAX_WASM_MODULES,
                    "modules",
                    offset,
                )?;
                Self::alias_module(components, count, index, offset)
            }
            crate::Alias::OuterComponent { count, index } => {
                check_max(
                    components.last().unwrap().components.len(),
                    1,
                    MAX_WASM_COMPONENTS,
                    "components",
                    offset,
                )?;
                Self::alias_component(components, count, index, offset)
            }
            crate::Alias::OuterType { count, index } => {
                check_max(
                    components.last().unwrap().types.len(),
                    1,
                    MAX_WASM_TYPES,
                    "types",
                    offset,
                )?;
                Self::alias_type(components, count, index, offset)
            }
        }
    }

    pub(super) fn export_to_entity_type(
        &mut self,
        export: &crate::ComponentExport,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<ComponentEntityType> {
        Ok(match &export.kind {
            crate::ComponentExportKind::Module(idx) => {
                ComponentEntityType::Module(self.module_at(*idx, offset)?)
            }
            crate::ComponentExportKind::Component(idx) => {
                ComponentEntityType::Component(self.component_at(*idx, offset)?)
            }
            crate::ComponentExportKind::Instance(idx) => {
                self.component_instance_at(*idx, types, offset)?;
                ComponentEntityType::Instance(self.instances[*idx as usize])
            }
            crate::ComponentExportKind::Function(idx) => {
                self.component_function_at(*idx, types, offset)?;
                ComponentEntityType::Func(self.functions[*idx as usize])
            }
            crate::ComponentExportKind::Value(idx) => {
                ComponentEntityType::Value(*self.value_at(*idx, offset)?)
            }
            crate::ComponentExportKind::Type(idx) => {
                ComponentEntityType::Type(self.type_at(*idx, offset)?)
            }
        })
    }

    fn check_options(
        &self,
        options: &[CanonicalOption],
        ty: &ComponentFuncType,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        fn display(option: CanonicalOption) -> &'static str {
            match option {
                CanonicalOption::UTF8 => "utf8",
                CanonicalOption::UTF16 => "utf16",
                CanonicalOption::CompactUTF16 => "compact-utf16",
                CanonicalOption::Into(_) => "into",
            }
        }

        fn check_into_func(
            into: &ModuleType,
            name: &str,
            params: &[Type],
            returns: &[Type],
            types: &SnapshotList<TypeDef>,
            offset: usize,
        ) -> Result<()> {
            match into.exports.get(name) {
                Some(EntityType::Func(ty)) => {
                    let ty = types[*ty].unwrap_func_type();
                    if ty.params.as_ref() != params || ty.returns.as_ref() != returns {
                        return Err(BinaryReaderError::new(
                            format!("instance specified by `into` option exports a function named `{}` with the wrong signature", name),
                            offset,
                        ));
                    }
                }
                _ => {
                    return Err(BinaryReaderError::new(
                        format!("instance specified by `into` option does not export a function named `{}`", name),
                        offset,
                    ));
                }
            }

            Ok(())
        }

        let mut encoding = None;
        let mut into = None;

        for option in options {
            match option {
                CanonicalOption::UTF8 | CanonicalOption::UTF16 | CanonicalOption::CompactUTF16 => {
                    match encoding {
                        Some(existing) => {
                            return Err(BinaryReaderError::new(
                                format!(
                                    "canonical option `{}` conflicts with option `{}`",
                                    display(existing),
                                    display(*option)
                                ),
                                offset,
                            ))
                        }
                        None => encoding = Some(*option),
                    }
                }
                CanonicalOption::Into(i) => {
                    into = match into {
                        None => Some(*i),
                        Some(_) => {
                            return Err(BinaryReaderError::new(
                                "canonical option `into` is specified more than once",
                                offset,
                            ))
                        }
                    }
                }
            }
        }

        match into {
            Some(idx) => {
                let into_ty = self.module_instance_at(idx, types, offset)?;

                match into_ty.exports.get("memory") {
                    Some(EntityType::Memory(_)) => {}
                    _ => {
                        return Err(BinaryReaderError::new(
                            "instance specified by `into` option does not export a memory named `memory`",
                            offset,
                        ));
                    }
                }

                check_into_func(
                    into_ty,
                    "canonical_abi_realloc",
                    &[Type::I32, Type::I32, Type::I32, Type::I32],
                    &[Type::I32],
                    types,
                    offset,
                )?;
                check_into_func(
                    into_ty,
                    "canonical_abi_free",
                    &[Type::I32, Type::I32, Type::I32],
                    &[],
                    types,
                    offset,
                )?;
            }
            None => {
                if ty.requires_into_option(types) {
                    return Err(BinaryReaderError::new(
                        "canonical option `into` is required",
                        offset,
                    ));
                }
            }
        }

        Ok(())
    }

    fn type_index_to_entity_type(
        &self,
        ty: u32,
        types: &SnapshotList<TypeDef>,
        desc: &str,
        offset: usize,
    ) -> Result<ComponentEntityType> {
        Ok(match &types[self.type_at(ty, offset)?] {
            TypeDef::Module(_) => ComponentEntityType::Module(self.types[ty as usize]),
            TypeDef::Component(_) => ComponentEntityType::Component(self.types[ty as usize]),
            TypeDef::Instance(_) => ComponentEntityType::Instance(self.types[ty as usize]),
            TypeDef::ComponentFunc(_) => ComponentEntityType::Func(self.types[ty as usize]),
            TypeDef::Value(ty) => ComponentEntityType::Value(*ty),
            TypeDef::Interface(_) => ComponentEntityType::Type(self.types[ty as usize]),
            TypeDef::Func(_) => {
                return Err(BinaryReaderError::new(
                    format!("core WebAssembly function types cannot be {}", desc),
                    offset,
                ))
            }
        })
    }

    fn create_module_type(
        &self,
        defs: Vec<crate::ModuleType>,
        features: &WasmFeatures,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<ModuleType> {
        let mut state = Module::default();

        for def in defs {
            match def {
                crate::ModuleType::Type(ty) => {
                    state.add_type(ty, features, types, offset)?;
                }
                crate::ModuleType::Export { name, ty } => {
                    state.add_export(
                        name,
                        state.check_type_ref(&ty, features, types, offset)?,
                        offset,
                    )?;
                }
                crate::ModuleType::Import(import) => {
                    state.add_import(import, features, types, offset)?;
                }
            }
        }

        state.validate_for_module_type(offset)?;

        Ok(ModuleType {
            imports: state.imports,
            exports: state.exports,
        })
    }

    fn create_component_type(
        components: &mut Vec<Self>,
        defs: Vec<crate::ComponentType>,
        features: &WasmFeatures,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<ComponentType> {
        components.push(ComponentState::default());

        for def in defs {
            match def {
                crate::ComponentType::Type(ty) => {
                    Self::add_type(components, ty, features, types, offset)?;
                }
                crate::ComponentType::Export { name, ty } => {
                    let component = components.last_mut().unwrap();
                    component.add_export(
                        name,
                        component.type_index_to_entity_type(ty, types, "exported", offset)?,
                        offset,
                    )?;
                }
                crate::ComponentType::Import(import) => {
                    components
                        .last_mut()
                        .unwrap()
                        .add_import(import, types, offset)?;
                }
                crate::ComponentType::OuterType { count, index } => {
                    Self::alias_type(components, count, index, offset)?;
                }
            };
        }

        let state = components.pop().unwrap();

        Ok(ComponentType {
            imports: state.imports,
            exports: state.exports,
        })
    }

    fn create_instance_type(
        components: &mut Vec<Self>,
        defs: Vec<crate::InstanceType>,
        features: &WasmFeatures,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<InstanceType> {
        components.push(ComponentState::default());

        for def in defs {
            match def {
                crate::InstanceType::Type(ty) => {
                    Self::add_type(components, ty, features, types, offset)?;
                }
                crate::InstanceType::Export { name, ty } => {
                    let component = components.last_mut().unwrap();
                    component.add_export(
                        name,
                        component.type_index_to_entity_type(ty, types, "exported", offset)?,
                        offset,
                    )?;
                }
                crate::InstanceType::OuterType { count, index } => {
                    Self::alias_type(components, count, index, offset)?;
                }
            };
        }

        let state = components.pop().unwrap();

        Ok(InstanceType {
            exports: state.exports,
        })
    }

    fn create_function_type(
        &self,
        ty: crate::ComponentFuncType,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<ComponentFuncType> {
        let mut core_params = Vec::new();
        let mut core_returns = Vec::new();

        let params = ty
            .params
            .iter()
            .map(|(name, ty)| {
                Self::check_name(name, "function parameter", offset)?;
                let ty = self.create_interface_type_ref(*ty, types, offset)?;
                ty.push_wasm_types(types, offset, &mut core_params)?;
                Ok((name.to_string(), ty))
            })
            .collect::<Result<_>>()?;

        let result = self.create_interface_type_ref(ty.result, types, offset)?;
        result.push_wasm_types(types, offset, &mut core_returns)?;

        Ok(ComponentFuncType {
            params,
            result,
            core_type: FuncType {
                params: core_params.into_boxed_slice(),
                returns: core_returns.into_boxed_slice(),
            },
        })
    }

    fn check_name(name: &str, desc: &str, offset: usize) -> Result<()> {
        if name.is_empty() {
            return Err(BinaryReaderError::new(
                format!("{} name cannot be empty", desc),
                offset,
            ));
        }

        Ok(())
    }

    pub fn type_at(&self, idx: u32, offset: usize) -> Result<usize> {
        if let Some(idx) = self.types.get(idx as usize) {
            Ok(*idx)
        } else {
            Err(BinaryReaderError::new(
                format!("unknown type {}: type index out of bounds", idx),
                offset,
            ))
        }
    }

    fn function_type_at<'a>(
        &self,
        idx: u32,
        types: &'a SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<&'a ComponentFuncType> {
        if let TypeDef::ComponentFunc(ty) = &types[self.type_at(idx, offset)?] {
            Ok(ty)
        } else {
            Err(BinaryReaderError::new(
                format!("type index {} is not a function type", idx),
                offset,
            ))
        }
    }

    fn instantiate_module(
        &self,
        module_index: u32,
        module_args: Vec<crate::ModuleArg>,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        fn insert_arg<'a>(
            module: &'a str,
            name: &'a str,
            arg: EntityType,
            args: &mut HashMap<(&'a str, &'a str), EntityType>,
            offset: usize,
        ) -> Result<()> {
            if args.insert((module, name), arg).is_some() {
                return Err(BinaryReaderError::new(
                    format!(
                        "duplicate instantiation argument name `{}::{}` already defined",
                        module, name
                    ),
                    offset,
                ));
            }

            Ok(())
        }

        let module_type = self.module_at(module_index, offset)?;
        let mut args = HashMap::new();

        // Populate the arguments
        for module_arg in module_args {
            match module_arg.kind {
                crate::ModuleArgKind::Instance(idx) => {
                    let instance_type = self.module_instance_at(idx, types, offset)?;
                    for (name, ty) in instance_type.exports.iter() {
                        insert_arg(module_arg.name, name, ty.clone(), &mut args, offset)?;
                    }
                }
            }
        }

        // Validate the arguments
        for ((module, name), b) in types[module_type].unwrap_module_type().imports.iter() {
            assert_eq!(b.len(), 1);
            match args.get(&(module.as_str(), name.as_str())) {
                Some(a) => {
                    let desc = match (a, &b[0]) {
                        (EntityType::Func(_), EntityType::Func(_)) => "function",
                        (EntityType::Table(_), EntityType::Table(_)) => "table",
                        (EntityType::Memory(_), EntityType::Memory(_)) => "memory",
                        (EntityType::Global(_), EntityType::Global(_)) => "global",
                        (EntityType::Tag(_), EntityType::Tag(_)) => "tag",
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!(
                                "expected module instantiation argument `{}::{}` to be of type `{}`",
                                module,
                                name,
                                b[0].desc()
                            ),
                                offset,
                            ))
                        }
                    };

                    if !a.is_subtype_of(&b[0]) {
                        return Err(BinaryReaderError::new(
                            format!(
                                "{} type mismatch for module instantiation argument `{}::{}`",
                                desc, module, name
                            ),
                            offset,
                        ));
                    }
                }
                None => {
                    return Err(BinaryReaderError::new(
                        format!(
                            "missing module instantiation argument named `{}::{}`",
                            module, name
                        ),
                        offset,
                    ));
                }
            }
        }

        Ok(module_type)
    }

    fn instantiate_component(
        &mut self,
        component_index: u32,
        component_args: Vec<crate::ComponentArg>,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        fn insert_arg<'a>(
            name: &'a str,
            arg: ComponentEntityType,
            args: &mut HashMap<&'a str, ComponentEntityType>,
            offset: usize,
        ) -> Result<()> {
            if args.insert(name, arg).is_some() {
                return Err(BinaryReaderError::new(
                    format!(
                        "duplicate instantiation argument name `{}` already defined",
                        name
                    ),
                    offset,
                ));
            }

            Ok(())
        }

        let ty = self.component_at(component_index, offset)?;
        let mut args = HashMap::new();

        // Populate the arguments
        for component_arg in component_args {
            match component_arg.kind {
                crate::ComponentArgKind::Module(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Module(self.module_at(idx, offset)?),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Component(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Component(self.component_at(idx, offset)?),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Instance(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Instance(
                            self.component_instance_at(idx, types, offset)?,
                        ),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Function(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Func(self.component_function_at(idx, types, offset)?),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Value(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Value(*self.value_at(idx, offset)?),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Type(_) => {
                    // Type arguments are ignored
                }
            }
        }

        // Validate the arguments
        for (name, b) in types[ty].unwrap_component_type().imports.iter() {
            match args.get(name.as_str()) {
                Some(a) => {
                    let desc = match (a, b) {
                        (ComponentEntityType::Module(_), ComponentEntityType::Module(_)) => {
                            "module"
                        }
                        (ComponentEntityType::Component(_), ComponentEntityType::Component(_)) => {
                            "component"
                        }
                        (ComponentEntityType::Instance(_), ComponentEntityType::Instance(_)) => {
                            "instance"
                        }
                        (ComponentEntityType::Func(_), ComponentEntityType::Func(_)) => "function",
                        (ComponentEntityType::Value(_), ComponentEntityType::Value(_)) => "value",
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!(
                                "expected component instantiation argument `{}` to be of type `{}`",
                                name,
                                b.desc()
                            ),
                                offset,
                            ))
                        }
                    };

                    if !a.is_subtype_of(b, types) {
                        return Err(BinaryReaderError::new(
                            format!(
                                "{} type mismatch for component instantiation argument `{}`",
                                desc, name
                            ),
                            offset,
                        ));
                    }
                }
                None => {
                    return Err(BinaryReaderError::new(
                        format!("missing component instantiation argument named `{}`", name),
                        offset,
                    ))
                }
            }
        }

        Ok(ty)
    }

    fn instantiate_exports(
        &mut self,
        exports: Vec<crate::ComponentExport>,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        fn insert_export(
            name: &str,
            export: ComponentEntityType,
            exports: &mut HashMap<String, ComponentEntityType>,
            offset: usize,
        ) -> Result<()> {
            if exports.insert(name.to_string(), export).is_some() {
                return Err(BinaryReaderError::new(
                    format!(
                        "duplicate instantiation export name `{}` already defined",
                        name
                    ),
                    offset,
                ));
            }

            Ok(())
        }

        let mut inst_exports = HashMap::new();
        for export in exports {
            match export.kind {
                crate::ComponentExportKind::Module(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Module(self.module_at(idx, offset)?),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Component(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Component(self.component_at(idx, offset)?),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Instance(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Instance(
                            self.component_instance_at(idx, types, offset)?,
                        ),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Function(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Func(self.component_function_at(idx, types, offset)?),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Value(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Value(*self.value_at(idx, offset)?),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Type(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Type(self.type_at(idx, offset)?),
                        &mut inst_exports,
                        offset,
                    )?;
                }
            }
        }

        let ty = types.len();
        types.push(TypeDef::Instance(InstanceType {
            exports: inst_exports,
        }));
        Ok(ty)
    }

    fn instantiate_core_exports(
        &mut self,
        exports: Vec<crate::Export>,
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        fn insert_export(
            name: &str,
            export: EntityType,
            exports: &mut HashMap<String, EntityType>,
            offset: usize,
        ) -> Result<()> {
            if exports.insert(name.to_string(), export).is_some() {
                return Err(BinaryReaderError::new(
                    format!(
                        "duplicate instantiation export name `{}` already defined",
                        name
                    ),
                    offset,
                ));
            }

            Ok(())
        }

        let mut inst_exports = HashMap::new();
        for export in exports {
            match export.kind {
                crate::ExternalKind::Func => {
                    insert_export(
                        export.name,
                        EntityType::Func(self.core_function_at(export.index, types, offset)?),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ExternalKind::Table => insert_export(
                    export.name,
                    EntityType::Table(*self.table_at(export.index, offset)?),
                    &mut inst_exports,
                    offset,
                )?,
                crate::ExternalKind::Memory => insert_export(
                    export.name,
                    EntityType::Memory(*self.memory_at(export.index, offset)?),
                    &mut inst_exports,
                    offset,
                )?,
                crate::ExternalKind::Global => {
                    insert_export(
                        export.name,
                        EntityType::Global(*self.global_at(export.index, offset)?),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ExternalKind::Tag => insert_export(
                    export.name,
                    EntityType::Tag(self.core_function_at(export.index, types, offset)?),
                    &mut inst_exports,
                    offset,
                )?,
            }
        }

        let ty = types.len();
        types.push(TypeDef::Module(ModuleType {
            imports: HashMap::default(),
            exports: inst_exports,
        }));
        Ok(ty)
    }

    fn alias_instance_export(
        &mut self,
        kind: crate::AliasKind,
        idx: u32,
        name: &str,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        macro_rules! push_module_export {
            ($expected:path, $collection:ident, $limit:ident, $ty:literal) => {{
                check_max(self.$collection.len(), 1, $limit, concat!($ty, "s"), offset)?;
                match self.module_instance_export(idx, name, types, offset)? {
                    $expected(ty) => {
                        self.$collection.push(*ty);
                        Ok(())
                    }
                    _ => {
                        return Err(BinaryReaderError::new(
                            format!("export `{}` for instance {} is not a {}", name, idx, $ty),
                            offset,
                        ))
                    }
                }
            }};
        }

        macro_rules! push_component_export {
            ($expected:path, $collection:ident, $limit:ident, $ty:literal) => {{
                check_max(self.$collection.len(), 1, $limit, concat!($ty, "s"), offset)?;
                match self.component_instance_export(idx, name, types, offset)? {
                    $expected(ty) => {
                        self.$collection.push(*ty);
                        Ok(())
                    }
                    _ => {
                        return Err(BinaryReaderError::new(
                            format!("export `{}` for instance {} is not a {}", name, idx, $ty),
                            offset,
                        ))
                    }
                }
            }};
        }

        match kind {
            crate::AliasKind::Module => {
                push_component_export!(
                    ComponentEntityType::Module,
                    modules,
                    MAX_WASM_MODULES,
                    "module"
                )
            }
            crate::AliasKind::Component => {
                push_component_export!(
                    ComponentEntityType::Component,
                    components,
                    MAX_WASM_COMPONENTS,
                    "component"
                )
            }
            crate::AliasKind::Instance => {
                check_max(
                    self.instances.len(),
                    1,
                    MAX_WASM_INSTANCES,
                    "instances",
                    offset,
                )?;
                match self.component_instance_export(idx, name, types, offset)? {
                    ComponentEntityType::Instance(ty) => {
                        self.instances.push(*ty);
                        Ok(())
                    }
                    _ => {
                        return Err(BinaryReaderError::new(
                            format!("export `{}` for instance {} is not an instance", name, idx),
                            offset,
                        ))
                    }
                }
            }
            crate::AliasKind::ComponentFunc => {
                push_component_export!(
                    ComponentEntityType::Func,
                    functions,
                    MAX_WASM_FUNCTIONS,
                    "function"
                )
            }
            crate::AliasKind::Value => {
                check_max(self.values.len(), 1, MAX_WASM_VALUES, "values", offset)?;
                match self.component_instance_export(idx, name, types, offset)? {
                    ComponentEntityType::Value(ty) => {
                        self.values.push((*ty, false));
                        Ok(())
                    }
                    _ => {
                        return Err(BinaryReaderError::new(
                            format!("export `{}` for instance {} is not a value", name, idx),
                            offset,
                        ))
                    }
                }
            }
            crate::AliasKind::Func => {
                push_module_export!(EntityType::Func, functions, MAX_WASM_FUNCTIONS, "function")
            }
            crate::AliasKind::Table => {
                push_module_export!(EntityType::Table, tables, MAX_WASM_TABLES, "table")
            }
            crate::AliasKind::Memory => {
                push_module_export!(EntityType::Memory, memories, MAX_WASM_MEMORIES, "memory")
            }
            crate::AliasKind::Global => {
                push_module_export!(EntityType::Global, globals, MAX_WASM_GLOBALS, "global")
            }
            crate::AliasKind::Tag => {
                push_module_export!(EntityType::Tag, tags, MAX_WASM_TAGS, "tag")
            }
        }
    }

    fn alias_module(
        components: &mut Vec<Self>,
        count: u32,
        index: u32,
        offset: usize,
    ) -> Result<()> {
        let component = Self::check_alias_count(components, count, offset)?;
        let ty = component.module_at(index, offset)?;
        components.last_mut().unwrap().modules.push(ty);
        Ok(())
    }

    fn alias_component(
        components: &mut Vec<Self>,
        count: u32,
        index: u32,
        offset: usize,
    ) -> Result<()> {
        let component = Self::check_alias_count(components, count, offset)?;
        let ty = component.component_at(index, offset)?;
        components.last_mut().unwrap().components.push(ty);
        Ok(())
    }

    fn alias_type(components: &mut Vec<Self>, count: u32, index: u32, offset: usize) -> Result<()> {
        let component = Self::check_alias_count(components, count, offset)?;
        let ty = component.type_at(index, offset)?;
        components.last_mut().unwrap().types.push(ty);
        Ok(())
    }

    fn check_alias_count(components: &[Self], count: u32, offset: usize) -> Result<&Self> {
        let count = count as usize;
        if count >= components.len() {
            return Err(BinaryReaderError::new(
                format!("invalid outer alias count of {}", count),
                offset,
            ));
        }

        Ok(&components[components.len() - count - 1])
    }

    fn create_interface_type(
        &self,
        ty: crate::InterfaceType,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<InterfaceType> {
        Ok(match ty {
            crate::InterfaceType::Primitive(ty) => InterfaceType::Primitive(ty),
            crate::InterfaceType::Record(fields) => InterfaceType::Record(
                fields
                    .iter()
                    .map(|(name, ty)| {
                        Self::check_name(name, "record field", offset)?;
                        Ok((
                            name.to_string(),
                            self.create_interface_type_ref(*ty, types, offset)?,
                        ))
                    })
                    .collect::<Result<_>>()?,
            ),
            crate::InterfaceType::Variant(cases) => InterfaceType::Variant(
                cases
                    .iter()
                    .map(|case| {
                        Self::check_name(case.name, "variant case", offset)?;
                        if let Some(default_to) = case.default_to {
                            if default_to >= cases.len() as u32 {
                                return Err(BinaryReaderError::new(
                                    format!(
                                        "variant case default index {} is out of bounds",
                                        default_to
                                    ),
                                    offset,
                                ));
                            }
                        }
                        Ok((
                            case.name.to_string(),
                            VariantCase {
                                ty: self.create_interface_type_ref(case.ty, types, offset)?,
                                default_to: case
                                    .default_to
                                    .map(|i| cases[i as usize].name.to_string()),
                            },
                        ))
                    })
                    .collect::<Result<_>>()?,
            ),
            crate::InterfaceType::List(ty) => {
                InterfaceType::List(self.create_interface_type_ref(ty, types, offset)?)
            }
            crate::InterfaceType::Tuple(tys) => InterfaceType::Tuple(
                tys.iter()
                    .map(|ty| self.create_interface_type_ref(*ty, types, offset))
                    .collect::<Result<_>>()?,
            ),
            crate::InterfaceType::Flags(names) => InterfaceType::Flags(
                names
                    .iter()
                    .map(|name| {
                        Self::check_name(name, "flag", offset)?;
                        Ok(name.to_string())
                    })
                    .collect::<Result<_>>()?,
            ),
            crate::InterfaceType::Enum(names) => InterfaceType::Enum(
                names
                    .iter()
                    .map(|name| {
                        Self::check_name(name, "enum tag", offset)?;
                        Ok(name.to_string())
                    })
                    .collect::<Result<_>>()?,
            ),
            crate::InterfaceType::Union(tys) => InterfaceType::Union(
                tys.iter()
                    .map(|ty| self.create_interface_type_ref(*ty, types, offset))
                    .collect::<Result<_>>()?,
            ),
            crate::InterfaceType::Optional(ty) => {
                InterfaceType::Optional(self.create_interface_type_ref(ty, types, offset)?)
            }
            crate::InterfaceType::Expected { ok, error } => InterfaceType::Expected(
                self.create_interface_type_ref(ok, types, offset)?,
                self.create_interface_type_ref(error, types, offset)?,
            ),
        })
    }

    fn create_interface_type_ref(
        &self,
        ty: crate::InterfaceTypeRef,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<InterfaceTypeRef> {
        Ok(match ty {
            crate::InterfaceTypeRef::Primitive(pt) => InterfaceTypeRef::Primitive(pt),
            crate::InterfaceTypeRef::Type(idx) => {
                InterfaceTypeRef::Type(self.interface_type_at(idx, types, offset)?)
            }
        })
    }

    fn function_at(&self, idx: u32, offset: usize) -> Result<usize> {
        match self.functions.get(idx as usize) {
            Some(ty) => Ok(*ty),
            None => Err(BinaryReaderError::new(
                format!("unknown function {}: function index out of bounds", idx),
                offset,
            )),
        }
    }

    fn component_function_at(
        &self,
        idx: u32,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        let ty = self.function_at(idx, offset)?;
        match &types[ty] {
            TypeDef::ComponentFunc(_) => Ok(ty),
            _ => Err(BinaryReaderError::new(
                format!("function {} is not a component function", idx),
                offset,
            )),
        }
    }

    fn core_function_at(
        &self,
        idx: u32,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        let ty = self.function_at(idx, offset)?;
        match &types[ty] {
            TypeDef::Func(_) => Ok(ty),
            _ => Err(BinaryReaderError::new(
                format!("function {} is not a core WebAssembly function", idx),
                offset,
            )),
        }
    }

    fn module_at(&self, idx: u32, offset: usize) -> Result<usize> {
        match self.modules.get(idx as usize) {
            Some(idx) => Ok(*idx),
            None => Err(BinaryReaderError::new(
                format!("unknown module {}: module index out of bounds", idx),
                offset,
            )),
        }
    }

    fn component_at(&self, idx: u32, offset: usize) -> Result<usize> {
        match self.components.get(idx as usize) {
            Some(idx) => Ok(*idx),
            None => Err(BinaryReaderError::new(
                format!("unknown component {}: component index out of bounds", idx),
                offset,
            )),
        }
    }

    fn instance_at(&self, idx: u32, offset: usize) -> Result<usize> {
        match self.instances.get(idx as usize) {
            Some(idx) => Ok(*idx),
            None => Err(BinaryReaderError::new(
                format!("unknown instance {}: instance index out of bounds", idx),
                offset,
            )),
        }
    }

    fn module_instance_at<'a>(
        &self,
        idx: u32,
        types: &'a SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<&'a ModuleType> {
        match &types[self.instance_at(idx, offset)?] {
            TypeDef::Module(ty) => Ok(ty),
            _ => Err(BinaryReaderError::new(
                format!("instance {} is not a module instance", idx),
                offset,
            )),
        }
    }

    fn component_instance_at(
        &self,
        idx: u32,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        let ty = self.instance_at(idx, offset)?;
        match &types[ty] {
            TypeDef::Instance(_) => Ok(ty),
            _ => Err(BinaryReaderError::new(
                format!("instance {} is not a component instance", idx),
                offset,
            )),
        }
    }

    fn module_instance_export<'a>(
        &self,
        idx: u32,
        name: &str,
        types: &'a SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<&'a EntityType> {
        match self
            .module_instance_at(idx, types, offset)?
            .exports
            .get(name)
        {
            Some(export) => Ok(export),
            None => {
                return Err(BinaryReaderError::new(
                    format!("instance {} has no export named `{}`", idx, name),
                    offset,
                ))
            }
        }
    }

    fn component_instance_export<'a>(
        &self,
        idx: u32,
        name: &str,
        types: &'a SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<&'a ComponentEntityType> {
        match types[self.component_instance_at(idx, types, offset)?]
            .unwrap_instance_type()
            .exports
            .get(name)
        {
            Some(export) => Ok(export),
            None => {
                return Err(BinaryReaderError::new(
                    format!("instance {} has no export named `{}`", idx, name),
                    offset,
                ))
            }
        }
    }

    fn value_at(&mut self, idx: u32, offset: usize) -> Result<&InterfaceTypeRef> {
        match self.values.get_mut(idx as usize) {
            Some((ty, used)) if !*used => {
                *used = true;
                Ok(ty)
            }
            Some(_) => Err(BinaryReaderError::new(
                format!("value {} cannot be used more than once", idx),
                offset,
            )),
            None => Err(BinaryReaderError::new(
                format!("unknown value {}: value index out of bounds", idx),
                offset,
            )),
        }
    }

    fn interface_type_at(
        &self,
        idx: u32,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        let idx = self.type_at(idx, offset)?;
        match &types[idx] {
            TypeDef::Interface(_) => Ok(idx),
            _ => Err(BinaryReaderError::new(
                format!("type {} is not an interface type", idx),
                offset,
            )),
        }
    }

    fn global_at(&self, idx: u32, offset: usize) -> Result<&GlobalType> {
        match self.globals.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown global {}: global index out of bounds", idx,),
                offset,
            )),
        }
    }

    fn table_at(&self, idx: u32, offset: usize) -> Result<&TableType> {
        match self.tables.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown table {}: table index out of bounds", idx),
                offset,
            )),
        }
    }

    fn memory_at(&self, idx: u32, offset: usize) -> Result<&MemoryType> {
        match self.memories.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown memory {}: memory index out of bounds", idx,),
                offset,
            )),
        }
    }
}

#[derive(Clone)]
pub enum ComponentEntityType {
    // Stores an index into the global type list.
    Module(usize),
    // Stores an index into the global type list.
    Component(usize),
    // Stores an index into the global type list.
    Instance(usize),
    // Stores an index into the global type list.
    Func(usize),
    Value(InterfaceTypeRef),
    // Stores an index into the global type list.
    Type(usize),
}

impl ComponentEntityType {
    fn is_subtype_of(&self, other: &Self, types: &SnapshotList<TypeDef>) -> bool {
        match (self, other) {
            (Self::Module(ty), Self::Module(other_ty)) => types[*ty]
                .unwrap_module_type()
                .is_subtype_of(types[*other_ty].unwrap_module_type()),
            (Self::Component(ty), Self::Component(other_ty)) => types[*ty]
                .unwrap_component_type()
                .is_subtype_of(types[*other_ty].unwrap_component_type(), types),
            (Self::Instance(ty), Self::Instance(other_ty)) => types[*ty]
                .unwrap_instance_type()
                .is_subtype_of(types[*other_ty].unwrap_instance_type(), types),
            (Self::Func(ty), Self::Func(other_ty)) => types[*ty]
                .unwrap_component_func_type()
                .is_subtype_of(types[*other_ty].unwrap_component_func_type(), types),
            (Self::Value(ty), Self::Value(other_ty)) => ty.is_subtype_of(other_ty, types),
            (Self::Type(ty), Self::Type(other_ty)) => types[*ty]
                .unwrap_interface_type()
                .is_subtype_of(types[*other_ty].unwrap_interface_type(), types),
            _ => false,
        }
    }

    fn desc(&self) -> &'static str {
        match self {
            Self::Module(_) => "module",
            Self::Component(_) => "component",
            Self::Instance(_) => "instance",
            Self::Func(_) => "function",
            Self::Value(_) => "value",
            Self::Type(_) => "type",
        }
    }
}
