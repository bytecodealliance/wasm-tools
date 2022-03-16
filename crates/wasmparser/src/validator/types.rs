//! Types relating to type information provided by validation.

use crate::{FuncType, GlobalType, MemoryType, PrimitiveInterfaceType, Result, TableType, Type};
use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
    mem,
    sync::Arc,
};

use super::{component::ComponentState, core::Module};

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
        PrimitiveInterfaceType::Float32 => wasm_types.push(Type::F32),
        PrimitiveInterfaceType::Float64 => wasm_types.push(Type::F64),
        PrimitiveInterfaceType::String => wasm_types.extend([Type::I32, Type::I32]),
    }
}

/// Represents a unique identifier for a type known to a [`crate::Validator`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(pub(crate) usize);

/// A unified type definition for inspecting WebAssembly modules and components.
pub enum TypeDef {
    /// The definition is for a core function type.
    Func(FuncType),
    /// The definition is for a module type.
    ///
    /// This variant is only supported when parsing a component.
    Module(ModuleType),
    /// The definition is for a component type.
    ///
    /// This variant is only supported when parsing a component.
    Component(ComponentType),
    /// The definition is for an instance type.
    ///
    /// This variant is only supported when parsing a component.
    Instance(InstanceType),
    /// The definition is for a component function type.
    ///
    /// This variant is only supported when parsing a component.
    ComponentFunc(ComponentFuncType),
    /// The definition is for a value type.
    ///
    /// This variant is only supported when parsing a component.
    Value(InterfaceTypeRef),
    /// The definition is for an interface type.
    ///
    /// This variant is only supported when parsing a component.
    Interface(InterfaceType),
}

impl TypeDef {
    pub(crate) fn unwrap_func_type(&self) -> &FuncType {
        match self {
            Self::Func(ty) => ty,
            _ => panic!("expected function type"),
        }
    }

    pub(crate) fn unwrap_module_type(&self) -> &ModuleType {
        match self {
            Self::Module(ty) => ty,
            _ => panic!("expected module type"),
        }
    }

    pub(crate) fn unwrap_component_type(&self) -> &ComponentType {
        match self {
            Self::Component(ty) => ty,
            _ => panic!("expected component type"),
        }
    }

    pub(crate) fn unwrap_instance_type(&self) -> &InstanceType {
        match self {
            Self::Instance(ty) => ty,
            _ => panic!("expected instance type"),
        }
    }

    pub(crate) fn unwrap_component_func_type(&self) -> &ComponentFuncType {
        match self {
            Self::ComponentFunc(ty) => ty,
            _ => panic!("expected component function type"),
        }
    }

    pub(crate) fn unwrap_interface_type(&self) -> &InterfaceType {
        match self {
            Self::Interface(ty) => ty,
            _ => panic!("expected interface type"),
        }
    }
}

/// A reference to an interface type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterfaceTypeRef {
    /// The interface type is one of the primitive types.
    Primitive(PrimitiveInterfaceType),
    /// The interface type is represented with the given type identifier.
    Type(TypeId),
}

impl InterfaceTypeRef {
    pub(crate) fn requires_into_option(&self, types: &TypeList) -> bool {
        match self {
            InterfaceTypeRef::Primitive(ty) => ty.requires_into_option(),
            InterfaceTypeRef::Type(ty) => types[*ty]
                .unwrap_interface_type()
                .requires_into_option(types),
        }
    }

    pub(crate) fn is_optional(&self, types: &TypeList) -> bool {
        match self {
            InterfaceTypeRef::Primitive(_) => false,
            InterfaceTypeRef::Type(ty) => {
                matches!(types[*ty].unwrap_interface_type(), InterfaceType::Option(_))
            }
        }
    }

    pub(crate) fn is_subtype_of(&self, other: &Self, types: &TypeList) -> bool {
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

    pub(crate) fn push_wasm_types(
        &self,
        types: &TypeList,
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

/// The entity type for imports and exports of a module.
#[derive(Clone, Copy)]
pub enum EntityType {
    /// The entity is a function.
    Func(TypeId),
    /// The entity is a table.
    Table(TableType),
    /// The entity is a memory.
    Memory(MemoryType),
    /// The entity is a global.
    Global(GlobalType),
    /// The entity is a tag.
    Tag(TypeId),
}

impl EntityType {
    pub(crate) fn is_subtype_of(&self, b: &Self) -> bool {
        macro_rules! limits_match {
            ($a:expr, $b:expr) => {{
                let a = $a;
                let b = $b;
                a.initial >= b.initial
                    && match b.maximum {
                        Some(b_max) => match a.maximum {
                            Some(a_max) => a_max <= b_max,
                            None => false,
                        },
                        None => true,
                    }
            }};
        }

        match (self, b) {
            (EntityType::Func(a), EntityType::Func(b)) => a == b,
            (EntityType::Table(a), EntityType::Table(b)) => {
                a.element_type == b.element_type && limits_match!(a, b)
            }
            (EntityType::Memory(a), EntityType::Memory(b)) => {
                a.shared == b.shared && a.memory64 == b.memory64 && limits_match!(a, b)
            }
            (EntityType::Global(a), EntityType::Global(b)) => a == b,
            (EntityType::Tag(a), EntityType::Tag(b)) => a == b,
            _ => false,
        }
    }

    pub(crate) fn desc(&self) -> &'static str {
        match self {
            Self::Func(_) => "function",
            Self::Table(_) => "table",
            Self::Memory(_) => "memory",
            Self::Global(_) => "global",
            Self::Tag(_) => "tag",
        }
    }
}

trait ModuleImportKey {
    fn module(&self) -> &str;
    fn name(&self) -> &str;
}

impl<'a> Borrow<dyn ModuleImportKey + 'a> for (String, String) {
    fn borrow(&self) -> &(dyn ModuleImportKey + 'a) {
        self
    }
}

impl Hash for (dyn ModuleImportKey + '_) {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.module().hash(state);
        self.name().hash(state);
    }
}

impl PartialEq for (dyn ModuleImportKey + '_) {
    fn eq(&self, other: &Self) -> bool {
        self.module() == other.module() && self.name() == other.name()
    }
}

impl Eq for (dyn ModuleImportKey + '_) {}

impl ModuleImportKey for (String, String) {
    fn module(&self) -> &str {
        &self.0
    }

    fn name(&self) -> &str {
        &self.1
    }
}

impl ModuleImportKey for (&str, &str) {
    fn module(&self) -> &str {
        self.0
    }

    fn name(&self) -> &str {
        self.1
    }
}

/// Represents a module type.
#[derive(Clone)]
pub struct ModuleType {
    /// The imports of the module type.
    pub imports: HashMap<(String, String), EntityType>,
    /// The exports of the module type.
    pub exports: HashMap<String, EntityType>,
}

impl ModuleType {
    /// Looks up an import by its module and name.
    ///
    /// Returns `None` if the import was not found.
    pub fn lookup_import(&self, module: &str, name: &str) -> Option<&EntityType> {
        self.imports.get(&(module, name) as &dyn ModuleImportKey)
    }

    pub(crate) fn is_subtype_of(&self, other: &Self) -> bool {
        // For module type subtyping, all exports in the other module type
        // must be present in this module type's exports (i.e. it can export
        // *more* than what this module type needs).
        // However, for imports, the check is reversed (i.e. it is okay
        // to import *less* than what this module type needs).
        self.imports
            .iter()
            .all(|(k, ty)| match other.imports.get(k) {
                Some(other) => other.is_subtype_of(ty),
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

/// The entity type for imports and exports of a component.
#[derive(Clone)]
pub enum ComponentEntityType {
    /// The entity is a module.
    Module(TypeId),
    /// The entity is a component.
    Component(TypeId),
    /// The entity is an instance.
    Instance(TypeId),
    /// The entity is a component function.
    Func(TypeId),
    /// The entity is a value.
    Value(InterfaceTypeRef),
    /// The entity is a type.
    Type(TypeId),
}

impl ComponentEntityType {
    pub(crate) fn is_subtype_of(&self, other: &Self, types: &TypeList) -> bool {
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

    pub(crate) fn desc(&self) -> &'static str {
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

/// Represents a type of a component.
#[derive(Clone)]
pub struct ComponentType {
    /// The imports of the component type.
    pub imports: HashMap<String, ComponentEntityType>,
    /// The exports of the component type.
    pub exports: HashMap<String, ComponentEntityType>,
}

impl ComponentType {
    pub(crate) fn is_subtype_of(&self, other: &Self, types: &TypeList) -> bool {
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

/// Represents a type of an instance.
#[derive(Clone)]
pub struct InstanceType {
    /// The exports of the instance type.
    pub exports: HashMap<String, ComponentEntityType>,
}

impl InstanceType {
    pub(crate) fn is_subtype_of(&self, other: &Self, types: &TypeList) -> bool {
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

/// Represents a type of a component function.
#[derive(Clone)]
pub struct ComponentFuncType {
    /// The function parameters.
    pub params: Box<[(Option<String>, InterfaceTypeRef)]>,
    /// The function's result type.
    pub result: InterfaceTypeRef,
    pub(crate) core_type: FuncType,
}

impl ComponentFuncType {
    pub(crate) fn requires_into_option(&self, types: &TypeList) -> bool {
        self.result.requires_into_option(types)
            || self
                .params
                .iter()
                .any(|(_, ty)| ty.requires_into_option(types))
    }

    pub(crate) fn is_subtype_of(&self, other: &Self, types: &TypeList) -> bool {
        // Subtyping rules:
        // https://github.com/WebAssembly/component-model/blob/17f94ed1270a98218e0e796ca1dad1feb7e5c507/design/mvp/Subtyping.md

        // Covariant on return type
        if !self.result.is_subtype_of(&other.result, types) {
            return false;
        }

        // All overlapping parameters must have the same name and are contravariant subtypes
        for ((name, ty), (other_name, other_ty)) in self.params.iter().zip(other.params.iter()) {
            if name != other_name {
                return false;
            }

            if !other_ty.is_subtype_of(ty, types) {
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

/// Represents a variant case.
#[derive(Debug, Clone)]
pub struct VariantCase {
    /// The variant case type.
    pub ty: InterfaceTypeRef,
    /// The value of the variant to default to.
    pub default_to: Option<String>,
}

/// Represents an interface type.
#[derive(Debug, Clone)]
pub enum InterfaceType {
    /// The type is a primitive interface type.
    Primitive(PrimitiveInterfaceType),
    /// The type is a record.
    Record(HashMap<String, InterfaceTypeRef>),
    /// The type is a variant.
    Variant(HashMap<String, VariantCase>),
    /// The type is a list.
    List(InterfaceTypeRef),
    /// The type is a tuple.
    Tuple(Box<[InterfaceTypeRef]>),
    /// The type is a set of flags.
    Flags(HashSet<String>),
    /// The type is an enumeration.
    Enum(HashSet<String>),
    /// The type is a union.
    Union(Box<[InterfaceTypeRef]>),
    /// The type is an `option`.
    Option(InterfaceTypeRef),
    /// The type is an `expected`.
    Expected(InterfaceTypeRef, InterfaceTypeRef),
}

impl InterfaceType {
    pub(crate) fn requires_into_option(&self, types: &TypeList) -> bool {
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
            InterfaceType::Option(ty) => ty.requires_into_option(types),
            InterfaceType::Expected(ok, error) => {
                ok.requires_into_option(types) || error.requires_into_option(types)
            }
        }
    }

    pub(crate) fn is_subtype_of(&self, other: &Self, types: &TypeList) -> bool {
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
                        // Covariant subtype on the case type
                        if !case.ty.is_subtype_of(&other_case.ty, types) {
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
            | (InterfaceType::Option(ty), InterfaceType::Option(other_ty)) => {
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

    pub(crate) fn push_wasm_types(
        &self,
        types: &TypeList,
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
            Self::Option(ty) => {
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
        types: &TypeList,
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

#[allow(clippy::large_enum_variant)]
enum TypesKind {
    Module(Arc<Module>),
    Component(ComponentState),
}

/// Represents the types known to a [`crate::Validator`] once validation has completed.
///
/// The type information is returned via the [`crate::Validator::end`] method.
pub struct Types {
    types: TypeList,
    kind: TypesKind,
}

impl Types {
    pub(crate) fn from_module(types: TypeList, module: Arc<Module>) -> Self {
        Self {
            types,
            kind: TypesKind::Module(module),
        }
    }

    pub(crate) fn from_component(types: TypeList, component: ComponentState) -> Self {
        Self {
            types,
            kind: TypesKind::Component(component),
        }
    }

    /// Gets a type based on its type id.
    ///
    /// Returns `None` if the type id is unknown.
    pub fn type_from_id(&self, id: TypeId) -> Option<&TypeDef> {
        self.types.get(id.0)
    }

    /// Gets a defined type at the given type index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn type_at(&self, index: u32) -> Option<&TypeDef> {
        let types = match &self.kind {
            TypesKind::Module(module) => &module.types,
            TypesKind::Component(component) => &component.types,
        };

        Some(&self.types[*types.get(index as usize)?])
    }

    /// Gets the count of defined types.
    pub fn type_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(module) => module.types.len(),
            TypesKind::Component(component) => component.types.len(),
        }
    }

    /// Gets the type of a table at the given table index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn table_at(&self, index: u32) -> Option<TableType> {
        let tables = match &self.kind {
            TypesKind::Module(module) => &module.tables,
            TypesKind::Component(component) => &component.tables,
        };

        tables.get(index as usize).copied()
    }

    /// Gets the count of imported and defined tables.
    pub fn table_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(module) => module.tables.len(),
            TypesKind::Component(component) => component.tables.len(),
        }
    }

    /// Gets the type of a memory at the given memory index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn memory_at(&self, index: u32) -> Option<MemoryType> {
        let memories = match &self.kind {
            TypesKind::Module(module) => &module.memories,
            TypesKind::Component(component) => &component.memories,
        };

        memories.get(index as usize).copied()
    }

    /// Gets the count of imported and defined memories.
    pub fn memory_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(module) => module.memories.len(),
            TypesKind::Component(component) => component.memories.len(),
        }
    }

    /// Gets the type of a global at the given global index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn global_at(&self, index: u32) -> Option<GlobalType> {
        let globals = match &self.kind {
            TypesKind::Module(module) => &module.globals,
            TypesKind::Component(component) => &component.globals,
        };

        globals.get(index as usize).copied()
    }

    /// Gets the count of imported and defined globals.
    pub fn global_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(module) => module.globals.len(),
            TypesKind::Component(component) => component.globals.len(),
        }
    }

    /// Gets the type of a tag at the given tag index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn tag_at(&self, index: u32) -> Option<&FuncType> {
        let tags = match &self.kind {
            TypesKind::Module(module) => &module.tags,
            TypesKind::Component(component) => &component.tags,
        };

        Some(self.types[*tags.get(index as usize)?].unwrap_func_type())
    }

    /// Gets the count of imported and defined tags.
    pub fn tag_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(module) => module.tags.len(),
            TypesKind::Component(component) => component.tags.len(),
        }
    }

    /// Gets the type of a core function at the given function index.
    ///
    /// Returns `None` if the index is out of bounds or when parsing
    /// a component and the function at the given index is not a core
    /// function type.
    pub fn function_at(&self, index: u32) -> Option<&FuncType> {
        let id = match &self.kind {
            TypesKind::Module(module) => {
                &module.types[*module.functions.get(index as usize)? as usize]
            }
            TypesKind::Component(component) => component.functions.get(index as usize)?,
        };

        match &self.types[*id] {
            TypeDef::Func(ty) => Some(ty),
            _ => None,
        }
    }

    /// Gets the type of a component function at the given function index.
    ///
    /// Returns `None` if the index is out of bounds or if the function at
    /// the given index is not a component function type.
    pub fn component_function_at(&self, index: u32) -> Option<&ComponentFuncType> {
        match &self.kind {
            TypesKind::Module(_) => None,
            TypesKind::Component(component) => {
                let id = component.functions.get(index as usize)?;
                match &self.types[*id] {
                    TypeDef::ComponentFunc(ty) => Some(ty),
                    _ => None,
                }
            }
        }
    }

    /// Gets the count of imported and defined functions.
    ///
    /// The count also includes aliased functions in components.
    pub fn function_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(module) => module.functions.len(),
            TypesKind::Component(component) => component.functions.len(),
        }
    }

    /// Gets the type of an element segment at the given element segment index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn element_at(&self, index: u32) -> Option<Type> {
        match &self.kind {
            TypesKind::Module(module) => module.element_types.get(index as usize).copied(),
            TypesKind::Component(_) => None,
        }
    }

    /// Gets the count of element segments.
    pub fn element_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(module) => module.element_types.len(),
            TypesKind::Component(_) => 0,
        }
    }

    /// Gets the type of a module at the given module index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn module_at(&self, index: u32) -> Option<&ModuleType> {
        match &self.kind {
            TypesKind::Module(_) => None,
            TypesKind::Component(component) => {
                Some(self.types[*component.modules.get(index as usize)?].unwrap_module_type())
            }
        }
    }

    /// Gets the count of imported, exported, or aliased modules.
    pub fn module_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(_) => 0,
            TypesKind::Component(component) => component.modules.len(),
        }
    }

    /// Gets the type of a component at the given component index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn component_at(&self, index: u32) -> Option<&ComponentType> {
        match &self.kind {
            TypesKind::Module(_) => None,
            TypesKind::Component(component) => {
                Some(self.types[*component.components.get(index as usize)?].unwrap_component_type())
            }
        }
    }

    /// Gets the count of imported, exported, or aliased components.
    pub fn component_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(_) => 0,
            TypesKind::Component(component) => component.components.len(),
        }
    }

    /// Gets the type of an instance at the given instance index.
    ///
    /// Returns `None` if the index is out of bounds or if the instance is not
    /// a component instance.
    pub fn instance_at(&self, index: u32) -> Option<&InstanceType> {
        match &self.kind {
            TypesKind::Module(_) => None,
            TypesKind::Component(component) => {
                let id = component.instances.get(index as usize)?;
                match &self.types[*id] {
                    TypeDef::Instance(ty) => Some(ty),
                    _ => None,
                }
            }
        }
    }

    /// Gets the type of a core module instance at the given instance index.
    ///
    /// Returns `None` if the index is out of bounds or if the instance is not
    /// a module instance.
    pub fn module_instance_at(&self, index: u32) -> Option<&ModuleType> {
        match &self.kind {
            TypesKind::Module(_) => None,
            TypesKind::Component(component) => {
                let id = component.instances.get(index as usize)?;
                match &self.types[*id] {
                    TypeDef::Module(ty) => Some(ty),
                    _ => None,
                }
            }
        }
    }

    /// Gets the count of imported, exported, or aliased instances.
    pub fn instance_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(_) => 0,
            TypesKind::Component(component) => component.instances.len(),
        }
    }

    /// Gets the type of a value at the given value index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn value_at(&self, index: u32) -> Option<InterfaceTypeRef> {
        match &self.kind {
            TypesKind::Module(_) => None,
            TypesKind::Component(component) => {
                component.values.get(index as usize).map(|(r, _)| *r)
            }
        }
    }

    /// Gets the count of imported, exported, or aliased values.
    pub fn value_count(&self) -> usize {
        match &self.kind {
            TypesKind::Module(_) => 0,
            TypesKind::Component(component) => component.values.len(),
        }
    }
}

/// This is a type which mirrors a subset of the `Vec<T>` API, but is intended
/// to be able to be cheaply snapshotted and cloned.
///
/// When each module's code sections start we "commit" the current list of types
/// in the global list of types. This means that the temporary `cur` vec here is
/// pushed onto `snapshots` and wrapped up in an `Arc`. At that point we clone
/// this entire list (which is then O(modules), not O(types in all modules)) and
/// pass out as a context to each function validator.
///
/// Otherwise, though, this type behaves as if it were a large `Vec<T>`, but
/// it's represented by lists of contiguous chunks.
pub(crate) struct SnapshotList<T> {
    // All previous snapshots, the "head" of the list that this type represents.
    // The first entry in this pair is the starting index for all elements
    // contained in the list, and the second element is the list itself. Note
    // the `Arc` wrapper around sub-lists, which makes cloning time for this
    // `SnapshotList` O(snapshots) rather than O(snapshots_total), which for
    // us in this context means the number of modules, not types.
    //
    // Note that this list is sorted least-to-greatest in order of the index for
    // binary searching.
    snapshots: Vec<(usize, Arc<Vec<T>>)>,

    // This is the total length of all lists in the `snapshots` array.
    snapshots_total: usize,

    // The current list of types for the current snapshot that are being built.
    cur: Vec<T>,
}

impl<T> SnapshotList<T> {
    /// Same as `<&[T]>::get`
    pub(crate) fn get(&self, index: usize) -> Option<&T> {
        // Check to see if this index falls on our local list
        if index >= self.snapshots_total {
            return self.cur.get(index - self.snapshots_total);
        }
        // ... and failing that we do a binary search to figure out which bucket
        // it's in. Note the `i-1` in the `Err` case because if we don't find an
        // exact match the type is located in the previous bucket.
        let i = match self.snapshots.binary_search_by_key(&index, |(i, _)| *i) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let (len, list) = &self.snapshots[i];
        Some(&list[index - len])
    }

    /// Same as `<&mut [T]>::get_mut`, except only works for indexes into the
    /// current snapshot being built.
    ///
    /// # Panics
    ///
    /// Panics if an index is passed in which falls within the
    /// previously-snapshotted list of types. This should never happen in our
    /// context and the panic is intended to weed out possible bugs in
    /// wasmparser.
    pub(crate) fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index >= self.snapshots_total {
            return self.cur.get_mut(index - self.snapshots_total);
        }
        panic!("cannot get a mutable reference in snapshotted part of list")
    }

    /// Same as `Vec::push`
    pub(crate) fn push(&mut self, val: T) {
        self.cur.push(val);
    }

    /// Same as `<[T]>::len`
    pub(crate) fn len(&self) -> usize {
        self.cur.len() + self.snapshots_total
    }

    /// Reserve space for an additional count of items.
    pub(crate) fn reserve(&mut self, additional: usize) {
        self.cur.reserve(additional);
    }

    /// Commits previously pushed types into this snapshot vector, and returns a
    /// clone of this list.
    ///
    /// The returned `SnapshotList` can be used to access all the same types as
    /// this list itself. This list also is not changed (from an external
    /// perspective) and can continue to access all the same types.
    pub(crate) fn commit(&mut self) -> SnapshotList<T> {
        // If the current chunk has new elements, commit them in to an
        // `Arc`-wrapped vector in the snapshots list. Note the `shrink_to_fit`
        // ahead of time to hopefully keep memory usage lower than it would
        // otherwise be.
        let len = self.cur.len();
        if len > 0 {
            self.cur.shrink_to_fit();
            self.snapshots
                .push((self.snapshots_total, Arc::new(mem::take(&mut self.cur))));
            self.snapshots_total += len;
        }
        SnapshotList {
            snapshots: self.snapshots.clone(),
            snapshots_total: self.snapshots_total,
            cur: Vec::new(),
        }
    }
}

impl<T> std::ops::Index<usize> for SnapshotList<T> {
    type Output = T;

    fn index(&self, index: usize) -> &T {
        self.get(index).unwrap()
    }
}

impl<T> std::ops::IndexMut<usize> for SnapshotList<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        self.get_mut(index).unwrap()
    }
}

impl<T> std::ops::Index<TypeId> for SnapshotList<T> {
    type Output = T;

    fn index(&self, id: TypeId) -> &T {
        self.get(id.0).unwrap()
    }
}

impl<T> std::ops::IndexMut<TypeId> for SnapshotList<T> {
    fn index_mut(&mut self, id: TypeId) -> &mut T {
        self.get_mut(id.0).unwrap()
    }
}

impl<T> Default for SnapshotList<T> {
    fn default() -> SnapshotList<T> {
        SnapshotList {
            snapshots: Vec::new(),
            snapshots_total: 0,
            cur: Vec::new(),
        }
    }
}

pub(crate) type TypeList = SnapshotList<TypeDef>;
