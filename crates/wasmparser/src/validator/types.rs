//! Types relating to type information provided by validation.

use super::{component::ComponentState, core::Module};
use crate::{FuncType, GlobalType, MemoryType, PrimitiveInterfaceType, TableType, Type};
use indexmap::{IndexMap, IndexSet};
use std::{
    borrow::Borrow,
    collections::HashMap,
    hash::{Hash, Hasher},
    mem,
    sync::Arc,
};

/// The maximum number of parameters in the canonical ABI that can be passed by value.
///
/// Functions that exceed this limit will instead pass parameters indirectly from
/// linear memory via a single pointer parameter.
const MAX_FLAT_FUNC_PARAMS: usize = 16;
/// The maximum number of results in the canonical ABI that can be returned by a function.
///
/// Functions that exceed this limit have their results written to linear memory via an
/// additional pointer parameter (imports) or return a single pointer value (exports).
const MAX_FLAT_FUNC_RESULTS: usize = 1;

/// The maximum lowered types, including a possible type for a return pointer parameter.
const MAX_LOWERED_TYPES: usize = MAX_FLAT_FUNC_PARAMS + 1;

/// A simple alloc-free list of types used for calculating lowered function signatures.
pub(crate) struct LoweredTypes {
    types: [Type; MAX_LOWERED_TYPES],
    len: usize,
    max: usize,
}

impl LoweredTypes {
    fn new(max: usize) -> Self {
        assert!(max <= MAX_LOWERED_TYPES);
        Self {
            types: [Type::I32; MAX_LOWERED_TYPES],
            len: 0,
            max,
        }
    }

    fn len(&self) -> usize {
        self.len
    }

    fn maxed(&self) -> bool {
        self.len == self.max
    }

    fn get_mut(&mut self, index: usize) -> Option<&mut Type> {
        if index < self.len {
            Some(&mut self.types[index])
        } else {
            None
        }
    }

    fn push(&mut self, ty: Type) -> bool {
        if self.maxed() {
            return false;
        }

        self.types[self.len] = ty;
        self.len += 1;
        true
    }

    fn clear(&mut self) {
        self.len = 0;
    }

    pub fn as_slice(&self) -> &[Type] {
        &self.types[..self.len]
    }

    pub fn iter(&self) -> impl Iterator<Item = Type> + '_ {
        self.as_slice().iter().copied()
    }
}

fn push_primitive_wasm_types(
    ty: &PrimitiveInterfaceType,
    lowered_types: &mut LoweredTypes,
) -> bool {
    match ty {
        PrimitiveInterfaceType::Unit => true,
        PrimitiveInterfaceType::Bool
        | PrimitiveInterfaceType::S8
        | PrimitiveInterfaceType::U8
        | PrimitiveInterfaceType::S16
        | PrimitiveInterfaceType::U16
        | PrimitiveInterfaceType::S32
        | PrimitiveInterfaceType::U32
        | PrimitiveInterfaceType::Char => lowered_types.push(Type::I32),
        PrimitiveInterfaceType::S64 | PrimitiveInterfaceType::U64 => lowered_types.push(Type::I64),
        PrimitiveInterfaceType::Float32 => lowered_types.push(Type::F32),
        PrimitiveInterfaceType::Float64 => lowered_types.push(Type::F64),
        PrimitiveInterfaceType::String => {
            lowered_types.push(Type::I32) && lowered_types.push(Type::I32)
        }
    }
}

/// Represents a unique identifier for a type known to a [`crate::Validator`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeId {
    /// The effective type size for the type.
    ///
    /// This is stored as part of the ID to avoid having to recurse through
    /// the global type list when calculating type sizes.
    pub(crate) type_size: usize,
    /// The index into the global list of types.
    pub(crate) index: usize,
}

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

    pub(crate) fn type_size(&self) -> usize {
        match self {
            Self::Func(ty) => 1 + ty.params.len() + ty.returns.len(),
            Self::Module(ty) => ty.type_size,
            Self::Component(ty) => ty.type_size,
            Self::Instance(ty) => ty.type_size,
            Self::ComponentFunc(ty) => ty.type_size,
            Self::Value(ty) => ty.type_size(),
            Self::Interface(ty) => ty.type_size(),
        }
    }
}

/// A reference to an interface type.
#[derive(Debug, Clone, Copy)]
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

    fn push_wasm_types(&self, types: &TypeList, lowered_types: &mut LoweredTypes) -> bool {
        match self {
            Self::Primitive(ty) => push_primitive_wasm_types(ty, lowered_types),
            Self::Type(id) => types[*id]
                .unwrap_interface_type()
                .push_wasm_types(types, lowered_types),
        }
    }

    pub(crate) fn type_size(&self) -> usize {
        match self {
            Self::Primitive(ty) => ty.type_size(),
            Self::Type(id) => id.type_size,
        }
    }
}

/// The entity type for imports and exports of a module.
#[derive(Debug, Clone, Copy)]
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
    pub(crate) fn is_subtype_of(&self, b: &Self, types: &TypeList) -> bool {
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
            (EntityType::Func(a), EntityType::Func(b)) => {
                types[*a].unwrap_func_type() == types[*b].unwrap_func_type()
            }
            (EntityType::Table(a), EntityType::Table(b)) => {
                a.element_type == b.element_type && limits_match!(a, b)
            }
            (EntityType::Memory(a), EntityType::Memory(b)) => {
                a.shared == b.shared && a.memory64 == b.memory64 && limits_match!(a, b)
            }
            (EntityType::Global(a), EntityType::Global(b)) => a == b,
            (EntityType::Tag(a), EntityType::Tag(b)) => {
                types[*a].unwrap_func_type() == types[*b].unwrap_func_type()
            }
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

    pub(crate) fn type_size(&self) -> usize {
        match self {
            Self::Func(id) | Self::Tag(id) => id.type_size,
            Self::Table(_) | Self::Memory(_) | Self::Global(_) => 1,
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
    /// The effective type size for the module type.
    pub(crate) type_size: usize,
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

    pub(crate) fn is_subtype_of(&self, other: &Self, types: &TypeList) -> bool {
        // For module type subtyping, all exports in the other module type
        // must be present in this module type's exports (i.e. it can export
        // *more* than what this module type needs).
        // However, for imports, the check is reversed (i.e. it is okay
        // to import *less* than what this module type needs).
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

/// The entity type for imports and exports of a component.
#[derive(Debug, Clone, Copy)]
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
                .is_subtype_of(types[*other_ty].unwrap_module_type(), types),
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

    pub(crate) fn type_size(&self) -> usize {
        match self {
            Self::Module(ty)
            | Self::Component(ty)
            | Self::Instance(ty)
            | Self::Func(ty)
            | Self::Type(ty) => ty.type_size,
            Self::Value(ty) => ty.type_size(),
        }
    }
}

/// Represents a type of a component.
#[derive(Clone)]
pub struct ComponentType {
    /// The effective type size for the component type.
    pub(crate) type_size: usize,
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
    /// The effective type size for the instance type.
    pub(crate) type_size: usize,
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
    /// The effective type size for the component function type.
    pub(crate) type_size: usize,
    /// The function parameters.
    pub params: Box<[(Option<String>, InterfaceTypeRef)]>,
    /// The function's result type.
    pub result: InterfaceTypeRef,
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

        // The supertype cannot have fewer parameters than the subtype.
        if other.params.len() < self.params.len() {
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

    /// Lowers the component function type to core parameter and result types for the
    /// canonical ABI.
    pub(crate) fn lower(&self, types: &TypeList, import: bool) -> (LoweredTypes, LoweredTypes) {
        let mut params = LoweredTypes::new(MAX_FLAT_FUNC_PARAMS);
        let mut results = LoweredTypes::new(MAX_FLAT_FUNC_RESULTS);

        for (_, ty) in self.params.iter() {
            if !ty.push_wasm_types(types, &mut params) {
                // Too many parameters to pass directly
                // Function will have a single pointer parameter to pass the arguments
                // via linear memory
                params.clear();
                assert!(params.push(Type::I32));
                break;
            }
        }

        if !self.result.push_wasm_types(types, &mut results) {
            // Too many results to return directly, either a retptr parameter will be used (import)
            // or a single pointer will be returned (export)
            results.clear();
            if import {
                params.max = MAX_LOWERED_TYPES;
                assert!(params.push(Type::I32));
            } else {
                assert!(results.push(Type::I32));
            }
        }

        (params, results)
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

/// Represents a record type.
#[derive(Debug, Clone)]
pub struct RecordType {
    /// The effective type size for the record type.
    pub(crate) type_size: usize,
    /// The map of record fields.
    pub fields: IndexMap<String, InterfaceTypeRef>,
}

/// Represents a variant type.
#[derive(Debug, Clone)]
pub struct VariantType {
    /// The effective type size for the variant type.
    pub(crate) type_size: usize,
    /// The map of variant cases.
    pub cases: IndexMap<String, VariantCase>,
}

/// Represents a tuple type.
#[derive(Debug, Clone)]
pub struct TupleType {
    /// The effective type size for the tuple type.
    pub(crate) type_size: usize,
    /// The types of the tuple.
    pub types: Box<[InterfaceTypeRef]>,
}

/// Represents a union type.
#[derive(Debug, Clone)]
pub struct UnionType {
    /// The inclusive type count for the union type.
    pub(crate) type_size: usize,
    /// The types of the union.
    pub types: Box<[InterfaceTypeRef]>,
}

/// Represents an interface type.
#[derive(Debug, Clone)]
pub enum InterfaceType {
    /// The type is a primitive interface type.
    Primitive(PrimitiveInterfaceType),
    /// The type is a record.
    Record(RecordType),
    /// The type is a variant.
    Variant(VariantType),
    /// The type is a list.
    List(InterfaceTypeRef),
    /// The type is a tuple.
    Tuple(TupleType),
    /// The type is a set of flags.
    Flags(IndexSet<String>),
    /// The type is an enumeration.
    Enum(IndexSet<String>),
    /// The type is a union.
    Union(UnionType),
    /// The type is an `option`.
    Option(InterfaceTypeRef),
    /// The type is an `expected`.
    Expected(InterfaceTypeRef, InterfaceTypeRef),
}

impl InterfaceType {
    pub(crate) fn requires_into_option(&self, types: &TypeList) -> bool {
        match self {
            InterfaceType::Primitive(ty) => ty.requires_into_option(),
            InterfaceType::Record(r) => r.fields.values().any(|ty| ty.requires_into_option(types)),
            InterfaceType::Variant(v) => v
                .cases
                .values()
                .any(|case| case.ty.requires_into_option(types)),
            InterfaceType::List(_) => true,
            InterfaceType::Tuple(t) => t.types.iter().any(|ty| ty.requires_into_option(types)),
            InterfaceType::Union(u) => u.types.iter().any(|ty| ty.requires_into_option(types)),
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
            (InterfaceType::Record(r), InterfaceType::Record(other_r)) => {
                for (name, ty) in r.fields.iter() {
                    if let Some(other_ty) = other_r.fields.get(name) {
                        if !ty.is_subtype_of(other_ty, types) {
                            return false;
                        }
                    } else {
                        // Superfluous fields can be ignored in the subtype
                    }
                }
                // Check for missing required fields in the supertype
                for (other_name, other_ty) in other_r.fields.iter() {
                    if !other_ty.is_optional(types) && !r.fields.contains_key(other_name) {
                        return false;
                    }
                }
                true
            }
            (InterfaceType::Variant(v), InterfaceType::Variant(other_v)) => {
                for (name, case) in v.cases.iter() {
                    if let Some(other_case) = other_v.cases.get(name) {
                        // Covariant subtype on the case type
                        if !case.ty.is_subtype_of(&other_case.ty, types) {
                            return false;
                        }
                    } else if let Some(default) = &case.default_to {
                        if !other_v.cases.contains_key(default) {
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
            (InterfaceType::Tuple(t), InterfaceType::Tuple(other_t)) => {
                if t.types.len() != other_t.types.len() {
                    return false;
                }
                t.types
                    .iter()
                    .zip(other_t.types.iter())
                    .all(|(ty, other_ty)| ty.is_subtype_of(other_ty, types))
            }
            (InterfaceType::Union(u), InterfaceType::Union(other_u)) => {
                if u.types.len() != other_u.types.len() {
                    return false;
                }
                u.types
                    .iter()
                    .zip(other_u.types.iter())
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

    pub(crate) fn type_size(&self) -> usize {
        match self {
            Self::Primitive(ty) => ty.type_size(),
            Self::Flags(_) | Self::Enum(_) => 1,
            Self::Record(r) => r.type_size,
            Self::Variant(v) => v.type_size,
            Self::Tuple(t) => t.type_size,
            Self::Union(u) => u.type_size,
            Self::List(ty) | Self::Option(ty) => ty.type_size(),
            Self::Expected(ok, error) => ok.type_size() + error.type_size(),
        }
    }

    fn push_wasm_types(&self, types: &TypeList, lowered_types: &mut LoweredTypes) -> bool {
        match self {
            Self::Primitive(ty) => push_primitive_wasm_types(ty, lowered_types),
            Self::Record(r) => r
                .fields
                .iter()
                .all(|(_, ty)| ty.push_wasm_types(types, lowered_types)),
            Self::Variant(v) => Self::push_variant_types(
                v.cases.iter().map(|(_, case)| &case.ty),
                types,
                lowered_types,
            ),
            Self::List(_) => lowered_types.push(Type::I32) && lowered_types.push(Type::I32),
            Self::Tuple(t) => t
                .types
                .iter()
                .all(|ty| ty.push_wasm_types(types, lowered_types)),
            Self::Flags(names) => {
                (0..(names.len() + 31) / 32).all(|_| lowered_types.push(Type::I32))
            }
            Self::Enum(_) => lowered_types.push(Type::I32),
            Self::Union(u) => Self::push_variant_types(u.types.iter(), types, lowered_types),
            Self::Option(ty) => Self::push_variant_types([ty].into_iter(), types, lowered_types),
            Self::Expected(ok, error) => {
                Self::push_variant_types([ok, error].into_iter(), types, lowered_types)
            }
        }
    }

    fn push_variant_types<'a>(
        cases: impl ExactSizeIterator<Item = &'a InterfaceTypeRef>,
        types: &TypeList,
        lowered_types: &mut LoweredTypes,
    ) -> bool {
        let pushed = if cases.len() <= u32::max_value() as usize {
            lowered_types.push(Type::I32)
        } else {
            lowered_types.push(Type::I64)
        };

        if !pushed {
            return false;
        }

        let start = lowered_types.len();

        for ty in cases {
            let mut temp = LoweredTypes::new(lowered_types.max);

            if !ty.push_wasm_types(types, &mut temp) {
                return false;
            }

            for (i, ty) in temp.iter().enumerate() {
                match lowered_types.get_mut(start + i) {
                    Some(prev) => *prev = Self::join_types(*prev, ty),
                    None => {
                        if !lowered_types.push(ty) {
                            return false;
                        }
                    }
                }
            }
        }

        true
    }

    fn join_types(a: Type, b: Type) -> Type {
        use Type::*;

        match (a, b) {
            (I32, I32) | (I64, I64) | (F32, F32) | (F64, F64) => a,
            (I32, F32) | (F32, I32) => I32,
            (_, I64 | F64) | (I64 | F64, _) => I64,
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
        self.types.get(id.index)
    }

    /// Gets a type id from a type index.
    ///
    /// Returns `None` if the type index is out of bounds.
    pub fn id_from_type_index(&self, index: u32) -> Option<TypeId> {
        let types = match &self.kind {
            TypesKind::Module(module) => &module.types,
            TypesKind::Component(component) => &component.types,
        };

        types.get(index as usize).copied()
    }

    /// Gets a defined type at the given type index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn type_at(&self, index: u32) -> Option<&TypeDef> {
        self.type_from_id(self.id_from_type_index(index)?)
    }

    /// Gets a defined core function type at the given type index.
    ///
    /// Returns `None` if the index is out of bounds.
    ///
    /// Additionally, this method always returns `None` for components
    /// because core function types are never present in a component's
    /// type index space.
    pub fn func_type_at(&self, index: u32) -> Option<&FuncType> {
        match self.type_at(index)? {
            TypeDef::Func(ty) => Some(ty),
            _ => None,
        }
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
        self.get(id.index).unwrap()
    }
}

impl<T> std::ops::IndexMut<TypeId> for SnapshotList<T> {
    fn index_mut(&mut self, id: TypeId) -> &mut T {
        self.get_mut(id.index).unwrap()
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
