//! State relating to validating a WebAssembly component.

use super::{check_max, core::Module, SnapshotList, TypeDef};
use crate::{
    limits::*, validator::core::EntityType, BinaryReaderError, CanonicalOption, ExternalKind,
    FuncType, GlobalType, MemoryType, Result, TableType, Type, WasmFeatures,
};
use std::collections::HashMap;

fn check_options(options: &[CanonicalOption], offset: usize) -> Result<()> {
    fn display(option: CanonicalOption) -> &'static str {
        match option {
            CanonicalOption::UTF8 => "utf8",
            CanonicalOption::UTF16 => "utf16",
            CanonicalOption::CompactUTF16 => "compact-utf16",
            CanonicalOption::Into(_) => "into",
        }
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

    Ok(())
}

#[derive(Clone)]
pub struct ModuleType {
    imports: HashMap<(String, String), Vec<EntityType>>,
    exports: HashMap<String, EntityType>,
}

impl ModuleType {
    fn is_subtype_of(&self, other: &Self) -> bool {
        self.imports
            .iter()
            .all(|(k, ty)| match other.imports.get(k) {
                Some(other) => ty[0].is_subtype_of(&other[0]),
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
        self.imports
            .iter()
            .all(|(k, ty)| match other.imports.get(k) {
                Some(other) => ty.is_subtype_of(other, types),
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
        other
            .exports
            .iter()
            .all(|(k, other)| match self.exports.get(k) {
                Some(ty) => ty.is_subtype_of(other, types),
                None => false,
            })
    }
}

#[derive(Clone, Eq)]
pub struct ComponentFuncType {
    params: Box<[InterfaceType]>,
    result: InterfaceType,
    core_type: FuncType,
}

impl PartialEq for ComponentFuncType {
    fn eq(&self, other: &Self) -> bool {
        self.params.iter().eq(other.params.iter()) && self.result.eq(&other.result)
    }
}

/// Represents an interface type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterfaceType {
    Unit,
    Bool,
    S8,
    U8,
    S16,
    U16,
    S32,
    U32,
    S64,
    U64,
    F32,
    F64,
    Char,
    String,
    /// The value is an index in the validator's type list.
    Compound(usize),
}

impl InterfaceType {
    fn push_wasm_types(
        &self,
        types: &SnapshotList<TypeDef>,
        offset: usize,
        wasm_types: &mut Vec<Type>,
    ) -> Result<()> {
        match self {
            Self::Unit => {}
            Self::Bool
            | Self::S8
            | Self::U8
            | Self::S16
            | Self::U16
            | Self::S32
            | Self::U32
            | Self::Char => {
                wasm_types.push(Type::I32);
            }
            Self::S64 | Self::U64 => {
                wasm_types.push(Type::I64);
            }
            Self::F32 => wasm_types.push(Type::F32),
            Self::F64 => wasm_types.push(Type::F64),
            Self::String => wasm_types.extend([Type::I32, Type::I32]),
            Self::Compound(idx) => match types[*idx].unwrap_compound_type() {
                CompoundType::Record(fields) => {
                    for ty in fields.iter() {
                        ty.push_wasm_types(types, offset, wasm_types)?;
                    }
                }
                CompoundType::Variant(cases) => {
                    Self::push_variant_types(cases.iter(), types, offset, wasm_types)?;
                }
                CompoundType::List(_) => {
                    wasm_types.extend([Type::I32, Type::I32]);
                }
                CompoundType::Tuple(tys) => {
                    for ty in tys.iter() {
                        ty.push_wasm_types(types, offset, wasm_types)?;
                    }
                }
                CompoundType::Flags(size) => {
                    if *size <= 32 {
                        wasm_types.push(Type::I32);
                    } else if *size <= 64 {
                        wasm_types.push(Type::I64);
                    } else {
                        for _ in 0..(*size + 31) / 32 {
                            wasm_types.push(Type::I32);
                        }
                    }
                }
                CompoundType::Enum(size) => {
                    if *size < u32::max_value() as usize {
                        wasm_types.push(Type::I32);
                    } else {
                        wasm_types.push(Type::I64);
                    }
                }
                CompoundType::Union(tys) => {
                    Self::push_variant_types(tys.iter(), types, offset, wasm_types)?;
                }
                CompoundType::Optional(ty) => {
                    Self::push_variant_types([ty].into_iter(), types, offset, wasm_types)?;
                }
                CompoundType::Expected(ok, error) => {
                    Self::push_variant_types([ok, error].into_iter(), types, offset, wasm_types)?;
                }
            },
        }

        Ok(())
    }

    fn push_variant_types<'a>(
        cases: impl ExactSizeIterator<Item = &'a InterfaceType>,
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

#[derive(Clone, Eq)]
pub enum CompoundType {
    Record(Box<[InterfaceType]>),
    Variant(Box<[InterfaceType]>),
    List(InterfaceType),
    Tuple(Box<[InterfaceType]>),
    Flags(usize),
    Enum(usize),
    Union(Box<[InterfaceType]>),
    Optional(InterfaceType),
    Expected(InterfaceType, InterfaceType),
}

impl PartialEq for CompoundType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Record(l), Self::Record(r))
            | (Self::Variant(l), Self::Variant(r))
            | (Self::Tuple(l), Self::Tuple(r))
            | (Self::Union(l), Self::Union(r)) => l == r,
            (Self::List(l), Self::List(r)) => l == r,
            (Self::Flags(l), Self::Flags(r)) | (Self::Enum(l), Self::Enum(r)) => l == r,
            (Self::Optional(l), Self::Optional(r)) => l == r,
            (Self::Expected(lo, le), Self::Expected(ro, re)) => lo == ro && le == re,
            _ => false,
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
    pub values: Vec<(InterfaceType, bool)>,
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
        &mut self,
        def: crate::ComponentTypeDef,
        features: &WasmFeatures,
        parents: &[Self],
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        let def = match def {
            crate::ComponentTypeDef::Module(defs) => TypeDef::Module(self.create_module_type(
                defs.into_vec(),
                features,
                types,
                offset,
            )?),
            crate::ComponentTypeDef::Component(defs) => TypeDef::Component(
                self.create_component_type(defs.into_vec(), features, parents, types, offset)?,
            ),
            crate::ComponentTypeDef::Instance(defs) => TypeDef::Instance(
                self.create_instance_type(defs.into_vec(), features, parents, types, offset)?,
            ),
            crate::ComponentTypeDef::Function(ty) => {
                TypeDef::ComponentFunc(self.create_function_type(ty, types, offset)?)
            }
            crate::ComponentTypeDef::Value(ty) => {
                TypeDef::Value(self.create_interface_type(ty, types, offset)?)
            }
            crate::ComponentTypeDef::Compound(ct) => {
                TypeDef::Compound(self.create_compound_type(ct, types, offset)?)
            }
        };

        self.types.push(types.len());
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

        if ty.core_type != *core_ty {
            return Err(BinaryReaderError::new(
                "lowered function type does not match core function type",
                offset,
            ));
        }

        check_options(&options, offset)?;
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
        let ty = self
            .function_type_at(func_index, types, offset)?
            .core_type
            .clone();

        check_options(&options, offset)?;

        self.functions.push(types.len());
        types.push(TypeDef::Func(ty));

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
                self.instantiate_module(index, args.to_vec(), types, offset)?
            }
            crate::Instance::Component { index, args } => {
                self.instantiate_component(index, args.to_vec(), types, offset)?
            }
            crate::Instance::FromExports(exports) => {
                self.instantiate_exports(exports.to_vec(), types, offset)?
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

        for (i, (ty, arg)) in ft.params.iter().zip(args).enumerate() {
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
            InterfaceType::Unit => {}
            ty => {
                self.values.push((ty, false));
            }
        }

        self.has_start = true;

        Ok(())
    }

    pub(super) fn add_alias(
        &mut self,
        alias: crate::Alias,
        parents: &[Self],
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        match alias {
            crate::Alias::InstanceExport {
                kind,
                instance,
                name,
            } => self.alias_instance_export(kind, instance, name, types, offset),
            crate::Alias::OuterModule { count, index } => {
                check_max(self.modules.len(), 1, MAX_WASM_MODULES, "modules", offset)?;
                self.alias_module(count, index, parents, offset)
            }
            crate::Alias::OuterComponent { count, index } => {
                check_max(
                    self.components.len(),
                    1,
                    MAX_WASM_COMPONENTS,
                    "components",
                    offset,
                )?;
                self.alias_component(count, index, parents, offset)
            }
            crate::Alias::OuterType { count, index } => {
                check_max(self.types.len(), 1, MAX_WASM_TYPES, "types", offset)?;
                self.alias_type(count, index, parents, offset)
            }
        }
    }

    fn alias_type(
        &mut self,
        count: u32,
        index: u32,
        parents: &[ComponentState],
        offset: usize,
    ) -> Result<()> {
        let count = count as usize;
        let ty = if count == 0 {
            self.type_at(index, offset)?
        } else {
            if count > parents.len() {
                return Err(BinaryReaderError::new(
                    format!("invalid outer alias count of {}", count),
                    offset,
                ));
            }

            let parent = &parents[parents.len() - count];
            parent.type_at(index, offset)?
        };

        self.types.push(ty);

        Ok(())
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
            crate::ComponentExportKind::Type(ty) => {
                ComponentEntityType::Type(self.create_interface_type(*ty, types, offset)?)
            }
            crate::ComponentExportKind::InstanceFromExports(exports) => {
                ComponentEntityType::Instance(self.instantiate_exports(
                    exports.to_vec(),
                    types,
                    offset,
                )?)
            }
        })
    }

    pub fn core_export_to_entity_type(
        &self,
        export: &crate::Export,
        offset: usize,
    ) -> Result<EntityType> {
        let check = |ty: &str, index: u32, total: usize| {
            if index as usize >= total {
                Err(BinaryReaderError::new(
                    format!(
                        "unknown {ty} {index}: exported {ty} index out of bounds",
                        index = index,
                        ty = ty,
                    ),
                    offset,
                ))
            } else {
                Ok(())
            }
        };

        Ok(match export.kind {
            ExternalKind::Func => {
                check("function", export.index, self.functions.len())?;
                EntityType::Func(self.types[self.functions[export.index as usize] as usize])
            }
            ExternalKind::Table => {
                check("table", export.index, self.tables.len())?;
                EntityType::Table(self.tables[export.index as usize])
            }
            ExternalKind::Memory => {
                check("memory", export.index, self.memories.len())?;
                EntityType::Memory(self.memories[export.index as usize])
            }
            ExternalKind::Global => {
                check("global", export.index, self.globals.len())?;
                EntityType::Global(self.globals[export.index as usize])
            }
            ExternalKind::Tag => {
                check("tag", export.index, self.tags.len())?;
                EntityType::Tag(self.types[self.tags[export.index as usize] as usize])
            }
        })
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
            TypeDef::ComponentFunc(_) => ComponentEntityType::Component(self.types[ty as usize]),
            TypeDef::Value(ty) => ComponentEntityType::Value(*ty),
            TypeDef::Func(_) => {
                return Err(BinaryReaderError::new(
                    format!("core WebAssembly function types cannot be {}", desc),
                    offset,
                ))
            }
            TypeDef::Compound(_) => {
                return Err(BinaryReaderError::new(
                    format!("compound types cannot be {}", desc),
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
        &self,
        defs: Vec<crate::ComponentType>,
        features: &WasmFeatures,
        parents: &[ComponentState],
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<ComponentType> {
        let mut state = ComponentState::default();

        // TODO: the state should be pushed on parents for recursive component types

        for def in defs {
            match def {
                crate::ComponentType::Type(ty) => {
                    state.add_type(ty, features, parents, types, offset)?;
                }
                crate::ComponentType::Export { name, ty } => {
                    state.add_export(
                        name,
                        self.type_index_to_entity_type(ty, types, "exported", offset)?,
                        offset,
                    )?;
                }
                crate::ComponentType::Import(import) => {
                    state.add_import(import, types, offset)?;
                }
                crate::ComponentType::OuterType { count, index } => {
                    state.alias_type(count, index, parents, offset)?;
                }
            };
        }

        Ok(ComponentType {
            imports: state.imports,
            exports: state.exports,
        })
    }

    fn create_instance_type(
        &self,
        defs: Vec<crate::InstanceType>,
        features: &WasmFeatures,
        parents: &[ComponentState],
        types: &mut SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<InstanceType> {
        let mut state = ComponentState::default();

        // TODO: the state should be pushed on parents for recursive instance types

        for def in defs {
            match def {
                crate::InstanceType::Type(ty) => {
                    state.add_type(ty, features, parents, types, offset)?;
                }
                crate::InstanceType::Export { name, ty } => {
                    state.add_export(
                        name,
                        self.type_index_to_entity_type(ty, types, "exported", offset)?,
                        offset,
                    )?;
                }
                crate::InstanceType::OuterType { count, index } => {
                    state.alias_type(count, index, parents, offset)?;
                }
            };
        }

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
                let ty = self.create_interface_type(*ty, types, offset)?;
                ty.push_wasm_types(types, offset, &mut core_params)?;
                Ok(ty)
            })
            .collect::<Result<_>>()?;

        let result = self.create_interface_type(ty.result, types, offset)?;
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

    fn create_compound_type(
        &self,
        ct: crate::CompoundType,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<CompoundType> {
        Ok(match ct {
            crate::CompoundType::Record(fields) => CompoundType::Record(
                fields
                    .iter()
                    .map(|(name, ty)| {
                        Self::check_name(name, "record field", offset)?;
                        self.create_interface_type(*ty, types, offset)
                    })
                    .collect::<Result<_>>()?,
            ),
            crate::CompoundType::Variant { cases, default } => {
                if let Some(default) = default {
                    if default >= cases.len() as u32 {
                        return Err(BinaryReaderError::new(
                            format!("variant default index {} is out of bounds", default),
                            offset,
                        ));
                    }
                }
                CompoundType::Variant(
                    cases
                        .iter()
                        .map(|(name, ty)| {
                            Self::check_name(name, "variant case", offset)?;
                            self.create_interface_type(*ty, types, offset)
                        })
                        .collect::<Result<_>>()?,
                )
            }
            crate::CompoundType::List(ty) => {
                CompoundType::List(self.create_interface_type(ty, types, offset)?)
            }
            crate::CompoundType::Tuple(tys) => CompoundType::Tuple(
                tys.iter()
                    .map(|ty| self.create_interface_type(*ty, types, offset))
                    .collect::<Result<_>>()?,
            ),
            crate::CompoundType::Flags(names) => {
                for name in names.iter() {
                    Self::check_name(name, "flag", offset)?
                }

                CompoundType::Flags(names.len())
            }
            crate::CompoundType::Enum(names) => {
                for name in names.iter() {
                    Self::check_name(name, "enum tag", offset)?
                }

                CompoundType::Enum(names.len())
            }
            crate::CompoundType::Union(tys) => CompoundType::Union(
                tys.iter()
                    .map(|ty| self.create_interface_type(*ty, types, offset))
                    .collect::<Result<_>>()?,
            ),
            crate::CompoundType::Optional(ty) => {
                CompoundType::Optional(self.create_interface_type(ty, types, offset)?)
            }
            crate::CompoundType::Expected { ok, error } => CompoundType::Expected(
                self.create_interface_type(ok, types, offset)?,
                self.create_interface_type(error, types, offset)?,
            ),
        })
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

    fn compound_type_at<'a>(
        &self,
        idx: u32,
        types: &'a SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<&'a CompoundType> {
        if let TypeDef::Compound(ty) = &types[self.type_at(idx, offset)?] {
            Ok(ty)
        } else {
            Err(BinaryReaderError::new(
                format!("type index {} is not a compound type", idx),
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
                    let instance_type =
                        types[self.module_instance_at(idx, types, offset)?].unwrap_module_type();
                    for (name, ty) in instance_type.exports.iter() {
                        insert_arg(module_arg.name, name, ty.clone(), &mut args, offset)?;
                    }
                }
                crate::ModuleArgKind::InstanceFromExports(exports) => {
                    for export in exports.to_vec() {
                        let ty = self.core_export_to_entity_type(&export, offset)?;
                        insert_arg(module_arg.name, export.name, ty, &mut args, offset)?;
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
                crate::ComponentArgKind::InstanceFromExports(exports) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Instance(self.instantiate_exports(
                            exports.to_vec(),
                            types,
                            offset,
                        )?),
                        &mut args,
                        offset,
                    )?;
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
                crate::ComponentExportKind::Type(ty) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Type(self.create_interface_type(ty, types, offset)?),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::InstanceFromExports(exports) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Instance(self.instantiate_exports(
                            exports.to_vec(),
                            types,
                            offset,
                        )?),
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

    fn alias_instance_export(
        &mut self,
        kind: crate::AliasKind,
        idx: u32,
        name: &str,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<()> {
        macro_rules! push_module_export {
            ($expected:path, $collection:ident, $ty:literal) => {{
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
            ($expected:path, $collection:ident, $ty:literal) => {{
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
                check_max(self.modules.len(), 1, MAX_WASM_MODULES, "modules", offset)?;
                push_component_export!(ComponentEntityType::Module, modules, "module")
            }
            crate::AliasKind::Component => {
                check_max(
                    self.components.len(),
                    1,
                    MAX_WASM_COMPONENTS,
                    "components",
                    offset,
                )?;
                push_component_export!(ComponentEntityType::Component, components, "component")
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
            crate::AliasKind::Function => {
                check_max(
                    self.functions.len(),
                    1,
                    MAX_WASM_FUNCTIONS,
                    "functions",
                    offset,
                )?;
                self.functions
                    .push(self.instance_exported_function(idx, name, types, offset)?);
                Ok(())
            }
            crate::AliasKind::Table => {
                check_max(self.tables.len(), 1, MAX_WASM_TABLES, "tables", offset)?;
                push_module_export!(EntityType::Table, tables, "table")
            }
            crate::AliasKind::Memory => {
                check_max(
                    self.memories.len(),
                    1,
                    MAX_WASM_MEMORIES,
                    "memories",
                    offset,
                )?;
                push_module_export!(EntityType::Memory, memories, "memory")
            }
            crate::AliasKind::Global => {
                check_max(self.globals.len(), 1, MAX_WASM_GLOBALS, "globals", offset)?;
                push_module_export!(EntityType::Global, globals, "global")
            }
            crate::AliasKind::Tag => {
                check_max(self.tags.len(), 1, MAX_WASM_TAGS, "tags", offset)?;
                push_module_export!(EntityType::Tag, tags, "tag")
            }
        }
    }

    fn alias_module(
        &mut self,
        count: u32,
        index: u32,
        parents: &[Self],
        offset: usize,
    ) -> Result<()> {
        let count = count as usize;
        let ty = if count == 0 {
            self.module_at(index, offset)?;
            self.modules[index as usize]
        } else {
            if count > parents.len() {
                return Err(BinaryReaderError::new(
                    format!("invalid outer alias count of {}", count),
                    offset,
                ));
            }

            let parent = &parents[parents.len() - count];
            parent.module_at(index, offset)?;
            parent.modules[index as usize]
        };

        self.modules.push(ty);

        Ok(())
    }

    fn alias_component(
        &mut self,
        count: u32,
        index: u32,
        parents: &[Self],
        offset: usize,
    ) -> Result<()> {
        let count = count as usize;
        let ty = if count == 0 {
            self.component_at(index, offset)?;
            self.components[index as usize]
        } else {
            if count > parents.len() {
                return Err(BinaryReaderError::new(
                    format!("invalid outer alias count of {}", count),
                    offset,
                ));
            }

            let parent = &parents[parents.len() - count];
            parent.component_at(index, offset)?;
            parent.components[index as usize]
        };

        self.components.push(ty);

        Ok(())
    }

    fn create_interface_type(
        &self,
        ty: crate::InterfaceType,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<InterfaceType> {
        Ok(match ty {
            crate::InterfaceType::Unit => InterfaceType::Unit,
            crate::InterfaceType::Bool => InterfaceType::Bool,
            crate::InterfaceType::S8 => InterfaceType::S8,
            crate::InterfaceType::U8 => InterfaceType::U8,
            crate::InterfaceType::S16 => InterfaceType::S16,
            crate::InterfaceType::U16 => InterfaceType::U16,
            crate::InterfaceType::S32 => InterfaceType::S32,
            crate::InterfaceType::U32 => InterfaceType::U32,
            crate::InterfaceType::S64 => InterfaceType::S64,
            crate::InterfaceType::U64 => InterfaceType::U64,
            crate::InterfaceType::F32 => InterfaceType::F32,
            crate::InterfaceType::F64 => InterfaceType::F64,
            crate::InterfaceType::Char => InterfaceType::Char,
            crate::InterfaceType::String => InterfaceType::String,
            crate::InterfaceType::Compound(idx) => {
                self.compound_type_at(idx, types, offset)?;
                InterfaceType::Compound(self.types[idx as usize])
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

    fn module_instance_at(
        &self,
        idx: u32,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        let ty = self.instance_at(idx, offset)?;
        match &types[self.instance_at(idx, offset)?] {
            TypeDef::Module(_) => Ok(ty),
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

    fn instance_exported_function(
        &self,
        idx: u32,
        name: &str,
        types: &SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<usize> {
        match &types[self.instance_at(idx, offset)?] {
            TypeDef::Instance(ty) => {
                if let Some(ty) = ty.exports.get(name) {
                    match ty {
                        ComponentEntityType::Func(ty) => return Ok(*ty),
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!("export `{}` for instance {} is not a function", name, idx),
                                offset,
                            ))
                        }
                    }
                }
            }
            TypeDef::Module(ty) => {
                if let Some(ty) = ty.exports.get(name) {
                    match ty {
                        EntityType::Func(ty) => return Ok(*ty),
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!("export `{}` for instance {} is not a function", name, idx),
                                offset,
                            ))
                        }
                    }
                }
            }
            _ => unreachable!(),
        }

        Err(BinaryReaderError::new(
            format!("instance {} has no export named `{}`", idx, name),
            offset,
        ))
    }

    fn module_instance_export<'a>(
        &self,
        idx: u32,
        name: &str,
        types: &'a SnapshotList<TypeDef>,
        offset: usize,
    ) -> Result<&'a EntityType> {
        match types[self.module_instance_at(idx, types, offset)?]
            .unwrap_module_type()
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

    fn value_at(&mut self, idx: u32, offset: usize) -> Result<&InterfaceType> {
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
    Value(InterfaceType),
    Type(InterfaceType),
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
            (Self::Func(ty), Self::Func(other_ty)) => ty == other_ty,
            (Self::Value(ty), Self::Value(other_ty)) => ty == other_ty,
            (Self::Type(ty), Self::Type(other_ty)) => ty == other_ty,
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
