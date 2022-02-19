//! State relating to validating a WebAssembly component.

use super::{check_max, core::Module};
use crate::{
    limits::*,
    validator::core::{EntityType, ModuleTypeSpace},
    BinaryReaderError, CanonicalOption, ExternalKind, FuncType, GlobalType, MemoryType, Result,
    TableType, Type, WasmFeatures,
};
use std::{collections::HashMap, rc::Rc, sync::Arc};

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
struct ModuleType {
    imports: Rc<HashMap<(String, String), EntityType>>,
    exports: Rc<HashMap<String, EntityType>>,
}

impl ModuleType {
    fn is_subtype_of(&self, other: &Self) -> bool {
        self.imports
            .iter()
            .all(|(k, ty)| match other.imports.get(k) {
                Some(other) => ty.is_subtype_of(other),
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
struct ModuleInstanceType {
    exports: Rc<HashMap<String, EntityType>>,
}

#[derive(Clone)]
struct ComponentType {
    imports: Rc<HashMap<String, ComponentEntityType>>,
    exports: Rc<HashMap<String, ComponentEntityType>>,
}

impl ComponentType {
    fn is_subtype_of(&self, other: &Self) -> bool {
        self.imports
            .iter()
            .all(|(k, ty)| match other.imports.get(k) {
                Some(other) => ty.is_subtype_of(other),
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
struct InstanceType {
    exports: Rc<HashMap<String, ComponentEntityType>>,
}

impl InstanceType {
    fn is_subtype_of(&self, other: &Self) -> bool {
        other
            .exports
            .iter()
            .all(|(k, other)| match self.exports.get(k) {
                Some(ty) => ty.is_subtype_of(other),
                None => false,
            })
    }
}

#[derive(Clone, Eq)]
struct ComponentFuncType {
    params: Rc<[(String, InterfaceType)]>,
    result: InterfaceType,
    core_type: Arc<FuncType>,
}

impl PartialEq for ComponentFuncType {
    fn eq(&self, other: &Self) -> bool {
        self.params
            .iter()
            .map(|(_, ty)| ty)
            .eq(other.params.iter().map(|(_, ty)| ty))
            && self.result.eq(&other.result)
    }
}

#[derive(Clone, Eq)]
enum CompoundType {
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

#[derive(Clone)]
enum ComponentTypeDef {
    Module(ModuleType),
    Component(ComponentType),
    Instance(InstanceType),
    Func(ComponentFuncType),
    Value(InterfaceType),
    Compound(Rc<CompoundType>),
}

#[derive(Default)]
pub struct ComponentState {
    types: ComponentTypeSpace,
    modules: Vec<ModuleType>,
    components: Vec<ComponentType>,
    instances: Vec<InstanceIndexType>,
    functions: Vec<FuncIndexType>,
    values: Vec<(InterfaceType, bool)>,
    memories: Vec<MemoryType>,
    tables: Vec<TableType>,
    globals: Vec<GlobalType>,
    tags: Vec<Arc<FuncType>>,
    has_start: bool,
    imports: HashMap<String, ComponentEntityType>,
    exports: HashMap<String, ComponentEntityType>,
}

impl ComponentState {
    pub fn type_count(&self) -> usize {
        self.types.len()
    }

    pub fn function_count(&self) -> usize {
        self.functions.len()
    }

    pub fn module_count(&self) -> usize {
        self.modules.len()
    }

    pub fn component_count(&self) -> usize {
        self.components.len()
    }

    pub fn instance_count(&self) -> usize {
        self.instances.len()
    }

    pub fn export_count(&self) -> usize {
        self.exports.len()
    }

    pub fn add_type(
        &mut self,
        def: crate::ComponentTypeDef,
        features: &WasmFeatures,
        parents: &[Self],
        offset: usize,
    ) -> Result<()> {
        self.types.type_def(def, features, parents, offset)
    }

    pub fn add_import(&mut self, import: crate::ComponentImport, offset: usize) -> Result<()> {
        let ty = self.types.get(import.ty, offset)?;
        let (len, max, desc) = match ty {
            ComponentTypeDef::Module(mt) => {
                self.modules.push(mt.clone());
                (self.modules.len(), MAX_WASM_MODULES, "modules")
            }
            ComponentTypeDef::Component(ct) => {
                self.components.push(ct.clone());
                (self.components.len(), MAX_WASM_COMPONENTS, "components")
            }
            ComponentTypeDef::Instance(it) => {
                self.instances
                    .push(InstanceIndexType::Component(it.clone()));
                (self.instances.len(), MAX_WASM_INSTANCES, "instances")
            }
            ComponentTypeDef::Func(ft) => {
                self.functions.push(FuncIndexType::Component(ft.clone()));
                (self.functions.len(), MAX_WASM_FUNCTIONS, "functions")
            }
            ComponentTypeDef::Value(it) => {
                self.values.push((it.clone(), false));
                (self.values.len(), MAX_WASM_VALUES, "values")
            }
            ComponentTypeDef::Compound(_) => {
                return Err(BinaryReaderError::new(
                    "cannot import compound types",
                    offset,
                ));
            }
        };

        check_max(len, 0, max, desc, offset)?;

        if self
            .imports
            .insert(
                import.name.to_string(),
                ComponentEntityType::from_type_def(ty.clone(), "import", offset)?,
            )
            .is_some()
        {
            return Err(BinaryReaderError::new(
                format!("duplicate import name `{}` already defined", import.name),
                offset,
            ));
        }

        Ok(())
    }

    pub fn add_export(&mut self, export: crate::ComponentExport, offset: usize) -> Result<()> {
        let ty = match export.kind {
            crate::ComponentExportKind::Module(idx) => {
                ComponentEntityType::Module(self.module_at(idx, offset)?.clone())
            }
            crate::ComponentExportKind::Component(idx) => {
                ComponentEntityType::Component(self.component_at(idx, offset)?.clone())
            }
            crate::ComponentExportKind::Instance(idx) => {
                ComponentEntityType::Instance(self.component_instance_at(idx, offset)?.clone())
            }
            crate::ComponentExportKind::Function(idx) => {
                ComponentEntityType::Func(self.component_function_at(idx, offset)?.clone())
            }
            crate::ComponentExportKind::Value(idx) => {
                ComponentEntityType::Value(self.value_at(idx, offset)?.clone())
            }
            crate::ComponentExportKind::Type(ty) => {
                ComponentEntityType::Type(InterfaceType::new(ty, &self.types, offset)?)
            }
            crate::ComponentExportKind::InstanceFromExports(exports) => {
                ComponentEntityType::Instance(self.instantiate_exports(exports.as_ref(), offset)?)
            }
        };

        if self.exports.insert(export.name.to_string(), ty).is_some() {
            return Err(BinaryReaderError::new(
                format!("duplicate export name `{}` already defined", export.name),
                offset,
            ));
        }

        Ok(())
    }

    pub fn lift_function(
        &mut self,
        type_index: u32,
        func_index: u32,
        options: &[CanonicalOption],
        offset: usize,
    ) -> Result<()> {
        let ty = self.types.function_type_at(type_index, offset)?;
        let core_ty = self.core_function_at(func_index, offset)?;

        if ty.core_type != *core_ty {
            return Err(BinaryReaderError::new(
                "lowered function type does not match core function type",
                offset,
            ));
        }

        check_options(options, offset)?;
        self.functions.push(FuncIndexType::Component(ty.clone()));

        Ok(())
    }

    pub fn lower_function(
        &mut self,
        func_index: u32,
        options: &[CanonicalOption],
        offset: usize,
    ) -> Result<()> {
        let ty = self.types.function_type_at(func_index, offset)?;
        check_options(options, offset)?;

        self.functions
            .push(FuncIndexType::Core(ty.core_type.clone()));

        Ok(())
    }

    pub fn add_module(&mut self, module: &Module, offset: usize) -> Result<()> {
        let imports = module
            .imports()
            .map(|((module, name), v)| {
                if v.len() != 1 {
                    return Err(BinaryReaderError::new(
                        format!(
                            "module has a duplicate import name `{}:{}` that is not allowed in components",
                            module, name
                        ),
                        offset,
                    ));
                }
                Ok(((module.clone(), name.clone()), v[0].clone()))
            })
            .collect::<Result<_>>()?;

        let exports = module
            .exports()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        self.modules.push(ModuleType {
            imports: Rc::new(imports),
            exports: Rc::new(exports),
        });

        Ok(())
    }

    pub fn add_component(&mut self, state: Self) {
        self.components.push(ComponentType {
            imports: Rc::new(state.imports),
            exports: Rc::new(state.exports),
        });
    }

    pub fn add_instance(&mut self, instance: crate::Instance, offset: usize) -> Result<()> {
        let instance = match instance {
            crate::Instance::Module { index, args } => {
                InstanceIndexType::Module(self.instantiate_module(index, args.as_ref(), offset)?)
            }
            crate::Instance::Component { index, args } => InstanceIndexType::Component(
                self.instantiate_component(index, args.as_ref(), offset)?,
            ),
            crate::Instance::FromExports(exports) => {
                InstanceIndexType::Component(self.instantiate_exports(exports.as_ref(), offset)?)
            }
        };

        self.instances.push(instance);

        Ok(())
    }

    pub fn add_start(&mut self, func_index: u32, args: &[u32], offset: usize) -> Result<()> {
        if self.has_start {
            return Err(BinaryReaderError::new(
                "component cannot have more than one start function",
                offset,
            ));
        }

        let ft = self.component_function_at(func_index, offset)?.clone();

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
            InterfaceType::Unit => {}
            ty => {
                self.values.push((ty, false));
            }
        }

        self.has_start = true;

        Ok(())
    }

    pub fn add_alias(
        &mut self,
        alias: crate::Alias,
        parents: &[Self],
        offset: usize,
    ) -> Result<()> {
        match alias {
            crate::Alias::InstanceExport {
                kind,
                instance,
                name,
            } => self.alias_instance_export(kind, instance, name, offset),
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
                self.types.alias_type(count, index, parents, offset)
            }
        }
    }

    fn instantiate_module(
        &self,
        module_index: u32,
        module_args: &[crate::ModuleArg],
        offset: usize,
    ) -> Result<ModuleInstanceType> {
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
            match &module_arg.kind {
                crate::ModuleArgKind::Instance(idx) => {
                    let instance_type = self.module_instance_at(*idx, offset)?;
                    for (name, ty) in instance_type.exports.iter() {
                        insert_arg(module_arg.name, name, ty.clone(), &mut args, offset)?;
                    }
                }
                crate::ModuleArgKind::InstanceFromExports(exports) => {
                    for export in exports.iter() {
                        insert_arg(
                            module_arg.name,
                            export.name,
                            self.export_to_entity_type(export, offset)?,
                            &mut args,
                            offset,
                        )?;
                    }
                }
            }
        }

        // Validate the arguments
        for ((module, name), b) in module_type.imports.iter() {
            match args.get(&(module.as_str(), name.as_str())) {
                Some(a) => {
                    let desc = match (a, b) {
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
                                b.desc()
                            ),
                                offset,
                            ))
                        }
                    };

                    if !a.is_subtype_of(b) {
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

        Ok(ModuleInstanceType {
            exports: module_type.exports.clone(),
        })
    }

    fn instantiate_component(
        &mut self,
        component_index: u32,
        component_args: &[crate::ComponentArg],
        offset: usize,
    ) -> Result<InstanceType> {
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

        let ty = self.component_at(component_index, offset)?.clone();
        let mut args = HashMap::new();

        // Populate the arguments
        for component_arg in component_args {
            match &component_arg.kind {
                crate::ComponentArgKind::Module(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Module(self.module_at(*idx, offset)?.clone()),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Component(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Component(self.component_at(*idx, offset)?.clone()),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Instance(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Instance(
                            self.component_instance_at(*idx, offset)?.clone(),
                        ),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Function(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Func(
                            self.component_function_at(*idx, offset)?.clone(),
                        ),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Value(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Value(self.value_at(*idx, offset)?.clone()),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::InstanceFromExports(exports) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Instance(
                            self.instantiate_exports(exports.as_ref(), offset)?,
                        ),
                        &mut args,
                        offset,
                    )?;
                }
            }
        }

        // Validate the arguments
        for (name, b) in ty.imports.iter() {
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

                    if !a.is_subtype_of(b) {
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

        Ok(InstanceType {
            exports: ty.exports,
        })
    }

    fn instantiate_exports(
        &mut self,
        exports: &[crate::ComponentExport],
        offset: usize,
    ) -> Result<InstanceType> {
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
            match &export.kind {
                crate::ComponentExportKind::Module(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Module(self.module_at(*idx, offset)?.clone()),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Component(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Component(self.component_at(*idx, offset)?.clone()),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Instance(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Instance(
                            self.component_instance_at(*idx, offset)?.clone(),
                        ),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Function(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Func(
                            self.component_function_at(*idx, offset)?.clone(),
                        ),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Value(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Value(self.value_at(*idx, offset)?.clone()),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Type(ty) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Type(InterfaceType::new(*ty, &self.types, offset)?),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::InstanceFromExports(exports) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Instance(
                            self.instantiate_exports(exports.as_ref(), offset)?,
                        ),
                        &mut inst_exports,
                        offset,
                    )?;
                }
            }
        }

        Ok(InstanceType {
            exports: Rc::new(inst_exports),
        })
    }

    fn alias_instance_export(
        &mut self,
        kind: crate::AliasKind,
        idx: u32,
        name: &str,
        offset: usize,
    ) -> Result<()> {
        macro_rules! push_module_export {
            ($expected:path, $collection:ident, $ty:literal) => {{
                match self.module_instance_export(idx, name, offset)?.clone() {
                    $expected(ty) => {
                        self.$collection.push(ty);
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
                match self.component_instance_export(idx, name, offset)?.clone() {
                    $expected(ty) => {
                        self.$collection.push(ty);
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
                match self.component_instance_export(idx, name, offset)?.clone() {
                    ComponentEntityType::Instance(ty) => {
                        self.instances.push(InstanceIndexType::Component(ty));
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
                match self.component_instance_export(idx, name, offset)?.clone() {
                    ComponentEntityType::Value(ty) => {
                        self.values.push((ty, false));
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
                    .push(self.instance_exported_function(idx, name, offset)?);
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
            self.module_at(index, offset)?.clone()
        } else {
            if count > parents.len() {
                return Err(BinaryReaderError::new(
                    format!("invalid outer alias count of {}", count),
                    offset,
                ));
            }

            parents[parents.len() - count]
                .module_at(index, offset)?
                .clone()
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
            self.component_at(index, offset)?.clone()
        } else {
            if count > parents.len() {
                return Err(BinaryReaderError::new(
                    format!("invalid outer alias count of {}", count),
                    offset,
                ));
            }

            parents[parents.len() - count]
                .component_at(index, offset)?
                .clone()
        };

        self.components.push(ty);

        Ok(())
    }

    fn export_to_entity_type(&self, export: &crate::Export, offset: usize) -> Result<EntityType> {
        Ok(match export.kind {
            ExternalKind::Func => {
                EntityType::Func(self.core_function_at(export.index, offset)?.clone())
            }
            ExternalKind::Table => EntityType::Table(*self.table_at(export.index, offset)?),
            ExternalKind::Memory => EntityType::Memory(*self.memory_at(export.index, offset)?),
            ExternalKind::Global => EntityType::Global(*self.global_at(export.index, offset)?),
            ExternalKind::Tag => EntityType::Tag(self.tag_at(export.index, offset)?.clone()),
        })
    }

    fn function_at(&self, idx: u32, offset: usize) -> Result<&FuncIndexType> {
        match self.functions.get(idx as usize) {
            Some(f) => Ok(f),
            None => Err(BinaryReaderError::new(
                format!("unknown function {}: function index out of bounds", idx),
                offset,
            )),
        }
    }

    fn component_function_at(&self, idx: u32, offset: usize) -> Result<&ComponentFuncType> {
        match self.function_at(idx, offset)? {
            FuncIndexType::Component(c) => Ok(c),
            FuncIndexType::Core(_) => Err(BinaryReaderError::new(
                format!("function {} is not a component function", idx),
                offset,
            )),
        }
    }

    fn core_function_at(&self, idx: u32, offset: usize) -> Result<&Arc<FuncType>> {
        match self.function_at(idx, offset)? {
            FuncIndexType::Core(c) => Ok(c),
            FuncIndexType::Component(_) => Err(BinaryReaderError::new(
                format!("function {} is not a core WebAssembly function", idx),
                offset,
            )),
        }
    }

    fn module_at(&self, idx: u32, offset: usize) -> Result<&ModuleType> {
        match self.modules.get(idx as usize) {
            Some(m) => Ok(m),
            None => Err(BinaryReaderError::new(
                format!("unknown module {}: module index out of bounds", idx),
                offset,
            )),
        }
    }

    fn component_at(&self, idx: u32, offset: usize) -> Result<&ComponentType> {
        match self.components.get(idx as usize) {
            Some(c) => Ok(c),
            None => Err(BinaryReaderError::new(
                format!("unknown component {}: component index out of bounds", idx),
                offset,
            )),
        }
    }

    fn instance_at(&self, idx: u32, offset: usize) -> Result<&InstanceIndexType> {
        match self.instances.get(idx as usize) {
            Some(i) => Ok(i),
            None => Err(BinaryReaderError::new(
                format!("unknown instance {}: instance index out of bounds", idx),
                offset,
            )),
        }
    }

    fn module_instance_at(&self, idx: u32, offset: usize) -> Result<&ModuleInstanceType> {
        match self.instance_at(idx, offset)? {
            InstanceIndexType::Module(m) => Ok(m),
            _ => Err(BinaryReaderError::new(
                format!("instance {} is not a module instance", idx),
                offset,
            )),
        }
    }

    fn component_instance_at(&self, idx: u32, offset: usize) -> Result<&InstanceType> {
        match self.instance_at(idx, offset)? {
            InstanceIndexType::Component(c) => Ok(c),
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
        offset: usize,
    ) -> Result<FuncIndexType> {
        match self.instance_at(idx, offset)? {
            InstanceIndexType::Component(ty) => {
                if let Some(ty) = ty.exports.get(name) {
                    match ty {
                        ComponentEntityType::Func(ty) => {
                            return Ok(FuncIndexType::Component(ty.clone()))
                        }
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!("export `{}` for instance {} is not a function", name, idx),
                                offset,
                            ))
                        }
                    }
                }
            }
            InstanceIndexType::Module(ty) => {
                if let Some(ty) = ty.exports.get(name) {
                    match ty {
                        EntityType::Func(ty) => return Ok(FuncIndexType::Core(ty.clone())),
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!("export `{}` for instance {} is not a function", name, idx),
                                offset,
                            ))
                        }
                    }
                }
            }
        }

        Err(BinaryReaderError::new(
            format!("instance {} has no export named `{}`", idx, name),
            offset,
        ))
    }

    fn module_instance_export(&self, idx: u32, name: &str, offset: usize) -> Result<&EntityType> {
        match self.module_instance_at(idx, offset)?.exports.get(name) {
            Some(export) => Ok(export),
            None => {
                return Err(BinaryReaderError::new(
                    format!("instance {} has no export named `{}`", idx, name),
                    offset,
                ))
            }
        }
    }

    fn component_instance_export(
        &self,
        idx: u32,
        name: &str,
        offset: usize,
    ) -> Result<&ComponentEntityType> {
        match self.component_instance_at(idx, offset)?.exports.get(name) {
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

    fn memory_at(&self, idx: u32, offset: usize) -> Result<&MemoryType> {
        match self.memories.get(idx as usize) {
            Some(m) => Ok(m),
            None => Err(BinaryReaderError::new(
                format!("unknown memory {}: memory index out of bounds", idx),
                offset,
            )),
        }
    }

    fn table_at(&self, idx: u32, offset: usize) -> Result<&TableType> {
        match self.tables.get(idx as usize) {
            Some(m) => Ok(m),
            None => Err(BinaryReaderError::new(
                format!("unknown table {}: table index out of bounds", idx),
                offset,
            )),
        }
    }

    fn global_at(&self, idx: u32, offset: usize) -> Result<&GlobalType> {
        match self.globals.get(idx as usize) {
            Some(g) => Ok(g),
            None => Err(BinaryReaderError::new(
                format!("unknown global {}: global index out of bounds", idx),
                offset,
            )),
        }
    }

    fn tag_at(&self, idx: u32, offset: usize) -> Result<&Arc<FuncType>> {
        match self.tags.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown tag {}: tag index out of bounds", idx),
                offset,
            )),
        }
    }
}

#[derive(Default)]
struct ComponentTypeSpace(Vec<ComponentTypeDef>);

impl ComponentTypeSpace {
    fn type_def(
        &mut self,
        def: crate::ComponentTypeDef,
        features: &WasmFeatures,
        parents: &[ComponentState],
        offset: usize,
    ) -> Result<()> {
        let def = match def {
            crate::ComponentTypeDef::Module(defs) => {
                ComponentTypeDef::Module(Self::module_type(defs.as_ref(), features, offset)?)
            }
            crate::ComponentTypeDef::Component(defs) => ComponentTypeDef::Component(
                Self::component_type(defs.as_ref(), features, parents, offset)?,
            ),
            crate::ComponentTypeDef::Instance(defs) => ComponentTypeDef::Instance(
                Self::instance_type(defs.as_ref(), features, parents, offset)?,
            ),
            crate::ComponentTypeDef::Function(ty) => {
                ComponentTypeDef::Func(self.function_type(ty, offset)?)
            }
            crate::ComponentTypeDef::Value(ty) => {
                ComponentTypeDef::Value(InterfaceType::new(ty, self, offset)?)
            }
            crate::ComponentTypeDef::Compound(ct) => {
                ComponentTypeDef::Compound(Rc::new(self.compound_type(ct, offset)?))
            }
        };

        self.0.push(def);

        Ok(())
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
            self.get(index, offset)?.clone()
        } else {
            if count > parents.len() {
                return Err(BinaryReaderError::new(
                    format!("invalid outer alias count of {}", count),
                    offset,
                ));
            }

            parents[parents.len() - count]
                .types
                .get(index, offset)?
                .clone()
        };

        self.0.push(ty);

        Ok(())
    }

    fn module_type(
        defs: &[crate::ModuleType],
        features: &WasmFeatures,
        offset: usize,
    ) -> Result<ModuleType> {
        let mut types = ModuleTypeSpace::default();
        let mut imports = HashMap::new();
        let mut exports = HashMap::new();

        for def in defs {
            match def {
                crate::ModuleType::Type(ty) => {
                    types.type_def(ty.clone(), features, offset)?;
                }
                crate::ModuleType::Export { name, ty } => {
                    let ty = types.check_type_ref(ty, features, offset)?;

                    if exports.insert(name.to_string(), ty).is_some() {
                        return Err(BinaryReaderError::new(
                            format!("duplicate export name `{}` already defined", name),
                            offset,
                        ));
                    }
                }
                crate::ModuleType::Import(i) => {
                    let ty = types.check_type_ref(&i.ty, features, offset)?;

                    if imports
                        .insert((i.module.to_string(), i.name.to_string()), ty)
                        .is_some()
                    {
                        return Err(BinaryReaderError::new(
                            format!(
                                "duplicate import name `{}::{}` already defined",
                                i.module, i.name
                            ),
                            offset,
                        ));
                    }
                }
            };
        }

        Ok(ModuleType {
            imports: Rc::new(imports),
            exports: Rc::new(exports),
        })
    }

    fn component_type(
        defs: &[crate::ComponentType],
        features: &WasmFeatures,
        parents: &[ComponentState],
        offset: usize,
    ) -> Result<ComponentType> {
        let mut types = ComponentTypeSpace::default();
        let mut imports = HashMap::new();
        let mut exports = HashMap::new();

        for def in defs {
            match def {
                crate::ComponentType::Type(ty) => {
                    types.type_def(ty.clone(), features, parents, offset)?;
                }
                crate::ComponentType::Export { name, ty } => {
                    if exports
                        .insert(
                            name.to_string(),
                            ComponentEntityType::from_type_def(
                                types.get(*ty, offset)?.clone(),
                                "export",
                                offset,
                            )?,
                        )
                        .is_some()
                    {
                        return Err(BinaryReaderError::new(
                            format!("duplicate export name `{}` already defined", name),
                            offset,
                        ));
                    }
                }
                crate::ComponentType::Import(i) => {
                    if imports
                        .insert(
                            i.name.to_string(),
                            ComponentEntityType::from_type_def(
                                types.get(i.ty, offset)?.clone(),
                                "import",
                                offset,
                            )?,
                        )
                        .is_some()
                    {
                        return Err(BinaryReaderError::new(
                            format!("duplicate import name `{}` already defined", i.name),
                            offset,
                        ));
                    }
                }
                crate::ComponentType::OuterType { count, index } => {
                    let ty = if *count == 0 {
                        types.get(*index, offset)?.clone()
                    } else {
                        let count = *count as usize;
                        if count > parents.len() {
                            return Err(BinaryReaderError::new(
                                format!("invalid outer alias count of {}", count),
                                offset,
                            ));
                        }

                        parents[parents.len() - count]
                            .types
                            .get(*index, offset)?
                            .clone()
                    };

                    types.0.push(ty);
                }
            };
        }

        Ok(ComponentType {
            imports: Rc::new(imports),
            exports: Rc::new(exports),
        })
    }

    fn instance_type(
        defs: &[crate::InstanceType],
        features: &WasmFeatures,
        parents: &[ComponentState],
        offset: usize,
    ) -> Result<InstanceType> {
        let mut types = ComponentTypeSpace::default();
        let mut exports = HashMap::new();

        for def in defs {
            match def {
                crate::InstanceType::Type(ty) => {
                    types.type_def(ty.clone(), features, parents, offset)?;
                }
                crate::InstanceType::Export { name, ty } => {
                    if exports
                        .insert(
                            name.to_string(),
                            ComponentEntityType::from_type_def(
                                types.get(*ty, offset)?.clone(),
                                "export",
                                offset,
                            )?,
                        )
                        .is_some()
                    {
                        return Err(BinaryReaderError::new(
                            format!("duplicate export name `{}` already defined", name),
                            offset,
                        ));
                    }
                }
                crate::InstanceType::OuterType { count, index } => {
                    let ty = if *count == 0 {
                        types.get(*index, offset)?.clone()
                    } else {
                        let count = *count as usize;
                        if count > parents.len() {
                            return Err(BinaryReaderError::new(
                                format!("invalid outer alias count of {}", count),
                                offset,
                            ));
                        }

                        parents[parents.len() - count]
                            .types
                            .get(*index, offset)?
                            .clone()
                    };

                    types.0.push(ty);
                }
            };
        }

        Ok(InstanceType {
            exports: Rc::new(exports),
        })
    }

    fn function_type(
        &self,
        ty: crate::ComponentFuncType,
        offset: usize,
    ) -> Result<ComponentFuncType> {
        let mut core_params = Vec::new();
        let mut core_returns = Vec::new();

        let params = ty
            .params
            .iter()
            .map(|(name, ty)| {
                Self::check_name(name, "function parameter", offset)?;
                let ty = InterfaceType::new(*ty, self, offset)?;
                ty.push_wasm_types(&mut core_params)?;
                Ok((name.to_string(), ty))
            })
            .collect::<Result<_>>()?;

        let result = InterfaceType::new(ty.result, self, offset)?;
        result.push_wasm_types(&mut core_returns)?;

        Ok(ComponentFuncType {
            params,
            result,
            core_type: Arc::new(FuncType {
                params: core_params.into_boxed_slice(),
                returns: core_returns.into_boxed_slice(),
            }),
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

    fn compound_type(&self, ct: crate::CompoundType, offset: usize) -> Result<CompoundType> {
        Ok(match ct {
            crate::CompoundType::Record(fields) => CompoundType::Record(
                fields
                    .iter()
                    .map(|(name, ty)| {
                        Self::check_name(name, "record field", offset)?;
                        InterfaceType::new(*ty, self, offset)
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
                            InterfaceType::new(*ty, self, offset)
                        })
                        .collect::<Result<_>>()?,
                )
            }
            crate::CompoundType::List(ty) => {
                CompoundType::List(InterfaceType::new(ty, self, offset)?)
            }
            crate::CompoundType::Tuple(types) => CompoundType::Tuple(
                types
                    .iter()
                    .map(|ty| InterfaceType::new(*ty, self, offset))
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
            crate::CompoundType::Union(types) => CompoundType::Union(
                types
                    .iter()
                    .map(|ty| InterfaceType::new(*ty, self, offset))
                    .collect::<Result<_>>()?,
            ),
            crate::CompoundType::Optional(ty) => {
                CompoundType::Optional(InterfaceType::new(ty, self, offset)?)
            }
            crate::CompoundType::Expected { ok, error } => CompoundType::Expected(
                InterfaceType::new(ok, self, offset)?,
                InterfaceType::new(error, self, offset)?,
            ),
        })
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn get(&self, idx: u32, offset: usize) -> Result<&ComponentTypeDef> {
        match self.0.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown type {}: type index out of bounds", idx),
                offset,
            )),
        }
    }

    fn compound_type_at(&self, idx: u32, offset: usize) -> Result<&Rc<CompoundType>> {
        match self.get(idx, offset)? {
            ComponentTypeDef::Compound(ct) => Ok(ct),
            _ => Err(BinaryReaderError::new(
                format!("type index {} is not a compound type", idx),
                offset,
            )),
        }
    }

    fn function_type_at(&self, idx: u32, offset: usize) -> Result<&ComponentFuncType> {
        match self.get(idx, offset)? {
            ComponentTypeDef::Func(ft) => Ok(ft),
            _ => Err(BinaryReaderError::new(
                format!("type index {} is not a function type", idx),
                offset,
            )),
        }
    }
}

// The function index space may have both core and component functions
// This is used to distinguish between the possible types of functions.
#[derive(Clone)]
enum FuncIndexType {
    Component(ComponentFuncType),
    Core(Arc<FuncType>),
}

// The instance index space may have both module, component, and "export" instances.
// This is used to distinguish between the possible types of instance.
#[derive(Clone)]
enum InstanceIndexType {
    Component(InstanceType),
    Module(ModuleInstanceType),
}

#[derive(Clone, PartialEq, Eq)]
enum InterfaceType {
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
    Compound(Rc<CompoundType>),
}

impl InterfaceType {
    fn new(ty: crate::InterfaceType, types: &ComponentTypeSpace, offset: usize) -> Result<Self> {
        Ok(match ty {
            crate::InterfaceType::Unit => Self::Unit,
            crate::InterfaceType::Bool => Self::Bool,
            crate::InterfaceType::S8 => Self::S8,
            crate::InterfaceType::U8 => Self::U8,
            crate::InterfaceType::S16 => Self::S16,
            crate::InterfaceType::U16 => Self::U16,
            crate::InterfaceType::S32 => Self::S32,
            crate::InterfaceType::U32 => Self::U32,
            crate::InterfaceType::S64 => Self::S64,
            crate::InterfaceType::U64 => Self::U64,
            crate::InterfaceType::F32 => Self::F32,
            crate::InterfaceType::F64 => Self::F64,
            crate::InterfaceType::Char => Self::Char,
            crate::InterfaceType::String => Self::String,
            crate::InterfaceType::Compound(idx) => {
                Self::Compound(types.compound_type_at(idx, offset)?.clone())
            }
        })
    }

    fn push_wasm_types(&self, types: &mut Vec<Type>) -> Result<()> {
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
                types.push(Type::I32);
            }
            Self::S64 | Self::U64 => {
                types.push(Type::I64);
            }
            Self::F32 => types.push(Type::F32),
            Self::F64 => types.push(Type::F64),
            Self::String => types.extend([Type::I32, Type::I32]),
            Self::Compound(ct) => match ct.as_ref() {
                CompoundType::Record(fields) => {
                    for ty in fields.iter() {
                        ty.push_wasm_types(types)?;
                    }
                }
                CompoundType::Variant(cases) => {
                    Self::push_variant_types(cases.iter(), types)?;
                }
                CompoundType::List(_) => {
                    types.extend([Type::I32, Type::I32]);
                }
                CompoundType::Tuple(tys) => {
                    for ty in tys.iter() {
                        ty.push_wasm_types(types)?;
                    }
                }
                CompoundType::Flags(size) => {
                    if *size <= 32 {
                        types.push(Type::I32);
                    } else if *size <= 64 {
                        types.push(Type::I64);
                    } else {
                        for _ in 0..(*size + 31) / 32 {
                            types.push(Type::I32);
                        }
                    }
                }
                CompoundType::Enum(size) => {
                    if *size < u32::max_value() as usize {
                        types.push(Type::I32);
                    } else {
                        types.push(Type::I64);
                    }
                }
                CompoundType::Union(tys) => {
                    Self::push_variant_types(tys.iter(), types)?;
                }
                CompoundType::Optional(ty) => {
                    Self::push_variant_types([ty].into_iter(), types)?;
                }
                CompoundType::Expected(ok, error) => {
                    Self::push_variant_types([ok, error].into_iter(), types)?;
                }
            },
        }

        Ok(())
    }

    fn push_variant_types<'a>(
        cases: impl ExactSizeIterator<Item = &'a InterfaceType>,
        types: &mut Vec<Type>,
    ) -> Result<()> {
        if cases.len() < u32::max_value() as usize {
            types.push(Type::I32);
        } else {
            types.push(Type::I64);
        }

        let start = types.len();
        let mut temp = Vec::new();

        for ty in cases {
            ty.push_wasm_types(&mut temp)?;

            for (i, ty) in temp.drain(..).enumerate() {
                match types.get_mut(start + i) {
                    Some(prev) => *prev = Self::unify_wasm_type(*prev, ty),
                    None => types.push(ty),
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

#[derive(Clone)]
enum ComponentEntityType {
    Module(ModuleType),
    Component(ComponentType),
    Instance(InstanceType),
    Func(ComponentFuncType),
    Value(InterfaceType),
    Type(InterfaceType),
}

impl ComponentEntityType {
    fn from_type_def(def: ComponentTypeDef, desc: &str, offset: usize) -> Result<Self> {
        Ok(match def {
            ComponentTypeDef::Module(ty) => Self::Module(ty),
            ComponentTypeDef::Component(ty) => Self::Component(ty),
            ComponentTypeDef::Instance(ty) => Self::Instance(ty),
            ComponentTypeDef::Func(ty) => Self::Func(ty),
            ComponentTypeDef::Value(ty) => Self::Value(ty),
            ComponentTypeDef::Compound(_) => {
                return Err(BinaryReaderError::new(
                    format!("{} of compound types are not supported", desc),
                    offset,
                ))
            }
        })
    }

    fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Module(ty), Self::Module(other_ty)) => ty.is_subtype_of(other_ty),
            (Self::Component(ty), Self::Component(other_ty)) => ty.is_subtype_of(other_ty),
            (Self::Instance(ty), Self::Instance(other_ty)) => ty.is_subtype_of(other_ty),
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
