//! State relating to validating a WebAssembly component.

use super::{
    check_max,
    core::Module,
    types::{
        ComponentFuncType, ComponentType, EntityType, InstanceType, InterfaceType,
        InterfaceTypeRef, ModuleType, TypeDef, TypeId, TypeList, VariantCase,
    },
};
use crate::{
    limits::*, types::ComponentEntityType, BinaryReaderError, CanonicalOption, FuncType,
    GlobalType, MemoryType, PrimitiveInterfaceType, Result, TableType, Type, WasmFeatures,
};
use std::{collections::HashMap, mem};

#[derive(Default)]
pub struct ComponentState {
    pub types: Vec<TypeId>,
    pub modules: Vec<TypeId>,
    pub components: Vec<TypeId>,
    pub instances: Vec<TypeId>,
    pub functions: Vec<TypeId>,
    pub values: Vec<(InterfaceTypeRef, bool)>,
    pub memories: Vec<MemoryType>,
    pub tables: Vec<TableType>,
    pub globals: Vec<GlobalType>,
    pub tags: Vec<TypeId>,
    pub imports: HashMap<String, ComponentEntityType>,
    pub exports: HashMap<String, ComponentEntityType>,
    has_start: bool,
}

impl ComponentState {
    pub(super) fn add_type(
        components: &mut Vec<Self>,
        def: crate::ComponentTypeDef,
        features: &WasmFeatures,
        types: &mut TypeList,
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

        components
            .last_mut()
            .unwrap()
            .types
            .push(TypeId(types.len()));
        types.push(def);

        Ok(())
    }

    pub(super) fn add_import(
        &mut self,
        import: crate::ComponentImport,
        types: &TypeList,
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
        types: &TypeList,
        offset: usize,
    ) -> Result<()> {
        let ty = self.function_type_at(type_index, types, offset)?;
        let core_ty = types[self.core_function_at(func_index, types, offset)?].unwrap_func_type();

        // Lifting a function is for an export, so match the expected canonical ABI
        // export signature
        let (params, results) = ty.lower(types, false);

        if core_ty.params.as_ref() != params.as_slice()
            || core_ty.returns.as_ref() != results.as_slice()
        {
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
        types: &mut TypeList,
        offset: usize,
    ) -> Result<()> {
        let ty = types[self.component_function_at(func_index, types, offset)?]
            .unwrap_component_func_type();

        self.check_options(&options, ty, types, offset)?;

        // Lowering a function is for an import, so use a function type that matches
        // the expected canonical ABI import signature.
        let (params, results) = ty.lower(types, true);

        self.functions.push(TypeId(types.len()));

        types.push(TypeDef::Func(FuncType {
            params: params.as_slice().to_vec().into_boxed_slice(),
            returns: results.as_slice().to_vec().into_boxed_slice(),
        }));

        Ok(())
    }

    pub(super) fn add_module(
        &mut self,
        module: &Module,
        types: &mut TypeList,
        offset: usize,
    ) -> Result<()> {
        let imports = module.imports_for_module_type(offset)?;

        // We have to clone the module's imports and exports here
        // because we cannot take the data out of the `MaybeOwned`
        // as it might be shared with a function validator.
        self.modules.push(TypeId(types.len()));
        types.push(TypeDef::Module(ModuleType {
            imports,
            exports: module.exports.clone(),
        }));

        Ok(())
    }

    pub(super) fn add_component(&mut self, component: &mut Self, types: &mut TypeList) {
        self.components.push(TypeId(types.len()));
        types.push(TypeDef::Component(ComponentType {
            imports: mem::take(&mut component.imports),
            exports: mem::take(&mut component.exports),
        }));
    }

    pub(super) fn add_instance(
        &mut self,
        instance: crate::Instance,
        types: &mut TypeList,
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
        types: &TypeList,
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
        components: &mut [Self],
        alias: crate::Alias,
        types: &TypeList,
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
        types: &mut TypeList,
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
        types: &TypeList,
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
            types: &TypeList,
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
        types: &TypeList,
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
        types: &mut TypeList,
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
                        features,
                        offset,
                    )?;
                }
                crate::ModuleType::Import(import) => {
                    state.add_import(import, features, types, offset)?;
                }
            }
        }

        let imports = state.imports_for_module_type(offset)?;

        Ok(ModuleType {
            imports,
            exports: state.exports,
        })
    }

    fn create_component_type(
        components: &mut Vec<Self>,
        defs: Vec<crate::ComponentType>,
        features: &WasmFeatures,
        types: &mut TypeList,
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
        types: &mut TypeList,
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
        types: &TypeList,
        offset: usize,
    ) -> Result<ComponentFuncType> {
        let params = ty
            .params
            .iter()
            .map(|(name, ty)| {
                if let Some(name) = name {
                    Self::check_name(name, "function parameter", offset)?;
                }
                let ty = self.create_interface_type_ref(*ty, types, offset)?;
                Ok((name.map(ToOwned::to_owned), ty))
            })
            .collect::<Result<_>>()?;

        let result = self.create_interface_type_ref(ty.result, types, offset)?;

        Ok(ComponentFuncType { params, result })
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

    pub fn type_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
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
        types: &'a TypeList,
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
        types: &mut TypeList,
        offset: usize,
    ) -> Result<TypeId> {
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
                        insert_arg(module_arg.name, name, *ty, &mut args, offset)?;
                    }
                }
            }
        }

        // Validate the arguments
        for ((module, name), b) in types[module_type].unwrap_module_type().imports.iter() {
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

                    if !a.is_subtype_of(b, types) {
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
        types: &mut TypeList,
        offset: usize,
    ) -> Result<TypeId> {
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
        types: &mut TypeList,
        offset: usize,
    ) -> Result<TypeId> {
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

        let ty = TypeId(types.len());
        types.push(TypeDef::Instance(InstanceType {
            exports: inst_exports,
        }));
        Ok(ty)
    }

    fn instantiate_core_exports(
        &mut self,
        exports: Vec<crate::Export>,
        types: &mut TypeList,
        offset: usize,
    ) -> Result<TypeId> {
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

        let ty = TypeId(types.len());
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
        types: &TypeList,
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

    fn alias_module(components: &mut [Self], count: u32, index: u32, offset: usize) -> Result<()> {
        let component = Self::check_alias_count(components, count, offset)?;
        let ty = component.module_at(index, offset)?;
        components.last_mut().unwrap().modules.push(ty);
        Ok(())
    }

    fn alias_component(
        components: &mut [Self],
        count: u32,
        index: u32,
        offset: usize,
    ) -> Result<()> {
        let component = Self::check_alias_count(components, count, offset)?;
        let ty = component.component_at(index, offset)?;
        components.last_mut().unwrap().components.push(ty);
        Ok(())
    }

    fn alias_type(components: &mut [Self], count: u32, index: u32, offset: usize) -> Result<()> {
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
        types: &TypeList,
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
            crate::InterfaceType::Enum(cases) => {
                if cases.len() > u32::max_value() as usize {
                    return Err(BinaryReaderError::new(
                        "enumeration type cannot be represented with a 32-bit discriminant value",
                        offset,
                    ));
                }
                InterfaceType::Enum(
                    cases
                        .iter()
                        .map(|name| {
                            Self::check_name(name, "enum tag", offset)?;
                            Ok(name.to_string())
                        })
                        .collect::<Result<_>>()?,
                )
            }
            crate::InterfaceType::Union(tys) => InterfaceType::Union(
                tys.iter()
                    .map(|ty| self.create_interface_type_ref(*ty, types, offset))
                    .collect::<Result<_>>()?,
            ),
            crate::InterfaceType::Option(ty) => {
                InterfaceType::Option(self.create_interface_type_ref(ty, types, offset)?)
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
        types: &TypeList,
        offset: usize,
    ) -> Result<InterfaceTypeRef> {
        Ok(match ty {
            crate::InterfaceTypeRef::Primitive(pt) => InterfaceTypeRef::Primitive(pt),
            crate::InterfaceTypeRef::Type(idx) => {
                InterfaceTypeRef::Type(self.interface_type_at(idx, types, offset)?)
            }
        })
    }

    fn function_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
        match self.functions.get(idx as usize) {
            Some(ty) => Ok(*ty),
            None => Err(BinaryReaderError::new(
                format!("unknown function {}: function index out of bounds", idx),
                offset,
            )),
        }
    }

    fn component_function_at(&self, idx: u32, types: &TypeList, offset: usize) -> Result<TypeId> {
        let ty = self.function_at(idx, offset)?;
        match &types[ty] {
            TypeDef::ComponentFunc(_) => Ok(ty),
            _ => Err(BinaryReaderError::new(
                format!("function {} is not a component function", idx),
                offset,
            )),
        }
    }

    fn core_function_at(&self, idx: u32, types: &TypeList, offset: usize) -> Result<TypeId> {
        let ty = self.function_at(idx, offset)?;
        match &types[ty] {
            TypeDef::Func(_) => Ok(ty),
            _ => Err(BinaryReaderError::new(
                format!("function {} is not a core WebAssembly function", idx),
                offset,
            )),
        }
    }

    fn module_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
        match self.modules.get(idx as usize) {
            Some(idx) => Ok(*idx),
            None => Err(BinaryReaderError::new(
                format!("unknown module {}: module index out of bounds", idx),
                offset,
            )),
        }
    }

    fn component_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
        match self.components.get(idx as usize) {
            Some(idx) => Ok(*idx),
            None => Err(BinaryReaderError::new(
                format!("unknown component {}: component index out of bounds", idx),
                offset,
            )),
        }
    }

    fn instance_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
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
        types: &'a TypeList,
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

    fn component_instance_at(&self, idx: u32, types: &TypeList, offset: usize) -> Result<TypeId> {
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
        types: &'a TypeList,
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
        types: &'a TypeList,
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

    fn interface_type_at(&self, idx: u32, types: &TypeList, offset: usize) -> Result<TypeId> {
        let idx = self.type_at(idx, offset)?;
        match &types[idx] {
            TypeDef::Interface(_) => Ok(idx),
            _ => Err(BinaryReaderError::new(
                format!("type index {} is not an interface type", idx.0),
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
