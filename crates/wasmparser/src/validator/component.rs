//! State relating to validating a WebAssembly component.

use super::{
    check_max, combine_type_sizes,
    core::Module,
    types::{
        ComponentFuncType, ComponentInstanceType, ComponentType, ComponentValType, EntityType,
        InstanceType, ModuleType, RecordType, Remapping, ResourceId, Type, TypeAlloc, TypeId,
        TypeList, VariantCase,
    },
};
use crate::validator::names::{KebabName, KebabNameKind, KebabStr, KebabString};
use crate::{
    limits::*,
    types::{
        ComponentDefinedType, ComponentEntityType, Context, InstanceTypeKind, LoweringInfo, Remap,
        SubtypeCx, TupleType, UnionType, VariantType,
    },
    BinaryReaderError, CanonicalOption, ComponentExternName, ComponentExternalKind,
    ComponentOuterAliasKind, ComponentTypeRef, ExternalKind, FuncType, GlobalType,
    InstantiationArgKind, MemoryType, Result, StructuralType, SubType, TableType, TypeBounds,
    ValType, WasmFeatures,
};
use indexmap::{map::Entry, IndexMap, IndexSet};
use std::collections::{HashMap, HashSet};
use std::mem;

fn to_kebab_str<'a>(s: &'a str, desc: &str, offset: usize) -> Result<&'a KebabStr> {
    match KebabStr::new(s) {
        Some(s) => Ok(s),
        None => {
            if s.is_empty() {
                bail!(offset, "{desc} name cannot be empty");
            }

            bail!(offset, "{desc} name `{s}` is not in kebab case");
        }
    }
}

pub(crate) struct ComponentState {
    /// Whether this state is a concrete component, an instance type, or a
    /// component type.
    kind: ComponentKind,

    // Core index spaces
    pub core_types: Vec<TypeId>,
    pub core_modules: Vec<TypeId>,
    pub core_instances: Vec<TypeId>,
    pub core_funcs: Vec<TypeId>,
    pub core_memories: Vec<MemoryType>,
    pub core_tables: Vec<TableType>,
    pub core_globals: Vec<GlobalType>,
    pub core_tags: Vec<TypeId>,

    // Component index spaces
    pub types: Vec<TypeId>,
    pub funcs: Vec<TypeId>,
    pub values: Vec<(ComponentValType, bool)>,
    pub instances: Vec<TypeId>,
    pub components: Vec<TypeId>,

    pub imports: IndexMap<String, ComponentEntityType>,
    pub exports: IndexMap<String, ComponentEntityType>,
    pub kebab_named_externs: IndexSet<KebabName>,

    has_start: bool,
    type_size: u32,

    /// A mapping of imported resources in this component.
    ///
    /// This mapping represents all "type variables" imported into the
    /// component, or resources. This could be resources imported directly as
    /// a top-level type import or additionally transitively through other
    /// imported instances.
    ///
    /// The mapping element here is a "path" which is a list of indexes into
    /// the import map that will be generated for this component. Each index
    /// is an index into an `IndexMap`, and each list is guaranteed to have at
    /// least one element.
    ///
    /// An example of this map is:
    ///
    /// ```wasm
    /// (component
    ///     ;; [0] - the first import
    ///     (import "r" (type (sub resource)))
    ///
    ///     ;; [1] - the second import
    ///     (import "r2" (type (sub resource)))
    ///
    ///     (import "i" (instance
    ///         ;; [2, 0] - the third import, and the first export the instance
    ///         (export "r3" (type (sub resource)))
    ///         ;; [2, 1] - the third import, and the second export the instance
    ///         (export "r4" (type (sub resource)))
    ///     ))
    ///
    ///     ;; ...
    /// )
    /// ```
    ///
    /// The `Vec<usize>` here can be thought of as `Vec<String>` but a
    /// (hopefully) more efficient representation.
    ///
    /// Finally note that this map is listed as an "append only" map because all
    /// insertions into it should always succeed. Any insertion which overlaps
    /// with a previous entry indicates a bug in the validator which needs to be
    /// corrected via other means.
    //
    // TODO: make these `SkolemResourceId` and then go fix all the compile
    // errors, don't add skolem things into the type area
    imported_resources: IndexMapAppendOnly<ResourceId, Vec<usize>>,

    /// A mapping of "defined" resources in this component, or those which
    /// are defined within the instantiation of this component.
    ///
    /// Defined resources, as the name implies, can sort of be thought of as
    /// "these are defined within the component". Note though that the means by
    /// which a local definition can occur are not simply those defined in the
    /// component but also in its transitively instantiated components
    /// internally. This means that this set closes over many transitive
    /// internal items in addition to those defined immediately in the component
    /// itself.
    ///
    /// The `Option<ValType>` in this mapping is whether or not the underlying
    /// reprsentation of the resource is known to this component. Immediately
    /// defined resources, for example, will have `Some(I32)` here. Resources
    /// that come from transitively defined components, for example, will have
    /// `None`. In the type context all entries here are `None`.
    ///
    /// Note that like `imported_resources` all insertions into this map are
    /// expected to succeed to it's declared as append-only.
    defined_resources: IndexMapAppendOnly<ResourceId, Option<ValType>>,

    /// A mapping of explicitly exported resources from this component in
    /// addition to the path that they're exported at.
    ///
    /// For more information on the path here see the documentation for
    /// `imported_resources`. Note that the indexes here index into the
    /// list of exports of this component.
    explicit_resources: IndexMap<ResourceId, Vec<usize>>,

    /// The set of types which are considered "exported" from this component.
    ///
    /// This is added to whenever a type export is found, or an instance export
    /// which itself contains a type export. This additionally includes all
    /// imported types since those are suitable for export as well.
    ///
    /// This set is consulted whenever an exported item is added since all
    /// referenced types must be members of this set.
    exported_types: HashSet<TypeId>,

    /// Same as `exported_types`, but for imports.
    imported_types: HashSet<TypeId>,

    /// The set of top-level resource exports and their names.
    ///
    /// This context is used to validate method names such as `[method]foo.bar`
    /// to ensure that `foo` is an exported resource and that the type mentioned
    /// in a function type is actually named `foo`.
    ///
    /// Note that imports/exports have disjoint contexts to ensure that they're
    /// validated correctly. Namely you can't retroactively attach methods to an
    /// import, for example.
    toplevel_exported_resources: KebabNameContext,

    /// Same as `toplevel_exported_resources`, but for imports.
    toplevel_imported_resources: KebabNameContext,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ComponentKind {
    Component,
    InstanceType,
    ComponentType,
}

/// Helper context used to track information about resource names for method
/// name validation.
#[derive(Default)]
struct KebabNameContext {
    /// A map from a resource type id to an index in the `all_resource_names`
    /// set for the name of that resource.
    resource_name_map: HashMap<TypeId, usize>,

    /// All known resource names in this context, used to validate static method
    /// names to by ensuring that static methods' resource names are somewhere
    /// in this set.
    all_resource_names: IndexSet<String>,
}

#[derive(Debug, Copy, Clone)]
pub enum ExternKind {
    Import,
    Export,
}

impl ExternKind {
    pub fn desc(&self) -> &'static str {
        match self {
            ExternKind::Import => "import",
            ExternKind::Export => "export",
        }
    }
}

impl ComponentState {
    pub fn new(kind: ComponentKind) -> Self {
        Self {
            kind,
            core_types: Default::default(),
            core_modules: Default::default(),
            core_instances: Default::default(),
            core_funcs: Default::default(),
            core_memories: Default::default(),
            core_tables: Default::default(),
            core_globals: Default::default(),
            core_tags: Default::default(),
            types: Default::default(),
            funcs: Default::default(),
            values: Default::default(),
            instances: Default::default(),
            components: Default::default(),
            imports: Default::default(),
            exports: Default::default(),
            kebab_named_externs: Default::default(),
            has_start: Default::default(),
            type_size: 1,
            imported_resources: Default::default(),
            defined_resources: Default::default(),
            explicit_resources: Default::default(),
            exported_types: Default::default(),
            imported_types: Default::default(),
            toplevel_exported_resources: Default::default(),
            toplevel_imported_resources: Default::default(),
        }
    }

    pub fn type_count(&self) -> usize {
        self.core_types.len() + self.types.len()
    }

    pub fn instance_count(&self) -> usize {
        self.core_instances.len() + self.instances.len()
    }

    pub fn function_count(&self) -> usize {
        self.core_funcs.len() + self.funcs.len()
    }

    pub fn add_core_type(
        components: &mut [Self],
        ty: crate::CoreType,
        features: &WasmFeatures,
        types: &mut TypeAlloc,
        offset: usize,
        check_limit: bool,
    ) -> Result<()> {
        let ty = match ty {
            crate::CoreType::Func(ty) => Type::Sub(SubType {
                is_final: false,
                supertype_idx: None,
                structural_type: StructuralType::Func(ty),
            }),
            crate::CoreType::Module(decls) => Type::Module(Box::new(Self::create_module_type(
                components,
                decls.into_vec(),
                features,
                types,
                offset,
            )?)),
        };

        let current = components.last_mut().unwrap();

        if check_limit {
            check_max(current.type_count(), 1, MAX_WASM_TYPES, "types", offset)?;
        }

        let id = types.push_ty(ty);
        current.core_types.push(id);

        Ok(())
    }

    pub fn add_core_module(
        &mut self,
        module: &Module,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let imports = module.imports_for_module_type(offset)?;

        // We have to clone the module's imports and exports here
        // because we cannot take the data out of the `MaybeOwned`
        // as it might be shared with a function validator.
        let ty = Type::Module(Box::new(ModuleType {
            type_size: module.type_size,
            imports,
            exports: module.exports.clone(),
        }));

        let id = types.push_ty(ty);
        self.core_modules.push(id);

        Ok(())
    }

    pub fn add_core_instance(
        &mut self,
        instance: crate::Instance,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let instance = match instance {
            crate::Instance::Instantiate { module_index, args } => {
                self.instantiate_module(module_index, args.into_vec(), types, offset)?
            }
            crate::Instance::FromExports(exports) => {
                self.instantiate_core_exports(exports.into_vec(), types, offset)?
            }
        };

        self.core_instances.push(instance);

        Ok(())
    }

    pub fn add_type(
        components: &mut Vec<Self>,
        ty: crate::ComponentType,
        features: &WasmFeatures,
        types: &mut TypeAlloc,
        offset: usize,
        check_limit: bool,
    ) -> Result<()> {
        assert!(!components.is_empty());
        let ty = match ty {
            crate::ComponentType::Defined(ty) => Type::Defined(
                components
                    .last_mut()
                    .unwrap()
                    .create_defined_type(ty, types, offset)?,
            ),
            crate::ComponentType::Func(ty) => Type::ComponentFunc(
                components
                    .last_mut()
                    .unwrap()
                    .create_function_type(ty, types, offset)?,
            ),
            crate::ComponentType::Component(decls) => Type::Component(Box::new(
                Self::create_component_type(components, decls.into_vec(), features, types, offset)?,
            )),
            crate::ComponentType::Instance(decls) => Type::ComponentInstance(Box::new(
                Self::create_instance_type(components, decls.into_vec(), features, types, offset)?,
            )),
            crate::ComponentType::Resource { rep, dtor } => {
                let component = components.last_mut().unwrap();

                // Resource types cannot be declared in a type context, only
                // within a component context.
                if component.kind != ComponentKind::Component {
                    bail!(
                        offset,
                        "resources can only be defined within a concrete component"
                    );
                }

                // Current MVP restriction of the component model.
                if rep != ValType::I32 {
                    bail!(offset, "resources can only be represented by `i32`");
                }

                // If specified validate that the destructor is both a valid
                // function and has the correct signature.
                if let Some(dtor) = dtor {
                    let ty = component.core_function_at(dtor, offset)?;
                    let ty = types[ty].unwrap_func();
                    if ty.params() != [rep] || ty.results() != [] {
                        bail!(
                            offset,
                            "core function {dtor} has wrong signature for a destructor"
                        );
                    }
                }

                // As this is the introduction of a resource create a fresh new
                // identifier for the resource. This is then added into the
                // list of defined resources for this component, notably with a
                // rep listed to enable getting access to various intrinsics
                // such as `resource.rep`.
                let id = types.alloc_resource_id();
                component.defined_resources.insert(id, Some(rep));
                Type::Resource(id)
            }
        };

        let current = components.last_mut().unwrap();
        if check_limit {
            check_max(current.type_count(), 1, MAX_WASM_TYPES, "types", offset)?;
        }

        let id = types.push_ty(ty);
        current.types.push(id);

        Ok(())
    }

    pub fn add_import(
        &mut self,
        import: crate::ComponentImport,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let mut entity = self.check_type_ref(&import.ty, types, offset)?;
        self.add_entity(
            &mut entity,
            Some((import.name.as_str(), ExternKind::Import)),
            types,
            offset,
        )?;
        self.toplevel_imported_resources.validate_extern(
            import.name,
            "import",
            &entity,
            types,
            offset,
            &mut self.kebab_named_externs,
            &mut self.imports,
            &mut self.type_size,
        )?;
        Ok(())
    }

    fn add_entity(
        &mut self,
        ty: &mut ComponentEntityType,
        name_and_kind: Option<(&str, ExternKind)>,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let kind = name_and_kind.map(|(_, k)| k);
        let (len, max, desc) = match ty {
            ComponentEntityType::Module(id) => {
                self.core_modules.push(*id);
                (self.core_modules.len(), MAX_WASM_MODULES, "modules")
            }
            ComponentEntityType::Component(id) => {
                self.components.push(*id);
                (self.components.len(), MAX_WASM_COMPONENTS, "components")
            }
            ComponentEntityType::Instance(id) => {
                match kind {
                    Some(ExternKind::Import) => self.prepare_instance_import(id, types),
                    Some(ExternKind::Export) => self.prepare_instance_export(id, types),
                    None => {}
                }
                self.instances.push(*id);
                (self.instance_count(), MAX_WASM_INSTANCES, "instances")
            }
            ComponentEntityType::Func(id) => {
                self.funcs.push(*id);
                (self.function_count(), MAX_WASM_FUNCTIONS, "functions")
            }
            ComponentEntityType::Value(ty) => {
                let value_used = match kind {
                    Some(ExternKind::Import) | None => false,
                    Some(ExternKind::Export) => true,
                };
                self.values.push((*ty, value_used));
                (self.values.len(), MAX_WASM_VALUES, "values")
            }
            ComponentEntityType::Type {
                created,
                referenced,
            } => {
                self.types.push(*created);

                // Extra logic here for resources being imported and exported.
                // Note that if `created` is the same as `referenced` then this
                // is the original introduction of the resource which is where
                // `self.{imported,defined}_resources` are updated.
                if let Type::Resource(id) = types[*created] {
                    match kind {
                        Some(ExternKind::Import) => {
                            // A fresh new resource is being imported into a
                            // component. This arises from the import section of
                            // a component or from the import declaration in a
                            // component type. In both cases a new imported
                            // resource is injected with a fresh new identifier
                            // into our state.
                            if created == referenced {
                                self.imported_resources.insert(id, vec![self.imports.len()]);
                            }
                        }

                        Some(ExternKind::Export) => {
                            // A fresh resource is being exported from this
                            // component. This arises as part of the
                            // declaration of a component type, for example. In
                            // this situation brand new resource identifier is
                            // allocated and a definition is added, unlike the
                            // import case where an imported resource is added.
                            // Notably the representation of this new resource
                            // is unknown so it's listed as `None`.
                            if created == referenced {
                                self.defined_resources.insert(id, None);
                            }

                            // If this is a type export of a resource type then
                            // update the `explicit_resources` list. A new
                            // export path is about to be created for this
                            // resource and this keeps track of that.
                            self.explicit_resources.insert(id, vec![self.exports.len()]);
                        }

                        None => {}
                    }
                }
                (self.types.len(), MAX_WASM_TYPES, "types")
            }
        };

        check_max(len, 0, max, desc, offset)?;

        // Before returning perform the final validation of the type of the item
        // being imported/exported. This will ensure that everything is
        // appropriately named with respect to type definitions, resources, etc.
        if let Some((name, kind)) = name_and_kind {
            if !self.validate_and_register_named_types(Some(name), kind, ty, types) {
                bail!(
                    offset,
                    "{} not valid to be used as {}",
                    ty.desc(),
                    kind.desc()
                );
            }
        }
        Ok(())
    }

    /// Validates that the `ty` referenced only refers to named types internally
    /// and then inserts anything necessary, if applicable, to the defined sets
    /// within this component.
    ///
    /// This function will validate that `ty` only refers to named types. For
    /// example if it's a record then all of its fields must refer to named
    /// types. This consults either `self.imported_types` or
    /// `self.exported_types` as specified by `kind`. Note that this is not
    /// inherently recursive itself but it ends up being recursive since if
    /// recursive members were named then all their components must also be
    /// named. Consequently this check stops at the "one layer deep" position,
    /// or more accurately the position where types must be named (e.g. tuples
    /// aren't required to be named).
    fn validate_and_register_named_types(
        &mut self,
        toplevel_name: Option<&str>,
        kind: ExternKind,
        ty: &ComponentEntityType,
        types: &TypeAlloc,
    ) -> bool {
        if let ComponentEntityType::Type { created, .. } = ty {
            // If this is a top-level resource then register it in the
            // appropriate context so later validation of method-like-names
            // works out.
            if let Some(name) = toplevel_name {
                if let Type::Resource(_) = types[*created] {
                    let cx = match kind {
                        ExternKind::Import => &mut self.toplevel_imported_resources,
                        ExternKind::Export => &mut self.toplevel_exported_resources,
                    };
                    cx.register(name, *created);
                }
            }
        }

        match self.kind {
            ComponentKind::Component | ComponentKind::ComponentType => {}
            ComponentKind::InstanceType => return true,
        }
        let set = match kind {
            ExternKind::Import => &self.imported_types,
            ExternKind::Export => &self.exported_types,
        };
        match ty {
            // When a type is imported or exported than any recursive type
            // referred to by that import/export must additionally be exported
            // or imported. Here this walks the "first layer" of the type which
            // delegates to `TypeAlloc::type_named_type_id` to determine whether
            // the components of the type being named here are indeed all they
            // themselves named.
            ComponentEntityType::Type {
                created,
                referenced,
            } => {
                if !self.all_valtypes_named(types, *referenced, set) {
                    return false;
                }
                match kind {
                    // Imported types are both valid for import and valid for
                    // export.
                    ExternKind::Import => {
                        self.imported_types.insert(*created);
                        self.exported_types.insert(*created);
                    }
                    ExternKind::Export => {
                        self.exported_types.insert(*created);
                    }
                }

                true
            }

            // Instances are slightly nuanced here. The general idea is that if
            // an instance is imported, then any type exported by the instance
            // is then also exported. Additionally for exports. To get this to
            // work out this arm will recursively call
            // `validate_and_register_named_types` which means that types are
            // inserted into `self.{imported,exported}_types` as-we-go rather
            // than all at once.
            //
            // This then recursively validates that all items in the instance
            // itself are valid to import/export, recursive instances are
            // captured, and everything is appropriately added to the right
            // imported/exported set.
            ComponentEntityType::Instance(i) => {
                let ty = types[*i].unwrap_component_instance();
                ty.exports
                    .values()
                    .all(|ty| self.validate_and_register_named_types(None, kind, ty, types))
            }

            // All types referred to by a function must be named.
            ComponentEntityType::Func(id) => self.all_valtypes_named(types, *id, set),

            ComponentEntityType::Value(ty) => types.type_named_valtype(ty, set),

            // Components/modules are always "closed" or "standalone" and don't
            // need validation with respect to their named types.
            ComponentEntityType::Component(_) | ComponentEntityType::Module(_) => true,
        }
    }
    fn all_valtypes_named(&self, types: &TypeAlloc, id: TypeId, set: &HashSet<TypeId>) -> bool {
        match &types[id] {
            Type::Defined(i) => match i {
                // These types do not contain anything which must be
                // named.
                ComponentDefinedType::Primitive(_)
                | ComponentDefinedType::Flags(_)
                | ComponentDefinedType::Enum(_) => true,

                // Referenced types of all these aggregates must all be
                // named.
                ComponentDefinedType::Record(r) => {
                    r.fields.values().all(|t| types.type_named_valtype(t, set))
                }
                ComponentDefinedType::Tuple(r) => {
                    r.types.iter().all(|t| types.type_named_valtype(t, set))
                }
                ComponentDefinedType::Union(r) => {
                    r.types.iter().all(|t| types.type_named_valtype(t, set))
                }
                ComponentDefinedType::Variant(r) => r
                    .cases
                    .values()
                    .filter_map(|t| t.ty.as_ref())
                    .all(|t| types.type_named_valtype(t, set)),
                ComponentDefinedType::Result { ok, err } => {
                    ok.as_ref()
                        .map(|t| types.type_named_valtype(t, set))
                        .unwrap_or(true)
                        && err
                            .as_ref()
                            .map(|t| types.type_named_valtype(t, set))
                            .unwrap_or(true)
                }
                ComponentDefinedType::List(ty) | ComponentDefinedType::Option(ty) => {
                    types.type_named_valtype(ty, set)
                }

                // The resource referred to by own/borrow must be named.
                ComponentDefinedType::Own(id) | ComponentDefinedType::Borrow(id) => {
                    set.contains(id)
                }
            },

            // Core wasm constructs are always valid with respect to
            // exported types, since they have none.
            Type::Module(_) | Type::Instance(_) | Type::Sub(_) => true,

            // Resource types, in isolation, are always valid to import
            // or export since they're either attached to an import or
            // being exported.
            //
            // Note that further validation of this happens in `finish`,
            // too.
            Type::Resource(_) => true,

            // Component types are validated as they are constructed,
            // so all component types are valid to export if they've
            // already been constructed.
            Type::Component(_) => true,

            // Function types must have all their parameters/results named.
            Type::ComponentFunc(ty) => ty
                .params
                .iter()
                .map(|(_, ty)| ty)
                .chain(ty.results.iter().map(|(_, ty)| ty))
                .all(|ty| types.type_named_valtype(ty, set)),

            // Instances must recursively have all referenced types named.
            Type::ComponentInstance(ty) => ty.exports.values().all(|ty| {
                let id = match ty {
                    ComponentEntityType::Module(id)
                    | ComponentEntityType::Func(id)
                    | ComponentEntityType::Value(ComponentValType::Type(id))
                    | ComponentEntityType::Type { created: id, .. }
                    | ComponentEntityType::Instance(id)
                    | ComponentEntityType::Component(id) => *id,
                    ComponentEntityType::Value(ComponentValType::Primitive(_)) => return true,
                };
                self.all_valtypes_named(types, id, set)
            }),
        }
    }

    /// Updates the type `id` specified, an identifier for a component instance
    /// type, to be imported into this component.
    ///
    /// Importing an instance type into a component specially handles the
    /// defined resources registered in the instance type. Notably all
    /// defined resources are "freshened" into brand new type variables and
    /// these new variables are substituted within the type. This is what
    /// creates a new `TypeId` and may update the `id` specified.
    ///
    /// One side effect of this operation, for example, is that if an instance
    /// type is used twice to import two different instances then the instances
    /// do not share resource types despite sharing the same original instance
    /// type.
    fn prepare_instance_import(&mut self, id: &mut TypeId, types: &mut TypeAlloc) {
        let ty = types[*id].unwrap_component_instance();

        // No special treatment for imports of instances which themselves have
        // no defined resources
        if ty.defined_resources.is_empty() {
            return;
        }

        let mut new_ty = ComponentInstanceType {
            // Copied from the input verbatim
            type_size: ty.type_size,

            // Copied over as temporary storage for now, and both of these are
            // filled out and expanded below.
            exports: ty.exports.clone(),
            explicit_resources: ty.explicit_resources.clone(),

            // Explicitly discard this field since the
            // defined resources are lifted into `self`
            defined_resources: Default::default(),
        };

        // Create brand new resources for all defined ones in the instance.
        let resources = (0..ty.defined_resources.len())
            .map(|_| types.alloc_resource_id())
            .collect::<IndexSet<_>>();

        // Build a map from the defined resources in `ty` to those in `new_ty`.
        //
        // As part of this same loop the new resources, which were previously
        // defined in `ty`, now become imported variables in `self`. Their
        // path for where they're imported is updated as well with
        // `self.next_import_index` as the import-to-be soon.
        let mut mapping = Remapping::default();
        let ty = types[*id].unwrap_component_instance();
        for (old, new) in ty.defined_resources.iter().zip(&resources) {
            let prev = mapping.resources.insert(*old, *new);
            assert!(prev.is_none());

            let mut base = vec![self.imports.len()];
            base.extend(ty.explicit_resources[old].iter().copied());
            self.imported_resources.insert(*new, base);
        }

        // Using the old-to-new resource mapping perform a substitution on
        // the `exports` and `explicit_resources` fields of `new_ty`
        for ty in new_ty.exports.values_mut() {
            types.remap_component_entity(ty, &mut mapping);
        }
        for (id, path) in mem::take(&mut new_ty.explicit_resources) {
            let id = *mapping.resources.get(&id).unwrap_or(&id);
            new_ty.explicit_resources.insert(id, path);
        }

        // Now that `new_ty` is complete finish its registration and then
        // update `id` on the way out.
        *id = types.push_ty(Type::ComponentInstance(Box::new(new_ty)));
    }

    /// Prepares an instance type, pointed to `id`, for being exported as a
    /// concrete instance from `self`.
    ///
    /// This will internally perform any resource "freshening" as required and
    /// then additionally update metadata within `self` about resources being
    /// exported or defined.
    fn prepare_instance_export(&mut self, id: &mut TypeId, types: &mut TypeAlloc) {
        // Exports of an instance mean that the enclosing context
        // is inheriting the resources that the instance
        // encapsulates. This means that the instance type
        // recorded for this export will itself have no
        // defined resources.
        let ty = types[*id].unwrap_component_instance();

        // Check to see if `defined_resources` is non-empty, and if so then
        // "freshen" all the resources and inherit them to our own defined
        // resources, updating `id` in the process.
        //
        // Note though that this specifically is not rewriting the resources of
        // exported instances. The `defined_resources` set on instance types is
        // a little subtle (see its documentation for more info), but the
        // general idea is that for a concrete instance it's always empty. Only
        // for instance type definitions does it ever have elements in it.
        //
        // That means that if this set is non-empty then what's happening is
        // that we're in a type context an exporting an instance of a previously
        // specified type. In this case all resources are required to be
        // "freshened" to ensure that multiple exports of the same type all
        // export different types of resources.
        //
        // And finally note that this operation empties out the
        // `defined_resources` set of the type that is registered for the
        // instance, as this export is modeled as producing a concrete instance.
        if !ty.defined_resources.is_empty() {
            let mut new_ty = ty.clone();
            let mut mapping = Remapping::default();
            for old in mem::take(&mut new_ty.defined_resources) {
                let new = types.alloc_resource_id();
                mapping.resources.insert(old, new);
                self.defined_resources.insert(new, None);
            }
            for ty in new_ty.exports.values_mut() {
                types.remap_component_entity(ty, &mut mapping);
            }
            for (id, path) in mem::take(&mut new_ty.explicit_resources) {
                let id = mapping.resources.get(&id).copied().unwrap_or(id);
                new_ty.explicit_resources.insert(id, path);
            }
            *id = types.push_ty(Type::ComponentInstance(Box::new(new_ty)));
        }

        // Any explicit resources in the instance are now additionally explicit
        // in this component since it's exported.
        //
        // The path to each explicit resources gets one element prepended which
        // is `self.next_export_index`, the index of the export about to be
        // generated.
        let ty = types[*id].unwrap_component_instance();
        for (id, path) in ty.explicit_resources.iter() {
            let mut new_path = vec![self.exports.len()];
            new_path.extend(path);
            self.explicit_resources.insert(*id, new_path);
        }
    }

    pub fn add_export(
        &mut self,
        name: ComponentExternName<'_>,
        mut ty: ComponentEntityType,
        types: &mut TypeAlloc,
        offset: usize,
        check_limit: bool,
    ) -> Result<()> {
        if check_limit {
            check_max(self.exports.len(), 1, MAX_WASM_EXPORTS, "exports", offset)?;
        }
        self.add_entity(
            &mut ty,
            Some((name.as_str(), ExternKind::Export)),
            types,
            offset,
        )?;
        self.toplevel_exported_resources.validate_extern(
            name.into(),
            "export",
            &ty,
            types,
            offset,
            &mut self.kebab_named_externs,
            &mut self.exports,
            &mut self.type_size,
        )?;
        Ok(())
    }

    pub fn lift_function(
        &mut self,
        core_func_index: u32,
        type_index: u32,
        options: Vec<CanonicalOption>,
        types: &TypeList,
        offset: usize,
    ) -> Result<()> {
        let ty = self.function_type_at(type_index, types, offset)?;
        let core_ty = types[self.core_function_at(core_func_index, offset)?].unwrap_func();

        // Lifting a function is for an export, so match the expected canonical ABI
        // export signature
        let info = ty.lower(types, false);
        self.check_options(Some(core_ty), &info, &options, types, offset)?;

        if core_ty.params() != info.params.as_slice() {
            bail!(
                offset,
                "lowered parameter types `{:?}` do not match parameter types \
                 `{:?}` of core function {core_func_index}",
                info.params.as_slice(),
                core_ty.params(),
            );
        }

        if core_ty.results() != info.results.as_slice() {
            bail!(
                offset,
                "lowered result types `{:?}` do not match result types \
                 `{:?}` of core function {core_func_index}",
                info.results.as_slice(),
                core_ty.results()
            );
        }

        self.funcs.push(self.types[type_index as usize]);

        Ok(())
    }

    pub fn lower_function(
        &mut self,
        func_index: u32,
        options: Vec<CanonicalOption>,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let ty = types[self.function_at(func_index, offset)?].unwrap_component_func();

        // Lowering a function is for an import, so use a function type that matches
        // the expected canonical ABI import signature.
        let info = ty.lower(types, true);

        self.check_options(None, &info, &options, types, offset)?;

        let lowered_ty = Type::Sub(SubType {
            is_final: false,
            supertype_idx: None,
            structural_type: StructuralType::Func(info.into_func_type()),
        });

        let id = types.push_ty(lowered_ty);
        self.core_funcs.push(id);

        Ok(())
    }

    pub fn resource_new(
        &mut self,
        resource: u32,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let rep = self.check_local_resource(resource, types, offset)?;
        let core_ty = Type::Sub(SubType {
            is_final: false,
            supertype_idx: None,
            structural_type: StructuralType::Func(FuncType::new([rep], [ValType::I32])),
        });
        self.core_funcs.push(types.push_ty(core_ty));
        Ok(())
    }

    pub fn resource_drop(
        &mut self,
        resource: u32,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        self.resource_at(resource, types, offset)?;
        let core_ty = Type::Sub(SubType {
            is_final: false,
            supertype_idx: None,
            structural_type: StructuralType::Func(FuncType::new([ValType::I32], [])),
        });
        self.core_funcs.push(types.push_ty(core_ty));
        Ok(())
    }

    pub fn resource_rep(
        &mut self,
        resource: u32,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let rep = self.check_local_resource(resource, types, offset)?;
        let core_ty = Type::Sub(SubType {
            is_final: false,
            supertype_idx: None,
            structural_type: StructuralType::Func(FuncType::new([ValType::I32], [rep])),
        });
        self.core_funcs.push(types.push_ty(core_ty));
        Ok(())
    }

    fn check_local_resource(&self, idx: u32, types: &TypeList, offset: usize) -> Result<ValType> {
        let id = self.resource_at(idx, types, offset)?;
        let resource = types[id].unwrap_resource();
        match self.defined_resources.get(&resource).and_then(|rep| *rep) {
            Some(ty) => Ok(ty),
            None => bail!(offset, "type {idx} is not a local resource"),
        }
    }

    fn resource_at<'a>(&self, idx: u32, types: &'a TypeList, offset: usize) -> Result<TypeId> {
        let id = self.type_at(idx, false, offset)?;
        match &types[id] {
            Type::Resource(_) => Ok(id),
            _ => bail!(offset, "type index {} is not a resource type", idx),
        }
    }

    pub fn add_component(&mut self, component: ComponentType, types: &mut TypeAlloc) -> Result<()> {
        let ty = Type::Component(Box::new(component));
        let id = types.push_ty(ty);
        self.components.push(id);
        Ok(())
    }

    pub fn add_instance(
        &mut self,
        instance: crate::ComponentInstance,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let instance = match instance {
            crate::ComponentInstance::Instantiate {
                component_index,
                args,
            } => self.instantiate_component(component_index, args.into_vec(), types, offset)?,
            crate::ComponentInstance::FromExports(exports) => {
                self.instantiate_exports(exports.into_vec(), types, offset)?
            }
        };

        self.instances.push(instance);

        Ok(())
    }

    pub fn add_alias(
        components: &mut [Self],
        alias: crate::ComponentAlias,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        match alias {
            crate::ComponentAlias::InstanceExport {
                instance_index,
                kind,
                name,
            } => components.last_mut().unwrap().alias_instance_export(
                instance_index,
                kind,
                name,
                types,
                offset,
            ),
            crate::ComponentAlias::CoreInstanceExport {
                instance_index,
                kind,
                name,
            } => components.last_mut().unwrap().alias_core_instance_export(
                instance_index,
                kind,
                name,
                types,
                offset,
            ),
            crate::ComponentAlias::Outer { kind, count, index } => match kind {
                ComponentOuterAliasKind::CoreModule => {
                    Self::alias_module(components, count, index, offset)
                }
                ComponentOuterAliasKind::CoreType => {
                    Self::alias_core_type(components, count, index, offset)
                }
                ComponentOuterAliasKind::Type => {
                    Self::alias_type(components, count, index, types, offset)
                }
                ComponentOuterAliasKind::Component => {
                    Self::alias_component(components, count, index, offset)
                }
            },
        }
    }

    pub fn add_start(
        &mut self,
        func_index: u32,
        args: &[u32],
        results: u32,
        types: &TypeList,
        offset: usize,
    ) -> Result<()> {
        if self.has_start {
            return Err(BinaryReaderError::new(
                "component cannot have more than one start function",
                offset,
            ));
        }

        let ft = types[self.function_at(func_index, offset)?].unwrap_component_func();

        if ft.params.len() != args.len() {
            bail!(
                offset,
                "component start function requires {} arguments but was given {}",
                ft.params.len(),
                args.len()
            );
        }

        if ft.results.len() as u32 != results {
            bail!(
                offset,
                "component start function has a result count of {results} \
                 but the function type has a result count of {type_results}",
                type_results = ft.results.len(),
            );
        }

        let cx = SubtypeCx::new(types, types);
        for (i, ((_, ty), arg)) in ft.params.iter().zip(args).enumerate() {
            // Ensure the value's type is a subtype of the parameter type
            cx.component_val_type(self.value_at(*arg, offset)?, ty, offset)
                .with_context(|| {
                    format!("value type mismatch for component start function argument {i}")
                })?;
        }

        for (_, ty) in ft.results.iter() {
            self.values.push((*ty, false));
        }

        self.has_start = true;

        Ok(())
    }

    fn check_options(
        &self,
        core_ty: Option<&FuncType>,
        info: &LoweringInfo,
        options: &[CanonicalOption],
        types: &TypeList,
        offset: usize,
    ) -> Result<()> {
        fn display(option: CanonicalOption) -> &'static str {
            match option {
                CanonicalOption::UTF8 => "utf8",
                CanonicalOption::UTF16 => "utf16",
                CanonicalOption::CompactUTF16 => "latin1-utf16",
                CanonicalOption::Memory(_) => "memory",
                CanonicalOption::Realloc(_) => "realloc",
                CanonicalOption::PostReturn(_) => "post-return",
            }
        }

        let mut encoding = None;
        let mut memory = None;
        let mut realloc = None;
        let mut post_return = None;

        for option in options {
            match option {
                CanonicalOption::UTF8 | CanonicalOption::UTF16 | CanonicalOption::CompactUTF16 => {
                    match encoding {
                        Some(existing) => {
                            bail!(
                                offset,
                                "canonical encoding option `{}` conflicts with option `{}`",
                                display(existing),
                                display(*option),
                            )
                        }
                        None => encoding = Some(*option),
                    }
                }
                CanonicalOption::Memory(idx) => {
                    memory = match memory {
                        None => {
                            self.memory_at(*idx, offset)?;
                            Some(*idx)
                        }
                        Some(_) => {
                            return Err(BinaryReaderError::new(
                                "canonical option `memory` is specified more than once",
                                offset,
                            ))
                        }
                    }
                }
                CanonicalOption::Realloc(idx) => {
                    realloc = match realloc {
                        None => {
                            let ty = types[self.core_function_at(*idx, offset)?].unwrap_func();
                            if ty.params()
                                != [ValType::I32, ValType::I32, ValType::I32, ValType::I32]
                                || ty.results() != [ValType::I32]
                            {
                                return Err(BinaryReaderError::new(
                                    "canonical option `realloc` uses a core function with an incorrect signature",
                                    offset,
                                ));
                            }
                            Some(*idx)
                        }
                        Some(_) => {
                            return Err(BinaryReaderError::new(
                                "canonical option `realloc` is specified more than once",
                                offset,
                            ))
                        }
                    }
                }
                CanonicalOption::PostReturn(idx) => {
                    post_return = match post_return {
                        None => {
                            let core_ty = core_ty.ok_or_else(|| {
                                BinaryReaderError::new(
                                    "canonical option `post-return` cannot be specified for lowerings",
                                    offset,
                                )
                            })?;

                            let ty = types[self.core_function_at(*idx, offset)?].unwrap_func();

                            if ty.params() != core_ty.results() || !ty.results().is_empty() {
                                return Err(BinaryReaderError::new(
                                    "canonical option `post-return` uses a core function with an incorrect signature",
                                    offset,
                                ));
                            }
                            Some(*idx)
                        }
                        Some(_) => {
                            return Err(BinaryReaderError::new(
                                "canonical option `post-return` is specified more than once",
                                offset,
                            ))
                        }
                    }
                }
            }
        }

        if info.requires_memory && memory.is_none() {
            return Err(BinaryReaderError::new(
                "canonical option `memory` is required",
                offset,
            ));
        }

        if info.requires_realloc && realloc.is_none() {
            return Err(BinaryReaderError::new(
                "canonical option `realloc` is required",
                offset,
            ));
        }

        Ok(())
    }

    fn check_type_ref(
        &mut self,
        ty: &ComponentTypeRef,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<ComponentEntityType> {
        Ok(match ty {
            ComponentTypeRef::Module(index) => {
                let id = self.type_at(*index, true, offset)?;
                match &types[id] {
                    Type::Module(_) => {}
                    _ => bail!(offset, "core type index {index} is not a module type"),
                }
                ComponentEntityType::Module(id)
            }
            ComponentTypeRef::Func(index) => {
                let id = self.type_at(*index, false, offset)?;
                match &types[id] {
                    Type::ComponentFunc(_) => {}
                    _ => bail!(offset, "type index {index} is not a function type"),
                }
                ComponentEntityType::Func(id)
            }
            ComponentTypeRef::Value(ty) => {
                let ty = match ty {
                    crate::ComponentValType::Primitive(ty) => ComponentValType::Primitive(*ty),
                    crate::ComponentValType::Type(index) => {
                        ComponentValType::Type(self.defined_type_at(*index, types, offset)?)
                    }
                };
                ComponentEntityType::Value(ty)
            }
            ComponentTypeRef::Type(TypeBounds::Eq(index)) => {
                let referenced = self.type_at(*index, false, offset)?;
                let created = types.with_unique(referenced);
                ComponentEntityType::Type {
                    referenced,
                    created,
                }
            }
            ComponentTypeRef::Type(TypeBounds::SubResource) => {
                let id = types.alloc_resource_id();
                let id = types.push_ty(Type::Resource(id));
                ComponentEntityType::Type {
                    referenced: id,
                    created: id,
                }
            }
            ComponentTypeRef::Instance(index) => {
                let id = self.type_at(*index, false, offset)?;
                match &types[id] {
                    Type::ComponentInstance(_) => {}
                    _ => bail!(offset, "type index {index} is not an instance type"),
                }
                ComponentEntityType::Instance(id)
            }
            ComponentTypeRef::Component(index) => {
                let id = self.type_at(*index, false, offset)?;
                match &types[id] {
                    Type::Component(_) => {}
                    _ => bail!(offset, "type index {index} is not a component type"),
                }
                ComponentEntityType::Component(id)
            }
        })
    }

    pub fn export_to_entity_type(
        &mut self,
        export: &crate::ComponentExport,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<ComponentEntityType> {
        let actual = match export.kind {
            ComponentExternalKind::Module => {
                ComponentEntityType::Module(self.module_at(export.index, offset)?)
            }
            ComponentExternalKind::Func => {
                ComponentEntityType::Func(self.function_at(export.index, offset)?)
            }
            ComponentExternalKind::Value => {
                ComponentEntityType::Value(*self.value_at(export.index, offset)?)
            }
            ComponentExternalKind::Type => {
                let referenced = self.type_at(export.index, false, offset)?;
                let created = types.with_unique(referenced);
                ComponentEntityType::Type {
                    referenced,
                    created,
                }
            }
            ComponentExternalKind::Instance => {
                ComponentEntityType::Instance(self.instance_at(export.index, offset)?)
            }
            ComponentExternalKind::Component => {
                ComponentEntityType::Component(self.component_at(export.index, offset)?)
            }
        };

        let ascribed = match &export.ty {
            Some(ty) => self.check_type_ref(ty, types, offset)?,
            None => return Ok(actual),
        };

        SubtypeCx::new(types, types)
            .component_entity_type(&actual, &ascribed, offset)
            .with_context(|| "ascribed type of export is not compatible with item's type")?;

        Ok(ascribed)
    }

    fn create_module_type(
        components: &[Self],
        decls: Vec<crate::ModuleTypeDeclaration>,
        features: &WasmFeatures,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<ModuleType> {
        let mut state = Module::default();

        for decl in decls {
            match decl {
                crate::ModuleTypeDeclaration::Type(ty) => {
                    state.add_type(ty, features, types, offset, true)?;
                }
                crate::ModuleTypeDeclaration::Export { name, ty } => {
                    let ty = state.check_type_ref(&ty, features, types, offset)?;
                    state.add_export(name, ty, features, offset, true)?;
                }
                crate::ModuleTypeDeclaration::OuterAlias { kind, count, index } => {
                    if count > 1 {
                        return Err(BinaryReaderError::new(
                                    "outer type aliases in module type declarations are limited to a maximum count of 1",
                                    offset,
                                ));
                    }
                    match kind {
                        crate::OuterAliasKind::Type => {
                            let ty = if count == 0 {
                                // Local alias, check the local module state
                                state.type_id_at(index, offset)?
                            } else {
                                // Otherwise, check the enclosing component state
                                let component =
                                    Self::check_alias_count(components, count - 1, offset)?;
                                component.type_at(index, true, offset)?
                            };

                            check_max(state.types.len(), 1, MAX_WASM_TYPES, "types", offset)?;

                            state.types.push(ty);
                        }
                    }
                }
                crate::ModuleTypeDeclaration::Import(import) => {
                    state.add_import(import, features, types, offset)?;
                }
            }
        }

        let imports = state.imports_for_module_type(offset)?;

        Ok(ModuleType {
            type_size: state.type_size,
            imports,
            exports: state.exports,
        })
    }

    fn create_component_type(
        components: &mut Vec<Self>,
        decls: Vec<crate::ComponentTypeDeclaration>,
        features: &WasmFeatures,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<ComponentType> {
        components.push(ComponentState::new(ComponentKind::ComponentType));

        for decl in decls {
            match decl {
                crate::ComponentTypeDeclaration::CoreType(ty) => {
                    Self::add_core_type(components, ty, features, types, offset, true)?;
                }
                crate::ComponentTypeDeclaration::Type(ty) => {
                    Self::add_type(components, ty, features, types, offset, true)?;
                }
                crate::ComponentTypeDeclaration::Export { name, ty } => {
                    let current = components.last_mut().unwrap();
                    let ty = current.check_type_ref(&ty, types, offset)?;
                    current.add_export(name, ty, types, offset, true)?;
                }
                crate::ComponentTypeDeclaration::Import(import) => {
                    components
                        .last_mut()
                        .unwrap()
                        .add_import(import, types, offset)?;
                }
                crate::ComponentTypeDeclaration::Alias(alias) => {
                    Self::add_alias(components, alias, types, offset)?;
                }
            };
        }

        components.pop().unwrap().finish(types, offset)
    }

    fn create_instance_type(
        components: &mut Vec<Self>,
        decls: Vec<crate::InstanceTypeDeclaration>,
        features: &WasmFeatures,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<ComponentInstanceType> {
        components.push(ComponentState::new(ComponentKind::InstanceType));

        for decl in decls {
            match decl {
                crate::InstanceTypeDeclaration::CoreType(ty) => {
                    Self::add_core_type(components, ty, features, types, offset, true)?;
                }
                crate::InstanceTypeDeclaration::Type(ty) => {
                    Self::add_type(components, ty, features, types, offset, true)?;
                }
                crate::InstanceTypeDeclaration::Export { name, ty } => {
                    let current = components.last_mut().unwrap();
                    let ty = current.check_type_ref(&ty, types, offset)?;
                    current.add_export(name, ty, types, offset, true)?;
                }
                crate::InstanceTypeDeclaration::Alias(alias) => {
                    Self::add_alias(components, alias, types, offset)?;
                }
            };
        }

        let mut state = components.pop().unwrap();

        assert!(state.imported_resources.is_empty());

        Ok(ComponentInstanceType {
            type_size: state.type_size,

            // The defined resources for this instance type are those listed on
            // the component state. The path to each defined resource is
            // guaranteed to live within the `explicit_resources` map since,
            // when in the type context, the introduction of any defined
            // resource must have been done with `(export "x" (type (sub
            // resource)))` which, in a sense, "fuses" the introduction of the
            // variable with the export. This means that all defined resources,
            // if any, should be guaranteed to have an `explicit_resources` path
            // listed.
            defined_resources: mem::take(&mut state.defined_resources)
                .into_iter()
                .map(|(id, rep)| {
                    assert!(rep.is_none());
                    id
                })
                .collect(),

            // The map of what resources are explicitly exported and where
            // they're exported is plumbed through as-is.
            explicit_resources: mem::take(&mut state.explicit_resources),

            exports: mem::take(&mut state.exports),
        })
    }

    fn create_function_type(
        &self,
        ty: crate::ComponentFuncType,
        types: &TypeList,
        offset: usize,
    ) -> Result<ComponentFuncType> {
        let mut type_size = 1;

        let mut set =
            HashSet::with_capacity(std::cmp::max(ty.params.len(), ty.results.type_count()));

        let params = ty
            .params
            .iter()
            .map(|(name, ty)| {
                let name = to_kebab_str(name, "function parameter", offset)?;
                if !set.insert(name) {
                    bail!(
                        offset,
                        "function parameter name `{name}` conflicts with previous parameter name `{prev}`",
                        prev = set.get(&name).unwrap(),
                    );
                }

                let ty = self.create_component_val_type(*ty, types, offset)?;
                type_size = combine_type_sizes(type_size, ty.type_size(), offset)?;
                Ok((name.to_owned(), ty))
            })
            .collect::<Result<_>>()?;

        set.clear();

        let results = ty
            .results
            .iter()
            .map(|(name, ty)| {
                let name = name
                    .map(|name| {
                        let name = to_kebab_str(name, "function result", offset)?;
                        if !set.insert(name) {
                            bail!(
                                offset,
                                "function result name `{name}` conflicts with previous result name `{prev}`",
                                prev = set.get(name).unwrap(),
                            );
                        }

                        Ok(name.to_owned())
                    })
                    .transpose()?;

                let ty = self.create_component_val_type(*ty, types, offset)?;
                type_size = combine_type_sizes(type_size, ty.type_size(), offset)?;
                Ok((name, ty))
            })
            .collect::<Result<_>>()?;

        Ok(ComponentFuncType {
            type_size,
            params,
            results,
        })
    }

    fn instantiate_module(
        &self,
        module_index: u32,
        module_args: Vec<crate::InstantiationArg>,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<TypeId> {
        fn insert_arg<'a>(
            name: &'a str,
            arg: &'a InstanceType,
            args: &mut IndexMap<&'a str, &'a InstanceType>,
            offset: usize,
        ) -> Result<()> {
            if args.insert(name, arg).is_some() {
                bail!(
                    offset,
                    "duplicate module instantiation argument named `{name}`"
                );
            }

            Ok(())
        }

        let module_type_id = self.module_at(module_index, offset)?;
        let mut args = IndexMap::new();

        // Populate the arguments
        for module_arg in module_args {
            match module_arg.kind {
                InstantiationArgKind::Instance => {
                    let instance_type =
                        types[self.core_instance_at(module_arg.index, offset)?].unwrap_instance();
                    insert_arg(module_arg.name, instance_type, &mut args, offset)?;
                }
            }
        }

        // Validate the arguments
        let module_type = types[module_type_id].unwrap_module();
        let cx = SubtypeCx::new(types, types);
        for ((module, name), expected) in module_type.imports.iter() {
            let instance = args.get(module.as_str()).ok_or_else(|| {
                format_err!(
                    offset,
                    "missing module instantiation argument named `{module}`"
                )
            })?;

            let arg = instance
                .internal_exports(types)
                .get(name.as_str())
                .ok_or_else(|| {
                    format_err!(
                        offset,
                        "module instantiation argument `{module}` does not \
                         export an item named `{name}`",
                    )
                })?;

            cx.entity_type(arg, expected, offset).with_context(|| {
                format!(
                    "type mismatch for export `{name}` of module \
                         instantiation argument `{module}`"
                )
            })?;
        }

        let ty = Type::Instance(Box::new(InstanceType {
            type_size: module_type
                .exports
                .iter()
                .fold(1, |acc, (_, ty)| acc + ty.type_size()),
            kind: InstanceTypeKind::Instantiated(module_type_id),
        }));

        Ok(types.push_ty(ty))
    }

    fn instantiate_component(
        &mut self,
        component_index: u32,
        component_args: Vec<crate::ComponentInstantiationArg>,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<TypeId> {
        let component_type_id = self.component_at(component_index, offset)?;
        let mut args = IndexMap::new();

        // Populate the arguments
        for component_arg in component_args {
            let ty = match component_arg.kind {
                ComponentExternalKind::Module => {
                    ComponentEntityType::Module(self.module_at(component_arg.index, offset)?)
                }
                ComponentExternalKind::Component => {
                    ComponentEntityType::Component(self.component_at(component_arg.index, offset)?)
                }
                ComponentExternalKind::Instance => {
                    ComponentEntityType::Instance(self.instance_at(component_arg.index, offset)?)
                }
                ComponentExternalKind::Func => {
                    ComponentEntityType::Func(self.function_at(component_arg.index, offset)?)
                }
                ComponentExternalKind::Value => {
                    ComponentEntityType::Value(*self.value_at(component_arg.index, offset)?)
                }
                ComponentExternalKind::Type => {
                    let ty = self.type_at(component_arg.index, false, offset)?;
                    ComponentEntityType::Type {
                        referenced: ty,
                        created: ty,
                    }
                }
            };
            match args.entry(component_arg.name.to_string()) {
                Entry::Occupied(e) => {
                    bail!(
                        offset,
                        "instantiation argument `{name}` conflicts with previous argument `{prev}`",
                        prev = e.key(),
                        name = component_arg.name
                    );
                }
                Entry::Vacant(e) => {
                    e.insert(ty);
                }
            }
        }

        // Here comes the fun part of the component model, we're instantiating
        // the component with type `component_type_id` with the `args`
        // specified. Easy enough!
        //
        // This operation, however, is one of the lynchpins of safety in the
        // component model. Additionally what this ends up implementing ranges
        // from "well just check the types are equal" to "let's have a
        // full-blown ML-style module type system in the component model". There
        // are primarily two major tricky pieces to the component model which
        // make this operation, instantiating components, hard:
        //
        // 1. Components can import and exports other components. This means
        //    that arguments to instantiation are along the lines of functions
        //    being passed to functions or similar. Effectively this means that
        //    the term "variance" comes into play with either contravariance
        //    or covariance depending on where you are in typechecking. This is
        //    one of the main rationales, however, that this check below is a
        //    check for subtyping as opposed to exact type equivalence. For
        //    example an instance that exports something is a subtype of an
        //    instance that exports nothing. Components get a bit trick since
        //    they both have imports and exports. My way of thinking about it
        //    is "who's asking for what". If you're asking for imports then
        //    I need to at least supply those imports, but I can possibly
        //    supply more. If you're asking for a thing which you'll give a set
        //    of imports, then I can give you something which takes less imports
        //    because what you give still suffices. (things like that). The
        //    real complication with components, however, comes with...
        //
        // 2. Resources. Resources in the component model are akin to "abstract
        //    types". They're not abstract in the sense that they have no
        //    representation, they're always backed by a 32-bit integer right
        //    now. Instead they're abstract in the sense that some components
        //    aren't allowed to understand the representation of a resource.
        //    For example if you import a resource you can't get the underlying
        //    internals of it. Furthermore the resource is strictly tracked
        //    within the component with `own` and `borrow` runtime semantics.
        //    The hardest part about resources, though, is handling them as
        //    part of instantiation and subtyping.
        //
        //    For example one major aspect of resources is that if a component
        //    exports a resource then each instantiation of the component
        //    produces a fresh resource type. This means that the type recorded
        //    for the instantiation here can't simply be "I instantiated
        //    component X" since in such a situation the type of all
        //    instantiations would be the same, which they aren't.
        //
        //    This sort of subtelty comes up quite frequently for resources.
        //    This file contains references to `imported_resources` and
        //    `defined_resources` for example which refer to the formal
        //    nature of components and their abstract variables. Specifically
        //    for instantiation though we're eventually faced with the problem
        //    of subtype checks where resource subtyping is defined as "does
        //    your id equal mine". Naively implemented that means anything with
        //    resources isn't subtypes of anything else since resource ids are
        //    unique between components. Instead what actually needs to happen
        //    is types need to be substituted.
        //
        // Much of the complexity here is not actually apparent here in this
        // literal one function. Instead it's spread out across validation
        // in this file and type-checking in the `types.rs` module. Note that
        // the "spread out" nature isn't because we're bad maintainers
        // (hopefully), but rather it's quite infectious how many parts need
        // to handle resources and account for defined/imported variables.
        //
        // For example only one subtyping method is called here where `args` is
        // passed in. This method is quite recursive in its nature though and
        // will internally touch all the fields that this file maintains to
        // end up putting into various bits and pieces of type information.
        //
        // Unfortunately there's probably not really a succinct way to read
        // this method and understand everything. If you've written ML module
        // type systems this will probably look quite familiar, but otherwise
        // the whole system is not really easily approachable at this time. It's
        // hoped in the future that there's a formalism to refer to which will
        // make things more clear as the code would be able to reference this
        // hypothetical formalism. Until that's the case, though, these
        // comments are hopefully enough when augmented with communication with
        // the authors.

        let component_type = types[component_type_id].unwrap_component();
        let mut exports = component_type.exports.clone();
        let type_size = component_type
            .exports
            .iter()
            .fold(1, |acc, (_, ty)| acc + ty.type_size());

        // Perform the subtype check that `args` matches the imports of
        // `component_type_id`. The result of this subtype check is the
        // production of a mapping of resource types from the imports to the
        // arguments provided. This is a substitution map which is then used
        // below to perform a substitution into the exports of the instance
        // since the types of the exports are now in terms of whatever was
        // supplied as imports.
        let mut mapping = SubtypeCx::new(types, types).open_instance_type(
            &args,
            component_type_id,
            ExternKind::Import,
            offset,
        )?;

        // Part of the instantiation of a component is that all of its
        // defined resources become "fresh" on each instantiation. This
        // means that each instantiation of a component gets brand new type
        // variables representing its defined resources, modeling that each
        // instantiation produces distinct types. The freshening is performed
        // here by allocating new ids and inserting them into `mapping`.
        //
        // Note that technically the `mapping` from subtyping should be applied
        // first and then the mapping for freshening should be applied
        // afterwards. The keys of the map from subtyping are the imported
        // resources from this component which are disjoint from its defined
        // resources. That means it should be possible to place everything
        // into one large map which maps from:
        //
        // * the component's imported resources go to whatever was explicitly
        //   supplied in the import map
        // * the component's defined resources go to fresh new resources
        //
        // These two remapping operations can then get folded into one by
        // placing everything in the same `mapping` and using that for a remap
        // only once.
        let fresh_defined_resources = (0..component_type.defined_resources.len())
            .map(|_| types.alloc_resource_id())
            .collect::<IndexSet<_>>();
        let component_type = types[component_type_id].unwrap_component();
        for ((old, _path), new) in component_type
            .defined_resources
            .iter()
            .zip(&fresh_defined_resources)
        {
            let prev = mapping.resources.insert(*old, *new);
            assert!(prev.is_none());
        }

        // Perform the remapping operation over all the exports that will be
        // listed for the final instance type. Note that this is performed
        // both for all the export types in addition to the explicitly exported
        // resources list.
        //
        // Note that this is a crucial step of the instantiation process which
        // is intentionally transforming the type of a component based on the
        // variables provided by imports and additionally ensuring that all
        // references to the component's defined resources are rebound to the
        // fresh ones introduced just above.
        for entity in exports.values_mut() {
            types.remap_component_entity(entity, &mut mapping);
        }
        let component_type = types[component_type_id].unwrap_component();
        let explicit_resources = component_type
            .explicit_resources
            .iter()
            .map(|(id, path)| {
                (
                    mapping.resources.get(id).copied().unwrap_or(*id),
                    path.clone(),
                )
            })
            .collect::<IndexMap<_, _>>();

        // Technically in the last formalism that was consulted in writing this
        // implementation there are two further steps that are part of the
        // instantiation process:
        //
        // 1. The set of defined resources from the instance created, which are
        //    added to the outer component, is the subset of the instance's
        //    original defined resources and the free variables of the exports.
        //
        // 2. Each element of this subset is required to be "explicit in" the
        //    instance, or otherwise explicitly exported somewhere within the
        //    instance.
        //
        // With the syntactic structure of the component model, however, neither
        // of these conditions should be necessary. The main reason for this is
        // that this function is specifically dealing with instantiation of
        // components which should already have these properties validated
        // about them. Subsequently we shouldn't have to re-check them.
        //
        // In debug mode, however, do a sanity check.
        if cfg!(debug_assertions) {
            let mut free = IndexSet::new();
            for ty in exports.values() {
                types.free_variables_component_entity(ty, &mut free);
            }
            assert!(fresh_defined_resources.is_subset(&free));
            for resource in fresh_defined_resources.iter() {
                assert!(explicit_resources.contains_key(resource));
            }
        }

        // And as the final step of the instantiation process all of the
        // new defined resources from this component instantiation are moved
        // onto `self`. Note that concrete instances never have defined
        // resources (see more comments in `instantiate_exports`) so the
        // `defined_resources` listing in the final type is always empty. This
        // represents how by having a concrete instance the definitions
        // referred to in that instance are now problems for the outer
        // component rather than the inner instance since the instance is bound
        // to the component.
        //
        // All defined resources here have no known representation, so they're
        // all listed with `None`. Also note that none of the resources were
        // exported yet so `self.explicit_resources` is not updated yet. If
        // this instance is exported, however, it'll consult the type's
        // `explicit_resources` array and use that appropriately.
        for resource in fresh_defined_resources {
            self.defined_resources.insert(resource, None);
        }

        let ty = Type::ComponentInstance(Box::new(ComponentInstanceType {
            type_size,
            defined_resources: Default::default(),
            explicit_resources,
            exports,
        }));
        Ok(types.push_ty(ty))
    }

    fn instantiate_exports(
        &mut self,
        exports: Vec<crate::ComponentExport>,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<TypeId> {
        let mut type_size = 1;
        let mut inst_exports = IndexMap::new();
        let mut explicit_resources = IndexMap::new();
        let mut kebab_names = IndexSet::new();

        // NB: It's intentional that this context is empty since no indices are
        // introduced in the bag-of-exports construct which means there's no
        // way syntactically to register something inside of this.
        let names = KebabNameContext::default();

        for export in exports {
            assert!(export.ty.is_none());
            let ty = match export.kind {
                ComponentExternalKind::Module => {
                    ComponentEntityType::Module(self.module_at(export.index, offset)?)
                }
                ComponentExternalKind::Component => {
                    ComponentEntityType::Component(self.component_at(export.index, offset)?)
                }
                ComponentExternalKind::Instance => {
                    let ty = self.instance_at(export.index, offset)?;

                    // When an instance is exported from an instance then
                    // all explicitly exported resources on the sub-instance are
                    // now also listed as exported resources on the outer
                    // instance, just with one more element in their path.
                    explicit_resources.extend(
                        types[ty]
                            .unwrap_component_instance()
                            .explicit_resources
                            .iter()
                            .map(|(id, path)| {
                                let mut new_path = vec![inst_exports.len()];
                                new_path.extend(path);
                                (*id, new_path)
                            }),
                    );
                    ComponentEntityType::Instance(ty)
                }
                ComponentExternalKind::Func => {
                    ComponentEntityType::Func(self.function_at(export.index, offset)?)
                }
                ComponentExternalKind::Value => {
                    ComponentEntityType::Value(*self.value_at(export.index, offset)?)
                }
                ComponentExternalKind::Type => {
                    let ty = self.type_at(export.index, false, offset)?;
                    // If this is an export of a resource type be sure to
                    // record that in the explicit list with the appropriate
                    // path because if this instance ends up getting used
                    // it'll count towards the "explicit in" check.
                    if let Type::Resource(id) = &types[ty] {
                        explicit_resources.insert(*id, vec![inst_exports.len()]);
                    }
                    ComponentEntityType::Type {
                        referenced: ty,
                        // The created type index here isn't used anywhere
                        // in index spaces because a "bag of exports"
                        // doesn't build up its own index spaces. Just fill
                        // in the same index here in this case as what's
                        // referenced.
                        created: ty,
                    }
                }
            };

            names.validate_extern(
                export.name.into(),
                "instance export",
                &ty,
                types,
                offset,
                &mut kebab_names,
                &mut inst_exports,
                &mut type_size,
            )?;
        }

        let ty = Type::ComponentInstance(Box::new(ComponentInstanceType {
            type_size,
            explicit_resources,
            exports: inst_exports,

            // NB: the list of defined resources for this instance itself
            // is always empty. Even if this instance exports resources,
            // it's empty.
            //
            // The reason for this is a bit subtle. The general idea, though, is
            // that the defined resources list here is only used for instance
            // types that are sort of "floating around" and haven't actually
            // been attached to something yet. For example when an instance type
            // is simply declared it can have defined resources introduced
            // through `(export "name" (type (sub resource)))`. These
            // definitions, however, are local to the instance itself and aren't
            // defined elsewhere.
            //
            // Here, though, no new definitions were introduced. The instance
            // created here is a "bag of exports" which could only refer to
            // preexisting items. This means that inherently no new resources
            // were created so there's nothing to put in this list. Any
            // resources referenced by the instance must be bound by the outer
            // component context or further above.
            //
            // Furthermore, however, actual instances of instances, which this
            // is, aren't allowed to have defined resources. Instead the
            // resources would have to be injected into the outer component
            // enclosing the instance. That means that even if bag-of-exports
            // could declare a new resource then the resource would be moved
            // from here to `self.defined_resources`. This doesn't exist at this
            // time, though, so this still remains empty and
            // `self.defined_resources` remains unperturbed.
            defined_resources: Default::default(),
        }));

        Ok(types.push_ty(ty))
    }

    fn instantiate_core_exports(
        &mut self,
        exports: Vec<crate::Export>,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<TypeId> {
        fn insert_export(
            name: &str,
            export: EntityType,
            exports: &mut IndexMap<String, EntityType>,
            type_size: &mut u32,
            offset: usize,
        ) -> Result<()> {
            *type_size = combine_type_sizes(*type_size, export.type_size(), offset)?;

            if exports.insert(name.to_string(), export).is_some() {
                bail!(
                    offset,
                    "duplicate instantiation export name `{name}` already defined",
                )
            }

            Ok(())
        }

        let mut type_size = 1;
        let mut inst_exports = IndexMap::new();
        for export in exports {
            match export.kind {
                ExternalKind::Func => {
                    insert_export(
                        export.name,
                        EntityType::Func(self.core_function_at(export.index, offset)?),
                        &mut inst_exports,
                        &mut type_size,
                        offset,
                    )?;
                }
                ExternalKind::Table => insert_export(
                    export.name,
                    EntityType::Table(*self.table_at(export.index, offset)?),
                    &mut inst_exports,
                    &mut type_size,
                    offset,
                )?,
                ExternalKind::Memory => insert_export(
                    export.name,
                    EntityType::Memory(*self.memory_at(export.index, offset)?),
                    &mut inst_exports,
                    &mut type_size,
                    offset,
                )?,
                ExternalKind::Global => {
                    insert_export(
                        export.name,
                        EntityType::Global(*self.global_at(export.index, offset)?),
                        &mut inst_exports,
                        &mut type_size,
                        offset,
                    )?;
                }
                ExternalKind::Tag => insert_export(
                    export.name,
                    EntityType::Tag(self.core_function_at(export.index, offset)?),
                    &mut inst_exports,
                    &mut type_size,
                    offset,
                )?,
            }
        }

        let ty = Type::Instance(Box::new(InstanceType {
            type_size,
            kind: InstanceTypeKind::Exports(inst_exports),
        }));

        Ok(types.push_ty(ty))
    }

    fn alias_core_instance_export(
        &mut self,
        instance_index: u32,
        kind: ExternalKind,
        name: &str,
        types: &TypeList,
        offset: usize,
    ) -> Result<()> {
        macro_rules! push_module_export {
            ($expected:path, $collection:ident, $ty:literal) => {{
                match self.core_instance_export(instance_index, name, types, offset)? {
                    $expected(ty) => {
                        self.$collection.push(*ty);
                        Ok(())
                    }
                    _ => {
                        bail!(
                            offset,
                            "export `{name}` for core instance {instance_index} is not a {}",
                            $ty
                        )
                    }
                }
            }};
        }

        match kind {
            ExternalKind::Func => {
                check_max(
                    self.function_count(),
                    1,
                    MAX_WASM_FUNCTIONS,
                    "functions",
                    offset,
                )?;
                push_module_export!(EntityType::Func, core_funcs, "function")
            }
            ExternalKind::Table => {
                check_max(self.core_tables.len(), 1, MAX_WASM_TABLES, "tables", offset)?;
                push_module_export!(EntityType::Table, core_tables, "table")
            }
            ExternalKind::Memory => {
                check_max(
                    self.core_memories.len(),
                    1,
                    MAX_WASM_MEMORIES,
                    "memories",
                    offset,
                )?;
                push_module_export!(EntityType::Memory, core_memories, "memory")
            }
            ExternalKind::Global => {
                check_max(
                    self.core_globals.len(),
                    1,
                    MAX_WASM_GLOBALS,
                    "globals",
                    offset,
                )?;
                push_module_export!(EntityType::Global, core_globals, "global")
            }
            ExternalKind::Tag => {
                check_max(self.core_tags.len(), 1, MAX_WASM_TAGS, "tags", offset)?;
                push_module_export!(EntityType::Tag, core_tags, "tag")
            }
        }
    }

    fn alias_instance_export(
        &mut self,
        instance_index: u32,
        kind: ComponentExternalKind,
        name: &str,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let mut ty = match types[self.instance_at(instance_index, offset)?]
            .unwrap_component_instance()
            .exports
            .get(name)
        {
            Some(ty) => *ty,
            None => bail!(
                offset,
                "instance {instance_index} has no export named `{name}`"
            ),
        };

        let ok = match (&ty, kind) {
            (ComponentEntityType::Module(_), ComponentExternalKind::Module) => true,
            (ComponentEntityType::Module(_), _) => false,
            (ComponentEntityType::Component(_), ComponentExternalKind::Component) => true,
            (ComponentEntityType::Component(_), _) => false,
            (ComponentEntityType::Func(_), ComponentExternalKind::Func) => true,
            (ComponentEntityType::Func(_), _) => false,
            (ComponentEntityType::Instance(_), ComponentExternalKind::Instance) => true,
            (ComponentEntityType::Instance(_), _) => false,
            (ComponentEntityType::Value(_), ComponentExternalKind::Value) => true,
            (ComponentEntityType::Value(_), _) => false,
            (ComponentEntityType::Type { .. }, ComponentExternalKind::Type) => true,
            (ComponentEntityType::Type { .. }, _) => false,
        };
        if !ok {
            bail!(
                offset,
                "export `{name}` for instance {instance_index} is not a {}",
                kind.desc(),
            );
        }

        self.add_entity(&mut ty, None, types, offset)?;
        Ok(())
    }

    fn alias_module(components: &mut [Self], count: u32, index: u32, offset: usize) -> Result<()> {
        let component = Self::check_alias_count(components, count, offset)?;
        let ty = component.module_at(index, offset)?;

        let current = components.last_mut().unwrap();
        check_max(
            current.core_modules.len(),
            1,
            MAX_WASM_MODULES,
            "modules",
            offset,
        )?;

        current.core_modules.push(ty);
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

        let current = components.last_mut().unwrap();
        check_max(
            current.components.len(),
            1,
            MAX_WASM_COMPONENTS,
            "components",
            offset,
        )?;

        current.components.push(ty);
        Ok(())
    }

    fn alias_core_type(
        components: &mut [Self],
        count: u32,
        index: u32,
        offset: usize,
    ) -> Result<()> {
        let component = Self::check_alias_count(components, count, offset)?;
        let ty = component.type_at(index, true, offset)?;

        let current = components.last_mut().unwrap();
        check_max(current.type_count(), 1, MAX_WASM_TYPES, "types", offset)?;

        current.core_types.push(ty);

        Ok(())
    }

    fn alias_type(
        components: &mut [Self],
        count: u32,
        index: u32,
        types: &mut TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let component = Self::check_alias_count(components, count, offset)?;
        let ty = component.type_at(index, false, offset)?;

        // If `count` "crossed a component boundary", meaning that it went from
        // one component to another, then this must additionally verify that
        // `ty` has no free variables with respect to resources. This is
        // intended to preserve the property for components where each component
        // is an isolated unit that can theoretically be extracted from other
        // components. If resources from other components were allowed to leak
        // in then it would prevent that.
        //
        // This check is done by calculating the `pos` within `components` that
        // our target `component` above was selected at. Once this is acquired
        // the component to the "right" is checked, and if that's a component
        // then it's considered as crossing a component boundary meaning the
        // free variables check runs.
        //
        // The reason this works is that in the list of `ComponentState` types
        // it's guaranteed that any `is_type` components are contiguous at the
        // end of the array. This means that if state one level deeper than the
        // target of this alias is a `!is_type` component, then the target must
        // be a component as well. If the one-level deeper state `is_type` then
        // the target is either a type or a component, both of which are valid
        // (as aliases can reach the enclosing component and have as many free
        // variables as they want).
        let pos_after_component = components.len() - (count as usize);
        if let Some(component) = components.get(pos_after_component) {
            if component.kind == ComponentKind::Component {
                let mut free = IndexSet::new();
                types.free_variables_type_id(ty, &mut free);
                if !free.is_empty() {
                    bail!(
                        offset,
                        "cannot alias outer type which transitively refers \
                         to resources not defined in the current component"
                    );
                }
            }
        }

        let current = components.last_mut().unwrap();
        check_max(current.type_count(), 1, MAX_WASM_TYPES, "types", offset)?;

        current.types.push(ty);

        Ok(())
    }

    fn check_alias_count(components: &[Self], count: u32, offset: usize) -> Result<&Self> {
        let count = count as usize;
        if count >= components.len() {
            bail!(offset, "invalid outer alias count of {count}");
        }

        Ok(&components[components.len() - count - 1])
    }

    fn create_defined_type(
        &self,
        ty: crate::ComponentDefinedType,
        types: &TypeList,
        offset: usize,
    ) -> Result<ComponentDefinedType> {
        match ty {
            crate::ComponentDefinedType::Primitive(ty) => Ok(ComponentDefinedType::Primitive(ty)),
            crate::ComponentDefinedType::Record(fields) => {
                self.create_record_type(fields.as_ref(), types, offset)
            }
            crate::ComponentDefinedType::Variant(cases) => {
                self.create_variant_type(cases.as_ref(), types, offset)
            }
            crate::ComponentDefinedType::List(ty) => Ok(ComponentDefinedType::List(
                self.create_component_val_type(ty, types, offset)?,
            )),
            crate::ComponentDefinedType::Tuple(tys) => {
                self.create_tuple_type(tys.as_ref(), types, offset)
            }
            crate::ComponentDefinedType::Flags(names) => {
                self.create_flags_type(names.as_ref(), offset)
            }
            crate::ComponentDefinedType::Enum(cases) => {
                self.create_enum_type(cases.as_ref(), offset)
            }
            crate::ComponentDefinedType::Union(tys) => {
                self.create_union_type(tys.as_ref(), types, offset)
            }
            crate::ComponentDefinedType::Option(ty) => Ok(ComponentDefinedType::Option(
                self.create_component_val_type(ty, types, offset)?,
            )),
            crate::ComponentDefinedType::Result { ok, err } => Ok(ComponentDefinedType::Result {
                ok: ok
                    .map(|ty| self.create_component_val_type(ty, types, offset))
                    .transpose()?,
                err: err
                    .map(|ty| self.create_component_val_type(ty, types, offset))
                    .transpose()?,
            }),
            crate::ComponentDefinedType::Own(idx) => Ok(ComponentDefinedType::Own(
                self.resource_at(idx, types, offset)?,
            )),
            crate::ComponentDefinedType::Borrow(idx) => Ok(ComponentDefinedType::Borrow(
                self.resource_at(idx, types, offset)?,
            )),
        }
    }

    fn create_record_type(
        &self,
        fields: &[(&str, crate::ComponentValType)],
        types: &TypeList,
        offset: usize,
    ) -> Result<ComponentDefinedType> {
        let mut type_size = 1;
        let mut field_map = IndexMap::with_capacity(fields.len());

        for (name, ty) in fields {
            let name = to_kebab_str(name, "record field", offset)?;
            let ty = self.create_component_val_type(*ty, types, offset)?;

            match field_map.entry(name.to_owned()) {
                Entry::Occupied(e) => bail!(
                    offset,
                    "record field name `{name}` conflicts with previous field name `{prev}`",
                    prev = e.key()
                ),
                Entry::Vacant(e) => {
                    type_size = combine_type_sizes(type_size, ty.type_size(), offset)?;
                    e.insert(ty);
                }
            }
        }

        Ok(ComponentDefinedType::Record(RecordType {
            type_size,
            fields: field_map,
        }))
    }

    fn create_variant_type(
        &self,
        cases: &[crate::VariantCase],
        types: &TypeList,
        offset: usize,
    ) -> Result<ComponentDefinedType> {
        let mut type_size = 1;
        let mut case_map: IndexMap<KebabString, VariantCase> = IndexMap::with_capacity(cases.len());

        if cases.is_empty() {
            return Err(BinaryReaderError::new(
                "variant type must have at least one case",
                offset,
            ));
        }

        if cases.len() > u32::MAX as usize {
            return Err(BinaryReaderError::new(
                "variant type cannot be represented with a 32-bit discriminant value",
                offset,
            ));
        }

        for (i, case) in cases.iter().enumerate() {
            if let Some(refines) = case.refines {
                if refines >= i as u32 {
                    return Err(BinaryReaderError::new(
                        "variant case can only refine a previously defined case",
                        offset,
                    ));
                }
            }

            let name = to_kebab_str(case.name, "variant case", offset)?;

            let ty = case
                .ty
                .map(|ty| self.create_component_val_type(ty, types, offset))
                .transpose()?;

            match case_map.entry(name.to_owned()) {
                Entry::Occupied(e) => bail!(
                    offset,
                    "variant case name `{name}` conflicts with previous case name `{prev}`",
                    name = case.name,
                    prev = e.key()
                ),
                Entry::Vacant(e) => {
                    type_size = combine_type_sizes(
                        type_size,
                        ty.map(|ty| ty.type_size()).unwrap_or(1),
                        offset,
                    )?;

                    // Safety: the use of `KebabStr::new_unchecked` here is safe because the string
                    // was already verified to be kebab case.
                    e.insert(VariantCase {
                        ty,
                        refines: case
                            .refines
                            .map(|i| KebabStr::new_unchecked(cases[i as usize].name).to_owned()),
                    });
                }
            }
        }

        Ok(ComponentDefinedType::Variant(VariantType {
            type_size,
            cases: case_map,
        }))
    }

    fn create_tuple_type(
        &self,
        tys: &[crate::ComponentValType],
        types: &TypeList,
        offset: usize,
    ) -> Result<ComponentDefinedType> {
        let mut type_size = 1;
        let types = tys
            .iter()
            .map(|ty| {
                let ty = self.create_component_val_type(*ty, types, offset)?;
                type_size = combine_type_sizes(type_size, ty.type_size(), offset)?;
                Ok(ty)
            })
            .collect::<Result<_>>()?;

        Ok(ComponentDefinedType::Tuple(TupleType { type_size, types }))
    }

    fn create_flags_type(&self, names: &[&str], offset: usize) -> Result<ComponentDefinedType> {
        let mut names_set = IndexSet::with_capacity(names.len());

        for name in names {
            let name = to_kebab_str(name, "flag", offset)?;
            if !names_set.insert(name.to_owned()) {
                bail!(
                    offset,
                    "flag name `{name}` conflicts with previous flag name `{prev}`",
                    prev = names_set.get(name).unwrap()
                );
            }
        }

        Ok(ComponentDefinedType::Flags(names_set))
    }

    fn create_enum_type(&self, cases: &[&str], offset: usize) -> Result<ComponentDefinedType> {
        if cases.len() > u32::MAX as usize {
            return Err(BinaryReaderError::new(
                "enumeration type cannot be represented with a 32-bit discriminant value",
                offset,
            ));
        }

        let mut tags = IndexSet::with_capacity(cases.len());

        for tag in cases {
            let tag = to_kebab_str(tag, "enum tag", offset)?;
            if !tags.insert(tag.to_owned()) {
                bail!(
                    offset,
                    "enum tag name `{tag}` conflicts with previous tag name `{prev}`",
                    prev = tags.get(tag).unwrap()
                );
            }
        }

        Ok(ComponentDefinedType::Enum(tags))
    }

    fn create_union_type(
        &self,
        tys: &[crate::ComponentValType],
        types: &TypeList,
        offset: usize,
    ) -> Result<ComponentDefinedType> {
        let mut type_size = 1;
        let types = tys
            .iter()
            .map(|ty| {
                let ty = self.create_component_val_type(*ty, types, offset)?;
                type_size = combine_type_sizes(type_size, ty.type_size(), offset)?;
                Ok(ty)
            })
            .collect::<Result<_>>()?;

        Ok(ComponentDefinedType::Union(UnionType { type_size, types }))
    }

    fn create_component_val_type(
        &self,
        ty: crate::ComponentValType,
        types: &TypeList,
        offset: usize,
    ) -> Result<ComponentValType> {
        Ok(match ty {
            crate::ComponentValType::Primitive(pt) => ComponentValType::Primitive(pt),
            crate::ComponentValType::Type(idx) => {
                ComponentValType::Type(self.defined_type_at(idx, types, offset)?)
            }
        })
    }

    pub fn type_at(&self, idx: u32, core: bool, offset: usize) -> Result<TypeId> {
        let types = if core { &self.core_types } else { &self.types };
        types
            .get(idx as usize)
            .copied()
            .ok_or_else(|| format_err!(offset, "unknown type {idx}: type index out of bounds"))
    }

    fn function_type_at<'a>(
        &self,
        idx: u32,
        types: &'a TypeList,
        offset: usize,
    ) -> Result<&'a ComponentFuncType> {
        match &types[self.type_at(idx, false, offset)?] {
            Type::ComponentFunc(f) => Ok(f),
            _ => bail!(offset, "type index {idx} is not a function type"),
        }
    }

    fn function_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
        self.funcs.get(idx as usize).copied().ok_or_else(|| {
            format_err!(
                offset,
                "unknown function {idx}: function index out of bounds"
            )
        })
    }

    fn component_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
        self.components.get(idx as usize).copied().ok_or_else(|| {
            format_err!(
                offset,
                "unknown component {idx}: component index out of bounds"
            )
        })
    }

    fn instance_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
        self.instances.get(idx as usize).copied().ok_or_else(|| {
            format_err!(
                offset,
                "unknown instance {idx}: instance index out of bounds"
            )
        })
    }

    fn value_at(&mut self, idx: u32, offset: usize) -> Result<&ComponentValType> {
        match self.values.get_mut(idx as usize) {
            Some((ty, used)) if !*used => {
                *used = true;
                Ok(ty)
            }
            Some(_) => bail!(offset, "value {idx} cannot be used more than once"),
            None => bail!(offset, "unknown value {idx}: value index out of bounds"),
        }
    }

    fn defined_type_at(&self, idx: u32, types: &TypeList, offset: usize) -> Result<TypeId> {
        let id = self.type_at(idx, false, offset)?;
        match &types[id] {
            Type::Defined(_) => Ok(id),
            _ => bail!(offset, "type index {} is not a defined type", idx),
        }
    }

    fn core_function_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
        match self.core_funcs.get(idx as usize) {
            Some(id) => Ok(*id),
            None => bail!(
                offset,
                "unknown core function {idx}: function index out of bounds"
            ),
        }
    }

    fn module_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
        match self.core_modules.get(idx as usize) {
            Some(id) => Ok(*id),
            None => bail!(offset, "unknown module {idx}: module index out of bounds"),
        }
    }

    fn core_instance_at(&self, idx: u32, offset: usize) -> Result<TypeId> {
        match self.core_instances.get(idx as usize) {
            Some(id) => Ok(*id),
            None => bail!(
                offset,
                "unknown core instance {idx}: instance index out of bounds"
            ),
        }
    }

    fn core_instance_export<'a>(
        &self,
        instance_index: u32,
        name: &str,
        types: &'a TypeList,
        offset: usize,
    ) -> Result<&'a EntityType> {
        match types[self.core_instance_at(instance_index, offset)?]
            .unwrap_instance()
            .internal_exports(types)
            .get(name)
        {
            Some(export) => Ok(export),
            None => bail!(
                offset,
                "core instance {instance_index} has no export named `{name}`"
            ),
        }
    }

    fn global_at(&self, idx: u32, offset: usize) -> Result<&GlobalType> {
        match self.core_globals.get(idx as usize) {
            Some(t) => Ok(t),
            None => bail!(offset, "unknown global {idx}: global index out of bounds"),
        }
    }

    fn table_at(&self, idx: u32, offset: usize) -> Result<&TableType> {
        match self.core_tables.get(idx as usize) {
            Some(t) => Ok(t),
            None => bail!(offset, "unknown table {idx}: table index out of bounds"),
        }
    }

    fn memory_at(&self, idx: u32, offset: usize) -> Result<&MemoryType> {
        match self.core_memories.get(idx as usize) {
            Some(t) => Ok(t),
            None => bail!(offset, "unknown memory {idx}: memory index out of bounds"),
        }
    }

    /// Completes the translation of this component, performing final
    /// validation of its structure.
    ///
    /// This method is required to be called for translating all components.
    /// Internally this will convert local data structures into a
    /// `ComponentType` which is suitable to use to describe the type of this
    /// component.
    pub fn finish(&mut self, types: &TypeAlloc, offset: usize) -> Result<ComponentType> {
        let mut ty = ComponentType {
            // Inherit some fields based on translation of the component.
            type_size: self.type_size,
            imports: self.imports.clone(),
            exports: self.exports.clone(),

            // This is filled in as a subset of `self.defined_resources`
            // depending on what's actually used by the exports. See the
            // bottom of this function.
            defined_resources: Default::default(),

            // These are inherited directly from what was calculated for this
            // component.
            imported_resources: mem::take(&mut self.imported_resources)
                .into_iter()
                .collect(),
            explicit_resources: mem::take(&mut self.explicit_resources),
        };

        // Collect all "free variables", or resources, from the imports of this
        // component. None of the resources defined within this component can
        // be used as part of the exports. This set is then used to reject any
        // of `self.defined_resources` which show up.
        let mut free = IndexSet::default();
        for ty in ty.imports.values() {
            types.free_variables_component_entity(ty, &mut free);
        }
        for (resource, _path) in self.defined_resources.iter() {
            // FIXME: this error message is quite opaque and doesn't indicate
            // more contextual information such as:
            //
            // * what was the exported resource found in the imports
            // * which import was the resource found within
            //
            // These are possible to calculate here if necessary, however.
            if free.contains(resource) {
                bail!(offset, "local resource type found in imports");
            }
        }

        // The next step in validation a component, with respect to resources,
        // is to minimize the set of defined resources to only those that
        // are actually used by the exports. This weeds out resources that are
        // defined, used within a component, and never exported, for example.
        //
        // The free variables of all exports are inserted into the `free` set
        // (which is reused from the imports after clearing it). The defined
        // resources calculated for this component are then inserted into this
        // type's list of defined resources if it's contained somewhere in
        // the free variables.
        //
        // Note that at the same time all defined resources must be exported,
        // somehow, transitively from this component. The `explicit_resources`
        // map is consulted for this purpose which lists all explicitly
        // exported resources in the component, regardless from whence they
        // came. If not present in this map then it's not exported and an error
        // is returned.
        //
        // NB: the "types are exported" check is probably sufficient nowadays
        // that the check of the `explicit_resources` map is probably not
        // necessary, but it's left here for completeness and out of an
        // abundance of caution.
        free.clear();
        for ty in ty.exports.values() {
            types.free_variables_component_entity(ty, &mut free);
        }
        for (id, _rep) in mem::take(&mut self.defined_resources) {
            if !free.contains(&id) {
                continue;
            }

            let path = match ty.explicit_resources.get(&id).cloned() {
                Some(path) => path,
                // FIXME: this error message is quite opaque and doesn't
                // indicate more contextual information such as:
                //
                // * which resource wasn't found in an export
                // * which export has a reference to the resource
                //
                // These are possible to calculate here if necessary, however.
                None => bail!(
                    offset,
                    "local resource type found in export but not exported itself"
                ),
            };

            ty.defined_resources.push((id, path));
        }

        Ok(ty)
    }
}

impl KebabNameContext {
    /// Registers that the resource `id` is named `name` within this context.
    fn register(&mut self, name: &str, id: TypeId) {
        let idx = self.all_resource_names.len();
        let prev = self.resource_name_map.insert(id, idx);
        assert!(prev.is_none());
        self.all_resource_names.insert(name.to_string());
    }

    fn validate_extern(
        &self,
        name: ComponentExternName<'_>,
        desc: &str,
        ty: &ComponentEntityType,
        types: &TypeAlloc,
        offset: usize,
        kebab_names: &mut IndexSet<KebabName>,
        items: &mut IndexMap<String, ComponentEntityType>,
        type_size: &mut u32,
    ) -> Result<()> {
        // First validate that `name` is even a valid kebab name, meaning it's
        // in kebab-case, is an ID, etc.
        let kebab = KebabName::new(name, offset).with_context(|| {
            format!("{desc} name `{}` is not a valid extern name", name.as_str())
        })?;

        // Validate that the kebab name, if it has structure such as
        // `[method]a.b`, is indeed valid with respect to known resources.
        self.validate(&kebab, ty, types, offset)
            .with_context(|| format!("{desc} name `{kebab}` is not valid"))?;

        // Top-level kebab-names must all be unique, even between both imports
        // and exports ot a component. For those names consult the `kebab_names`
        // set.
        if let ComponentExternName::Kebab(_) = name {
            if let Some(prev) = kebab_names.replace(kebab.clone()) {
                bail!(
                    offset,
                    "{desc} name `{kebab}` conflicts with previous name `{prev}`",
                );
            }
        }

        // Otherwise all strings must be unique, regardless of their name, so
        // consult the `items` set to ensure that we're not for example
        // importing the same interface ID twice.
        match items.entry(kebab.into()) {
            Entry::Occupied(e) => {
                bail!(
                    offset,
                    "{desc} name `{name}` conflicts with previous name `{prev}`",
                    name = name.as_str(),
                    prev = e.key(),
                );
            }
            Entry::Vacant(e) => {
                e.insert(*ty);
                *type_size = combine_type_sizes(*type_size, ty.type_size(), offset)?;
            }
        }
        Ok(())
    }

    /// Validates that the `name` provided is allowed to have the type `ty`.
    fn validate(
        &self,
        name: &KebabName,
        ty: &ComponentEntityType,
        types: &TypeAlloc,
        offset: usize,
    ) -> Result<()> {
        let func = || {
            let id = match ty {
                ComponentEntityType::Func(id) => *id,
                _ => bail!(offset, "item is not a func"),
            };
            Ok(types[id].unwrap_component_func())
        };
        match name.kind() {
            // Normal kebab name or id? No validation necessary.
            KebabNameKind::Normal(_) | KebabNameKind::Id { .. } => {}

            // Constructors must return `(own $resource)` and the `$resource`
            // must be named within this context to match `rname`
            KebabNameKind::Constructor(rname) => {
                let ty = func()?;
                if ty.results.len() != 1 {
                    bail!(offset, "function should return one value");
                }
                let ty = ty.results[0].1;
                let resource = match ty {
                    ComponentValType::Primitive(_) => None,
                    ComponentValType::Type(ty) => match &types[ty] {
                        Type::Defined(ComponentDefinedType::Own(id)) => Some(id),
                        _ => None,
                    },
                };
                let resource = match resource {
                    Some(id) => id,
                    None => bail!(offset, "function should return `(own $T)`"),
                };
                self.validate_resource_name(*resource, rname, offset)?;
            }

            // Methods must take `(param "self" (borrow $resource))` as the
            // first argument where `$resources` matches the name `resource` as
            // named in this context.
            KebabNameKind::Method { resource, .. } => {
                let ty = func()?;
                if ty.params.len() == 0 {
                    bail!(offset, "function should have at least one argument");
                }
                let (pname, pty) = &ty.params[0];
                if pname.as_str() != "self" {
                    bail!(
                        offset,
                        "function should have a first argument called `self`",
                    );
                }
                let id = match pty {
                    ComponentValType::Primitive(_) => None,
                    ComponentValType::Type(ty) => match &types[*ty] {
                        Type::Defined(ComponentDefinedType::Borrow(id)) => Some(id),
                        _ => None,
                    },
                };
                let id = match id {
                    Some(id) => id,
                    None => bail!(
                        offset,
                        "function should take a first argument of `(borrow $T)`"
                    ),
                };
                self.validate_resource_name(*id, resource, offset)?;
            }

            // Static methods don't have much validation beyond that they must
            // be a function and the resource name referred to must already be
            // in this context.
            KebabNameKind::Static { resource, .. } => {
                func()?;
                if !self.all_resource_names.contains(resource.as_str()) {
                    bail!(offset, "static resource name is not known in this context");
                }
            }
        }

        Ok(())
    }

    fn validate_resource_name(&self, id: TypeId, name: &KebabStr, offset: usize) -> Result<()> {
        let expected_name_idx = match self.resource_name_map.get(&id) {
            Some(idx) => *idx,
            None => {
                bail!(
                    offset,
                    "resource used in function does not have a name in this context"
                )
            }
        };
        let expected_name = &self.all_resource_names[expected_name_idx];
        if name.as_str() != expected_name {
            bail!(
                offset,
                "function does not match expected \
                         resource name `{expected_name}`"
            );
        }
        Ok(())
    }
}

use self::append_only::*;

mod append_only {
    use indexmap::IndexMap;
    use std::hash::Hash;
    use std::ops::Deref;

    pub struct IndexMapAppendOnly<K, V>(IndexMap<K, V>);

    impl<K, V> IndexMapAppendOnly<K, V>
    where
        K: Hash + Eq + PartialEq,
    {
        pub fn insert(&mut self, key: K, value: V) {
            let prev = self.0.insert(key, value);
            assert!(prev.is_none());
        }
    }

    impl<K, V> Deref for IndexMapAppendOnly<K, V> {
        type Target = IndexMap<K, V>;
        fn deref(&self) -> &IndexMap<K, V> {
            &self.0
        }
    }

    impl<K, V> Default for IndexMapAppendOnly<K, V> {
        fn default() -> Self {
            Self(Default::default())
        }
    }

    impl<K, V> IntoIterator for IndexMapAppendOnly<K, V> {
        type IntoIter = <IndexMap<K, V> as IntoIterator>::IntoIter;
        type Item = <IndexMap<K, V> as IntoIterator>::Item;
        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }
}
