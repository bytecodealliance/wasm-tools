use crate::encoding::{Instance, Item, LibraryInfo, MainOrAdapter};
use crate::metadata::Bindgen;
use anyhow::{bail, Context, Result};
use indexmap::{map::Entry, IndexMap, IndexSet};
use std::mem;
use wasm_encoder::ExportKind;
use wasmparser::names::{ComponentName, ComponentNameKind};
use wasmparser::{
    types::TypesRef, Encoding, ExternalKind, FuncType, Parser, Payload, TypeRef, ValType,
    ValidPayload, Validator,
};
use wit_parser::{
    abi::{AbiVariant, WasmSignature, WasmType},
    Function, InterfaceId, PackageName, Resolve, TypeDefKind, TypeId, WorldId, WorldItem, WorldKey,
};

fn wasm_sig_to_func_type(signature: WasmSignature) -> FuncType {
    fn from_wasm_type(ty: &WasmType) -> ValType {
        match ty {
            WasmType::I32 => ValType::I32,
            WasmType::I64 => ValType::I64,
            WasmType::F32 => ValType::F32,
            WasmType::F64 => ValType::F64,
            WasmType::Pointer => ValType::I32,
            WasmType::PointerOrI64 => ValType::I64,
            WasmType::Length => ValType::I32,
        }
    }

    FuncType::new(
        signature.params.iter().map(from_wasm_type),
        signature.results.iter().map(from_wasm_type),
    )
}

pub const MAIN_MODULE_IMPORT_NAME: &str = "__main_module__";

/// The module name used when a top-level function in a world is imported into a
/// core wasm module. Note that this is not a valid WIT identifier to avoid
/// clashes with valid WIT interfaces. This is also not empty because LLVM
/// interprets an empty module import string as "not specified" which means it
/// turns into `env`.
pub const BARE_FUNC_MODULE_NAME: &str = "$root";

pub const RESOURCE_DROP: &str = "[resource-drop]";
pub const RESOURCE_REP: &str = "[resource-rep]";
pub const RESOURCE_NEW: &str = "[resource-new]";

pub const POST_RETURN_PREFIX: &str = "cabi_post_";

/// Metadata about a validated module and what was found internally.
///
/// This structure houses information about `imports` and `exports` to the
/// module. Each of these specialized types contains "connection" information
/// between a module's imports/exports and the WIT or component-level constructs
/// they correspond to.
#[derive(Default)]
pub struct ValidatedModule {
    /// Information about a module's imports.
    pub imports: ImportMap,

    /// Information about a module's exports.
    pub exports: ExportMap,
}

impl ValidatedModule {
    fn new(
        bytes: &[u8],
        resolve: &Resolve,
        world: WorldId,
        exports: &IndexSet<WorldKey>,
        adapters: &IndexSet<&str>,
        info: Option<&LibraryInfo>,
    ) -> Result<ValidatedModule> {
        let mut validator = Validator::new();
        let mut ret = ValidatedModule::default();

        for payload in Parser::new(0).parse_all(bytes) {
            let payload = payload?;
            if let ValidPayload::End(_) = validator.payload(&payload)? {
                break;
            }

            let types = validator.types(0).unwrap();

            match payload {
                Payload::Version { encoding, .. } if encoding != Encoding::Module => {
                    bail!("data is not a WebAssembly module");
                }
                Payload::ImportSection(s) => {
                    for import in s {
                        let import = import?;
                        ret.imports
                            .add(import, resolve, world, adapters, info, types)?;
                    }
                }
                Payload::ExportSection(s) => {
                    for export in s {
                        let export = export?;
                        ret.exports.add(export, resolve, world, &exports, types)?;
                    }
                }
                _ => continue,
            }
        }

        ret.exports.validate(resolve, world, exports)?;

        Ok(ret)
    }
}

/// Metadata information about a module's imports.
///
/// This structure maintains the connection between component model "things" and
/// core wasm "things" by ensuring that all imports to the core wasm module are
/// classified by the `Import` enumeration.
#[derive(Default)]
pub struct ImportMap {
    /// The first level of the map here is the module namespace of the import
    /// and the second level of the map is the field namespace. The item is then
    /// how the import is satisfied.
    names: IndexMap<String, ImportInstance>,
}

pub enum ImportInstance {
    /// This import is satisfied by an entire instance of another
    /// adapter/module.
    Whole(MainOrAdapter),

    /// This import is satisfied by filling out each name possibly differently.
    Names(IndexMap<String, Import>),
}

/// The different kinds of items that a module or an adapter can import.
///
/// This is intended to be an exhaustive definition of what can be imported into
/// core modules within a component that wit-component supports.
#[derive(Debug, Clone)]
pub enum Import {
    /// A top-level world function, with the name provided here, is imported
    /// into the module.
    WorldFunc(String),

    /// An interface's function is imported into the module.
    ///
    /// The `WorldKey` here is the name of the interface in the world in
    /// question. The `InterfaceId` is the interface that was imported from and
    /// `String` is the WIT name of the function.
    InterfaceFunc(WorldKey, InterfaceId, String),

    /// An imported resource's destructor is imported.
    ///
    /// The key provided indicates whether it's for the top-level types of the
    /// world (`None`) or an interface (`Some` with the name of the interface).
    /// The `TypeId` is what resource is being dropped.
    ImportedResourceDrop(Option<WorldKey>, TypeId),

    /// A `canon resource.drop` intrinsic for an exported item is being
    /// imported.
    ///
    /// This lists the key of the interface that's exporting the resource plus
    /// the id within that interface.
    ExportedResourceDrop(WorldKey, TypeId),

    /// A `canon resource.new` intrinsic for an exported item is being
    /// imported.
    ///
    /// This lists the key of the interface that's exporting the resource plus
    /// the id within that interface.
    ExportedResourceNew(WorldKey, TypeId),

    /// A `canon resource.rep` intrinsic for an exported item is being
    /// imported.
    ///
    /// This lists the key of the interface that's exporting the resource plus
    /// the id within that interface.
    ExportedResourceRep(WorldKey, TypeId),

    /// An export of an adapter is being imported with the specified type.
    ///
    /// This is used for when the main module imports an adapter function. The
    /// adapter name and function name match the module's own import, and the
    /// type must match that listed here.
    AdapterExport(FuncType),

    /// An adapter is importing the memory of the main module.
    ///
    /// (should be combined with `MainModuleExport` below one day)
    MainModuleMemory,

    /// An adapter is importing an arbitrary item from the main module.
    MainModuleExport { name: String, kind: ExportKind },

    /// An arbitrary item from either the main module or an adapter is being
    /// imported.
    ///
    /// (should probably subsume `MainModule*` and maybe `AdapterExport` above
    /// one day.
    Item(Item),
}

impl ImportMap {
    /// Returns whether the top-level world function `func` is imported.
    pub fn uses_toplevel_func(&self, func: &str) -> bool {
        let item = self
            .names
            .get(BARE_FUNC_MODULE_NAME)
            .and_then(|map| match map {
                ImportInstance::Names(names) => names.get(func),
                _ => None,
            });
        matches!(item, Some(Import::WorldFunc(_)))
    }

    /// Returns whether the interface function specified is imported.
    pub fn uses_interface_func(&self, interface: InterfaceId, func: &str) -> bool {
        self.imports().any(|(_, _, import)| match import {
            Import::InterfaceFunc(_, id, name) => *id == interface && name == func,
            _ => false,
        })
    }

    /// Returns whether the specified resource's drop method is needed to import.
    pub fn uses_imported_resource_drop(&self, resource: TypeId) -> bool {
        self.imports().any(|(_, _, import)| match import {
            Import::ImportedResourceDrop(_, id) => resource == *id,
            _ => false,
        })
    }

    /// Returns the list of items that the adapter named `name` must export.
    pub fn required_from_adapter(&self, name: &str) -> IndexMap<String, FuncType> {
        let names = match self.names.get(name) {
            Some(ImportInstance::Names(names)) => names,
            _ => return IndexMap::new(),
        };
        names
            .iter()
            .map(|(name, import)| {
                (
                    name.clone(),
                    match import {
                        Import::AdapterExport(ty) => ty.clone(),
                        _ => unreachable!(),
                    },
                )
            })
            .collect()
    }

    /// Returns an iterator over all individual imports registered in this map.
    ///
    /// Note that this doesn't iterate over the "whole instance" imports.
    pub fn imports(&self) -> impl Iterator<Item = (&str, &str, &Import)> + '_ {
        self.names
            .iter()
            .filter_map(|(module, m)| match m {
                ImportInstance::Names(names) => Some((module, names)),
                ImportInstance::Whole(_) => None,
            })
            .flat_map(|(module, m)| {
                m.iter()
                    .map(move |(field, import)| (module.as_str(), field.as_str(), import))
            })
    }

    /// Returns the map for how all imports must be satisfied.
    pub fn modules(&self) -> &IndexMap<String, ImportInstance> {
        &self.names
    }

    /// Helper function used during validation to build up this `ImportMap`.
    fn add(
        &mut self,
        import: wasmparser::Import<'_>,
        resolve: &Resolve,
        world: WorldId,
        adapters: &IndexSet<&str>,
        library_info: Option<&LibraryInfo>,
        types: TypesRef<'_>,
    ) -> Result<()> {
        if self.classify_import_with_library(import, library_info)? {
            return Ok(());
        }
        let item = self
            .classify(import, resolve, world, adapters, types)
            .with_context(|| {
                format!(
                    "failed to resolve import `{}::{}`",
                    import.module, import.name,
                )
            })?;
        self.insert_import(import, item)
    }

    fn classify(
        &self,
        import: wasmparser::Import<'_>,
        resolve: &Resolve,
        world_id: WorldId,
        adapters: &IndexSet<&str>,
        types: TypesRef<'_>,
    ) -> Result<Import> {
        let world = &resolve.worlds[world_id];

        // Special-case the main module's memory imported into adapters which
        // currently with `wasm-ld` is not easily configurable.
        if import.module == "env" && import.name == "memory" {
            return Ok(Import::MainModuleMemory);
        }

        // Special-case imports from the main module into adapters.
        if import.module == MAIN_MODULE_IMPORT_NAME {
            return Ok(Import::MainModuleExport {
                name: import.name.to_string(),
                kind: match import.ty {
                    TypeRef::Func(_) => ExportKind::Func,
                    TypeRef::Table(_) => ExportKind::Table,
                    TypeRef::Memory(_) => ExportKind::Memory,
                    TypeRef::Global(_) => ExportKind::Global,
                    TypeRef::Tag(_) => ExportKind::Tag,
                },
            });
        }

        let ty_index = match import.ty {
            TypeRef::Func(ty) => ty,
            _ => bail!("module is only allowed to import functions"),
        };
        let ty = types[types.core_type_at(ty_index).unwrap_sub()].unwrap_func();

        // Handle top-level function imports if they're going through the "bare
        // name" representing the world root.
        if import.module == BARE_FUNC_MODULE_NAME {
            let name = import.name;
            let key = WorldKey::Name(name.to_string());
            if let Some(WorldItem::Function(func)) = world.imports.get(&key) {
                validate_func(resolve, ty, func, AbiVariant::GuestImport)?;
                return Ok(Import::WorldFunc(func.name.clone()));
            }

            let get_resource = resource_test_for_world(resolve, world_id);
            if let Some(id) = valid_resource_drop(name, ty, get_resource)? {
                return Ok(Import::ImportedResourceDrop(None, id));
            }

            match world.imports.get(&key) {
                Some(_) => bail!("expected world top-level import `{name}` to be a function"),
                None => bail!("no top-level imported function `{name}` specified"),
            }
        }

        // Handle main module imports that match known adapters and set it up as
        // an import of an adapter export.
        if adapters.contains(import.module) {
            return Ok(Import::AdapterExport(ty.clone()));
        }

        // Handle imports which are used to manipulate state for exported
        // resources.
        if let Some(suffix) = import.module.strip_prefix("[export]") {
            let (key, id) = self.module_to_interface(suffix, resolve, &world.exports)?;
            let get_resource = resource_test_for_interface(resolve, id);

            return if let Some(ty) = valid_resource_drop(import.name, ty, &get_resource)? {
                Ok(Import::ExportedResourceDrop(key, ty))
            } else if let Some(id) = import
                .name
                .strip_prefix(RESOURCE_NEW)
                .and_then(&get_resource)
            {
                let expected = FuncType::new([ValType::I32], [ValType::I32]);
                validate_func_sig(import.name, &expected, ty)?;
                Ok(Import::ExportedResourceNew(key, id))
            } else if let Some(id) = import
                .name
                .strip_prefix(RESOURCE_REP)
                .and_then(&get_resource)
            {
                let expected = FuncType::new([ValType::I32], [ValType::I32]);
                validate_func_sig(import.name, &expected, ty)?;
                Ok(Import::ExportedResourceRep(key, id))
            } else {
                bail!("unknown function `{}`", import.name)
            };
        }

        // And finally handle imports of functions from interfaces here.
        let (key, id) = self.module_to_interface(import.module, resolve, &world.imports)?;
        let interface = &resolve.interfaces[id];
        let get_resource = resource_test_for_interface(resolve, id);
        if let Some(f) = interface.functions.get(import.name) {
            validate_func(resolve, ty, f, AbiVariant::GuestImport).with_context(|| {
                let name = resolve.name_world_key(&key);
                format!("failed to validate import interface `{name}`")
            })?;
            Ok(Import::InterfaceFunc(key, id, f.name.clone()))
        } else if let Some(ty) = valid_resource_drop(import.name, ty, get_resource)? {
            Ok(Import::ImportedResourceDrop(Some(key), ty))
        } else {
            bail!(
                "import interface `{}` is missing function \
                 `{}` that is required by the module",
                import.module,
                import.name,
            )
        }
    }

    fn module_to_interface(
        &self,
        module: &str,
        resolve: &Resolve,
        items: &IndexMap<WorldKey, WorldItem>,
    ) -> Result<(WorldKey, InterfaceId)> {
        // First see if this is a bare name
        let bare_name = WorldKey::Name(module.to_string());
        if let Some(WorldItem::Interface { id, .. }) = items.get(&bare_name) {
            return Ok((bare_name, *id));
        }

        // ... and if this isn't a bare name then it's time to do some parsing
        // related to interfaces, versions, and such. First up the `module` name
        // is parsed as a normal component name from `wasmparser` to see if it's
        // of the "interface kind". If it's not then that means the above match
        // should have been a hit but it wasn't, so an error is returned.
        let kebab_name = ComponentName::new(module, 0);
        let name = match kebab_name.as_ref().map(|k| k.kind()) {
            Ok(ComponentNameKind::Interface(name)) => name,
            _ => bail!("module requires an import interface named `{module}`"),
        };

        // Prioritize an exact match based on versions, so try that first.
        let pkgname = PackageName {
            namespace: name.namespace().to_string(),
            name: name.package().to_string(),
            version: name.version(),
        };
        if let Some(pkg) = resolve.package_names.get(&pkgname) {
            if let Some(id) = resolve.packages[*pkg]
                .interfaces
                .get(name.interface().as_str())
            {
                let key = WorldKey::Interface(*id);
                if items.contains_key(&key) {
                    return Ok((key, *id));
                }
            }
        }

        // If an exact match wasn't found then instead search for the first
        // match based on versions. This means that a core wasm import for
        // "1.2.3" might end up matching an interface at "1.2.4", for example.
        // (or "1.2.2", depending on what's available).
        for (key, _) in items {
            let id = match key {
                WorldKey::Interface(id) => *id,
                WorldKey::Name(_) => continue,
            };
            // Make sure the interface names match
            let interface = &resolve.interfaces[id];
            if interface.name.as_ref().unwrap() != name.interface().as_str() {
                continue;
            }

            // Make sure the package name (without version) matches
            let pkg = &resolve.packages[interface.package.unwrap()];
            if pkg.name.namespace != pkgname.namespace || pkg.name.name != pkgname.name {
                continue;
            }

            let module_version = match name.version() {
                Some(version) => version,
                None => continue,
            };
            let pkg_version = match &pkg.name.version {
                Some(version) => version,
                None => continue,
            };

            // Test if the two semver versions are compatible
            let module_compat = PackageName::version_compat_track(&module_version);
            let pkg_compat = PackageName::version_compat_track(pkg_version);
            if module_compat == pkg_compat {
                return Ok((key.clone(), id));
            }
        }

        bail!("module requires an import interface named `{module}`")
    }

    fn classify_import_with_library(
        &mut self,
        import: wasmparser::Import<'_>,
        library_info: Option<&LibraryInfo>,
    ) -> Result<bool> {
        let info = match library_info {
            Some(info) => info,
            None => return Ok(false),
        };
        let Some((_, instance)) = info
            .arguments
            .iter()
            .find(|(name, _items)| *name == import.module)
        else {
            return Ok(false);
        };
        match instance {
            Instance::MainOrAdapter(module) => match self.names.get(import.module) {
                Some(ImportInstance::Whole(which)) => {
                    if which != module {
                        bail!("different whole modules imported under the same name");
                    }
                }
                Some(ImportInstance::Names(_)) => {
                    bail!("cannot mix individual imports and whole module imports")
                }
                None => {
                    let instance = ImportInstance::Whole(module.clone());
                    self.names.insert(import.module.to_string(), instance);
                }
            },
            Instance::Items(items) => {
                let Some(item) = items.iter().find(|i| i.alias == import.name) else {
                    return Ok(false);
                };
                self.insert_import(import, Import::Item(item.clone()))?;
            }
        }
        Ok(true)
    }

    fn insert_import(&mut self, import: wasmparser::Import<'_>, item: Import) -> Result<()> {
        let entry = self
            .names
            .entry(import.module.to_string())
            .or_insert(ImportInstance::Names(IndexMap::default()));
        let names = match entry {
            ImportInstance::Names(names) => names,
            _ => bail!("cannot mix individual imports with module imports"),
        };
        let entry = match names.entry(import.name.to_string()) {
            Entry::Occupied(_) => {
                bail!(
                    "module has duplicate import for `{}::{}`",
                    import.module,
                    import.name
                );
            }
            Entry::Vacant(v) => v,
        };
        log::trace!(
            "classifying import `{}::{} as {item:?}",
            import.module,
            import.name
        );
        entry.insert(item);
        Ok(())
    }
}

/// Dual of `ImportMap` except describes the exports of a module instead of the
/// imports.
#[derive(Default)]
pub struct ExportMap {
    names: IndexMap<String, Export>,
    raw_exports: IndexMap<String, FuncType>,
}

/// All possible (known) exports from a core wasm module that are recognized and
/// handled during the componentization process.
pub enum Export {
    /// An export of a top-level function of a world, where the world function
    /// is named here.
    WorldFunc(String),

    /// A post-return for a top-level function of a world.
    WorldFuncPostReturn(String),

    /// An export of a function in an interface.
    InterfaceFunc(InterfaceId, String),

    /// A post-return for the above function.
    InterfaceFuncPostReturn(InterfaceId, String),

    /// A destructor for an exported resource.
    ResourceDtor(TypeId),

    /// Memory, typically for an adapter.
    Memory,

    /// `cabi_realloc`
    GeneralPurposeRealloc,

    /// `cabi_export_realloc`
    GeneralPurposeExportRealloc,

    /// `cabi_import_realloc`
    GeneralPurposeImportRealloc,

    /// `_initialize`
    Initialize,

    /// `cabi_realloc_adapter`
    ReallocForAdapter,
}

impl ExportMap {
    fn add(
        &mut self,
        export: wasmparser::Export<'_>,
        resolve: &Resolve,
        world: WorldId,
        exports: &IndexSet<WorldKey>,
        types: TypesRef<'_>,
    ) -> Result<()> {
        if let Some(item) = self.classify(export, resolve, world, exports, types)? {
            let prev = self.names.insert(export.name.to_string(), item);
            assert!(prev.is_none());
        }
        Ok(())
    }

    fn classify(
        &mut self,
        export: wasmparser::Export<'_>,
        resolve: &Resolve,
        world: WorldId,
        exports: &IndexSet<WorldKey>,
        types: TypesRef<'_>,
    ) -> Result<Option<Export>> {
        match export.kind {
            ExternalKind::Func => {}
            ExternalKind::Memory => return Ok(Some(Export::Memory)),
            _ => return Ok(None),
        }

        let ty = types[types.core_function_at(export.index)].unwrap_func();
        self.raw_exports.insert(export.name.to_string(), ty.clone());

        // Handle a few special-cased names first.
        if export.name == "cabi_realloc" || export.name == "canonical_abi_realloc" {
            return Ok(Some(Export::GeneralPurposeRealloc));
        } else if export.name == "cabi_import_realloc" {
            return Ok(Some(Export::GeneralPurposeImportRealloc));
        } else if export.name == "cabi_export_realloc" {
            return Ok(Some(Export::GeneralPurposeExportRealloc));
        } else if export.name == "cabi_realloc_adapter" {
            return Ok(Some(Export::ReallocForAdapter));
        } else if export.name == "_initialize" {
            return Ok(Some(Export::Initialize));
        }

        // Try to match this to a known WIT export that `exports` allows.
        if let Some((key, id, f)) = self.match_wit_export(export.name, resolve, world, exports) {
            validate_func(resolve, ty, f, AbiVariant::GuestExport).with_context(|| {
                let key = resolve.name_world_key(key);
                format!("failed to validate export for `{key}`")
            })?;
            match id {
                Some(id) => {
                    return Ok(Some(Export::InterfaceFunc(id, f.name.clone())));
                }
                None => {
                    return Ok(Some(Export::WorldFunc(f.name.clone())));
                }
            }
        }

        // See if this is a post-return for any known WIT export.
        if let Some(suffix) = export.name.strip_prefix(POST_RETURN_PREFIX) {
            if let Some((key, id, f)) = self.match_wit_export(suffix, resolve, world, exports) {
                validate_post_return(resolve, ty, f).with_context(|| {
                    let key = resolve.name_world_key(key);
                    format!("failed to validate export for `{key}`")
                })?;
                match id {
                    Some(id) => {
                        return Ok(Some(Export::InterfaceFuncPostReturn(id, f.name.clone())));
                    }
                    None => {
                        return Ok(Some(Export::WorldFuncPostReturn(f.name.clone())));
                    }
                }
            }
        }

        // And, finally, see if it matches a known destructor.
        if let Some(dtor) = self.match_wit_resource_dtor(export.name, resolve, world, exports) {
            let expected = FuncType::new([ValType::I32], []);
            validate_func_sig(export.name, &expected, ty)?;
            return Ok(Some(Export::ResourceDtor(dtor)));
        }

        log::debug!("unknown export `{}`", export.name);
        Ok(None)
    }

    fn match_wit_export<'a>(
        &self,
        export_name: &str,
        resolve: &'a Resolve,
        world: WorldId,
        exports: &'a IndexSet<WorldKey>,
    ) -> Option<(&'a WorldKey, Option<InterfaceId>, &'a Function)> {
        let world = &resolve.worlds[world];
        for name in exports {
            match &world.exports[name] {
                WorldItem::Function(f) => {
                    if f.core_export_name(None) == export_name {
                        return Some((name, None, f));
                    }
                }
                WorldItem::Interface { id, .. } => {
                    let string = resolve.name_world_key(name);
                    for (_, func) in resolve.interfaces[*id].functions.iter() {
                        if func.core_export_name(Some(&string)) == export_name {
                            return Some((name, Some(*id), func));
                        }
                    }
                }

                WorldItem::Type(_) => unreachable!(),
            }
        }

        None
    }

    fn match_wit_resource_dtor<'a>(
        &self,
        export_name: &str,
        resolve: &'a Resolve,
        world: WorldId,
        exports: &'a IndexSet<WorldKey>,
    ) -> Option<TypeId> {
        let world = &resolve.worlds[world];
        for name in exports {
            let id = match &world.exports[name] {
                WorldItem::Interface { id, .. } => *id,
                WorldItem::Function(_) => continue,
                WorldItem::Type(_) => unreachable!(),
            };
            let name = resolve.name_world_key(name);
            let resource = match export_name
                .strip_prefix(&name)
                .and_then(|s| s.strip_prefix("#[dtor]"))
                .and_then(|r| resolve.interfaces[id].types.get(r))
            {
                Some(id) => *id,
                None => continue,
            };

            match resolve.types[resource].kind {
                TypeDefKind::Resource => {}
                _ => continue,
            }

            return Some(resource);
        }

        None
    }

    /// Returns the name of the post-return export, if any, for the `interface`
    /// and `func` combo.
    pub fn post_return(&self, interface: Option<InterfaceId>, func: &Function) -> Option<&str> {
        self.find(|m| match (m, interface) {
            (Export::WorldFuncPostReturn(f), None) => func.name == *f,
            (Export::InterfaceFuncPostReturn(i, f), Some(id)) => *i == id && func.name == *f,
            _ => false,
        })
    }

    /// Returns the realloc that the exported function `interface` and `func`
    /// are using.
    pub fn export_realloc_for(
        &self,
        interface: Option<InterfaceId>,
        func: &Function,
    ) -> Option<&str> {
        // TODO: This realloc detection should probably be improved with
        // some sort of scheme to have per-function reallocs like
        // `cabi_realloc_{name}` or something like that.
        let _ = (interface, func);

        if let Some(name) = self.find(|m| matches!(m, Export::GeneralPurposeExportRealloc)) {
            return Some(name);
        }
        self.general_purpose_realloc()
    }

    /// Returns the realloc that the imported function `interface` and `func`
    /// are using.
    pub fn import_realloc_for(&self, interface: Option<InterfaceId>, func: &str) -> Option<&str> {
        // TODO: This realloc detection should probably be improved with
        // some sort of scheme to have per-function reallocs like
        // `cabi_realloc_{name}` or something like that.
        let _ = (interface, func);

        if let Some(name) = self.find(|m| matches!(m, Export::GeneralPurposeImportRealloc)) {
            return Some(name);
        }
        self.general_purpose_realloc()
    }

    /// Returns the realloc that the main module is exporting into the adapter.
    pub fn realloc_to_import_into_adapter(&self) -> Option<&str> {
        if let Some(name) = self.find(|m| matches!(m, Export::ReallocForAdapter)) {
            return Some(name);
        }
        self.general_purpose_realloc()
    }

    fn general_purpose_realloc(&self) -> Option<&str> {
        self.find(|m| matches!(m, Export::GeneralPurposeRealloc))
    }

    /// Returns the memory, if exported, for this module.
    pub fn memory(&self) -> Option<&str> {
        self.find(|m| matches!(m, Export::Memory))
    }

    /// Returns the `_initialize` intrinsic, if exported, for this module.
    pub fn initialize(&self) -> Option<&str> {
        self.find(|m| matches!(m, Export::Initialize))
    }

    /// Returns destructor for the exported resource `ty`, if it was listed.
    pub fn resource_dtor(&self, ty: TypeId) -> Option<&str> {
        self.find(|m| match m {
            Export::ResourceDtor(t) => *t == ty,
            _ => false,
        })
    }

    /// NB: this is a linear search and if that's ever a problem this should
    /// build up an inverse map during construction to accelerate it.
    fn find(&self, f: impl Fn(&Export) -> bool) -> Option<&str> {
        let (name, _) = self.names.iter().filter(|(_, m)| f(m)).next()?;
        Some(name)
    }

    /// Iterates over all exports of this module.
    pub fn iter(&self) -> impl Iterator<Item = (&str, &Export)> + '_ {
        self.names.iter().map(|(n, e)| (n.as_str(), e))
    }

    fn validate(
        &self,
        resolve: &Resolve,
        world: WorldId,
        exports: &IndexSet<WorldKey>,
    ) -> Result<()> {
        // Multi-memory isn't supported because otherwise we don't know what
        // memory to put things in.
        if self
            .names
            .values()
            .filter(|m| matches!(m, Export::Memory))
            .count()
            > 1
        {
            bail!("cannot componentize module that exports multiple memories")
        }

        // All of `exports` must be exported and found within this module.
        for export in exports {
            let require_interface_func = |interface: InterfaceId, name: &str| -> Result<()> {
                let result = self.find(|e| match e {
                    Export::InterfaceFunc(id, s) => interface == *id && name == s,
                    _ => false,
                });
                if result.is_some() {
                    Ok(())
                } else {
                    let export = resolve.name_world_key(export);
                    bail!("failed to find export of interface `{export}` function `{name}`")
                }
            };
            let require_world_func = |name: &str| -> Result<()> {
                let result = self.find(|e| match e {
                    Export::WorldFunc(s) => name == s,
                    _ => false,
                });
                if result.is_some() {
                    Ok(())
                } else {
                    bail!("failed to find export of function `{name}`")
                }
            };
            match &resolve.worlds[world].exports[export] {
                WorldItem::Interface { id, .. } => {
                    for (name, _) in resolve.interfaces[*id].functions.iter() {
                        require_interface_func(*id, name)?;
                    }
                }
                WorldItem::Function(f) => {
                    require_world_func(&f.name)?;
                }
                WorldItem::Type(_) => unreachable!(),
            }
        }

        Ok(())
    }
}

/// This function validates the following:
///
/// * The `bytes` represent a valid core WebAssembly module.
/// * The module's imports are all satisfied by the given `imports` interfaces
///   or the `adapters` set.
/// * The given default and exported interfaces are satisfied by the module's
///   exports.
///
/// The `ValidatedModule` return value contains the metadata which describes the
/// input module on success. This is then further used to generate a component
/// for this module.
pub fn validate_module(
    bytes: &[u8],
    metadata: &Bindgen,
    exports: &IndexSet<WorldKey>,
    adapters: &IndexSet<&str>,
) -> Result<ValidatedModule> {
    ValidatedModule::new(
        bytes,
        &metadata.resolve,
        metadata.world,
        exports,
        adapters,
        None,
    )
}

/// This function will validate the `bytes` provided as a wasm adapter module.
/// Notably this will validate the wasm module itself in addition to ensuring
/// that it has the "shape" of an adapter module. Current constraints are:
///
/// * The adapter module can import only one memory
/// * The adapter module can only import from the name of `interface` specified,
///   and all function imports must match the `required` types which correspond
///   to the lowered types of the functions in `interface`.
///
/// The wasm module passed into this function is the output of the GC pass of an
/// adapter module's original source. This means that the adapter module is
/// already minimized and this is a double-check that the minimization pass
/// didn't accidentally break the wasm module.
///
/// If `is_library` is true, we waive some of the constraints described above,
/// allowing the module to import tables and globals, as well as import
/// functions at the world level, not just at the interface level.
pub fn validate_adapter_module(
    bytes: &[u8],
    resolve: &Resolve,
    world: WorldId,
    required_by_import: &IndexMap<String, FuncType>,
    exports: &IndexSet<WorldKey>,
    library_info: Option<&LibraryInfo>,
    adapters: &IndexSet<&str>,
) -> Result<ValidatedModule> {
    let ret = ValidatedModule::new(bytes, resolve, world, exports, adapters, library_info)?;

    for (name, required_ty) in required_by_import {
        let actual = match ret.exports.raw_exports.get(name) {
            Some(ty) => ty,
            None => bail!("adapter module did not export `{name}`"),
        };
        validate_func_sig(name, required_ty, &actual)?;
    }

    Ok(ret)
}

fn valid_resource_drop(
    func_name: &str,
    ty: &FuncType,
    get_resource: impl Fn(&str) -> Option<TypeId>,
) -> Result<Option<TypeId>> {
    if let Some(resource_name) = func_name.strip_prefix(RESOURCE_DROP) {
        if let Some(id) = get_resource(resource_name) {
            let expected = FuncType::new([ValType::I32], []);
            validate_func_sig(func_name, &expected, ty)?;
            return Ok(Some(id));
        }
    }
    Ok(None)
}

fn resource_test_for_interface<'a>(
    resolve: &'a Resolve,
    id: InterfaceId,
) -> impl Fn(&str) -> Option<TypeId> + 'a {
    let interface = &resolve.interfaces[id];
    move |name: &str| {
        let ty = match interface.types.get(name) {
            Some(ty) => *ty,
            None => return None,
        };
        if matches!(resolve.types[ty].kind, TypeDefKind::Resource) {
            Some(ty)
        } else {
            None
        }
    }
}

fn resource_test_for_world<'a>(
    resolve: &'a Resolve,
    id: WorldId,
) -> impl Fn(&str) -> Option<TypeId> + 'a {
    let world = &resolve.worlds[id];
    move |name: &str| match world.imports.get(&WorldKey::Name(name.to_string()))? {
        WorldItem::Type(r) => {
            if matches!(resolve.types[*r].kind, TypeDefKind::Resource) {
                Some(*r)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn validate_func(
    resolve: &Resolve,
    ty: &wasmparser::FuncType,
    func: &Function,
    abi: AbiVariant,
) -> Result<()> {
    validate_func_sig(
        &func.name,
        &wasm_sig_to_func_type(resolve.wasm_signature(abi, func)),
        ty,
    )
}

fn validate_post_return(
    resolve: &Resolve,
    ty: &wasmparser::FuncType,
    func: &Function,
) -> Result<()> {
    // The expected signature of a post-return function is to take all the
    // parameters that are returned by the guest function and then return no
    // results. Model this by calculating the signature of `func` and then
    // moving its results into the parameters list while emptying out the
    // results.
    let mut sig = resolve.wasm_signature(AbiVariant::GuestExport, func);
    sig.params = mem::take(&mut sig.results);
    validate_func_sig(
        &format!("{} post-return", func.name),
        &wasm_sig_to_func_type(sig),
        ty,
    )
}

fn validate_func_sig(name: &str, expected: &FuncType, ty: &wasmparser::FuncType) -> Result<()> {
    if ty != expected {
        bail!(
            "type mismatch for function `{}`: expected `{:?} -> {:?}` but found `{:?} -> {:?}`",
            name,
            expected.params(),
            expected.results(),
            ty.params(),
            ty.results()
        );
    }

    Ok(())
}
