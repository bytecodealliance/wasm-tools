use super::{ComponentEncoder, RequiredOptions};
use crate::validation::{
    validate_adapter_module, validate_module, ValidatedAdapter, ValidatedModule,
    BARE_FUNC_MODULE_NAME,
};
use anyhow::{Context, Result};
use indexmap::{IndexMap, IndexSet};
use std::collections::HashSet;
use wasmparser::FuncType;
use wit_parser::{
    abi::{AbiVariant, WasmSignature, WasmType},
    Function, InterfaceId, LiveTypes, Resolve, TypeId, TypeOwner, WorldId, WorldItem,
};

/// Metadata discovered from the state configured in a `ComponentEncoder`.
///
/// This is stored separately from `EncodingState` to be stored as a borrow in
/// `EncodingState` as this information doesn't change throughout the encoding
/// process.
pub struct ComponentWorld<'a> {
    /// Encoder configuration with modules, the document ,etc.
    pub encoder: &'a ComponentEncoder,
    /// Validation information of the input module, or `None` in `--types-only`
    /// mode.
    pub info: ValidatedModule<'a>,
    /// Validation information about adapters populated only for required
    /// adapters. Additionally stores the gc'd wasm for each adapter.
    pub adapters: IndexMap<&'a str, (ValidatedAdapter<'a>, Vec<u8>)>,
    /// Map of all imports and descriptions of what they're importing.
    pub import_map: IndexMap<Option<&'a str>, ImportedInterface<'a>>,
    /// Set of all live types which must be exported either because they're
    /// directly used or because they're transitively used.
    pub live_types: IndexMap<InterfaceId, IndexSet<TypeId>>,
}

#[derive(Debug)]
pub struct ImportedInterface<'a> {
    pub direct: Vec<DirectLowering<'a>>,
    pub indirect: Vec<IndirectLowering<'a>>,
    /// Required functions on the interface, or the filter on the functions list
    /// in `interface`.
    pub required: HashSet<&'a str>,
    pub interface: Option<(InterfaceId, String)>,
}

#[derive(Debug)]
pub struct DirectLowering<'a> {
    pub name: &'a str,
}

#[derive(Debug)]
pub struct IndirectLowering<'a> {
    pub name: &'a str,
    pub sig: WasmSignature,
    pub options: RequiredOptions,
}

impl<'a> ComponentWorld<'a> {
    pub fn new(encoder: &'a ComponentEncoder) -> Result<Self> {
        let adapters = encoder
            .adapters
            .keys()
            .map(|s| s.as_str())
            .collect::<IndexSet<_>>();
        let info = validate_module(&encoder.module, &encoder.metadata, &adapters)?;

        let mut ret = ComponentWorld {
            encoder,
            info,
            adapters: IndexMap::new(),
            import_map: IndexMap::new(),
            live_types: Default::default(),
        };

        ret.process_adapters()?;
        ret.process_imports()?;
        ret.process_live_types();

        Ok(ret)
    }

    /// Process adapters which are required here. Iterate over all
    /// adapters and figure out what functions are required from the
    /// adapter itself, either because the functions are imported by the
    /// main module or they're part of the adapter's exports.
    fn process_adapters(&mut self) -> Result<()> {
        let resolve = &self.encoder.metadata.resolve;
        for (name, (wasm, metadata, world)) in self.encoder.adapters.iter() {
            let required_by_import = self.info.adapters_required.get(name.as_str());
            let required = self.required_adapter_exports(resolve, *world, required_by_import);
            if required.is_empty() {
                continue;
            }
            let wasm = crate::gc::run(wasm, &required, self.info.realloc)
                .context("failed to reduce input adapter module to its minimal size")?;
            let info = validate_adapter_module(&wasm, resolve, *world, metadata, &required)
                .context("failed to validate the imports of the minimized adapter module")?;
            self.adapters.insert(name, (info, wasm));
        }
        Ok(())
    }

    /// Returns the set of functions required to be exported from an adapter,
    /// either because they're exported from the adapter's world or because
    /// they're required as an import to the main module.
    fn required_adapter_exports(
        &self,
        resolve: &Resolve,
        world: WorldId,
        required_by_import: Option<&IndexMap<&str, FuncType>>,
    ) -> IndexMap<String, FuncType> {
        use wasmparser::ValType;

        let mut required = IndexMap::new();
        if let Some(imports) = required_by_import {
            for (name, ty) in imports {
                required.insert(name.to_string(), ty.clone());
            }
        }
        let mut add_func = |func: &Function, name: Option<&str>| {
            let name = func.core_export_name(name);
            let ty = resolve.wasm_signature(AbiVariant::GuestExport, func);
            let prev = required.insert(
                name.into_owned(),
                wasmparser::FuncType::new(
                    ty.params.iter().map(to_valty),
                    ty.results.iter().map(to_valty),
                ),
            );
            assert!(prev.is_none());
        };
        for (name, item) in resolve.worlds[world].exports.iter() {
            match item {
                WorldItem::Function(func) => add_func(func, None),
                WorldItem::Interface(id) => {
                    for (_, func) in resolve.interfaces[*id].functions.iter() {
                        add_func(func, Some(name));
                    }
                }
                WorldItem::Type(_) => {}
            }
        }
        return required;

        fn to_valty(ty: &WasmType) -> ValType {
            match ty {
                WasmType::I32 => ValType::I32,
                WasmType::I64 => ValType::I64,
                WasmType::F32 => ValType::F32,
                WasmType::F64 => ValType::F64,
            }
        }
    }

    /// Fills out the `import_map` field of `self` by determining the live
    /// functions from all imports. This additionally classifies imported
    /// functions into direct or indirect lowerings for managing shims.
    fn process_imports(&mut self) -> Result<()> {
        let resolve = &self.encoder.metadata.resolve;
        let world = self.encoder.metadata.world;
        for (name, item) in resolve.worlds[world].imports.iter() {
            add_item(
                &mut self.import_map,
                resolve,
                name,
                item,
                &self.info.required_imports,
            )?;
        }
        for (adapter_name, (info, _wasm)) in self.adapters.iter() {
            let (_, _, world) = self.encoder.adapters[*adapter_name];
            for (name, item) in resolve.worlds[world].imports.iter() {
                add_item(
                    &mut self.import_map,
                    resolve,
                    name,
                    item,
                    &info.required_imports,
                )?;
            }
        }
        return Ok(());

        fn add_item<'a>(
            import_map: &mut IndexMap<Option<&'a str>, ImportedInterface<'a>>,
            resolve: &'a Resolve,
            name: &'a str,
            item: &'a WorldItem,
            required: &IndexMap<&str, IndexSet<&str>>,
        ) -> Result<()> {
            let empty = IndexSet::new();
            match item {
                WorldItem::Function(func) => {
                    let required = required.get(BARE_FUNC_MODULE_NAME).unwrap_or(&empty);
                    // If this function isn't actually required then skip it
                    if !required.contains(name) {
                        return Ok(());
                    }
                    let interface = import_map.entry(None).or_insert_with(|| ImportedInterface {
                        interface: None,
                        direct: Default::default(),
                        indirect: Default::default(),
                        required: Default::default(),
                    });
                    add_import(interface, resolve, func)
                }
                WorldItem::Interface(id) => {
                    let required = required.get(name).unwrap_or(&empty);
                    let url = resolve.url_of(*id).unwrap_or(String::new());
                    let interface =
                        import_map
                            .entry(Some(name))
                            .or_insert_with(|| ImportedInterface {
                                interface: Some((*id, url)),
                                direct: Default::default(),
                                indirect: Default::default(),
                                required: Default::default(),
                            });
                    for (_name, func) in resolve.interfaces[*id].functions.iter() {
                        // If this function isn't actually required then skip it
                        if required.contains(func.name.as_str()) {
                            add_import(interface, resolve, func)?;
                        }
                    }
                    Ok(())
                }
                WorldItem::Type(_) => Ok(()),
            }
        }

        fn add_import<'a>(
            interface: &mut ImportedInterface<'a>,
            resolve: &'a Resolve,
            func: &'a Function,
        ) -> Result<()> {
            if !interface.required.insert(func.name.as_str()) {
                return Ok(());
            }
            let options = RequiredOptions::for_import(resolve, func);
            if options.is_empty() {
                interface.direct.push(DirectLowering { name: &func.name });
            } else {
                let sig = resolve.wasm_signature(AbiVariant::GuestImport, func);
                interface.indirect.push(IndirectLowering {
                    name: &func.name,
                    sig,
                    options,
                });
            }

            Ok(())
        }
    }

    /// Determines the set of live types which must be exported from each
    /// individual interface by walking over the set of live functions in
    /// imports and recursively walking types.
    fn process_live_types(&mut self) {
        let mut live = LiveTypes::default();
        let resolve = &self.encoder.metadata.resolve;
        let world = &resolve.worlds[self.encoder.metadata.world];
        self.add_live_imports(
            self.encoder.metadata.world,
            &self.info.required_imports,
            &mut live,
        );
        for (_, item) in world.exports.iter() {
            live.add_world_item(resolve, item);
        }
        for (adapter_name, (info, _wasm)) in self.adapters.iter() {
            let (_, _, world) = self.encoder.adapters[*adapter_name];
            self.add_live_imports(world, &info.required_imports, &mut live);
            for (_, item) in resolve.worlds[world].exports.iter() {
                live.add_world_item(resolve, item);
            }
        }

        for live in live.iter() {
            let owner = match resolve.types[live].owner {
                TypeOwner::Interface(id) => id,
                _ => continue,
            };
            self.live_types
                .entry(owner)
                .or_insert(Default::default())
                .insert(live);
        }
    }

    fn add_live_imports(
        &self,
        world: WorldId,
        required: &IndexMap<&str, IndexSet<&str>>,
        live: &mut LiveTypes,
    ) {
        let resolve = &self.encoder.metadata.resolve;
        for (name, item) in resolve.worlds[world].imports.iter() {
            match item {
                WorldItem::Function(func) => {
                    let required = match required.get(BARE_FUNC_MODULE_NAME) {
                        Some(set) => set,
                        None => continue,
                    };
                    if !required.contains(name.as_str()) {
                        continue;
                    }
                    live.add_func(resolve, func);
                }
                WorldItem::Interface(id) => {
                    let required = match required.get(name.as_str()) {
                        Some(set) => set,
                        None => continue,
                    };
                    for (name, func) in resolve.interfaces[*id].functions.iter() {
                        if required.contains(name.as_str()) {
                            live.add_func(resolve, func);
                        }
                    }
                }
                WorldItem::Type(id) => live.add_type_id(resolve, *id),
            }
        }
    }
}
