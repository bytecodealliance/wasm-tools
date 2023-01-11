use super::{ComponentEncoder, RequiredOptions};
use crate::validation::{
    validate_adapter_module, validate_module, ValidatedAdapter, ValidatedModule,
};
use anyhow::{Context, Result};
use indexmap::{IndexMap, IndexSet};
use std::collections::HashSet;
use std::mem;
use wasmparser::FuncType;
use wit_parser::{
    abi::{AbiVariant, WasmSignature, WasmType},
    Document, Function, InterfaceId, Type, TypeDefKind, TypeId, WorldId,
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
    pub info: Option<ValidatedModule<'a>>,
    /// Validation information about adapters populated only for required
    /// adapters. Additionally stores the gc'd wasm for each adapter.
    pub adapters: IndexMap<&'a str, (ValidatedAdapter<'a>, Vec<u8>)>,
    /// Map of all imports and descriptions of what they're importing.
    pub import_map: IndexMap<&'a str, ImportedInterface<'a>>,
    /// Set of all live types which must be exported either because they're
    /// directly used or because they're transitively used.
    pub live_types: IndexMap<InterfaceId, IndexSet<TypeId>>,
}

#[derive(Debug)]
pub struct ImportedInterface<'a> {
    pub url: &'a str,
    pub direct: Vec<DirectLowering<'a>>,
    pub indirect: Vec<IndirectLowering<'a>>,
    /// Required functions on the interface, or the filter on the functions list
    /// in `interface`.
    pub required: HashSet<&'a str>,
    pub interface: InterfaceId,
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
        let info = if !encoder.module.is_empty() {
            let adapters = encoder
                .adapters
                .keys()
                .map(|s| s.as_str())
                .collect::<IndexSet<_>>();
            Some(validate_module(
                &encoder.module,
                &encoder.metadata,
                &adapters,
            )?)
        } else {
            None
        };

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
        let doc = &self.encoder.metadata.doc;
        for (name, (wasm, metadata, world)) in self.encoder.adapters.iter() {
            let required_by_import = self
                .info
                .as_ref()
                .and_then(|info| info.adapters_required.get(name.as_str()));
            let required = self.required_adapter_exports(doc, *world, required_by_import);
            if required.is_empty() {
                continue;
            }
            let wasm = crate::gc::run(wasm, &required)
                .context("failed to reduce input adapter module to its minimal size")?;
            let info = validate_adapter_module(&wasm, doc, *world, metadata, &required)
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
        doc: &Document,
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
        for (interface, name) in doc.worlds[world].exports() {
            for func in doc.interfaces[interface].functions.iter() {
                let name = func.core_export_name(name);
                let ty = doc.wasm_signature(AbiVariant::GuestExport, func);
                let prev = required.insert(
                    name.into_owned(),
                    wasmparser::FuncType::new(
                        ty.params.iter().map(to_valty),
                        ty.results.iter().map(to_valty),
                    ),
                );
                assert!(prev.is_none());
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
        let doc = &self.encoder.metadata.doc;
        let world = self.encoder.metadata.world;
        let empty = IndexSet::new();
        for (name, interface) in doc.worlds[world].imports.iter() {
            let required = match &self.info {
                Some(info) => Some(info.required_imports.get(name.as_str()).unwrap_or(&empty)),
                None => None,
            };
            add_interface(&mut self.import_map, doc, name, *interface, required)?;
        }
        for (adapter_name, (info, _wasm)) in self.adapters.iter() {
            for (name, required) in info.required_imports.iter() {
                let (_, _, world) = self.encoder.adapters[*adapter_name];
                let interface = doc.worlds[world].imports[*name];
                add_interface(&mut self.import_map, doc, name, interface, Some(required))?;
            }
        }
        return Ok(());

        fn add_interface<'a>(
            import_map: &mut IndexMap<&'a str, ImportedInterface<'a>>,
            doc: &'a Document,
            name: &'a str,
            id: InterfaceId,
            required: Option<&IndexSet<&str>>,
        ) -> Result<()> {
            import_map.entry(name).or_insert_with(|| ImportedInterface {
                interface: id,
                url: doc.interfaces[id].url.as_deref().unwrap_or(""),
                direct: Default::default(),
                indirect: Default::default(),
                required: Default::default(),
            });
            for func in doc.interfaces[id].functions.iter() {
                // If this function isn't actually required then skip it
                if let Some(required) = required {
                    if !required.contains(func.name.as_str()) {
                        continue;
                    }
                }
                add_import(import_map, doc, name, id, func)?;
            }
            Ok(())
        }

        fn add_import<'a>(
            import_map: &mut IndexMap<&'a str, ImportedInterface<'a>>,
            doc: &'a Document,
            name: &'a str,
            id: InterfaceId,
            func: &'a Function,
        ) -> Result<()> {
            let interface = import_map.get_mut(name).unwrap();
            assert_eq!(interface.interface, id);
            if !interface.required.insert(func.name.as_str()) {
                return Ok(());
            }
            let options = RequiredOptions::for_import(doc, func);
            if options.is_empty() {
                interface.direct.push(DirectLowering { name: &func.name });
            } else {
                let sig = doc.wasm_signature(AbiVariant::GuestImport, func);
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
        let mut live_types = mem::take(&mut self.live_types);
        let doc = &self.encoder.metadata.doc;
        for (_name, info) in self.import_map.iter() {
            let interface = &doc.interfaces[info.interface];
            for func in interface.functions.iter() {
                if !info.required.contains(func.name.as_str()) {
                    continue;
                }
                self.add_live_func(func, &mut live_types);
            }
        }
        for (id, _) in doc.worlds[self.encoder.metadata.world].exports() {
            let interface = &doc.interfaces[id];
            for func in interface.functions.iter() {
                self.add_live_func(func, &mut live_types);
            }
        }
        for (_, (_, _, world)) in self.encoder.adapters.iter() {
            for (id, _) in doc.worlds[*world].exports() {
                let interface = &doc.interfaces[id];
                for func in interface.functions.iter() {
                    self.add_live_func(func, &mut live_types);
                }
            }
        }

        self.live_types = live_types;
    }

    fn add_live_func(
        &self,
        func: &Function,
        live_types: &mut IndexMap<InterfaceId, IndexSet<TypeId>>,
    ) {
        for ty in func
            .params
            .iter()
            .map(|(_, t)| t)
            .chain(func.results.iter_types())
        {
            self.add_live(ty, live_types);
        }
    }

    fn add_live(&self, ty: &Type, live_types: &mut IndexMap<InterfaceId, IndexSet<TypeId>>) {
        let id = match *ty {
            Type::Id(id) => id,
            _ => return,
        };
        let ty = &self.encoder.metadata.doc.types[id];
        let interface = match ty.interface {
            Some(id) => id,
            None => return,
        };
        let set = live_types.entry(interface).or_insert_with(Default::default);
        if !set.insert(id) {
            return;
        }
        log::trace!("live {id:?} in {interface:?}");
        match &ty.kind {
            TypeDefKind::Record(t) => {
                for f in t.fields.iter() {
                    self.add_live(&f.ty, live_types);
                }
            }
            TypeDefKind::Tuple(t) => {
                for ty in t.types.iter() {
                    self.add_live(ty, live_types);
                }
            }
            TypeDefKind::Union(t) => {
                for c in t.cases.iter() {
                    self.add_live(&c.ty, live_types);
                }
            }
            TypeDefKind::Variant(t) => {
                for c in t.cases.iter() {
                    if let Some(ty) = &c.ty {
                        self.add_live(ty, live_types);
                    }
                }
            }
            TypeDefKind::Result(t) => {
                if let Some(t) = &t.ok {
                    self.add_live(t, live_types);
                }
                if let Some(t) = &t.err {
                    self.add_live(t, live_types);
                }
            }
            TypeDefKind::Option(t) | TypeDefKind::Type(t) | TypeDefKind::List(t) => {
                self.add_live(t, live_types);
            }
            TypeDefKind::Enum(_) | TypeDefKind::Flags(_) => {}
            TypeDefKind::Stream(_) => todo!(),
            TypeDefKind::Future(_) => todo!(),
        }
    }
}
