use super::{ComponentEncoder, RequiredOptions};
use crate::validation::{
    validate_adapter_module, validate_module, ValidatedAdapter, ValidatedModule,
};
use anyhow::{Context, Result};
use indexmap::{IndexMap, IndexSet};
use std::collections::HashSet;
use wasmparser::FuncType;
use wit_parser::{
    abi::{AbiVariant, WasmSignature, WasmType},
    Document, Function, InterfaceId, WorldId,
};

pub struct ComponentWorld<'a> {
    pub encoder: &'a ComponentEncoder,
    pub info: Option<ValidatedModule<'a>>,
    pub adapters: IndexMap<&'a str, (ValidatedAdapter<'a>, Vec<u8>)>,
    pub import_map: IndexMap<&'a str, ImportedInterface<'a>>,
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

#[derive(Debug)]
pub struct ImportedInterface<'a> {
    pub url: &'a str,
    pub direct: Vec<DirectLowering<'a>>,
    pub indirect: Vec<IndirectLowering<'a>>,
    pub required: HashSet<&'a str>,
    pub interface: InterfaceId,
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
        };

        ret.process_adapters()?;
        ret.process_imports()?;

        Ok(ret)
    }

    // Process adapters which are required here. Iterate over all
    // adapters and figure out what functions are required from the
    // adapter itself, either because the functions are imported by the
    // main module or they're part of the adapter's exports.
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

    fn process_imports(&mut self) -> Result<()> {
        let doc = &self.encoder.metadata.doc;
        let world = self.encoder.metadata.world;
        for (name, interface) in doc.worlds[world].imports.iter() {
            let required = match &self.info {
                Some(info) => match info.required_imports.get(name.as_str()) {
                    Some(required) => Some(required),
                    None => continue,
                },
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
            let interface = import_map.entry(name).or_insert_with(|| ImportedInterface {
                interface: id,
                url: doc.interfaces[id].url.as_deref().unwrap_or(""),
                direct: Default::default(),
                indirect: Default::default(),
                required: Default::default(),
            });
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
}
