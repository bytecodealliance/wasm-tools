//! Support for encoding a core wasm module into a component.
//!
//! This module, at a high level, is tasked with transforming a core wasm
//! module into a component. This will process the imports/exports of the core
//! wasm module and translate between the `wit-parser` AST and the component
//! model binary format, producing a final component which sill import
//! `*.wit` defined interfaces and export `*.wit` defined interfaces as well
//! with everything wired up internally according to the canonical ABI and such.
//!
//! This doc block here is not currently 100% complete and doesn't cover the
//! full functionality of this module.
//!
//! # Adapter Modules
//!
//! One feature of this encoding process which is non-obvious is the support for
//! "adapter modules". The general idea here is that historical host API
//! definitions have been around for quite some time, such as
//! `wasi_snapshot_preview1`, but these host API definitions are not compatible
//! with the canonical ABI or component model exactly. These APIs, however, can
//! in most situations be roughly adapted to component-model equivalents. This
//! is where adapter modules come into play, they're converting from some
//! arbitrary API/ABI into a component-model using API.
//!
//! An adapter module is a separately compiled `*.wasm` blob which will export
//! functions matching the desired ABI (e.g. exporting functions matching the
//! `wasi_snapshot_preview1` ABI). The `*.wasm` blob will then import functions
//! in the canonical ABI and internally adapt the exported functions to the
//! imported functions. The encoding support in this module is what wires
//! everything up and makes sure that everything is imported and exported to the
//! right place. Adapter modules currently always use "indirect lowerings"
//! meaning that a shim module is created and provided as the imports to the
//! main core wasm module, and the shim module is "filled in" at a later time
//! during the instantiation process.
//!
//! Adapter modules are not intended to be general purpose and are currently
//! very restrictive, namely:
//!
//! * They must import a linear memory and not define their own linear memory
//!   otherwise. In other words they import memory and cannot use multi-memory.
//! * They cannot define any `elem` or `data` segments since otherwise there's
//!   no knowledge ahead-of-time of where their data or element segments could
//!   go. This means things like no panics, no indirect calls, etc.
//! * Only one mutable global is allowed and it's assumed to be the stack
//!   pointer. This stack pointer is automatically configured with an injected
//!   `start` function that is allocated with `memory.grow (i32.const 1)`,
//!   meaning that the shim module has 64k of stack space and no protection if
//!   that overflows.
//!
//! This means that adapter modules are not meant to be written by everyone.
//! It's assumed that these will be relatively few and far between yet still a
//! crucial part of the transition process from to the component model since
//! otherwise there's no way to run a `wasi_snapshot_preview1` module within the
//! component model.

use crate::builder::ComponentBuilder;
use crate::metadata::{self, Bindgen, ModuleMetadata};
use crate::validation::{ValidatedModule, MAIN_MODULE_IMPORT_NAME};
use crate::StringEncoding;
use anyhow::{anyhow, bail, Context, Result};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::hash::Hash;
use wasm_encoder::*;
use wasmparser::{Validator, WasmFeatures};
use wit_parser::{
    abi::{AbiVariant, WasmSignature, WasmType},
    Function, InterfaceId, Resolve, Type, TypeDefKind, TypeId, TypeOwner, WorldId, WorldItem,
};

const INDIRECT_TABLE_NAME: &str = "$imports";

mod wit;
pub use wit::encode;

mod types;
use types::{InstanceTypeEncoder, RootTypeEncoder, ValtypeEncoder};
mod world;
use world::{ComponentWorld, ImportedInterface};

fn to_val_type(ty: &WasmType) -> ValType {
    match ty {
        WasmType::I32 => ValType::I32,
        WasmType::I64 => ValType::I64,
        WasmType::F32 => ValType::F32,
        WasmType::F64 => ValType::F64,
    }
}

bitflags::bitflags! {
    /// Options in the `canon lower` or `canon lift` required for a particular
    /// function.
    pub struct RequiredOptions: u8 {
        /// A memory must be specified, typically the "main module"'s memory
        /// export.
        const MEMORY = 1 << 0;
        /// A `realloc` function must be specified, typically named
        /// `cabi_realloc`.
        const REALLOC = 1 << 1;
        /// A string encoding must be specified, which is always utf-8 for now
        /// today.
        const STRING_ENCODING = 1 << 2;
    }
}

impl RequiredOptions {
    fn for_import(resolve: &Resolve, func: &Function) -> RequiredOptions {
        let sig = resolve.wasm_signature(AbiVariant::GuestImport, func);
        let mut ret = RequiredOptions::empty();
        // Lift the params and lower the results for imports
        ret.add_lift(TypeContents::for_types(
            resolve,
            func.params.iter().map(|(_, t)| t),
        ));
        ret.add_lower(TypeContents::for_types(resolve, func.results.iter_types()));

        // If anything is indirect then `memory` will be required to read the
        // indirect values.
        if sig.retptr || sig.indirect_params {
            ret |= RequiredOptions::MEMORY;
        }
        ret
    }

    fn for_export(resolve: &Resolve, func: &Function) -> RequiredOptions {
        let sig = resolve.wasm_signature(AbiVariant::GuestExport, func);
        let mut ret = RequiredOptions::empty();
        // Lower the params and lift the results for exports
        ret.add_lower(TypeContents::for_types(
            resolve,
            func.params.iter().map(|(_, t)| t),
        ));
        ret.add_lift(TypeContents::for_types(resolve, func.results.iter_types()));

        // If anything is indirect then `memory` will be required to read the
        // indirect values, but if the arguments are indirect then `realloc` is
        // additionally required to allocate space for the parameters.
        if sig.retptr || sig.indirect_params {
            ret |= RequiredOptions::MEMORY;
            if sig.indirect_params {
                ret |= RequiredOptions::REALLOC;
            }
        }
        ret
    }

    fn add_lower(&mut self, types: TypeContents) {
        // If lists/strings are lowered into wasm then memory is required as
        // usual but `realloc` is also required to allow the external caller to
        // allocate space in the destination for the list/string.
        if types.contains(TypeContents::LIST) {
            *self |= RequiredOptions::MEMORY | RequiredOptions::REALLOC;
        }
        if types.contains(TypeContents::STRING) {
            *self |= RequiredOptions::MEMORY
                | RequiredOptions::STRING_ENCODING
                | RequiredOptions::REALLOC;
        }
    }

    fn add_lift(&mut self, types: TypeContents) {
        // Unlike for `lower` when lifting a string/list all that's needed is
        // memory, since the string/list already resides in memory `realloc`
        // isn't needed.
        if types.contains(TypeContents::LIST) {
            *self |= RequiredOptions::MEMORY;
        }
        if types.contains(TypeContents::STRING) {
            *self |= RequiredOptions::MEMORY | RequiredOptions::STRING_ENCODING;
        }
    }

    fn into_iter(
        self,
        encoding: StringEncoding,
        memory_index: Option<u32>,
        realloc_index: Option<u32>,
    ) -> Result<impl ExactSizeIterator<Item = CanonicalOption>> {
        #[derive(Default)]
        struct Iter {
            options: [Option<CanonicalOption>; 3],
            current: usize,
            count: usize,
        }

        impl Iter {
            fn push(&mut self, option: CanonicalOption) {
                assert!(self.count < self.options.len());
                self.options[self.count] = Some(option);
                self.count += 1;
            }
        }

        impl Iterator for Iter {
            type Item = CanonicalOption;

            fn next(&mut self) -> Option<Self::Item> {
                if self.current == self.count {
                    return None;
                }
                let option = self.options[self.current];
                self.current += 1;
                option
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.count - self.current, Some(self.count - self.current))
            }
        }

        impl ExactSizeIterator for Iter {}

        let mut iter = Iter::default();

        if self.contains(RequiredOptions::MEMORY) {
            iter.push(CanonicalOption::Memory(memory_index.ok_or_else(|| {
                anyhow!("module does not export a memory named `memory`")
            })?));
        }

        if self.contains(RequiredOptions::REALLOC) {
            iter.push(CanonicalOption::Realloc(realloc_index.ok_or_else(
                || anyhow!("module does not export a function named `cabi_realloc`"),
            )?));
        }

        if self.contains(RequiredOptions::STRING_ENCODING) {
            iter.push(encoding.into());
        }

        Ok(iter)
    }
}

bitflags::bitflags! {
    /// Flags about what kinds of types are present within the recursive
    /// structure of a type.
    struct TypeContents: u8 {
        const STRING = 1 << 0;
        const LIST = 1 << 1;
    }
}

impl TypeContents {
    fn for_types<'a>(resolve: &Resolve, types: impl Iterator<Item = &'a Type>) -> Self {
        let mut cur = TypeContents::empty();
        for ty in types {
            cur |= Self::for_type(resolve, ty);
        }
        cur
    }

    fn for_optional_types<'a>(
        resolve: &Resolve,
        types: impl Iterator<Item = Option<&'a Type>>,
    ) -> Self {
        Self::for_types(resolve, types.flatten())
    }

    fn for_optional_type(resolve: &Resolve, ty: Option<&Type>) -> Self {
        match ty {
            Some(ty) => Self::for_type(resolve, ty),
            None => Self::empty(),
        }
    }

    fn for_type(resolve: &Resolve, ty: &Type) -> Self {
        match ty {
            Type::Id(id) => match &resolve.types[*id].kind {
                TypeDefKind::Record(r) => Self::for_types(resolve, r.fields.iter().map(|f| &f.ty)),
                TypeDefKind::Tuple(t) => Self::for_types(resolve, t.types.iter()),
                TypeDefKind::Flags(_) => Self::empty(),
                TypeDefKind::Option(t) => Self::for_type(resolve, t),
                TypeDefKind::Result(r) => {
                    Self::for_optional_type(resolve, r.ok.as_ref())
                        | Self::for_optional_type(resolve, r.err.as_ref())
                }
                TypeDefKind::Variant(v) => {
                    Self::for_optional_types(resolve, v.cases.iter().map(|c| c.ty.as_ref()))
                }
                TypeDefKind::Union(v) => Self::for_types(resolve, v.cases.iter().map(|c| &c.ty)),
                TypeDefKind::Enum(_) => Self::empty(),
                TypeDefKind::List(t) => Self::for_type(resolve, t) | Self::LIST,
                TypeDefKind::Type(t) => Self::for_type(resolve, t),
                TypeDefKind::Future(_) => todo!("encoding for future"),
                TypeDefKind::Stream(_) => todo!("encoding for stream"),
                TypeDefKind::Unknown => unreachable!(),
            },
            Type::String => Self::STRING,
            _ => Self::empty(),
        }
    }
}

/// State relating to encoding a component.
pub struct EncodingState<'a> {
    /// The component being encoded.
    component: ComponentBuilder,
    /// The index into the core module index space for the inner core module.
    ///
    /// If `None`, the core module has not been encoded.
    module_index: Option<u32>,
    /// The index into the core instance index space for the inner core module.
    ///
    /// If `None`, the core module has not been instantiated.
    instance_index: Option<u32>,
    /// The index in the core memory index space for the exported memory.
    ///
    /// If `None`, then the memory has not yet been aliased.
    memory_index: Option<u32>,
    /// The index in the core function index space for the realloc function.
    ///
    /// If `None`, then the realloc function has not yet been aliased.
    realloc_index: Option<u32>,
    /// The index of the shim instance used for lowering imports into the core instance.
    ///
    /// If `None`, then the shim instance how not yet been encoded.
    shim_instance_index: Option<u32>,
    /// The index of the fixups module to instantiate to fill in the lowered imports.
    ///
    /// If `None`, then a fixup module has not yet been encoded.
    fixups_module_index: Option<u32>,

    /// A map of named adapter modules and the index that the module was defined
    /// at.
    adapter_modules: IndexMap<&'a str, u32>,
    /// A map of adapter module instances and the index of their instance.
    adapter_instances: IndexMap<&'a str, u32>,
    /// A map of the index of the aliased realloc function for each adapter
    /// module. Note that adapters have two realloc functions, one for imports
    /// and one for exports.
    adapter_import_reallocs: IndexMap<&'a str, Option<u32>>,
    adapter_export_reallocs: IndexMap<&'a str, Option<u32>>,

    /// Imported instances and what index they were imported as.
    imported_instances: IndexMap<InterfaceId, u32>,
    imported_funcs: IndexMap<&'a str, u32>,
    exported_instances: IndexMap<InterfaceId, u32>,

    /// Map of types defined within the component's root index space.
    type_map: HashMap<TypeId, u32>,
    /// Map of function types defined within the component's root index space.
    func_type_map: HashMap<types::FunctionKey<'a>, u32>,

    /// Metadata about the world inferred from the input to `ComponentEncoder`.
    info: &'a ComponentWorld<'a>,
}

impl<'a> EncodingState<'a> {
    fn encode_core_modules(&mut self) {
        assert!(self.module_index.is_none());
        let idx = self.component.core_module_raw(&self.info.encoder.module);
        self.module_index = Some(idx);

        for (name, (_, wasm)) in self.info.adapters.iter() {
            let idx = self.component.core_module_raw(wasm);
            let prev = self.adapter_modules.insert(name, idx);
            assert!(prev.is_none());
        }
    }

    fn root_type_encoder(&mut self, interface: Option<InterfaceId>) -> RootTypeEncoder<'_, 'a> {
        RootTypeEncoder {
            state: self,
            type_exports: Vec::new(),
            interface,
        }
    }

    fn instance_type_encoder(&mut self, interface: InterfaceId) -> InstanceTypeEncoder<'_, 'a> {
        InstanceTypeEncoder {
            state: self,
            interface,
            type_map: Default::default(),
            func_type_map: Default::default(),
            ty: Default::default(),
        }
    }

    fn encode_imports(&mut self) -> Result<()> {
        for (name, info) in self.info.import_map.iter() {
            match name {
                Some(name) => self.encode_interface_import(name, info)?,
                None => self.encode_root_import_funcs(info)?,
            }
        }
        Ok(())
    }

    fn encode_interface_import(&mut self, name: &str, info: &ImportedInterface) -> Result<()> {
        let resolve = &self.info.encoder.metadata.resolve;
        let (interface_id, url) = info.interface.as_ref().unwrap();
        let interface_id = *interface_id;
        let interface = &resolve.interfaces[interface_id];
        log::trace!("encoding imports for `{name}` as {:?}", interface_id);
        let mut encoder = self.instance_type_encoder(interface_id);

        // Encode all required functions from this imported interface
        // into the instance type.
        for (_, func) in interface.functions.iter() {
            if !info.required.contains(func.name.as_str()) {
                continue;
            }
            log::trace!("encoding function type for `{}`", func.name);
            let idx = encoder.encode_func_type(resolve, func)?;

            encoder
                .ty
                .export(&func.name, "", ComponentTypeRef::Func(idx));
        }

        // If there were any live types from this instance which weren't
        // otherwise reached through the above function types then this
        // will forward them through.
        if let Some(live) = encoder.state.info.live_types.get(&interface_id) {
            for ty in live {
                log::trace!("encoding extra type {ty:?}");
                encoder.encode_valtype(resolve, &Type::Id(*ty))?;
            }
        }

        let ty = encoder.ty;
        // Don't encode empty instance types since they're not
        // meaningful to the runtime of the component anyway.
        if ty.is_empty() {
            return Ok(());
        }
        let instance_type_idx = self.component.instance_type(&ty);
        let instance_idx =
            self.component
                .import(name, url, ComponentTypeRef::Instance(instance_type_idx));
        let prev = self.imported_instances.insert(interface_id, instance_idx);
        assert!(prev.is_none());
        Ok(())
    }

    fn encode_root_import_funcs(&mut self, info: &ImportedInterface) -> Result<()> {
        let resolve = &self.info.encoder.metadata.resolve;
        let world = self.info.encoder.metadata.world;
        for (name, item) in resolve.worlds[world].imports.iter() {
            let func = match item {
                WorldItem::Function(f) => f,
                WorldItem::Interface(_) => continue,
            };
            if !info.required.contains(name.as_str()) {
                continue;
            }
            log::trace!("encoding function type for `{}`", func.name);
            let mut encoder = self.root_type_encoder(None);
            let idx = encoder.encode_func_type(resolve, func)?;
            assert!(encoder.type_exports.is_empty());
            let func_idx = self.component.import(name, "", ComponentTypeRef::Func(idx));
            let prev = self.imported_funcs.insert(name, func_idx);
            assert!(prev.is_none());
        }
        Ok(())
    }

    fn index_of_type_export(&mut self, id: TypeId) -> u32 {
        // Using the original `interface` definition of `id` and its name create
        // an alias which refers to the type export of that instance which must
        // have previously been imported.
        let ty = &self.info.encoder.metadata.resolve.types[id];
        let interface = match ty.owner {
            TypeOwner::Interface(id) => id,
            _ => panic!("cannot import anonymous type across interfaces"),
        };
        let name = ty
            .name
            .as_ref()
            .expect("cannot import anonymous type across interfaces");
        let instance = self
            .exported_instances
            .get(&interface)
            .copied()
            .unwrap_or_else(|| self.imported_instances[&interface]);
        self.component.alias_type_export(instance, name)
    }

    fn encode_core_instantiation(&mut self) -> Result<()> {
        let info = &self.info.info;
        // Encode a shim instantiation if needed
        let shims = self.encode_shim_instantiation();

        // For each instance import into the main module create a
        // pseudo-core-wasm-module via a bag-of-exports.
        let mut args = Vec::new();
        for core_wasm_name in info.required_imports.keys() {
            let index = self.import_instance_to_lowered_core_instance(
                CustomModule::Main,
                *core_wasm_name,
                &shims,
                info.metadata,
            );
            args.push((*core_wasm_name, ModuleArg::Instance(index)));
        }

        // For each adapter module instance imported into the core wasm module
        // the appropriate shim is packaged up into a bag-of-exports instance.
        // Note that adapter modules currently don't deal with
        // indirect-vs-direct lowerings, everything is indirect.
        for (adapter, funcs) in info.adapters_required.iter() {
            let shim_instance = self
                .shim_instance_index
                .expect("shim should be instantiated");
            let mut exports = Vec::new();

            for (func, _ty) in funcs {
                let index = self.component.alias_core_item(
                    shim_instance,
                    ExportKind::Func,
                    &shims.shim_names[&ShimKind::Adapter { adapter, func }],
                );
                exports.push((*func, ExportKind::Func, index));
            }

            let index = self.component.instantiate_core_exports(exports);
            args.push((*adapter, ModuleArg::Instance(index)));
        }

        // Instantiate the main module now that all of its arguments have been
        // prepared. With this we know have the main linear memory for
        // liftings/lowerings later on as well as the adapter modules, if any,
        // instantiated after the core wasm module.
        self.instantiate_core_module(args, info);
        self.instantiate_adapter_modules(&shims);

        // With all the core wasm instances in play now the original shim
        // module, if present, can be filled in with lowerings/adapters/etc.
        self.encode_indirect_lowerings(shims)
    }

    /// Lowers a named imported interface a core wasm instances suitable to
    /// provide as an instantiation argument to another core wasm module.
    ///
    /// * `for_module` the module that this instance is being created for, or
    ///   otherwise which `realloc` option is used for the lowerings.
    /// * `name` - the name of the imported interface that's being lowered.
    /// * `imports` - the list of all imports known for this encoding.
    /// * `shims` - the indirect/adapter shims created prior, if any.
    fn import_instance_to_lowered_core_instance(
        &mut self,
        for_module: CustomModule<'_>,
        core_wasm_name: &str,
        shims: &Shims<'_>,
        metadata: &ModuleMetadata,
    ) -> u32 {
        let interface = if core_wasm_name.is_empty() {
            None
        } else {
            Some(core_wasm_name)
        };
        let import = &self.info.import_map[&interface];
        let mut exports = Vec::with_capacity(import.direct.len() + import.indirect.len());

        // Add an entry for all indirect lowerings which come as an export of
        // the shim module.
        for (i, lowering) in import.indirect.iter().enumerate() {
            let encoding =
                metadata.import_encodings[&(core_wasm_name.to_string(), lowering.name.to_string())];
            let index = self.component.alias_core_item(
                self.shim_instance_index
                    .expect("shim should be instantiated"),
                ExportKind::Func,
                &shims.shim_names[&ShimKind::IndirectLowering {
                    interface,
                    indirect_index: i,
                    realloc: for_module,
                    encoding,
                }],
            );
            exports.push((lowering.name, ExportKind::Func, index));
        }

        // All direct lowerings can be `canon lower`'d here immediately and
        // passed as arguments.
        for lowering in &import.direct {
            let func_index = match &import.interface {
                Some((interface, _url)) => {
                    let instance_index = self.imported_instances[interface];
                    self.component.alias_func(instance_index, lowering.name)
                }
                None => self.imported_funcs[lowering.name],
            };
            let core_func_index = self.component.lower_func(func_index, []);
            exports.push((lowering.name, ExportKind::Func, core_func_index));
        }

        self.component.instantiate_core_exports(exports)
    }

    fn encode_exports(&mut self, opts: &'a ComponentEncoder, module: CustomModule) -> Result<()> {
        let resolve = &opts.metadata.resolve;
        let world = match module {
            CustomModule::Main => opts.metadata.world,
            CustomModule::Adapter(name) => opts.adapters[name].2,
        };
        let world = &resolve.worlds[world];
        for (export_name, export) in world.exports.iter() {
            match export {
                WorldItem::Function(func) => {
                    let mut enc = self.root_type_encoder(None);
                    let ty = enc.encode_func_type(resolve, func)?;
                    for (idx, name) in enc.type_exports {
                        self.component
                            .export(name, "", ComponentExportKind::Type, idx);
                    }
                    let core_name = func.core_export_name(None);
                    let idx = self.encode_lift(opts, module, &core_name, func, ty)?;
                    self.component
                        .export(export_name, "", ComponentExportKind::Func, idx);
                }
                WorldItem::Interface(export) => {
                    let mut interface_exports = Vec::new();

                    // All types are encoded into the root component here using this
                    // encoder which keeps track of type exports to determine how to
                    // export them later as well.
                    let mut enc = self.root_type_encoder(Some(*export));
                    for (_, func) in &resolve.interfaces[*export].functions {
                        let core_name = func.core_export_name(Some(export_name));
                        let ty = enc.encode_func_type(resolve, func)?;
                        let func_index =
                            enc.state.encode_lift(opts, module, &core_name, func, ty)?;
                        interface_exports.push((
                            func.name.as_str(),
                            ComponentExportKind::Func,
                            func_index,
                        ));
                    }

                    if let Some(live) = enc.state.info.live_types.get(export) {
                        for ty in live {
                            enc.encode_valtype(resolve, &Type::Id(*ty))?;
                        }
                    }

                    // Place type imports first for now. The lifted function
                    // types in theory should reference the indices of these
                    // exports but that's not possible in the binary encoding
                    // right now.
                    interface_exports.splice(
                        0..0,
                        enc.type_exports
                            .into_iter()
                            .map(|(idx, name)| (name, ComponentExportKind::Type, idx)),
                    );

                    if export_name.is_empty() {
                        bail!("cannot export an unnamed interface");
                    }

                    let instance_index = self.component.instantiate_exports(interface_exports);
                    let url = resolve.url_of(*export).unwrap_or(String::new());
                    let idx = self.component.export(
                        export_name,
                        &url,
                        ComponentExportKind::Instance,
                        instance_index,
                    );
                    let prev = self.exported_instances.insert(*export, idx);
                    assert!(prev.is_none());
                }
            }
        }

        Ok(())
    }

    fn encode_lift(
        &mut self,
        opts: &'a ComponentEncoder,
        module: CustomModule<'_>,
        core_name: &str,
        func: &Function,
        ty: u32,
    ) -> Result<u32> {
        let resolve = &opts.metadata.resolve;
        let metadata = match module {
            CustomModule::Main => &opts.metadata.metadata,
            CustomModule::Adapter(name) => &opts.adapters[name].1,
        };
        let instance_index = match module {
            CustomModule::Main => self.instance_index.expect("instantiated by now"),
            CustomModule::Adapter(name) => self.adapter_instances[name],
        };
        let core_func_index =
            self.component
                .alias_core_item(instance_index, ExportKind::Func, core_name);

        let options = RequiredOptions::for_export(resolve, func);

        let encoding = metadata.export_encodings[core_name];
        // TODO: This realloc detection should probably be improved with
        // some sort of scheme to have per-function reallocs like
        // `cabi_realloc_{name}` or something like that.
        let realloc_index = match module {
            CustomModule::Main => self.realloc_index,
            CustomModule::Adapter(name) => self.adapter_export_reallocs[name],
        };
        let mut options = options
            .into_iter(encoding, self.memory_index, realloc_index)?
            .collect::<Vec<_>>();

        // TODO: This should probe for the existence of
        // `cabi_post_{name}` but not require its existence.
        if resolve.guest_export_needs_post_return(func) {
            let post_return = self.component.alias_core_item(
                instance_index,
                ExportKind::Func,
                &format!("cabi_post_{core_name}"),
            );
            options.push(CanonicalOption::PostReturn(post_return));
        }
        let func_index = self.component.lift_func(core_func_index, ty, options);
        Ok(func_index)
    }

    fn encode_shim_instantiation(&mut self) -> Shims<'a> {
        let mut signatures = Vec::new();
        let mut ret = Shims::default();
        let info = &self.info.info;

        // For all interfaces imported into the main module record all of their
        // indirect lowerings into `Shims`.
        for core_wasm_name in info.required_imports.keys() {
            let import_name = if core_wasm_name.is_empty() {
                None
            } else {
                Some(*core_wasm_name)
            };
            let import = &self.info.import_map[&import_name];
            ret.append_indirect(
                core_wasm_name,
                CustomModule::Main,
                import,
                info.metadata,
                &mut signatures,
            );
        }

        // For all required adapter modules a shim is created for each required
        // function and additionally a set of shims are created for the
        // interface imported into the shim module itself.
        for (adapter, funcs) in info.adapters_required.iter() {
            let (info, _wasm) = &self.info.adapters[adapter];
            for (name, _) in info.required_imports.iter() {
                let import = &self.info.import_map[&Some(*name)];
                ret.append_indirect(
                    name,
                    CustomModule::Adapter(adapter),
                    import,
                    info.metadata,
                    &mut signatures,
                );
            }
            for (func, ty) in funcs {
                let name = ret.list.len().to_string();
                log::debug!("shim {name} is adapter `{adapter}::{func}`");
                signatures.push(WasmSignature {
                    params: ty.params().iter().map(to_wasm_type).collect(),
                    results: ty.results().iter().map(to_wasm_type).collect(),
                    indirect_params: false,
                    retptr: false,
                });
                ret.list.push(Shim {
                    name,
                    debug_name: format!("adapt-{adapter}-{func}"),
                    // Pessimistically assume that all adapters require memory
                    // in one form or another. While this isn't technically true
                    // it's true enough for WASI.
                    options: RequiredOptions::MEMORY,
                    kind: ShimKind::Adapter { adapter, func },
                });
            }
        }
        if ret.list.is_empty() {
            return ret;
        }

        for shim in ret.list.iter() {
            ret.shim_names.insert(shim.kind, shim.name.clone());
        }

        assert!(self.shim_instance_index.is_none());
        assert!(self.fixups_module_index.is_none());

        // This function encodes two modules:
        // - A shim module that defines a table and exports functions
        //   that indirectly call through the table.
        // - A fixup module that imports that table and a set of functions
        //   and populates the imported table via active element segments. The
        //   fixup module is used to populate the shim's table once the
        //   imported functions have been lowered.

        let mut types = TypeSection::new();
        let mut tables = TableSection::new();
        let mut functions = FunctionSection::new();
        let mut exports = ExportSection::new();
        let mut code = CodeSection::new();
        let mut sigs = IndexMap::new();
        let mut imports_section = ImportSection::new();
        let mut elements = ElementSection::new();
        let mut func_indexes = Vec::new();
        let mut func_names = NameMap::new();

        for (i, (sig, shim)) in signatures.iter().zip(&ret.list).enumerate() {
            let i = i as u32;
            let type_index = *sigs.entry(sig).or_insert_with(|| {
                let index = types.len();
                types.function(
                    sig.params.iter().map(to_val_type),
                    sig.results.iter().map(to_val_type),
                );
                index
            });

            functions.function(type_index);
            Self::encode_shim_function(type_index, i, &mut code, sig.params.len() as u32);
            exports.export(&shim.name, ExportKind::Func, i);

            imports_section.import("", &shim.name, EntityType::Function(type_index));
            func_indexes.push(i);
            func_names.append(i, &shim.debug_name);
        }
        let mut names = NameSection::new();
        names.functions(&func_names);

        let table_type = TableType {
            element_type: ValType::FuncRef,
            minimum: signatures.len() as u32,
            maximum: Some(signatures.len() as u32),
        };

        tables.table(table_type);

        exports.export(INDIRECT_TABLE_NAME, ExportKind::Table, 0);
        imports_section.import("", INDIRECT_TABLE_NAME, table_type);

        elements.active(
            None,
            &ConstExpr::i32_const(0),
            ValType::FuncRef,
            Elements::Functions(&func_indexes),
        );

        let mut shim = Module::new();
        shim.section(&types);
        shim.section(&functions);
        shim.section(&tables);
        shim.section(&exports);
        shim.section(&code);
        shim.section(&names);

        let mut fixups = Module::default();
        fixups.section(&types);
        fixups.section(&imports_section);
        fixups.section(&elements);

        let shim_module_index = self.component.core_module(&shim);
        self.fixups_module_index = Some(self.component.core_module(&fixups));
        self.shim_instance_index = Some(self.component.instantiate(shim_module_index, []));

        return ret;

        fn to_wasm_type(ty: &wasmparser::ValType) -> WasmType {
            match ty {
                wasmparser::ValType::I32 => WasmType::I32,
                wasmparser::ValType::I64 => WasmType::I64,
                wasmparser::ValType::F32 => WasmType::F32,
                wasmparser::ValType::F64 => WasmType::F64,
                _ => unreachable!(),
            }
        }
    }

    fn encode_shim_function(
        type_index: u32,
        func_index: u32,
        code: &mut CodeSection,
        param_count: u32,
    ) {
        let mut func = wasm_encoder::Function::new(std::iter::empty());
        for i in 0..param_count {
            func.instruction(&Instruction::LocalGet(i));
        }
        func.instruction(&Instruction::I32Const(func_index as i32));
        func.instruction(&Instruction::CallIndirect {
            ty: type_index,
            table: 0,
        });
        func.instruction(&Instruction::End);
        code.function(&func);
    }

    fn encode_indirect_lowerings(&mut self, shims: Shims<'_>) -> Result<()> {
        if shims.list.is_empty() {
            return Ok(());
        }

        let shim_instance_index = self
            .shim_instance_index
            .expect("must have an instantiated shim");

        let table_index = self.component.alias_core_item(
            shim_instance_index,
            ExportKind::Table,
            INDIRECT_TABLE_NAME,
        );

        let mut exports = Vec::new();
        exports.push((INDIRECT_TABLE_NAME, ExportKind::Table, table_index));

        for shim in shims.list.iter() {
            let core_func_index = match &shim.kind {
                // Indirect lowerings are a `canon lower`'d function with
                // options specified from a previously instantiated instance.
                // This previous instance could either be the main module or an
                // adapter module, which affects the `realloc` option here.
                // Currently only one linear memory is supported so the linear
                // memory always comes from the main module.
                ShimKind::IndirectLowering {
                    interface,
                    indirect_index,
                    realloc,
                    encoding,
                } => {
                    let interface = &self.info.import_map[interface];
                    let name = interface.indirect[*indirect_index].name;
                    let func_index = match &interface.interface {
                        Some((interface_id, _url)) => {
                            let instance_index = self.imported_instances[interface_id];
                            self.component.alias_func(instance_index, name)
                        }
                        None => self.imported_funcs[name],
                    };

                    let realloc = match realloc {
                        CustomModule::Main => self.realloc_index,
                        CustomModule::Adapter(name) => self.adapter_import_reallocs[name],
                    };

                    self.component.lower_func(
                        func_index,
                        shim.options
                            .into_iter(*encoding, self.memory_index, realloc)?,
                    )
                }

                // Adapter shims are defined by an export from and adapter
                // instance, so use the specified name here and the previously
                // created instances to get the core item that represents the
                // shim.
                ShimKind::Adapter { adapter, func } => self.component.alias_core_item(
                    self.adapter_instances[adapter],
                    ExportKind::Func,
                    func,
                ),
            };

            exports.push((shim.name.as_str(), ExportKind::Func, core_func_index));
        }

        let instance_index = self.component.instantiate_core_exports(exports);
        self.component.instantiate(
            self.fixups_module_index.expect("must have fixup module"),
            [("", ModuleArg::Instance(instance_index))],
        );
        Ok(())
    }

    fn instantiate_core_module<'b, A>(&mut self, args: A, info: &ValidatedModule<'_>)
    where
        A: IntoIterator<Item = (&'b str, ModuleArg)>,
        A::IntoIter: ExactSizeIterator,
    {
        assert!(self.instance_index.is_none());

        let instance_index = self
            .component
            .instantiate(self.module_index.expect("core module encoded"), args);

        if info.has_memory {
            self.memory_index = Some(self.component.alias_core_item(
                instance_index,
                ExportKind::Memory,
                "memory",
            ));
        }

        if let Some(name) = &info.realloc {
            self.realloc_index = Some(self.component.alias_core_item(
                instance_index,
                ExportKind::Func,
                name,
            ));
        }

        self.instance_index = Some(instance_index);
    }

    /// This function will instantiate all required adapter modules required by
    /// the main module (specified by `info`).
    ///
    /// Each adapter here is instantiated with its required imported interface,
    /// if any.
    fn instantiate_adapter_modules(&mut self, shims: &Shims<'_>) {
        for (name, (info, _wasm)) in self.info.adapters.iter() {
            let mut args = Vec::new();

            let mut core_exports = Vec::new();
            for export_name in info.needs_core_exports.iter() {
                let index = self.component.alias_core_item(
                    self.instance_index
                        .expect("adaptee index set at this point"),
                    ExportKind::Func,
                    export_name,
                );
                core_exports.push((export_name.as_str(), ExportKind::Func, index));
            }
            if !core_exports.is_empty() {
                let instance = self.component.instantiate_core_exports(core_exports);
                args.push((MAIN_MODULE_IMPORT_NAME, ModuleArg::Instance(instance)));
            }
            // If the adapter module requires a `memory` import then specify
            // that here. For now assume that the module name of the memory is
            // different from the imported interface. That's true enough for now
            // since it's `env::memory`.
            if let Some((module, name)) = &info.needs_memory {
                for (import_name, _) in info.required_imports.iter() {
                    assert!(module != import_name);
                }
                assert!(module != name);
                let memory = self.memory_index.unwrap();
                let instance = self.component.instantiate_core_exports([(
                    name.as_str(),
                    ExportKind::Memory,
                    memory,
                )]);
                args.push((module.as_str(), ModuleArg::Instance(instance)));
            }
            for (import_name, _) in info.required_imports.iter() {
                let instance = self.import_instance_to_lowered_core_instance(
                    CustomModule::Adapter(name),
                    import_name,
                    shims,
                    info.metadata,
                );
                args.push((import_name, ModuleArg::Instance(instance)));
            }
            let instance = self.component.instantiate(self.adapter_modules[name], args);
            self.adapter_instances.insert(name, instance);

            let realloc = info.export_realloc.as_ref().map(|name| {
                self.component
                    .alias_core_item(instance, ExportKind::Func, name)
            });
            self.adapter_export_reallocs.insert(name, realloc);
            let realloc = info.import_realloc.as_ref().map(|name| {
                self.component
                    .alias_core_item(instance, ExportKind::Func, name)
            });
            self.adapter_import_reallocs.insert(name, realloc);
        }
    }
}

/// A list of "shims" which start out during the component instantiation process
/// as functions which immediately trap due to a `call_indirect`-to-`null` but
/// will get filled in by the time the component instantiation process
/// completes.
///
/// Shims currently include:
///
/// * "Indirect functions" lowered from imported instances where the lowering
///   requires an item exported from the main module. These are indirect due to
///   the circular dependency between the module needing an import and the
///   import needing the module.
///
/// * Adapter modules which convert from a historical ABI to the component
///   model's ABI (e.g. wasi preview1 to preview2) get a shim since the adapters
///   are currently indicated as always requiring the memory of the main module.
///
/// This structure is created by `encode_shim_instantiation`.
#[derive(Default)]
struct Shims<'a> {
    /// The list of all shims that a module will require.
    list: Vec<Shim<'a>>,

    /// A map from a shim to the name of the shim in the shim instance.
    shim_names: IndexMap<ShimKind<'a>, String>,
}

struct Shim<'a> {
    /// Canonical ABI options required by this shim, used during `canon lower`
    /// operations.
    options: RequiredOptions,

    /// The name, in the shim instance, of this shim.
    ///
    /// Currently this is `"0"`, `"1"`, ...
    name: String,

    /// A human-readable debugging name for this shim, used in a core wasm
    /// `name` section.
    debug_name: String,

    /// Precise information about what this shim is a lowering of.
    kind: ShimKind<'a>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum ShimKind<'a> {
    /// This shim is a late indirect lowering of an imported function in a
    /// component which is only possible after prior core wasm modules are
    /// instantiated so their memories and functions are available.
    IndirectLowering {
        /// The name of the interface that's being lowered.
        interface: Option<&'a str>,
        /// The index within the `indirect` array of the function being lowered.
        indirect_index: usize,
        /// Which instance to pull the `realloc` function from, if necessary.
        realloc: CustomModule<'a>,
        /// The string encoding that this lowering is going to use.
        encoding: StringEncoding,
    },
    /// This shim is a core wasm function defined in an adapter module but isn't
    /// available until the adapter module is itself instantiated.
    Adapter {
        /// The name of the adapter module this shim comes from.
        adapter: &'a str,
        /// The name of the export in the adapter module this shim points to.
        func: &'a str,
    },
}

/// Indicator for which module is being used for a lowering or where options
/// like `realloc` are drawn from.
///
/// This is necessary for situations such as an imported function being lowered
/// into the main module and additionally into an adapter module. For example an
/// adapter might adapt from preview1 to preview2 for the standard library of a
/// programming language but the main module's custom application code may also
/// explicitly import from preview2. These two different lowerings of a preview2
/// function are parameterized by this enumeration.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum CustomModule<'a> {
    /// This points to the "main module" which is generally the "output of LLVM"
    /// or what a user wrote.
    Main,
    /// This is selecting an adapter module, identified by name here, where
    /// something is being lowered into.
    Adapter(&'a str),
}

impl<'a> Shims<'a> {
    /// Adds all shims necessary for the `import` provided, namely iterating
    /// over its indirect lowerings and appending a shim per lowering.
    fn append_indirect(
        &mut self,
        core_wasm_module: &'a str,
        for_module: CustomModule<'a>,
        import: &ImportedInterface<'a>,
        metadata: &ModuleMetadata,
        sigs: &mut Vec<WasmSignature>,
    ) {
        let interface = if core_wasm_module.is_empty() {
            None
        } else {
            Some(core_wasm_module)
        };
        for (indirect_index, lowering) in import.indirect.iter().enumerate() {
            let shim_name = self.list.len().to_string();
            log::debug!(
                "shim {shim_name} is import `{core_wasm_module}` lowering {indirect_index} `{}`",
                lowering.name
            );
            sigs.push(lowering.sig.clone());
            let encoding = metadata.import_encodings
                [&(core_wasm_module.to_string(), lowering.name.to_string())];
            self.list.push(Shim {
                name: shim_name,
                debug_name: format!("indirect-{core_wasm_module}-{}", lowering.name),
                options: lowering.options,
                kind: ShimKind::IndirectLowering {
                    interface,
                    indirect_index,
                    realloc: for_module,
                    encoding,
                },
            });
        }
    }
}

/// An encoder of components based on `wit` interface definitions.
#[derive(Default)]
pub struct ComponentEncoder {
    module: Vec<u8>,
    metadata: Bindgen,
    validate: bool,

    // This is a map from the name of the adapter to a pair of:
    //
    // * The wasm of the adapter itself, with `component-type` sections
    //   stripped.
    // * the metadata for the adapter, verified to have no exports and only
    //   imports.
    // * The world within `self.metadata.doc` which the adapter works with.
    adapters: IndexMap<String, (Vec<u8>, ModuleMetadata, WorldId)>,
}

impl ComponentEncoder {
    /// Set the core module to encode as a component.
    /// This method will also parse any component type information stored in custom sections
    /// inside the module, and add them as the interface, imports, and exports.
    pub fn module(mut self, module: &[u8]) -> Result<Self> {
        let (wasm, metadata) = metadata::decode(module)?;
        self.module = wasm;
        self.metadata.merge(metadata)?;
        Ok(self)
    }

    /// Sets whether or not the encoder will validate its output.
    pub fn validate(mut self, validate: bool) -> Self {
        self.validate = validate;
        self
    }

    /// Specifies a new adapter which is used to translate from a historical
    /// wasm ABI to the canonical ABI and the `interface` provided.
    ///
    /// This is primarily used to polyfill, for example,
    /// `wasi_snapshot_preview1` with a component-model using interface. The
    /// `name` provided is the module name of the adapter that is being
    /// polyfilled, for example `"wasi_snapshot_preview1"`.
    ///
    /// The `bytes` provided is a core wasm module which implements the `name`
    /// interface in terms of the `interface` interface. This core wasm module
    /// is severely restricted in its shape, for example it cannot have any data
    /// segments or element segments.
    ///
    /// The `interface` provided is the component-model-using-interface that the
    /// wasm module specified by `bytes` imports. The `bytes` will then import
    /// `interface` and export functions to get imported from the module `name`
    /// in the core wasm that's being wrapped.
    pub fn adapter(mut self, name: &str, bytes: &[u8]) -> Result<Self> {
        let (wasm, metadata) = metadata::decode(bytes)?;
        // Merge the adapter's document into our own document to have one large
        // document, but the adapter's world isn't merged in to our world so
        // retain it separately.
        let world = self.metadata.resolve.merge(metadata.resolve).worlds[metadata.world.index()];
        self.adapters
            .insert(name.to_string(), (wasm, metadata.metadata, world));
        Ok(self)
    }

    /// Encode the component and return the bytes.
    pub fn encode(&self) -> Result<Vec<u8>> {
        if self.module.is_empty() {
            bail!("a module is required when encoding a component");
        }

        let world = ComponentWorld::new(self)?;
        let mut state = EncodingState {
            component: ComponentBuilder::default(),
            module_index: None,
            instance_index: None,
            memory_index: None,
            realloc_index: None,
            shim_instance_index: None,
            fixups_module_index: None,
            adapter_modules: IndexMap::new(),
            adapter_instances: IndexMap::new(),
            adapter_import_reallocs: IndexMap::new(),
            adapter_export_reallocs: IndexMap::new(),
            type_map: HashMap::new(),
            func_type_map: HashMap::new(),
            imported_instances: Default::default(),
            imported_funcs: Default::default(),
            exported_instances: Default::default(),
            info: &world,
        };
        state.encode_imports()?;
        state.encode_core_modules();
        state.encode_core_instantiation()?;
        state.encode_exports(self, CustomModule::Main)?;
        for name in self.adapters.keys() {
            state.encode_exports(self, CustomModule::Adapter(name))?;
        }
        let bytes = state.component.finish();

        if self.validate {
            let mut validator = Validator::new_with_features(WasmFeatures {
                component_model: true,
                ..Default::default()
            });

            validator
                .validate_all(&bytes)
                .context("failed to validate component output")?;
        }

        Ok(bytes)
    }
}
