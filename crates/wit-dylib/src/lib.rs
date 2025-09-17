use crate::metadata::Metadata;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::mem;
use wasm_encoder::{
    CodeSection, ConstExpr, CustomSection, DataSection, ElementSection, Elements, Encode,
    EntityType, ExportKind, ExportSection, Function, FunctionSection, GlobalType, ImportSection,
    MemoryType, Module, NameMap, NameSection, RefType, TableType, TypeSection, ValType,
};
use wit_parser::abi::{WasmSignature, WasmType};
use wit_parser::{
    Handle, LiftLowerAbi, LiveTypes, ManglingAndAbi, Resolve, ResourceIntrinsic, SizeAlign, Type,
    TypeDefKind, TypeId, TypeOwner, WasmExport, WasmExportKind, WasmImport, WorldId, WorldItem,
    WorldKey,
};

mod bindgen;
mod metadata;

pub const C_HEADER: &'static str = include_str!("../wit_dylib.h");

#[derive(Default, Clone, Debug)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
pub struct DylibOpts {
    /// The interpreter name to insert into the `WASM_DYLINK_NEEDED` section
    /// encoded as `dylink.0`.
    #[cfg_attr(feature = "clap", clap(long))]
    pub interpreter: Option<String>,
}

pub fn create(resolve: &Resolve, world_id: WorldId, opts: Option<&DylibOpts>) -> Vec<u8> {
    let mut adapter = Adapter::default();
    if let Some(opts) = opts {
        adapter.opts = opts.clone();
    }
    adapter.encode(resolve, world_id)
}

#[derive(Default)]
struct Adapter {
    types: TypeSection,
    wasm_type_map: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    imports: ImportSection,
    imports_done: bool,
    global_index: u32,
    table_base: Option<u32>,
    memory_base: Option<u32>,
    stack_pointer: Option<u32>,
    func_index: u32,
    functions: FunctionSection,
    exports: ExportSection,
    code: CodeSection,
    global_names: NameMap,
    function_names: NameMap,
    metadata: Metadata,
    type_map: HashMap<TypeId, metadata::Type>,
    resource_map: HashMap<TypeId, usize>,
    export_resource_map: HashMap<TypeId, usize>,
    intrinsics: Option<bindgen::WitInterpreterIntrinsics>,
    sizes: SizeAlign,
    opts: DylibOpts,

    /// Contents of the element segment of this module, which will start at
    /// `__table_base`.
    ///
    /// Elements of this list are function indices in this module which will be
    /// placed into the element segment.
    elem_segment: Vec<u32>,
}

#[derive(Default)]
struct Imports<'a> {
    funcs: Vec<(&'a wit_parser::Function, Option<&'a WorldKey>, u32)>,
}

impl Adapter {
    pub fn encode(&mut self, resolve: &Resolve, world_id: WorldId) -> Vec<u8> {
        self.sizes.fill(resolve);

        // First define all imports that will go into the wasm module since
        // they're required to be first in their index spaces anyway. This will
        // import intrinsics necessary for exported resources, for example, as
        // well.
        let imports = self.add_imports(resolve, world_id);
        self.imports_done = true;

        // Ensure that `cabi_realloc` is reexported from our module to indicate
        // that our module's allocations are routed through the same
        // `cabi_realloc` we're importing.
        let cabi_realloc = self.intrinsics().cabi_realloc;
        self.exports
            .export("cabi_realloc", ExportKind::Func, cabi_realloc);

        // Generate/add metadata for all functions that are either imported or
        // exported.
        self.bindgen_world(resolve, world_id, &imports);

        // Now that all functions have been learned about the metadata data
        // segment can be finalized and emitted. Here this additionally invokes
        // the interpreter initialization function as part of a ctor to ensure
        // that the interpreter is aware of where metadata ended up in linear
        // memory.
        let (metadata_offset, metadata) = self.encode_metadata();
        let mut ctor = Function::new([]);
        ctor.instructions().i32_const(metadata_offset as i32);
        ctor.instructions().global_get(self.memory_base());
        ctor.instructions().i32_add();
        ctor.instructions().call(self.intrinsics().initialize);
        ctor.instructions().end();
        let ty = self.define_ty([], []);
        self.define_func("__wasm_call_ctors", ty, ctor, true);

        self.finish(&metadata)
    }

    fn mangling(&self) -> ManglingAndAbi {
        // TODO: this probably wants CLI parameters and/or a `Function` argument
        // to configure how exactly name mangling and ABIs work out.
        ManglingAndAbi::Legacy(LiftLowerAbi::Sync)
    }

    fn add_imports<'a>(&mut self, resolve: &'a Resolve, world_id: WorldId) -> Imports<'a> {
        let mut ret = Imports::default();
        let world = &resolve.worlds[world_id];

        // First up import everything from the interpreter itself that we're
        // going to possibly need.
        self.intrinsics = Some(bindgen::WitInterpreterIntrinsics::new(self));

        // Generate function imports for all world imports, in addition to
        // intrinsics for all resources imported as well. This will additionally
        // populate the `Imports` return value with all functions that were
        // found.
        for (interface, import) in world.imports.iter() {
            match import {
                WorldItem::Interface { id, .. } => {
                    for (_, func) in resolve.interfaces[*id].functions.iter() {
                        self.add_imported_func(resolve, Some(interface), func, &mut ret);
                    }
                    for (_, ty) in resolve.interfaces[*id].types.iter() {
                        self.add_imported_type_intrinsics(resolve, Some(interface), *ty);
                    }
                }
                WorldItem::Type(ty) => {
                    self.add_imported_type_intrinsics(resolve, None, *ty);
                }
                WorldItem::Function(func) => {
                    self.add_imported_func(resolve, None, func, &mut ret);
                }
            }
        }

        // Exported resources need to have intrinsics imported for manipulation,
        // so do so here.
        for (name, export) in world.exports.iter() {
            match export {
                WorldItem::Function(_func) => {}
                WorldItem::Interface { id: export, .. } => {
                    for (_, ty) in resolve.interfaces[*export].types.iter() {
                        self.add_imported_type_intrinsics_for_export(resolve, Some(name), *ty);
                    }
                }
                WorldItem::Type(_) => unreachable!(),
            }
        }

        let const_i32_global = GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        };
        let mut_i32_global = GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        };

        self.table_base = Some(self.import_global("env", "__table_base", const_i32_global));
        self.memory_base = Some(self.import_global("env", "__memory_base", const_i32_global));
        self.stack_pointer = Some(self.import_global("env", "__stack_pointer", mut_i32_global));

        self.imports.import(
            "env",
            "memory",
            EntityType::Memory(MemoryType {
                minimum: 0,
                maximum: None,
                memory64: false,
                shared: false,
                page_size_log2: None,
            }),
        );

        self.imports.import(
            "env",
            "__indirect_function_table",
            EntityType::Table(TableType {
                element_type: RefType::FUNCREF,
                minimum: 0,
                maximum: None,
                table64: false,
                shared: false,
            }),
        );
        ret
    }

    fn add_imported_func<'a>(
        &mut self,
        resolve: &'a Resolve,
        interface: Option<&'a WorldKey>,
        func: &'a wit_parser::Function,
        imports: &mut Imports<'a>,
    ) {
        let mangling = self.mangling();
        let (module, name) =
            resolve.wasm_import_name(mangling, WasmImport::Func { interface, func });
        let sig = resolve.wasm_signature(mangling.import_variant(), func);
        let ty = self.define_wasm_sig(sig);
        let idx = self.import_func(&module, &name, ty);
        imports.funcs.push((func, interface, idx));
    }

    fn add_imported_type_intrinsics<'a>(
        &mut self,
        resolve: &Resolve,
        interface: Option<&'a WorldKey>,
        id: TypeId,
    ) {
        let mangling = self.mangling();
        let ty = &resolve.types[id];
        match ty.kind {
            TypeDefKind::Resource => {
                let (module, name) = resolve.wasm_import_name(
                    mangling,
                    WasmImport::ResourceIntrinsic {
                        interface,
                        resource: id,
                        intrinsic: ResourceIntrinsic::ImportedDrop,
                    },
                );
                let core_ty = self.define_ty([ValType::I32], []);
                let drop = self.import_func(&module, &name, core_ty);
                let drop_elem_index = self.push_elem(drop);
                let resource_index = self.metadata.resources.len();
                self.metadata.resources.push(metadata::Resource {
                    interface: interface.map(|i| resolve.name_world_key(i)),
                    name: ty.name.clone().unwrap(),
                    drop_elem_index,
                    new_elem_index: None,
                    rep_elem_index: None,
                });
                let prev = self.resource_map.insert(id, resource_index);
                assert!(prev.is_none());
            }

            // No other types with intrinsics at this time (futures/streams are
            // relative to where they show up in function types.
            _ => {}
        }
    }

    fn add_imported_type_intrinsics_for_export<'a>(
        &mut self,
        resolve: &Resolve,
        interface: Option<&'a WorldKey>,
        id: TypeId,
    ) {
        let ty = &resolve.types[id];
        let mangling = self.mangling();
        match ty.kind {
            TypeDefKind::Resource => {
                let drop_ty = self.define_ty([ValType::I32], []);
                let new_rep_ty = self.define_ty([ValType::I32], [ValType::I32]);

                let mut import = |ty, intrinsic| {
                    let (module, name) = resolve.wasm_import_name(
                        mangling,
                        WasmImport::ResourceIntrinsic {
                            interface,
                            resource: id,
                            intrinsic,
                        },
                    );
                    self.import_func(&module, &name, ty)
                };

                let drop = import(drop_ty, ResourceIntrinsic::ExportedDrop);
                let new = import(new_rep_ty, ResourceIntrinsic::ExportedNew);
                let rep = import(new_rep_ty, ResourceIntrinsic::ExportedRep);

                let drop_elem_index = self.push_elem(drop);
                let new_elem_index = Some(self.push_elem(new));
                let rep_elem_index = Some(self.push_elem(rep));

                let resource_index = self.metadata.resources.len();
                self.metadata.resources.push(metadata::Resource {
                    interface: interface.map(|i| resolve.name_world_key(i)),
                    name: ty.name.clone().unwrap(),
                    drop_elem_index,
                    new_elem_index,
                    rep_elem_index,
                });

                // Note that this populates an `export_resource_map` instead of
                // `resource_map` to ensure that if an interface is both
                // imported and exported that we don't clobber the import
                // version here. The clobber here happens later when bindings
                // are generated for exports.
                let prev = self.export_resource_map.insert(id, resource_index);
                assert!(prev.is_none());
            }

            // No other types with intrinsics at this time (futures/streams
            // relative to where they are in a function).
            _ => {}
        }
    }

    fn bindgen_world(&mut self, resolve: &Resolve, world_id: WorldId, imports: &Imports<'_>) {
        let world = &resolve.worlds[world_id];

        // Build up a map for all types of all imports. This pushes all type
        // information into the metadata section that the interpreter will end
        // up learning about.
        let mut import_types = LiveTypes::default();
        let mut interface_names = HashMap::new();
        for (interface, import) in world.imports.iter() {
            import_types.add_world_item(resolve, import);
            if let WorldItem::Interface { id, .. } = import {
                interface_names.insert(*id, interface);
            }
        }
        for (_, export) in world.exports.iter() {
            match export {
                WorldItem::Function(func) => import_types.add_func(resolve, func),
                WorldItem::Interface { .. } => {}
                WorldItem::Type(_) => unreachable!(),
            }
        }
        for ty in import_types.iter() {
            let key = match resolve.types[ty].owner {
                TypeOwner::Interface(id) => Some(interface_names[&id]),
                _ => None,
            };
            self.register_type(resolve, key, ty);
        }

        // Using the populated type map for imports generate functions to invoke
        // these imports. Also generate exported functions in the world since
        // they use imported types as well.
        for (func, key, func_idx) in imports.funcs.iter() {
            self.bindgen_world_func_import(resolve, *key, func, *func_idx);
        }
        for (_, export) in world.exports.iter() {
            match export {
                WorldItem::Function(func) => {
                    self.bindgen_world_func_export(resolve, None, func);
                }
                WorldItem::Interface { .. } => {}
                WorldItem::Type(_) => unreachable!(),
            }
        }

        // Next handle exported interfaces. This is a bit tricky since an
        // interface can be both exported and imported. To handle that first the
        // `type_map` set is pruned to only include imported types required, and
        // then all exported types are added. Export types skip over all
        // types present in `self.type_map`, though, since those are already
        // retained from imports.
        let to_keep = imported_types_used_by_exported_interfaces(resolve, world_id);
        self.type_map.retain(|id, _| to_keep.contains(*id));
        let mut exported_types = LiveTypes::default();
        let mut export_names = HashMap::new();
        for (interface, import) in world.exports.iter() {
            if let WorldItem::Interface { id, .. } = import {
                exported_types.add_world_item(resolve, import);
                export_names.insert(*id, interface);
            }
        }
        for (ty, index) in mem::take(&mut self.export_resource_map) {
            self.resource_map.insert(ty, index);
        }
        for ty in exported_types.iter() {
            if self.type_map.contains_key(&ty) {
                continue;
            }
            let key = match resolve.types[ty].owner {
                TypeOwner::Interface(id) => Some(export_names[&id]),
                _ => None,
            };
            self.register_type(resolve, key, ty);

            if let Some(index) = self.resource_map.get(&ty) {
                self.bindgen_world_export_resource_dtor(resolve, key.unwrap(), ty, *index);
            }
        }

        // With export types all in place now run bindgen for all exported
        // functions.
        for (interface, export) in world.exports.iter() {
            match export {
                WorldItem::Interface { id, .. } => {
                    for (_, func) in resolve.interfaces[*id].functions.iter() {
                        self.bindgen_world_func_export(resolve, Some(interface), func);
                    }
                }
                WorldItem::Type(_) => unreachable!(),
                WorldItem::Function(_) => {}
            }
        }
    }

    /// Push WIT type information into metadata for the interpreter.
    ///
    /// This will insert `id` into metadata and build up the interpreter data
    /// structures for it. The end-result is the population of `self.type_map`
    /// here.
    fn register_type(&mut self, resolve: &Resolve, key: Option<&WorldKey>, id: TypeId) {
        let ty = &resolve.types[id];
        let interface = key.map(|key| resolve.name_world_key(key));
        let name = ty.name.clone();
        let result = match &ty.kind {
            TypeDefKind::Record(r) => {
                let index = self.metadata.records.len();
                let fields = r
                    .fields
                    .iter()
                    .map(|field| (field.name.clone(), self.lookup_ty(&field.ty)))
                    .collect();
                self.metadata.records.push(metadata::Record {
                    interface,
                    name: name.unwrap(),
                    fields,
                });
                metadata::Type::Record(index)
            }
            TypeDefKind::Flags(t) => {
                let index = self.metadata.flags.len();
                let names = t.flags.iter().map(|f| f.name.clone()).collect();
                self.metadata.flags.push(metadata::Flags {
                    interface,
                    name: name.unwrap(),
                    names,
                });
                metadata::Type::Flags(index)
            }
            TypeDefKind::Tuple(t) => {
                let index = self.metadata.tuples.len();
                let types = t.types.iter().map(|t| self.lookup_ty(t)).collect();
                self.metadata.tuples.push(metadata::Tuple {
                    interface,
                    name,
                    types,
                });
                metadata::Type::Tuple(index)
            }
            TypeDefKind::Variant(t) => {
                let index = self.metadata.variants.len();
                let cases = t
                    .cases
                    .iter()
                    .map(|c| (c.name.clone(), c.ty.map(|t| self.lookup_ty(&t))))
                    .collect();
                self.metadata.variants.push(metadata::Variant {
                    interface,
                    name: name.unwrap(),
                    cases,
                });
                metadata::Type::Variant(index)
            }
            TypeDefKind::Enum(t) => {
                let index = self.metadata.enums.len();
                let names = t.cases.iter().map(|f| f.name.clone()).collect();
                self.metadata.enums.push(metadata::Enum {
                    interface,
                    name: name.unwrap(),
                    names,
                });
                metadata::Type::Enum(index)
            }
            TypeDefKind::Option(t) => {
                let index = self.metadata.options.len();
                self.metadata.options.push(metadata::WitOption {
                    interface,
                    name,
                    ty: self.lookup_ty(t),
                });
                metadata::Type::Option(index)
            }
            TypeDefKind::Result(t) => {
                let index = self.metadata.results.len();
                self.metadata.results.push(metadata::WitResult {
                    interface,
                    name,
                    ok: t.ok.map(|t| self.lookup_ty(&t)),
                    err: t.err.map(|t| self.lookup_ty(&t)),
                });
                metadata::Type::Result(index)
            }
            TypeDefKind::List(t) => {
                let index = self.metadata.lists.len();
                self.metadata.lists.push(metadata::List {
                    interface,
                    name,
                    ty: self.lookup_ty(t),
                });
                metadata::Type::List(index)
            }
            TypeDefKind::FixedSizeList(t, len) => {
                let index = self.metadata.fixed_size_lists.len();
                self.metadata
                    .fixed_size_lists
                    .push(metadata::FixedSizeList {
                        interface,
                        name,
                        len: *len,
                        ty: self.lookup_ty(t),
                    });
                metadata::Type::FixedSizeList(index)
            }
            TypeDefKind::Future(t) => {
                let index = self.metadata.futures.len();
                self.metadata.futures.push(metadata::Future {
                    interface,
                    name,
                    ty: t.map(|t| self.lookup_ty(&t)),
                });
                metadata::Type::Future(index)
            }
            TypeDefKind::Stream(t) => {
                let index = self.metadata.streams.len();
                self.metadata.streams.push(metadata::Stream {
                    interface,
                    name,
                    ty: t.map(|t| self.lookup_ty(&t)),
                });
                metadata::Type::Stream(index)
            }
            TypeDefKind::Type(t) => {
                let index = self.metadata.aliases.len();
                self.metadata.aliases.push(metadata::Alias {
                    interface,
                    name: name.unwrap(),
                    ty: self.lookup_ty(t),
                });
                metadata::Type::Alias(index)
            }
            TypeDefKind::Resource => metadata::Type::Own(self.resource_map[&id]),

            // Own/Borrow handles should have already inserted the resource into
            // `self.resource_map` so this is just a simple lookup.
            TypeDefKind::Handle(Handle::Own(t)) => {
                metadata::Type::Own(self.resource_map[&dealias(resolve, *t)])
            }
            TypeDefKind::Handle(Handle::Borrow(t)) => {
                metadata::Type::Borrow(self.resource_map[&dealias(resolve, *t)])
            }
            TypeDefKind::Unknown => unreachable!(),
        };
        self.type_map.insert(id, result);
    }

    fn lookup_ty(&self, ty: &Type) -> metadata::Type {
        match ty {
            Type::U8 => metadata::Type::U8,
            Type::U16 => metadata::Type::U16,
            Type::U32 => metadata::Type::U32,
            Type::U64 => metadata::Type::U64,
            Type::S8 => metadata::Type::S8,
            Type::S16 => metadata::Type::S16,
            Type::S32 => metadata::Type::S32,
            Type::S64 => metadata::Type::S64,
            Type::F32 => metadata::Type::F32,
            Type::F64 => metadata::Type::F64,
            Type::Bool => metadata::Type::Bool,
            Type::Char => metadata::Type::Char,
            Type::String => metadata::Type::String,
            Type::ErrorContext => metadata::Type::ErrorContext,
            // All id-based types should already be registered via
            // `register_type` so the hard work is already done and this is a
            // simple lookup.
            Type::Id(id) => self.type_map[id],
        }
    }

    fn bindgen_world_func_import(
        &mut self,
        resolve: &Resolve,
        key: Option<&WorldKey>,
        func: &wit_parser::Function,
        func_idx: u32,
    ) {
        let mangling = self.mangling();
        let body = bindgen::import(self, resolve, func, mangling.import_variant(), func_idx);

        let ty = self.define_ty([ValType::I32], []);

        let idx = self.define_func(&format!("adapter {}", func.name), ty, body, false);
        let elem_index = self.push_elem(idx);

        self.metadata.funcs.push(metadata::Func {
            interface: key.map(|k| resolve.name_world_key(k)),
            name: func.name.clone(),
            elem_index: Some(elem_index),
            args: func
                .params
                .iter()
                .map(|(_, ty)| self.lookup_ty(ty))
                .collect(),
            result: func.result.map(|t| self.lookup_ty(&t)),
        })
    }

    fn bindgen_world_func_export(
        &mut self,
        resolve: &Resolve,
        key: Option<&WorldKey>,
        func: &wit_parser::Function,
    ) {
        let mangling = self.mangling();
        let sig = resolve.wasm_signature(mangling.export_variant(), func);
        let ty = self.define_wasm_sig(sig);
        let name = resolve.wasm_export_name(
            mangling,
            WasmExport::Func {
                interface: key,
                func,
                kind: WasmExportKind::Normal,
            },
        );

        let (body, may_have_dynamic_lists_to_free) = bindgen::export(
            self,
            resolve,
            func,
            mangling.export_variant(),
            self.metadata.funcs.len(),
        );
        self.define_func(&name, ty, body, true);

        match mangling {
            ManglingAndAbi::Standard32 | ManglingAndAbi::Legacy(LiftLowerAbi::Sync) => {
                let post_return_name = resolve.wasm_export_name(
                    mangling,
                    WasmExport::Func {
                        interface: key,
                        func,
                        kind: WasmExportKind::PostReturn,
                    },
                );
                let post_return = bindgen::post_return(
                    self,
                    resolve,
                    func,
                    mangling.export_variant(),
                    self.metadata.funcs.len(),
                    may_have_dynamic_lists_to_free,
                );
                let mut sig = resolve.wasm_signature(mangling.export_variant(), func);
                sig.params = mem::take(&mut sig.results);
                let post_return_ty = self.define_wasm_sig(sig);
                self.define_func(&post_return_name, post_return_ty, post_return, true);
            }

            ManglingAndAbi::Legacy(LiftLowerAbi::AsyncCallback) => unimplemented!(),

            ManglingAndAbi::Legacy(LiftLowerAbi::AsyncStackful) => unimplemented!(),
        }

        self.metadata.funcs.push(metadata::Func {
            interface: key.map(|k| resolve.name_world_key(k)),
            name: func.name.clone(),
            elem_index: None,
            args: func
                .params
                .iter()
                .map(|(_, ty)| self.lookup_ty(ty))
                .collect(),
            result: func.result.map(|t| self.lookup_ty(&t)),
        })
    }

    fn bindgen_world_export_resource_dtor(
        &mut self,
        resolve: &Resolve,
        interface: &WorldKey,
        resource: TypeId,
        index: usize,
    ) {
        let mangling = self.mangling();
        let name = resolve.wasm_export_name(
            mangling,
            WasmExport::ResourceDtor {
                interface,
                resource,
            },
        );
        let dtor = self.intrinsics().resource_dtor;
        let mut func = Function::new([]);
        let mut ins = func.instructions();
        ins.i32_const(index.try_into().unwrap());
        ins.local_get(0);
        ins.call(dtor);
        ins.end();
        let ty = self.define_ty([ValType::I32], []);
        self.define_func(&name, ty, func, true);
    }

    fn encode_metadata(&mut self) -> (u32, Vec<u8>) {
        let (metadata_offset, metadata, apply_relocs) =
            self.metadata.encode(self.table_base(), self.memory_base());
        if let Some(apply_relocs) = apply_relocs {
            let ty = self.define_ty([], []);
            self.define_func("__wasm_apply_data_relocs", ty, apply_relocs, true);
        }
        (metadata_offset, metadata)
    }

    fn table_base(&self) -> u32 {
        self.table_base.unwrap()
    }

    fn intrinsics(&self) -> &bindgen::WitInterpreterIntrinsics {
        self.intrinsics.as_ref().unwrap()
    }

    fn stack_pointer(&self) -> u32 {
        self.stack_pointer.unwrap()
    }

    fn memory_base(&self) -> u32 {
        self.memory_base.unwrap()
    }

    fn push_elem(&mut self, elem: u32) -> u32 {
        let ret = self.elem_segment.len();
        self.elem_segment.push(elem);
        u32::try_from(ret).unwrap()
    }

    fn finish(&mut self, metadata: &[u8]) -> Vec<u8> {
        // Create the element segment dynamically added to the table, if necessary.
        let mut elements = ElementSection::new();
        if !self.elem_segment.is_empty() {
            elements.active(
                Some(0),
                &ConstExpr::global_get(self.table_base()),
                Elements::Functions(Cow::Borrowed(&self.elem_segment)),
            );
        }

        // Add a data segment for the interpreter metadata encoded data in-memory.
        let mut data = DataSection::new();
        data.active(
            0,
            &ConstExpr::global_get(self.memory_base()),
            metadata.iter().copied(),
        );

        let mut names = NameSection::new();
        names.functions(&self.function_names);
        names.globals(&self.global_names);

        let dylink0 = {
            struct MemInfo {
                memory_size: u32,
                memory_alignment: u32,
                table_size: u32,
                table_alignment: u32,
            }

            let mem_info = MemInfo {
                memory_size: metadata.len().try_into().unwrap(),
                memory_alignment: 2,
                table_size: self.elem_segment.len().try_into().unwrap(),
                table_alignment: 0,
            };

            let mut mem_info_subsection = Vec::new();
            mem_info.memory_size.encode(&mut mem_info_subsection);
            mem_info.memory_alignment.encode(&mut mem_info_subsection);
            mem_info.table_size.encode(&mut mem_info_subsection);
            mem_info.table_alignment.encode(&mut mem_info_subsection);

            let mut needed_subsection = Vec::new();
            if let Some(name) = &self.opts.interpreter {
                [name.as_str()].encode(&mut needed_subsection);
            }

            const WASM_DYLINK_MEM_INFO: u8 = 1;
            const WASM_DYLINK_NEEDED: u8 = 2;

            let mut dylink0 = Vec::new();
            dylink0.push(WASM_DYLINK_MEM_INFO);
            mem_info_subsection.encode(&mut dylink0);
            if self.opts.interpreter.is_some() {
                dylink0.push(WASM_DYLINK_NEEDED);
                needed_subsection.encode(&mut dylink0);
            }
            dylink0
        };

        let mut result = Module::new();
        result.section(&CustomSection {
            name: Cow::Borrowed("dylink.0"),
            data: Cow::Borrowed(&dylink0),
        });
        result.section(&self.types);
        result.section(&self.imports);
        result.section(&self.functions);
        result.section(&self.exports);
        if !elements.is_empty() {
            result.section(&elements);
        }
        result.section(&self.code);
        result.section(&data);
        result.section(&names);

        result.finish()
    }

    fn define_wasm_sig(&mut self, sig: WasmSignature) -> u32 {
        let ret = self.define_ty(
            sig.params
                .iter()
                .map(|t| self.map_wasm_type(*t))
                .collect::<Vec<_>>(),
            sig.results
                .iter()
                .map(|t| self.map_wasm_type(*t))
                .collect::<Vec<_>>(),
        );
        return ret;
    }

    fn map_wasm_type(&self, a: WasmType) -> ValType {
        match a {
            WasmType::I32 => ValType::I32,
            WasmType::I64 => ValType::I64,
            WasmType::F32 => ValType::F32,
            WasmType::F64 => ValType::F64,
            WasmType::PointerOrI64 => ValType::I64,
            WasmType::Length | WasmType::Pointer => ValType::I32,
        }
    }

    fn import_global(&mut self, module: &str, name: &str, ty: GlobalType) -> u32 {
        assert!(!self.imports_done);
        self.imports.import(module, name, ty);
        let ret = self.global_index;
        self.global_index += 1;
        self.global_names.append(ret, name);
        ret
    }

    fn import_func(&mut self, module: &str, name: &str, ty: u32) -> u32 {
        assert!(!self.imports_done);
        self.imports.import(module, name, EntityType::Function(ty));
        let ret = self.func_index;
        self.func_index += 1;
        self.function_names.append(ret, name);
        ret
    }

    fn define_func(&mut self, name: &str, ty: u32, body: Function, export: bool) -> u32 {
        assert!(self.imports_done);
        let ret = self.func_index;
        self.func_index += 1;
        self.functions.function(ty);
        self.code.function(&body);
        self.function_names.append(ret, name);
        if export {
            self.exports.export(name, ExportKind::Func, ret);
        }
        ret
    }

    fn define_ty<P, R>(&mut self, params: P, results: R) -> u32
    where
        P: IntoIterator<Item = ValType> + Clone,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType> + Clone,
        R::IntoIter: ExactSizeIterator,
    {
        let param_vec = params.clone().into_iter().collect::<Vec<_>>();
        let result_vec = results.clone().into_iter().collect::<Vec<_>>();
        *self
            .wasm_type_map
            .entry((param_vec, result_vec))
            .or_insert_with(|| {
                let ret = self.types.len();
                self.types.ty().function(params, results);
                ret
            })
    }
}

fn imported_types_used_by_exported_interfaces(resolve: &Resolve, world: WorldId) -> LiveTypes {
    // First build up a set of all types used by exported interfaces which
    // define their own types.
    let mut live_export_types = LiveTypes::default();
    let mut exported_interfaces = HashSet::new();
    for (_, export) in resolve.worlds[world].exports.iter() {
        match export {
            WorldItem::Function(_) => {}
            WorldItem::Interface { id, .. } => {
                exported_interfaces.insert(*id);
                live_export_types.add_interface(resolve, *id)
            }
            WorldItem::Type(_) => unreachable!(),
        }
    }

    // Using the above sets a new set is built of all types that aren't
    // reexported. All types used by exports, which are defined by an interface
    // that is NOT an export, is an imported type used by an export.
    let mut live_import_types = LiveTypes::default();
    for ty in live_export_types.iter() {
        if let TypeOwner::Interface(id) = resolve.types[ty].owner {
            if !exported_interfaces.contains(&id) {
                live_import_types.add_interface(resolve, id);
            }
        }
    }

    live_import_types
}

fn dealias(resolve: &Resolve, mut id: TypeId) -> TypeId {
    loop {
        match resolve.types[id].kind {
            TypeDefKind::Type(Type::Id(other)) => id = other,
            _ => break id,
        }
    }
}
