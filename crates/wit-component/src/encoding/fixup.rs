use crate::ComponentEncoder;
use crate::encoding::{CustomModule, EncodingState, Shim, ShimKind, Shims};
use crate::validation::Export;
use anyhow::{Result, bail};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::mem;
use wasm_encoder::*;
use wit_parser::WorldItem;
use wit_parser::abi::{AbiVariant, WasmSignature};

#[derive(Default)]
pub struct FixupModule {
    // Incrementally-built-up sections of the module-to-be.
    types: TypeSection,
    imports: ImportSection,
    function_names: NameMap,
    global_names: NameMap,
    elements: ElementSection,
    data: DataSection,

    /// Interning map for type signatures.
    type_map: HashMap<(Vec<ValType>, Vec<ValType>), u32>,

    /// The start function, if needed. This is split between "ABI details" and
    /// "user funcs" where ABI details must happen first and user funcs need to
    /// be wrapped in task intrinsics if provided.
    start_abi_details: Vec<StartAction>,
    start_user_funcs: Vec<StartAction>,

    /// Entries used to initialize the imported shim's table, if necessary.
    shims_in_table: Vec<ShimFill>,

    /// Entries to go into a declared element segment.
    declared_funcs: Vec<ShimFill>,

    /// The instance names that are imported into this module, and what those
    /// instances correspond to.
    imported_instances: IndexMap<String, ImportedInstance>,

    /// Functions that will be defined in this module itself.
    ///
    /// This is deferred to get emitted until the very end so all imports have
    /// been settled and then these import indices can be known.
    defined_functions: Vec<(u32, DefinedFunction)>,

    // Current counters for index spaces.
    funcs: u32,
    globals: u32,
    tables: u32,
    memories: u32,
}

/// Different instances that can be imported into the fixup module.
#[derive(Clone, PartialEq, Debug)]
pub enum ImportedInstance {
    /// The original "shim" module with stubs that need to be filled in.
    Shim,
    /// The "main" module.
    Main,
    /// A synthetic instance created by collecting actual implementations of
    /// shims from various locations.
    Actual,
    /// A named adapter module.
    Adapter(String),
}

pub enum StartAction {
    /// Sets `$global` to `ref.func $shim`
    InitGlobal {
        /// The index in `declared_funcs` of the function to fill in.
        shim: usize,
        /// The global being set.
        global: u32,
    },
    /// Calls the specified function index.
    Call(u32),
    /// Used during the `link`-part of `wit-component` this adds the
    /// `memory_base` and `export` globals together and stores the result in
    /// `dest`.
    InitializeAddress {
        memory_base: u32,
        export: u32,
        dest: AddressDest,
    },
    /// Invoke the `func` hook with the `hook` argument.
    CallHook { func: u32, hook: TaskHook },
}

pub enum AddressDest {
    /// A linear memory address that's statically known.
    LinearMemory { address: u32, memory: u32 },
    /// A global indexed by the argument here.
    Global(u32),
}

/// What can be used to fill in a shim that was originally defined in the shim
/// module.
///
/// Functions specified here either go into an element segment to fill in the
/// original shim module's table or they're inserted into globals.
enum ShimFill {
    /// An imported function.
    Import(u32),
    /// An imported function which is also known to be a resource destructor.
    /// Used to know when to insert task hooks.
    ImportedResourceDtor(u32),
    /// A function defined in this fixup module.
    DefinedFunction(usize),
}

/// Different functions that can be defined in the fixup module, apart from the
/// `start` function.
enum DefinedFunction {
    /// A function which wraps a core export to ensure that task state is set up
    /// before the core export is called.
    HookedCoreExport {
        /// The task hook function.
        task_hook: u32,
        /// The core function to call and delegate to.
        to_wrap: u32,
        /// The number of parameters to forward to `export`.
        params: usize,
        /// The debug-related core name of this function it's hooking.
        core_name: String,
        /// Whether or not to export this function.
        export: bool,
        /// The kind of hook to use at the start of this function.
        start: TaskHook,
        /// The kind of hook to use at the end of this function.
        end: EndTaskHook,
    },
}

#[derive(Copy, Clone)]
pub enum TaskHook {
    /// A synchronous task has started.
    SyncStart = 0,
    /// A synchronous task has finished.
    SyncFinish = 1,
    /// An async task has started.
    AsyncStart = 2,
    /// An async has resumed in its `callback` option.
    AsyncResume = 3,
    /// An async blocked, but not yet completed, and it's returning from either
    /// the main entrypoint or the `callback` option.
    AsyncBlock = 4,
    /// An async task has finished, returning from either the main entrypoint or
    /// the `callback` option.
    AsyncFinish = 5,
    /// The `_initialize` function and other ctors are being called.
    InitializeStart = 6,
    /// The `_initialize` function and other ctors are finished.
    InitializeFinish = 7,
    /// A resource destructor is starting.
    ResourceDtorStart = 8,
    /// A resource destructor is finished.
    ResourceDtorFinish = 9,
    /// A call to post-return is starting.
    PostReturnStart = 10,
    /// A call to post-return is finished.
    PostReturnFinish = 11,
}

enum EndTaskHook {
    AsyncCode,
    Normal(TaskHook),
}

impl FixupModule {
    /// Returns the 0-parameter 0-result function type.
    pub fn type_thunk(&mut self) -> u32 {
        self.type_intern(Vec::new(), Vec::new())
    }

    /// Interns `params`/`results` as a function type.
    pub fn type_intern(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        *self
            .type_map
            .entry((params.clone(), results.clone()))
            .or_insert_with(|| {
                let index = self.types.len();
                self.types
                    .ty()
                    .function(params.iter().copied(), results.iter().copied());
                index
            })
    }

    fn type_index(&mut self, sig: &WasmSignature) -> u32 {
        self.type_intern(
            sig.params.iter().map(super::to_val_type).collect(),
            sig.results.iter().map(super::to_val_type).collect(),
        )
    }

    fn import_instance(&mut self, kind: &ImportedInstance) -> Result<String> {
        let name = match kind {
            ImportedInstance::Shim => "shim",
            ImportedInstance::Main => "main",
            ImportedInstance::Actual => "actual",
            ImportedInstance::Adapter(name) => name.as_str(),
        };
        let name = name.to_string();
        if let Some(prev) = self.imported_instances.insert(name.clone(), kind.clone()) {
            if prev != *kind {
                bail!("instance `{name}` was already imported as a different kind: {prev:?}");
            }
        }
        Ok(name)
    }

    /// Imports a new item into this fixup module to get used by later
    /// functions/etc.
    ///
    /// Returns the index that the item is imported as.
    pub fn import(
        &mut self,
        instance: &ImportedInstance,
        name: &str,
        ty: EntityType,
    ) -> Result<u32> {
        let module = self.import_instance(instance)?;
        let ret = match ty {
            EntityType::Function(_) => inc(&mut self.funcs),
            EntityType::Global(_) => inc(&mut self.globals),
            EntityType::Table(_) => inc(&mut self.tables),
            EntityType::Memory(_) => inc(&mut self.memories),
            EntityType::Tag(_) | EntityType::FunctionExact(_) => unimplemented!(),
        };
        self.imports.import(&module, name, ty);
        Ok(ret)
    }

    /// Helper over `self.import(...)`
    pub fn import_global(
        &mut self,
        instance: &ImportedInstance,
        name: &str,
        ty: GlobalType,
    ) -> Result<u32> {
        let index = self.import(instance, name, ty.into())?;
        self.global_names.append(index, name);
        Ok(index)
    }

    /// Helper over `self.import(...)`
    pub fn import_func(&mut self, instance: &ImportedInstance, name: &str, ty: u32) -> Result<u32> {
        let index = self.import(instance, name, EntityType::Function(ty))?;
        self.function_names.append(index, name);
        Ok(index)
    }

    /// Appends to the `start` function that'll get generated.
    pub fn add_start_abi_detail(&mut self, action: StartAction) {
        self.start_abi_details.push(action);
    }

    /// Appends to the `start` function that'll get generated.
    pub fn add_start_user_func(&mut self, action: StartAction) {
        self.start_user_funcs.push(action);
    }

    /// Returns the underlying element section.
    pub fn elements(&mut self) -> &mut ElementSection {
        &mut self.elements
    }

    /// Returns the underlying data section.
    pub fn data(&mut self) -> &mut DataSection {
        &mut self.data
    }

    /// Adds `shim` to this fixup module to get filled in.
    pub(super) fn add_shim(&mut self, opts: &ComponentEncoder, shim: &Shim) -> Result<()> {
        let type_index = self.type_index(&shim.sig);
        let func = self.import_func(&ImportedInstance::Actual, &shim.name, type_index)?;

        // Classify this as either a resource destructor or not as resource
        // destructors need some special handling with task hooks below.
        let shim_fill = if matches!(shim.kind, ShimKind::ResourceDtor { .. }) {
            ShimFill::ImportedResourceDtor(func)
        } else {
            ShimFill::Import(func)
        };

        if opts.shim_return_call_ref {
            let global = self.import_global(
                &ImportedInstance::Shim,
                &format!("g{}", &shim.name),
                GlobalType {
                    val_type: RefType {
                        heap_type: HeapType::Concrete(type_index),
                        nullable: false,
                    }
                    .into(),
                    mutable: true,
                    shared: false,
                },
            )?;

            self.start_abi_details.push(StartAction::InitGlobal {
                shim: self.declared_funcs.len(),
                global,
            });
            self.declared_funcs.push(shim_fill);
        } else {
            self.shims_in_table.push(shim_fill);
        }
        Ok(())
    }

    /// Ensures there's a `start` function which invokes `initialize`.
    pub fn add_initialize(&mut self, initialize: &str) -> Result<()> {
        let ty = self.type_thunk();
        let func = self.import_func(&ImportedInstance::Main, initialize, ty)?;
        self.start_user_funcs.push(StartAction::Call(func));
        Ok(())
    }

    pub(super) fn instantiate(
        &mut self,
        shims: &Shims<'_>,
        state: &mut EncodingState<'_>,
    ) -> Result<()> {
        // First inject task hooks as necessary throughout the module which is
        // the final step before actually encoding the module.
        self.prepare_task_hooks(state)?;

        // Generate the module, and this'll return `None` if the module isn't
        // necessary in which case we bail out.
        let Some(module) = self.encode(state)? else {
            return Ok(());
        };

        // Embed this fixup module in the component.
        let module_index = state
            .component
            .core_module(Some("wit-component-fixup"), &module);

        // Prepare the arguments used to instantiate this component based on the
        // `imported_instances` map that's been generated.
        let mut instance_args = Vec::new();
        for (name, instance) in self.imported_instances.iter() {
            let index = match instance {
                ImportedInstance::Main => state
                    .instance_index
                    .expect("must have an instantiated module"),
                ImportedInstance::Adapter(name) => state.adapter_instances[name.as_str()],
                ImportedInstance::Shim => state
                    .shim_instance_index
                    .expect("must have an instantiated shim"),
                ImportedInstance::Actual => {
                    let mut actual = Vec::new();
                    for shim in shims.shims.values() {
                        let core_func_index = state.encode_shim(shim)?;
                        actual.push((shim.name.as_str(), ExportKind::Func, core_func_index));
                    }
                    state
                        .component
                        .core_instantiate_exports(Some("actual"), actual)
                }
            };
            instance_args.push((name.as_str(), ModuleArg::Instance(index)));
        }

        // The side-effectful instantiation of the fixup instance.
        let instance = state
            .component
            .core_instantiate(Some("fixup"), module_index, instance_args);

        // If there are hooked core exports in this module then register within
        // `state` that they should be preferred over their raw brethren.
        for (i, (_ty, func)) in self.defined_functions.iter().enumerate() {
            match func {
                DefinedFunction::HookedCoreExport {
                    core_name,
                    export: true,
                    ..
                } => {
                    let name = format!("hook{i}");
                    let wrapper =
                        state.core_alias_export(Some(&name), instance, &name, ExportKind::Func);
                    state
                        .export_task_initialization_wrappers
                        .insert(core_name.clone(), wrapper);
                }
                DefinedFunction::HookedCoreExport { export: false, .. } => {}
            }
        }
        Ok(())
    }

    fn encode(&mut self, state: &EncodingState<'_>) -> Result<Option<Module>> {
        let opts = &state.info.encoder;
        if self.start_abi_details.is_empty()
            && self.start_user_funcs.is_empty()
            && self.shims_in_table.is_empty()
            && self.declared_funcs.is_empty()
            && self.elements.is_empty()
            && self.data.is_empty()
            && self.defined_functions.is_empty()
        {
            return Ok(None);
        }

        let mut functions = FunctionSection::new();
        let mut code = CodeSection::new();
        let mut exports = ExportSection::new();

        // First define functions within this module now that the import space
        // for functions has settled.
        let mut defined_func_indices = Vec::new();
        for (i, (ty, func)) in self.defined_functions.iter().enumerate() {
            functions.function(*ty);
            let index = inc(&mut self.funcs);
            defined_func_indices.push(index);
            let mut func = match func {
                DefinedFunction::HookedCoreExport {
                    task_hook,
                    to_wrap,
                    params,
                    core_name,
                    export,
                    start,
                    end,
                } => {
                    let mut locals = Vec::new();
                    if let EndTaskHook::AsyncCode = end {
                        locals.push((1, ValType::I32));
                    }
                    let mut f = Function::new(locals);
                    f.instructions().i32_const(*start as i32).call(*task_hook);
                    for i in 0..*params {
                        f.instructions().local_get(i as u32);
                    }
                    f.instructions().call(*to_wrap);
                    match end {
                        EndTaskHook::AsyncCode => {
                            f.instructions()
                                .local_set(*params as u32)
                                // 0 == EXIT, 1/2 == yield/wait, so delegate to
                                // appropriate code.
                                //
                                // First thing pushed on the stack is "if true",
                                // which in this case nonzero means blocking.
                                // Second thing is "if false" meaning if 0
                                // meaning EXIT meaning "done". Final thing is
                                // what to test, the return code.
                                .i32_const(TaskHook::AsyncBlock as i32)
                                .i32_const(TaskHook::AsyncFinish as i32)
                                .local_get(*params as u32)
                                .select()
                                .call(*task_hook)
                                // put the return code back on the stack to
                                // actually return
                                .local_get(*params as u32);
                        }
                        EndTaskHook::Normal(hook) => {
                            f.instructions().i32_const(*hook as i32).call(*task_hook);
                        }
                    }
                    if *export {
                        exports.export(&format!("hook{i}"), ExportKind::Func, index);
                    }
                    self.function_names
                        .append(index, &format!("hook-{core_name}"));
                    f
                }
            };
            func.instructions().end();
            code.function(&func);
        }

        // Using our defined functions above it's possible to implement
        // resolution of `ShimFill` items.
        let resolve_shim = |shim: &ShimFill| match shim {
            ShimFill::Import(func) => *func,
            ShimFill::ImportedResourceDtor(func) => *func,
            ShimFill::DefinedFunction(i) => defined_func_indices[*i],
        };

        // If this fixup is filling in a table, then import the table and use an
        // element segment for its initialization.
        if !self.shims_in_table.is_empty() {
            let table_type = TableType {
                element_type: RefType::FUNCREF,
                minimum: self.shims_in_table.len() as u64,
                maximum: Some(self.shims_in_table.len() as u64),
                table64: false,
                shared: false,
            };
            let table = self.import(
                &ImportedInstance::Shim,
                super::INDIRECT_TABLE_NAME,
                table_type.into(),
            )?;
            self.elements.active(
                if table == 0 { None } else { Some(table) },
                &ConstExpr::i32_const(0),
                Elements::Functions(
                    self.shims_in_table
                        .iter()
                        .map(resolve_shim)
                        .collect::<Vec<_>>()
                        .into(),
                ),
            );
        }

        // Codegen the start function here. Special case an empty start function
        // or just a single function call as the start function, otherwise each
        // item needs to be handled individually.
        let start = match (&self.start_abi_details[..], &self.start_user_funcs[..]) {
            ([], []) => None,
            ([], [StartAction::Call(func)]) => Some(*func),
            (first, second) => {
                let mut start = Function::new(Vec::new());
                for action in first.iter().chain(second.iter()) {
                    match action {
                        StartAction::InitGlobal { shim, global } => {
                            let func = resolve_shim(&self.declared_funcs[*shim]);
                            start.instructions().ref_func(func).global_set(*global);
                        }
                        StartAction::Call(func) => {
                            start.instructions().call(*func);
                        }
                        StartAction::CallHook { func, hook } => {
                            start.instructions().i32_const(*hook as i32).call(*func);
                        }
                        StartAction::InitializeAddress {
                            memory_base,
                            export,
                            dest,
                        } => match dest {
                            AddressDest::LinearMemory { address, memory } => {
                                start
                                    .instructions()
                                    .i32_const(*address as i32)
                                    .global_get(*memory_base)
                                    .global_get(*export)
                                    .i32_add()
                                    .i32_store(MemArg {
                                        offset: 0,
                                        align: 2,
                                        memory_index: *memory,
                                    });
                            }
                            AddressDest::Global(global) => {
                                start
                                    .instructions()
                                    .global_get(*memory_base)
                                    .global_get(*export)
                                    .i32_add()
                                    .global_set(*global);
                            }
                        },
                    }
                }
                start.instructions().end();
                let index = inc(&mut self.funcs);
                let ty = self.type_thunk();
                functions.function(ty);
                code.function(&start);
                self.function_names.append(index, "start");
                Some(index)
            }
        };

        // Fill in the declared element segment, if necessary, used for
        // `ref.func` in the start function above.
        if !self.declared_funcs.is_empty() {
            self.elements.declared(Elements::Functions(
                self.declared_funcs
                    .iter()
                    .map(resolve_shim)
                    .collect::<Vec<_>>()
                    .into(),
            ));
        }

        // Now that we've got all the pieces weave everything into a `Module`.
        let mut module = Module::default();
        if !self.types.is_empty() {
            module.section(&self.types);
        }
        if !self.imports.is_empty() {
            module.section(&self.imports);
        }
        if !functions.is_empty() {
            module.section(&functions);
        }
        if !exports.is_empty() {
            module.section(&exports);
        }
        if let Some(start) = start {
            module.section(&StartSection {
                function_index: start,
            });
        }
        if !self.elements.is_empty() {
            module.section(&self.elements);
        }
        if !code.is_empty() {
            module.section(&code);
        }
        if !self.data.is_empty() {
            module.section(&self.data);
        }
        module.section(&RawCustomSection(
            &crate::base_producers().raw_custom_section(),
        ));

        if opts.debug_names {
            let mut names = NameSection::new();
            names.module("wit-component:fixups");
            if !self.function_names.is_empty() {
                names.functions(&self.function_names);
            }
            if !self.global_names.is_empty() {
                names.globals(&self.global_names);
            }
            module.section(&names);
        }

        Ok(Some(module))
    }

    /// Prepares task hooks, like `__wasm_init_task`, to be configured in
    /// various locations throughout this fixup module.
    ///
    /// This handles:
    ///
    /// * The `_initialize` function (needs task setup first)
    /// * Hooks for all exports which are lifted.
    fn prepare_task_hooks(&mut self, state: &mut EncodingState<'_>) -> Result<()> {
        let info_main = state.info.exports_for(CustomModule::Main);
        let Some(task_hook) = info_main.wasm_task_hook() else {
            return Ok(());
        };
        let ty = self.type_intern(vec![ValType::I32], Vec::new());
        let task_hook = self.import_func(&ImportedInstance::Main, task_hook, ty)?;

        // Handle the start function first where if there's something registered
        // in the "user function" area we need to configure that to work.
        if !self.start_user_funcs.is_empty() {
            self.start_user_funcs.insert(
                0,
                StartAction::CallHook {
                    func: task_hook,
                    hook: TaskHook::InitializeStart,
                },
            );
            self.start_user_funcs.push(StartAction::CallHook {
                func: task_hook,
                hook: TaskHook::InitializeFinish,
            });
        }

        // Afterwards handle all exports for the main/adapter modules.
        self.prepare_export_task_hooks_for(state, CustomModule::Main, task_hook)?;
        for adapter in state.info.adapters.keys() {
            self.prepare_export_task_hooks_for(state, CustomModule::Adapter(adapter), task_hook)?;
        }

        // And finally redirect all resource destructors to a local hook which
        // ensures that task state is configured correctly there.
        let mut shims_in_table = mem::take(&mut self.shims_in_table);
        let mut declared_funcs = mem::take(&mut self.declared_funcs);
        for shim in shims_in_table.iter_mut().chain(declared_funcs.iter_mut()) {
            let export = match shim {
                ShimFill::ImportedResourceDtor(func) => *func,
                ShimFill::Import(_) | ShimFill::DefinedFunction(_) => continue,
            };
            let ty = self.type_intern(vec![ValType::I32], Vec::new());
            *shim = ShimFill::DefinedFunction(self.defined_functions.len());
            self.defined_functions.push((
                ty,
                DefinedFunction::HookedCoreExport {
                    task_hook,
                    to_wrap: export,
                    params: 1,
                    start: TaskHook::ResourceDtorStart,
                    end: EndTaskHook::Normal(TaskHook::ResourceDtorFinish),
                    export: false,
                    core_name: format!("resource-dtor"),
                },
            ));
        }

        self.shims_in_table = shims_in_table;
        self.declared_funcs = declared_funcs;

        Ok(())
    }

    /// Looks over the exports of `for_module` and adds any functions as
    /// necessary to `self.defined_functions` which hook the original export to
    /// ensure it's got task state set up when called first.
    fn prepare_export_task_hooks_for(
        &mut self,
        state: &mut EncodingState<'_>,
        for_module: CustomModule<'_>,
        task_hook: u32,
    ) -> Result<()> {
        let resolve = &state.info.encoder.metadata.resolve;
        let world = &resolve.worlds[state.info.encoder.metadata.world];
        let info = state.info.exports_for(for_module);
        let imported_instance = match for_module {
            CustomModule::Main => ImportedInstance::Main,
            CustomModule::Adapter(name) => ImportedInstance::Adapter(name.to_string()),
        };

        for (core_name, export) in info.iter() {
            let (key, f, abi) = match export {
                Export::WorldFunc(key, _, abi) => match &world.exports[key] {
                    WorldItem::Function(f) => (key, f, abi),
                    _ => continue,
                },
                Export::InterfaceFunc(key, id, func_name, abi) => {
                    let func = &resolve.interfaces[*id].functions[func_name.as_str()];
                    (key, func, abi)
                }
                _ => continue,
            };

            let sig = resolve.wasm_signature(*abi, f);
            let ty = self.type_index(&sig);
            let func = self.import_func(&imported_instance, core_name, ty)?;
            self.defined_functions.push((
                ty,
                DefinedFunction::HookedCoreExport {
                    task_hook,
                    to_wrap: func,
                    export: true,
                    params: sig.params.len(),
                    core_name: core_name.to_string(),
                    start: if abi.is_async() {
                        TaskHook::AsyncStart
                    } else {
                        TaskHook::SyncStart
                    },
                    end: if abi.is_async() {
                        if *abi == AbiVariant::GuestExportAsyncStackful {
                            EndTaskHook::Normal(TaskHook::AsyncFinish)
                        } else {
                            EndTaskHook::AsyncCode
                        }
                    } else {
                        EndTaskHook::Normal(TaskHook::SyncFinish)
                    },
                },
            ));

            if let Some(post_return) = info.post_return(key, f) {
                let mut post_return_sig = sig.clone();
                post_return_sig.params = mem::take(&mut post_return_sig.results);
                let ty = self.type_index(&post_return_sig);
                let func = self.import_func(&imported_instance, post_return, ty)?;
                self.defined_functions.push((
                    ty,
                    DefinedFunction::HookedCoreExport {
                        task_hook,
                        to_wrap: func,
                        export: true,
                        params: post_return_sig.params.len(),
                        core_name: post_return.to_string(),
                        start: TaskHook::PostReturnStart,
                        end: EndTaskHook::Normal(TaskHook::PostReturnFinish),
                    },
                ));
            }

            if let Some(callback) = info.callback(key, f) {
                let ty = self.type_intern(vec![ValType::I32; 3], vec![ValType::I32]);
                let func = self.import_func(&imported_instance, callback, ty)?;
                self.defined_functions.push((
                    ty,
                    DefinedFunction::HookedCoreExport {
                        task_hook,
                        to_wrap: func,
                        export: true,
                        params: 3,
                        core_name: callback.to_string(),
                        start: TaskHook::AsyncResume,
                        end: EndTaskHook::AsyncCode,
                    },
                ));
            }
        }

        Ok(())
    }
}

fn inc(i: &mut u32) -> u32 {
    let ret = *i;
    *i += 1;
    ret
}
