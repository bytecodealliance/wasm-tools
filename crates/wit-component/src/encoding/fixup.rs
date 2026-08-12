use crate::ComponentEncoder;
use crate::encoding::{CustomModule, EncodingState, Shim, Shims};
use anyhow::{Result, bail};
use indexmap::IndexMap;
use std::collections::HashMap;
use wasm_encoder::*;
use wit_parser::abi::WasmSignature;

#[derive(Default)]
pub struct FixupModule {
    // Incrementally-built-up sections of the module-to-be.
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    code: CodeSection,
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
    shims_in_table: Vec<u32>,

    /// Entries to go into a declared element segment.
    declared_funcs: Vec<u32>,

    /// The instance names that are imported into this module, and what those
    /// instances correspond to.
    imported_instances: IndexMap<String, ImportedInstance>,

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
    /// Sets `$global` to `ref.func $func`
    InitGlobal { func: u32, global: u32 },
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
}

pub enum AddressDest {
    /// A linear memory address that's statically known.
    LinearMemory { address: u32, memory: u32 },
    /// A global indexed by the argument here.
    Global(u32),
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

            self.start_abi_details
                .push(StartAction::InitGlobal { func, global });
            self.declared_funcs.push(func);
        } else {
            self.shims_in_table.push(func);
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
        // Generate the module, and this'll return `None` if the module isn't
        // necessary in which case we bail out.
        let Some(module) = self.encode(state)? else {
            return Ok(());
        };

        // Embed this fixup module in the component.
        let module_index = state
            .component
            .core_module(Some("wit-component-fixup"), &module);

        let mut instance_args = Vec::new();

        for (name, instance) in self.imported_instances.iter() {
            let index = match instance {
                ImportedInstance::Shim => state
                    .shim_instance_index
                    .expect("must have an instantiated shim"),
                ImportedInstance::Main => state
                    .instance_index
                    .expect("must have an instantiated module"),
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
                ImportedInstance::Adapter(name) => state.adapter_instances[name.as_str()],
            };
            instance_args.push((name.as_str(), ModuleArg::Instance(index)));
        }

        // The side-effectful instantiation of the fixup instance.
        state
            .component
            .core_instantiate(Some("fixup"), module_index, instance_args);
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
        {
            return Ok(None);
        }

        // Be sure to hook `__wasm_init_task` around `_initialize`
        if !self.start_user_funcs.is_empty() {
            if let Some(name) = state.info.exports_for(CustomModule::Main).wasm_init_task() {
                let ty = self.type_thunk();
                let init = self.import_func(&ImportedInstance::Main, name, ty)?;
                self.start_user_funcs.insert(0, StartAction::Call(init));
            }
        }

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
                Elements::Functions((&self.shims_in_table).into()),
            );
        }

        let start = match (&self.start_abi_details[..], &self.start_user_funcs[..]) {
            ([], []) => None,
            ([], [StartAction::Call(func)]) => Some(*func),
            (first, second) => {
                let mut start = Function::new(Vec::new());
                for action in first.iter().chain(second.iter()) {
                    match action {
                        StartAction::InitGlobal { func, global } => {
                            start.instructions().ref_func(*func).global_set(*global);
                        }
                        StartAction::Call(func) => {
                            start.instructions().call(*func);
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
                self.functions.function(ty);
                self.code.function(&start);
                self.function_names.append(index, "start");
                Some(index)
            }
        };
        if !self.declared_funcs.is_empty() {
            self.elements
                .declared(Elements::Functions((&self.declared_funcs).into()));
        }

        let mut module = Module::default();
        if !self.types.is_empty() {
            module.section(&self.types);
        }
        if !self.imports.is_empty() {
            module.section(&self.imports);
        }
        if !self.functions.is_empty() {
            module.section(&self.functions);
        }
        if let Some(start) = start {
            module.section(&StartSection {
                function_index: start,
            });
        }
        if !self.elements.is_empty() {
            module.section(&self.elements);
        }
        if !self.code.is_empty() {
            module.section(&self.code);
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
}

fn inc(i: &mut u32) -> u32 {
    let ret = *i;
    *i += 1;
    ret
}
