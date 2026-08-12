use crate::ComponentEncoder;
use crate::encoding::{EncodingState, Shim, Shims};
use anyhow::Result;
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

    /// Interning map for type signatures.
    type_map: HashMap<WasmSignature, u32>,

    /// The start function, if needed.
    start_function: Vec<StartAction>,

    /// Entries used to initialize the imported shim's table, if necessary.
    shims_in_table: Vec<u32>,

    /// Entries to go into a declared element segment.
    declared_funcs: Vec<u32>,

    /// Whether or not this module imports the shim instance itself.
    imports_shim_instance: bool,

    // Current counters for index spaces.
    funcs: u32,
    globals: u32,
}

enum StartAction {
    InitGlobal { func: u32, global: u32 },
}

impl FixupModule {
    fn type_index(&mut self, sig: &WasmSignature) -> u32 {
        *self.type_map.entry(sig.clone()).or_insert_with(|| {
            let index = self.types.len();
            self.types.ty().function(
                sig.params.iter().map(super::to_val_type),
                sig.results.iter().map(super::to_val_type),
            );
            index
        })
    }

    /// Adds `shim` to this fixup module to get filled in.
    pub fn add_shim(&mut self, opts: &ComponentEncoder, shim: &Shim) {
        let type_index = self.type_index(&shim.sig);
        self.imports
            .import("actual", &shim.name, EntityType::Function(type_index));
        let func = self.funcs;
        self.funcs += 1;

        if opts.shim_return_call_ref {
            self.imports.import(
                "shim",
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
            );
            self.imports_shim_instance = true;
            let global = self.globals;
            self.globals += 1;

            self.start_function
                .push(StartAction::InitGlobal { func, global });
            self.declared_funcs.push(func);
        } else {
            self.shims_in_table.push(func);
        }
    }

    pub fn instantiate(&mut self, shims: &Shims<'_>, state: &mut EncodingState<'_>) -> Result<()> {
        // Generate the module, and this'll return `None` if the module isn't
        // necessary in which case we bail out.
        let Some(module) = self.encode(&state.info.encoder) else {
            return Ok(());
        };

        // Embed this fixup module in the component.
        let module_index = state
            .component
            .core_module(Some("wit-component-fixup"), &module);

        // Incrementally build up the arguments to instantiation. First start
        // out with the shim instance itself if that's imported directly.
        let mut instance_args = Vec::new();
        if self.imports_shim_instance {
            let shim_instance_index = state
                .shim_instance_index
                .expect("must have an instantiated shim");
            instance_args.push(("shim", ModuleArg::Instance(shim_instance_index)));
        }

        // Shims get imported under the instance name "actual" since they're
        // filled in with the actual implementation here.
        if shims.shims.len() > 0 {
            let mut actual = Vec::new();
            for shim in shims.shims.values() {
                let core_func_index = state.encode_shim(shim)?;
                actual.push((shim.name.as_str(), ExportKind::Func, core_func_index));
            }
            let actual_index = state
                .component
                .core_instantiate_exports(Some("actual"), actual);
            instance_args.push(("actual", ModuleArg::Instance(actual_index)));
        }

        // The side-effectful instantiation of the fixup instance.
        state
            .component
            .core_instantiate(Some("fixup"), module_index, instance_args);
        Ok(())
    }

    fn encode(&mut self, opts: &ComponentEncoder) -> Option<Module> {
        if self.start_function.is_empty()
            && self.shims_in_table.is_empty()
            && self.declared_funcs.is_empty()
        {
            return None;
        }

        let mut elements = ElementSection::new();
        if !self.shims_in_table.is_empty() {
            let table_type = TableType {
                element_type: RefType::FUNCREF,
                minimum: self.shims_in_table.len() as u64,
                maximum: Some(self.shims_in_table.len() as u64),
                table64: false,
                shared: false,
            };
            self.imports
                .import("shim", super::INDIRECT_TABLE_NAME, table_type);
            self.imports_shim_instance = true;
            elements.active(
                None,
                &ConstExpr::i32_const(0),
                Elements::Functions((&self.shims_in_table).into()),
            );
        }

        let start = if !self.start_function.is_empty() {
            let mut start = Function::new(Vec::new());
            for action in &self.start_function {
                match action {
                    StartAction::InitGlobal { func, global } => {
                        start.instructions().ref_func(*func).global_set(*global);
                    }
                }
            }
            start.instructions().end();
            let index = self.funcs;
            self.funcs += 1;
            let ty = self.type_index(&WasmSignature {
                params: Vec::new(),
                results: Vec::new(),
                indirect_params: false,
                retptr: false,
            });
            self.functions.function(ty);
            self.code.function(&start);
            self.function_names.append(index, "start");
            Some(index)
        } else {
            None
        };
        if !self.declared_funcs.is_empty() {
            elements.declared(Elements::Functions((&self.declared_funcs).into()));
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
        if !elements.is_empty() {
            module.section(&elements);
        }
        if !self.code.is_empty() {
            module.section(&self.code);
        }
        module.section(&RawCustomSection(
            &crate::base_producers().raw_custom_section(),
        ));

        if opts.debug_names {
            let mut names = NameSection::new();
            names.module("wit-component:fixups");
            names.functions(&self.function_names);
            module.section(&names);
        }

        Some(module)
    }
}
