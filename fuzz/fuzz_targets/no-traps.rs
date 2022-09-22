#![no_main]

use arbitrary::Unstructured;
use libfuzzer_sys::fuzz_target;
use wasmtime::*;

#[cfg(feature = "wasmtime")]
#[path = "../../crates/fuzz-stats/src/dummy.rs"]
pub mod dummy;

// Define a fuzz target that accepts arbitrary
// `Module`s as input.
fuzz_target!(|data: &[u8]| {
    let mut u = Unstructured::new(data);
    let (wasm_bytes, config) = match wasm_tools_fuzz::generate_valid_module(&mut u, |config, _| {
        config.disallow_traps = true;
        config.threads_enabled = false;
        config.allow_start_export = false;
        config.max_memories = config.max_memories.min(1);
        config.memory64_enabled = true;
        config.exceptions_enabled = false;
        Ok(())
    }) {
        Ok(m) => m,
        Err(_) => return,
    };
    // Validate the module or component and assert that it passes validation.
    let mut validator = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
        component_model: false,
        multi_value: config.multi_value_enabled,
        multi_memory: config.max_memories > 1,
        bulk_memory: true,
        reference_types: true,
        simd: config.simd_enabled,
        relaxed_simd: config.relaxed_simd_enabled,
        memory64: config.memory64_enabled,
        threads: config.threads_enabled,
        exceptions: config.exceptions_enabled,
        ..wasmparser::WasmFeatures::default()
    });
    if let Err(e) = validator.validate_all(&wasm_bytes) {
        panic!("Invalid module: {}", e);
    }
    let mut eng_conf = Config::new();
    eng_conf.wasm_memory64(true);
    let engine = Engine::new(&eng_conf).unwrap();
    let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
    let mut store = Store::new(&engine, ());
    let instance = dummy::dummy_imports(&mut store, &module)
        .and_then(|imports| Instance::new(&mut store, &module, &imports))
        .unwrap();
    for export in module.exports() {
        match export.ty() {
            ExternType::Func(func_ty) => {
                let args = dummy::dummy_values(func_ty.params());
                let mut results = dummy::dummy_values(func_ty.results());
                let func = instance.get_func(&mut store, export.name()).unwrap();
                func_ty.results();
                store.add_fuel(1_000).unwrap();
                match func.call(&mut store, &args, &mut results) {
                    Ok(_) => continue,
                    Err(_) => panic!("wasm trapped"),
                }
            }
            _ => continue,
        }
    }
});
