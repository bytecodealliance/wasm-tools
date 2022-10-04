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
        config.exceptions_enabled = false;
        config.max_memory_pages = config.max_memory_pages.min(100);
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
    eng_conf.wasm_multi_memory(true);
    eng_conf.consume_fuel(true);
    let engine = Engine::new(&eng_conf).unwrap();
    let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
    let mut store = Store::new(&engine, ());
    set_fuel(&mut store, 1_000);
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
                set_fuel(&mut store, 1_000);
                match func.call(&mut store, &args, &mut results) {
                    Ok(_) => continue,
                    Err(err) if err.to_string().contains("all fuel consumed") => continue,
                    Err(_) => panic!("generated wasm trapped in non-trapping mode"),
                }
            }
            _ => continue,
        }
    }
});

fn set_fuel<T>(store: &mut Store<T>, fuel: u64) {
    // This is necessary because consume_fuel below will err if there is <=0
    // fuel in the store. Since we are just using that call to get the current
    // amount of fuel AND we are immediately adjusting the fuel to the value we
    // can safely add 1 fuel here as a hacky work-around for the time being.
    store.add_fuel(1).unwrap();
    // Determine the amount of fuel already within the store, if any, and
    // add/consume as appropriate to set the remaining amount to` fuel`.
    let remaining = store.consume_fuel(0).unwrap();
    if fuel > remaining {
        store.add_fuel(fuel - remaining).unwrap();
    } else {
        store.consume_fuel(remaining - fuel).unwrap();
    }
    // double-check that the store has the expected amount of fuel remaining
    assert_eq!(store.consume_fuel(0).unwrap(), fuel);
}
