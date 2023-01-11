#![no_main]

use arbitrary::Unstructured;
use libfuzzer_sys::fuzz_target;
use wasm_smith::SwarmConfig;
#[cfg(feature = "wasmtime")]
use wasmtime::*;

#[cfg(feature = "wasmtime")]
#[path = "../../crates/fuzz-stats/src/lib.rs"]
pub mod fuzz_stats;

// Define a fuzz target that accepts arbitrary
// `Module`s as input.
fuzz_target!(|data: &[u8]| {
    // Use data to generate a random wasm module
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
    validate_module(config, &wasm_bytes);

    #[cfg(feature = "wasmtime")]
    {
        // Configure the engine, module, and store
        let mut eng_conf = Config::new();
        eng_conf.wasm_memory64(true);
        eng_conf.wasm_multi_memory(true);
        eng_conf.consume_fuel(true);
        let engine = Engine::new(&eng_conf).unwrap();
        let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
        let mut store = Store::new(
            &engine,
            fuzz_stats::limits::StoreLimits {
                remaining_memory: 1 << 30,
                oom: false,
            },
        );
        store.limiter(|s| s as &mut dyn ResourceLimiter);
        set_fuel(&mut store, 1_000);

        // Instantiate the module
        let inst_result = fuzz_stats::dummy::dummy_imports(&mut store, &module)
            .and_then(|imports| Instance::new(&mut store, &module, &imports));
        let instance = match inst_result {
            Ok(r) => r,
            Err(err) => return check_err(err),
        };

        // Call all exported functions
        for export in module.exports() {
            match export.ty() {
                ExternType::Func(func_ty) => {
                    let args = fuzz_stats::dummy::dummy_values(func_ty.params());
                    let mut results = fuzz_stats::dummy::dummy_values(func_ty.results());
                    let func = instance.get_func(&mut store, export.name()).unwrap();
                    set_fuel(&mut store, 1_000);
                    match func.call(&mut store, &args, &mut results) {
                        Ok(_) => {}
                        Err(err) => check_err(err),
                    }
                }
                _ => continue,
            }
        }

        fn check_err(err: anyhow::Error) {
            // Allow stack overflow since this generally can't be protected
            // against as it's an implementation detail of cranelift we could
            // expose regardless of the limits placed on the function.
            if let Some(wasmtime::Trap::StackOverflow) = err.downcast_ref::<wasmtime::Trap>() {
                return;
            }

            // Allow out of fuel on module instantiation
            if let Some(wasmtime::Trap::OutOfFuel) = err.downcast_ref::<wasmtime::Trap>() {
                return;
            }

            let s = err.to_string();
            // Allow "nominal" traps such as running out of fuel and the
            // module trying to allocate more resources than we'd like to
            // allow it (e.g. lots of memories or lots of tables).
            if s.contains("all fuel consumed") || s.contains("Insufficient resources") {
                return;
            }

            // Otherwise though this is a bug.
            panic!("generated wasm trapped in non-trapping mode: {}", err)
        }
    }
});

fn validate_module(config: SwarmConfig, wasm_bytes: &Vec<u8>) {
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
    if let Err(e) = validator.validate_all(wasm_bytes) {
        panic!("Invalid module: {}", e);
    }
}

#[cfg(feature = "wasmtime")]
fn set_fuel<T>(store: &mut Store<T>, fuel: u64) {
    // This is necessary because consume_fuel below will err if there is <=0
    // fuel in the store. Since we are just using that call to get the current
    // amount of fuel AND we are immediately adjusting the fuel to the value we
    // can safely add 1 fuel here as a hacky work-around for the time being.
    store.add_fuel(1_000).unwrap();
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
