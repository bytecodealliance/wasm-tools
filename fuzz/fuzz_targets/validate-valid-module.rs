#![no_main]

use arbitrary::Unstructured;
use libfuzzer_sys::fuzz_target;

// Define a fuzz target that accepts arbitrary
// `Module`s or `Component`s as input.
fuzz_target!(|data: &[u8]| {
    let mut u = Unstructured::new(data);

    // We want to prioritize fuzzing of modules for the time being
    // so we'll only generate a component 10% of the time
    let generate_component = match u.ratio::<u8>(1, 10) {
        Ok(b) => b,
        Err(_) => false,
    };
    let (wasm_bytes, config) = if generate_component {
        match wasm_tools_fuzz::generate_valid_component(&mut u, |c, u| {
            c.max_components = u.int_in_range(0..=1_000)?;
            c.max_instances = u.int_in_range(0..=1_000)?;
            c.max_values = u.int_in_range(0..=1_000)?;
            Ok(())
        }) {
            Ok(c) => c,
            Err(_) => return,
        }
    } else {
        match wasm_tools_fuzz::generate_valid_module(&mut u, |_, _| Ok(())) {
            Ok(m) => m,
            Err(_) => return,
        }
    };

    // Validate the module or component and assert that it passes validation.
    let mut validator = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
        component_model: generate_component,
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
        let component_or_module = if generate_component {
            "component"
        } else {
            "module"
        };
        panic!("Invalid {}: {}", component_or_module, e);
    }
});
