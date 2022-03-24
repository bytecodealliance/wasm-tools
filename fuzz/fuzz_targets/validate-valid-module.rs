#![no_main]

use libfuzzer_sys::fuzz_target;

// Define a fuzz target that accepts arbitrary
// `Module`s as input.
fuzz_target!(|m: &[u8]| {
    let (bytes, config) = match wasm_tools_fuzz::generate_valid_module(m, |_, _| Ok(())) {
        Ok(m) => m,
        Err(_) => return,
    };

    // Validate the module and assert that it passes
    // validation.
    let mut validator = wasmparser::Validator::new();
    validator.wasm_features(wasmparser::WasmFeatures {
        multi_value: config.multi_value_enabled,
        multi_memory: config.max_memories > 1,
        bulk_memory: true,
        reference_types: true,
        component_model: false,
        simd: config.simd_enabled,
        relaxed_simd: config.relaxed_simd_enabled,
        memory64: config.memory64_enabled,
        exceptions: config.exceptions_enabled,
        ..wasmparser::WasmFeatures::default()
    });
    if let Err(e) = validator.validate_all(&bytes) {
        panic!("Invalid module: {}", e);
    }
});
