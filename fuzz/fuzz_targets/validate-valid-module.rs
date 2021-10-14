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
        multi_value: true,
        multi_memory: config.max_memories > 1,
        bulk_memory: true,
        reference_types: true,
        module_linking: config.module_linking_enabled,
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
