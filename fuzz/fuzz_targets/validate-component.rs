#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let (bytes, config) = match wasm_tools_fuzz::generate_valid_component(data, |_, _| Ok(())) {
        Ok(c) => c,
        Err(_) => return,
    };

    // Validate the component and assert that it passes validation.
    let mut validator = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
        component_model: true,
        multi_value: config.multi_value_enabled,
        multi_memory: config.max_memories > 1,
        bulk_memory: true,
        reference_types: true,
        simd: config.simd_enabled,
        relaxed_simd: config.relaxed_simd_enabled,
        memory64: config.memory64_enabled,
        exceptions: config.exceptions_enabled,
        ..wasmparser::WasmFeatures::default()
    });
    if let Err(e) = validator.validate_all(&bytes) {
        panic!("Invalid component: {}", e);
    }
});
