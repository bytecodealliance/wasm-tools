#![no_main]

use libfuzzer_sys::fuzz_target;
use wasm_smith::{Config, ConfiguredModule, SwarmConfig};

fuzz_target!(|m: ConfiguredModule<SwarmConfig>| {
    let bytes = m.to_bytes();

    let mut validator = wasmparser::Validator::new();
    validator.wasm_features(wasmparser::WasmFeatures {
        multi_value: true,
        bulk_memory: m.config().bulk_memory_enabled(),
        multi_memory: m.config().max_memories() > 1,
        ..wasmparser::WasmFeatures::default()
    });
    if let Err(e) = validator.validate_all(&bytes) {
        std::fs::write("test.wasm", bytes).unwrap();
        panic!("Invalid module: {}", e);
    }
});
