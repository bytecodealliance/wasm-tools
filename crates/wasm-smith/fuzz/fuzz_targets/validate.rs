#![no_main]

use libfuzzer_sys::fuzz_target;
use wasm_smith::Module;

fuzz_target!(|m: Module| {
    let bytes = m.to_bytes();

    let mut validator = wasmparser::Validator::new();
    validator.wasm_features(wasmparser::WasmFeatures {
        multi_value: true,
        ..wasmparser::WasmFeatures::default()
    });
    if let Err(e) = validator.validate_all(&bytes) {
        std::fs::write("test.wasm", bytes).unwrap();
        panic!("Invalid module: {}", e);
    }
});
