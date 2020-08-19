fn main() {
    let mut validator = wasmparser::Validator::new();
    validator.wasm_features(wasmparser::WasmFeatures {
        multi_value: true,
        ..wasmparser::WasmFeatures::default()
    });

    let bytes = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();
    if let Err(e) = validator.validate_all(&bytes) {
        eprintln!("error: {}", e);
    }
}
