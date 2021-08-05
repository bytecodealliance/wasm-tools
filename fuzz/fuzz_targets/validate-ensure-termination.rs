#![no_main]

use libfuzzer_sys::fuzz_target;
use wasm_smith::Module;

fuzz_target!(|m: Module| {
    let mut m = m;
    m.ensure_termination(100);
    let bytes = m.to_bytes();
    wasm_tools_fuzz::log_wasm(&bytes, ());

    let mut validator = wasmparser::Validator::new();
    if let Err(e) = validator.validate_all(&bytes) {
        panic!("Invalid module: {}", e);
    }
});
