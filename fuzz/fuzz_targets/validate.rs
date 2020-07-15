#![no_main]

use libfuzzer_sys::*;
use wasmparser::Validator;

fuzz_target!(|data: &[u8]| {
    let mut validator = Validator::new();
    let byte = match data.get(0) {
        Some(byte) => byte,
        None => return,
    };
    validator.wasm_reference_types((byte & 0b0000_0001) != 0);
    validator.wasm_multi_value((byte & 0b0000_0010) != 0);
    validator.wasm_threads((byte & 0b0000_0100) != 0);
    validator.wasm_simd((byte & 0b0000_1000) != 0);
    validator.wasm_module_linking((byte & 0b0001_0000) != 0);
    validator.wasm_tail_call((byte & 0b0010_0000) != 0);
    validator.wasm_bulk_memory((byte & 0b0100_0000) != 0);
    validator.deterministic_only((byte & 0b1000_0000) != 0);

    drop(validator.validate_all(&data[1..]));
});
