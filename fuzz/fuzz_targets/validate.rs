#![no_main]

use libfuzzer_sys::*;
use wasmparser::{Validator, WasmFeatures};

fuzz_target!(|data: &[u8]| {
    let mut validator = Validator::new();
    let byte = match data.get(0) {
        Some(byte) => byte,
        None => return,
    };
    validator.wasm_features(WasmFeatures {
        reference_types: (byte & 0b0000_0001) != 0,
        multi_value: (byte & 0b0000_0010) != 0,
        threads: (byte & 0b0000_0100) != 0,
        simd: (byte & 0b0000_1000) != 0,
        module_linking: (byte & 0b0001_0000) != 0,
        tail_call: (byte & 0b0010_0000) != 0,
        bulk_memory: (byte & 0b0100_0000) != 0,
        deterministic_only: (byte & 0b1000_0000) != 0,
    });

    drop(validator.validate_all(&data[1..]));
});
