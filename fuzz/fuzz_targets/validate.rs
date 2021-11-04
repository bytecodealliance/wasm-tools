#![no_main]

use libfuzzer_sys::*;
use wasmparser::{Validator, WasmFeatures};

fuzz_target!(|data: &[u8]| {
    let mut validator = Validator::new();
    let byte1 = match data.get(0) {
        Some(byte) => byte,
        None => return,
    };
    let byte2 = match data.get(1) {
        Some(byte) => byte,
        None => return,
    };
    validator.wasm_features(WasmFeatures {
        reference_types: (byte1 & 0b0000_0001) != 0,
        multi_value: (byte1 & 0b0000_0010) != 0,
        threads: (byte1 & 0b0000_0100) != 0,
        simd: (byte1 & 0b0000_1000) != 0,
        module_linking: (byte1 & 0b0001_0000) != 0,
        tail_call: (byte1 & 0b0010_0000) != 0,
        bulk_memory: (byte1 & 0b0100_0000) != 0,
        deterministic_only: (byte1 & 0b1000_0000) != 0,
        multi_memory: (byte2 & 0b0000_0001) != 0,
        memory64: (byte2 & 0b0000_0010) != 0,
        exceptions: (byte2 & 0b0000_0100) != 0,
        relaxed_simd: (byte2 & 0b0000_1000) != 0,
        extended_const: (byte2 & 0b0001_0000) != 0,
    });

    drop(validator.validate_all(&data[2..]));
});
