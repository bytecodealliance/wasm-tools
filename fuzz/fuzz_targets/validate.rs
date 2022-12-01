#![no_main]

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::*;
use wasm_smith::MaybeInvalidModule;
use wasmparser::{Validator, WasmFeatures};

fuzz_target!(|data: &[u8]| {
    drop(env_logger::try_init());
    let byte1 = match data.get(0) {
        Some(byte) => byte,
        None => return,
    };
    let byte2 = match data.get(1) {
        Some(byte) => byte,
        None => return,
    };
    let byte3 = match data.get(2) {
        Some(byte) => byte,
        None => return,
    };
    let mut validator = Validator::new_with_features(WasmFeatures {
        reference_types: (byte1 & 0b0000_0001) != 0,
        multi_value: (byte1 & 0b0000_0010) != 0,
        threads: (byte1 & 0b0000_0100) != 0,
        simd: (byte1 & 0b0000_1000) != 0,
        component_model: (byte1 & 0b0001_0000) != 0,
        tail_call: (byte1 & 0b0010_0000) != 0,
        bulk_memory: (byte1 & 0b0100_0000) != 0,
        floats: (byte1 & 0b1000_0000) != 0,
        multi_memory: (byte2 & 0b0000_0001) != 0,
        memory64: (byte2 & 0b0000_0010) != 0,
        exceptions: (byte2 & 0b0000_0100) != 0,
        relaxed_simd: (byte2 & 0b0000_1000) != 0,
        extended_const: (byte2 & 0b0001_0000) != 0,
        mutable_global: (byte2 & 0b0010_0000) != 0,
        saturating_float_to_int: (byte2 & 0b0100_0000) != 0,
        sign_extension: (byte2 & 0b1000_0000) != 0,
    });
    let use_maybe_invalid = byte3 & 0b0000_0001 != 0;

    let wasm = &data[3..];
    if log::log_enabled!(log::Level::Debug) {
        log::debug!("writing input to `test.wasm`");
        std::fs::write("test.wasm", wasm).unwrap();
    }
    if use_maybe_invalid {
        let mut u = Unstructured::new(wasm);
        if let Ok(module) = MaybeInvalidModule::arbitrary(&mut u) {
            drop(validator.validate_all(&module.to_bytes()));
        }
    } else {
        drop(validator.validate_all(wasm));
    }
});
