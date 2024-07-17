//! Test that NaN bitpatterns are not propagated through Wave values.
//!
//! The component-model floating-point types only have a single NaN value, to
//! make it easier to exchange values with source languages and protocols where
//! there is only one NaN value. To help users avoid depending on NaN bits being
//! propagated, we canonicalize NaNs.

use std::{f32, f64};

use wasm_wave::wasm::WasmValue;

#[test]
fn nan() {
    for bits in [
        0,
        i32::MIN as u32,
        1.0_f32.to_bits(),
        (-f32::consts::TAU).to_bits(),
        0xffffffff,
        0x7fff0f0f,
        0x8f800000,
        f32::NAN.to_bits(),
    ] {
        let val = f32::from_bits(bits);
        let expected = if val.is_nan() { 0x7fc00000 } else { bits };

        {
            use wasm_wave::value::Value;
            assert_eq!(
                Value::make_float32(val).unwrap_float32().to_bits(),
                expected
            );
        }
    }

    for bits in [
        0,
        i64::MIN as u64,
        1.0_f64.to_bits(),
        (-f64::consts::TAU).to_bits(),
        0xffffffffffffffff,
        0x7fff0f0f0f0f0f0f,
        0x8ff0000000000000,
        f64::NAN.to_bits(),
    ] {
        let val = f64::from_bits(bits);
        let expected = if val.is_nan() {
            0x7ff8000000000000
        } else {
            bits
        };

        {
            use wasm_wave::value::Value;
            assert_eq!(
                Value::make_float64(val).unwrap_float64().to_bits(),
                expected
            );
        }
    }
}
