use arbitrary::{Arbitrary, Result, Unstructured};
use wasm_smith::MaybeInvalidModule;
use wasmparser::{Validator, WasmFeatures};

pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    let mut validator = Validator::new_with_features(WasmFeatures {
        reference_types: u.arbitrary()?,
        multi_value: u.arbitrary()?,
        threads: u.arbitrary()?,
        simd: u.arbitrary()?,
        component_model: u.arbitrary()?,
        tail_call: u.arbitrary()?,
        bulk_memory: u.arbitrary()?,
        floats: u.arbitrary()?,
        multi_memory: u.arbitrary()?,
        memory64: u.arbitrary()?,
        exceptions: u.arbitrary()?,
        relaxed_simd: u.arbitrary()?,
        extended_const: u.arbitrary()?,
        mutable_global: u.arbitrary()?,
        saturating_float_to_int: u.arbitrary()?,
        sign_extension: u.arbitrary()?,
        memory_control: u.arbitrary()?,
        function_references: u.arbitrary()?,
        gc: u.arbitrary()?,
        component_model_values: u.arbitrary()?,
    });
    let use_maybe_invalid = u.arbitrary()?;

    if use_maybe_invalid {
        if let Ok(module) = MaybeInvalidModule::arbitrary(u) {
            let wasm = module.to_bytes();
            crate::log_wasm(&wasm, "");
            drop(validator.validate_all(&wasm));
        }
    } else {
        let wasm = u.bytes(u.len())?;
        crate::log_wasm(wasm, "");
        drop(validator.validate_all(wasm));
    }
    Ok(())
}
