use arbitrary::{Result, Unstructured};
use wasmparser::{Validator, WasmFeatures};

pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    // Either use `wasm-smith` to generate a module with possibly invalid
    // functions or try validating raw bytes from the input itself.
    if u.arbitrary()? {
        validate_maybe_invalid_module(u)?;
    } else {
        validate_raw_bytes(u)?;
    }
    Ok(())
}

pub fn validate_maybe_invalid_module(u: &mut Unstructured<'_>) -> Result<()> {
    // Generate a "valid" module but specifically allow invalid functions which
    // means that some functions may be defined from the input bytes raw. This
    // means that most of the module is valid but only some functions may be
    // invalid which can help stress various bits and pieces of validation.
    let (wasm, config) = crate::generate_valid_module(u, |config, _| {
        config.allow_invalid_funcs = true;
        Ok(())
    })?;
    let mut validator = crate::validator_for_config(&config);
    drop(validator.validate_all(&wasm));
    Ok(())
}

pub fn validate_raw_bytes(u: &mut Unstructured<'_>) -> Result<()> {
    // Enable arbitrary combinations of features to validate the input bytes.
    let mut validator = Validator::new_with_features(WasmFeatures {
        reference_types: u.arbitrary()?,
        multi_value: u.arbitrary()?,
        threads: u.arbitrary()?,
        shared_everything_threads: u.arbitrary()?,
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
        component_model_nested_names: u.arbitrary()?,
    });
    let wasm = u.bytes(u.len())?;
    crate::log_wasm(wasm, "");
    drop(validator.validate_all(wasm));
    Ok(())
}
