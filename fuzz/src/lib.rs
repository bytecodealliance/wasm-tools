use libfuzzer_sys::arbitrary::{Result, Unstructured};
use std::fmt::Debug;
use wasm_smith::{Component, Config, Module};

pub mod incremental_parse;
pub mod mutate;
pub mod no_traps;
pub mod print;
pub mod roundtrip;
pub mod roundtrip_wit;
pub mod text_parser;
pub mod validate;
pub mod validate_valid_module;

pub fn generate_valid_module(
    u: &mut Unstructured,
    configure: impl FnOnce(&mut Config, &mut Unstructured<'_>) -> Result<()>,
) -> Result<(Vec<u8>, Config)> {
    let mut config: Config = u.arbitrary()?;

    // These are disabled in the swarm config by default, but we want to test
    // them. Use the input data to determine whether these features are enabled.
    config.simd_enabled = u.arbitrary()?;
    config.relaxed_simd_enabled = config.simd_enabled && u.arbitrary()?;
    config.memory64_enabled = u.arbitrary()?;
    config.threads_enabled = u.arbitrary()?;
    config.exceptions_enabled = u.arbitrary()?;
    config.canonicalize_nans = u.arbitrary()?;
    config.tail_call_enabled = u.arbitrary()?;

    config.gc_enabled = u.arbitrary()?;
    config.reference_types_enabled = config.reference_types_enabled || config.gc_enabled;

    configure(&mut config, u)?;

    // Use wasm-smith to generate an arbitrary module and convert it to wasm
    // bytes.
    let mut module = Module::new(config.clone(), u)?;
    let bytes = module.to_bytes();

    // 10% of the time, ish, test that the `ensure_termination` method will
    // still produce a valid module.
    if u.ratio(1, 10)? {
        log::debug!("ensuring termination with 100 fuel");
        let _ = module.ensure_termination(100);
    }

    log_wasm(&bytes, &config);

    Ok((bytes, config))
}

pub fn generate_valid_component(
    u: &mut Unstructured,
    configure: impl FnOnce(&mut Config, &mut Unstructured<'_>) -> Result<()>,
) -> Result<(Vec<u8>, Config)> {
    let mut config: Config = u.arbitrary()?;

    // These are disabled in the swarm config by default, but we want to test
    // them. Use the input data to determine whether these features are enabled.
    config.simd_enabled = u.arbitrary()?;
    config.relaxed_simd_enabled = config.simd_enabled && u.arbitrary()?;
    config.memory64_enabled = u.arbitrary()?;
    config.exceptions_enabled = u.arbitrary()?;
    config.canonicalize_nans = u.arbitrary()?;

    configure(&mut config, u)?;

    // Use wasm-smith to generate an arbitrary component and convert it to wasm
    // bytes.
    let component = Component::new(config.clone(), u)?;
    let bytes = component.to_bytes();

    log_wasm(&bytes, &config);

    Ok((bytes, config))
}

pub fn validator_for_config(config: &Config) -> wasmparser::Validator {
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
        multi_value: config.multi_value_enabled,
        multi_memory: config.max_memories > 1,
        bulk_memory: config.bulk_memory_enabled,
        reference_types: config.reference_types_enabled,
        simd: config.simd_enabled,
        relaxed_simd: config.relaxed_simd_enabled,
        memory64: config.memory64_enabled,
        threads: config.threads_enabled,
        exceptions: config.exceptions_enabled,
        // TODO: determine our larger story for function-references in
        // wasm-tools and whether we should just have a Wasm GC flag since
        // function-references is effectively part of the Wasm GC proposal at
        // this point.
        function_references: config.gc_enabled,
        gc: config.gc_enabled,
        ..wasmparser::WasmFeatures::default()
    })
}

// Optionally log the module and its configuration if we've gotten this
// far. Note that we don't do this unconditionally to avoid slowing down
// fuzzing, but this is expected to be enabled when debugging a failing
// fuzzer.
pub fn log_wasm(wasm: &[u8], config: impl Debug) {
    drop(env_logger::try_init());

    if log::log_enabled!(log::Level::Debug) {
        log::debug!("writing test case to `test.wasm` ...");
        std::fs::write("test.wasm", wasm).unwrap();
        std::fs::write("test.config", format!("{:#?}", config)).unwrap();
        if let Ok(wat) = wasmprinter::print_bytes(wasm) {
            log::debug!("writing text format to `test.wat` ...");
            std::fs::write("test.wat", wat).unwrap();
        } else {
            drop(std::fs::remove_file("test.wat"));
        }
    }
}
