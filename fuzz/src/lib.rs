use libfuzzer_sys::arbitrary::{Result, Unstructured};
use std::fmt::Debug;
use wasm_smith::{Module, SwarmConfig};

pub fn generate_valid_module(
    input: &[u8],
    configure: impl FnOnce(&mut SwarmConfig, &mut Unstructured<'_>) -> Result<()>,
) -> Result<(Vec<u8>, SwarmConfig)> {
    let mut u = Unstructured::new(input);
    let mut config: SwarmConfig = u.arbitrary()?;

    // These are disabled in the swarm config by default, but we want to test
    // them. Use the input data to determine whether these features are enabled.
    config.simd_enabled = u.arbitrary()?;
    config.relaxed_simd_enabled = config.simd_enabled && u.arbitrary()?;
    config.module_linking_enabled = u.arbitrary()?;
    config.memory64_enabled = u.arbitrary()?;
    config.exceptions_enabled = u.arbitrary()?;
    config.canonicalize_nans = u.arbitrary()?;

    configure(&mut config, &mut u)?;

    // Use wasm-smith to generate an arbitrary module and convert it to wasm
    // bytes.
    let module = Module::new(config.clone(), &mut u)?;
    let bytes = module.to_bytes();

    log_wasm(&bytes, &config);

    Ok((bytes, config))
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
