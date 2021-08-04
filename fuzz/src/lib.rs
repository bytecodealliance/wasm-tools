use libfuzzer_sys::arbitrary::{Result, Unstructured};
use wasm_smith::{ConfiguredModule, SwarmConfig};

pub fn generate_valid_module(input: &[u8]) -> Result<(Vec<u8>, SwarmConfig)> {
    drop(env_logger::try_init());

    let mut u = Unstructured::new(input);
    let mut config: SwarmConfig = u.arbitrary()?;

    // These are disabled in the swarm config by default, but we want to test
    // them. Use the input data to determine whether these features are enabled.
    config.simd_enabled = u.arbitrary()?;
    config.module_linking_enabled = u.arbitrary()?;
    config.memory64_enabled = u.arbitrary()?;

    // Use wasm-smith to generate an arbitrary module and convert it to wasm
    // bytes.
    let module = ConfiguredModule::new(config.clone(), &mut u)?;
    let bytes = module.to_bytes();

    // Optionally log the module and its configuration if we've gotten this
    // far. Note that we don't do this unconditionally to avoid slowing down
    // fuzzing, but this is expected to be enabled when debugging a failing
    // fuzzer.
    if log::log_enabled!(log::Level::Debug) {
        std::fs::write("test.wasm", &bytes).unwrap();
        std::fs::write("test.config", format!("{:#?}", config)).unwrap();
        if let Ok(wat) = wasmprinter::print_bytes(&bytes) {
            std::fs::write("test.wat", wat).unwrap();
        }
    }

    Ok((module.to_bytes(), config))
}
