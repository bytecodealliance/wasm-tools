#![no_main]

use libfuzzer_sys::fuzz_target;
use std::sync::atomic::{AtomicU64, Ordering};
use wasmparser::WasmFeatures;

static NUM_RUNS: AtomicU64 = AtomicU64::new(0);
static NUM_SUCCESSFUL_MUTATIONS: AtomicU64 = AtomicU64::new(0);

fuzz_target!(|bytes: &[u8]| {
    let _ = env_logger::try_init();

    let mut seed = 0;
    let (wasm, _config) = match wasm_tools_fuzz::generate_valid_module(bytes, |_config, u| {
        seed = u.arbitrary()?;
        Ok(())
    }) {
        Ok(m) => m,
        Err(_) => return,
    };

    log::debug!("seed = {}", seed);

    let old_num_runs = NUM_RUNS.fetch_add(1, Ordering::Relaxed);
    if old_num_runs % 4096 == 4095 && log::log_enabled!(log::Level::Info) {
        let successful = NUM_SUCCESSFUL_MUTATIONS.load(Ordering::Relaxed);
        let percent = successful as f64 / old_num_runs as f64 * 100.0;
        log::info!(
            "{} / {} ({:.2}%) successful mutations.",
            successful,
            old_num_runs,
            percent
        );
    }

    let mutated_wasm = wasm_mutate::WasmMutate::default()
        .seed(seed)
        .preserve_semantics(true)
        .run(&wasm);
    let mutated_wasm = match mutated_wasm {
        Ok(w) => {
            NUM_SUCCESSFUL_MUTATIONS.fetch_add(1, Ordering::Relaxed);
            w
        }
        Err(e) => match e {
            wasm_mutate::Error::NoMutationsApplicable => return,
            _ => panic!("Invalid mutation process"),
        },
    };

    if log::log_enabled!(log::Level::Debug) {
        std::fs::write("mutated.wasm", &mutated_wasm).expect("should write `mutated.wasm` okay");
        if let Ok(mutated_wat) = wasmprinter::print_bytes(&mutated_wasm) {
            std::fs::write("mutated.wat", &mutated_wat).expect("should write `mutated.wat` okay");
        }
    }

    let features = WasmFeatures::default();
    let mut validator = wasmparser::Validator::new();
    validator.wasm_features(features);

    let validation_result = validator.validate_all(&mutated_wasm);
    log::debug!("validation result = {:?}", validation_result);
    assert!(
        validation_result.is_ok(),
        "`wasm-mutate` should always produce a valid Wasm file"
    );
});
