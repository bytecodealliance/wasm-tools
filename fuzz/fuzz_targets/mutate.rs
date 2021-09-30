#![no_main]

use libfuzzer_sys::fuzz_target;
use std::sync::atomic::{AtomicU64, Ordering};

static NUM_RUNS: AtomicU64 = AtomicU64::new(0);
static NUM_SUCCESSFUL_MUTATIONS: AtomicU64 = AtomicU64::new(0);

fuzz_target!(|inputs: (wasm_smith::Module, u64)| {
    let _ = env_logger::try_init();

    let old_num_runs = NUM_RUNS.fetch_add(1, Ordering::Relaxed);
    if old_num_runs % 4096 == 0 && log::log_enabled!(log::Level::Info) {
        let successful = NUM_SUCCESSFUL_MUTATIONS.load(Ordering::Relaxed);
        let percent = successful as f64 / old_num_runs as f64 * 100.0;
        log::info!(
            "{} / {} ({:.2}%) successful mutations.",
            successful,
            old_num_runs,
            percent
        );
    }

    let (wasm, seed) = inputs;
    log::debug!("seed = {}", seed);

    let wasm = wasm.to_bytes();
    if log::log_enabled!(log::Level::Debug) {
        log::debug!("writing input Wasm to `test.wasm`");
        std::fs::write("test.wasm", &wasm).expect("should write `test.wasm` okay");
        log::debug!("writing wat disassembly to `test.wat`");
        let wat = wasmprinter::print_bytes(&wasm).expect("should disassemble Wasm okay");
        std::fs::write("test.wat", &wat).expect("should write `test.wat` okay");
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
        Err(e) => {
            log::debug!("failed to mutate the Wasm: {:?}", e);
            return;
        }
    };

    let validation_result = wasmparser::validate(&mutated_wasm);
    log::debug!("validation result = {:?}", validation_result);
    assert!(
        validation_result.is_ok(),
        "`wasm-mutate` should always produce a valid Wasm file"
    );
});
