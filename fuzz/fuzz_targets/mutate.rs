#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|inputs: (wasm_smith::Module, u64)| {
    let _ = env_logger::try_init();

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
        Ok(w) => w,
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
