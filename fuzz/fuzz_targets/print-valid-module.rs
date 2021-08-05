#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|bytes: &[u8]| {
    let (bytes, _config) = match wasm_tools_fuzz::generate_valid_module(bytes, |_, _| Ok(())) {
        Ok(m) => m,
        Err(_) => return,
    };

    // Print the Wasm module.
    if let Err(e) = wasmprinter::print_bytes(&bytes) {
        panic!("Failed to print valid module: {}", e);
    }
});
