#![no_main]

use libfuzzer_sys::fuzz_target;
use wasm_smith::{ConfiguredModule, SwarmConfig};

// Define a fuzz target that accepts arbitrary
// `Module`s as input.
fuzz_target!(|m: ConfiguredModule<SwarmConfig>| {
    // Convert the module into Wasm bytes.
    let bytes = m.to_bytes();

    // Print the Wasm module.
    if let Err(e) = wasmprinter::print_bytes(&bytes) {
        std::fs::write("test.wasm", bytes).unwrap();
        panic!("Failed to print valid module: {}", e);
    }
});
