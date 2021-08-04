#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|bytes: &[u8]| {
    let (bytes, _config) = match wasm_tools_fuzz::generate_valid_module(bytes) {
        Ok(m) => m,
        Err(_) => return,
    };

    let wat_string = wasmprinter::print_bytes(&bytes).unwrap_or_else(|e| {
        panic!(
            "failed first disassembly of Wasm into wat with `wasmprinter::print_bytes`: {}",
            e
        )
    });
    let wasm_bytes = wat::parse_str(&wat_string).unwrap_or_else(|e| {
        panic!(
            "failed to assemble wat into Wasm with `wat::parse_str`: {}",
            e
        )
    });
    let wat_string2 = wasmprinter::print_bytes(&wasm_bytes).unwrap_or_else(|e| {
        panic!(
            "failed second disassembly of Wasm into wat with `wasmprinter::print_bytes`: {}",
            e
        )
    });

    if wat_string != wat_string2 {
        panic!("failed to roundtrip valid module");
    }
});
