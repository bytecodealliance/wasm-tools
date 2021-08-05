#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|bytes: &[u8]| {
    let (bytes, _config) = match wasm_tools_fuzz::generate_valid_module(bytes, |config, _u| {
        // It's a known bug that the textual format for module linking is not
        // round-trip-able. This is because the encoder will sometimes reorder
        // fields before others, but it technically shouldn't do that if module
        // linking is present. This should be fixed with future iterations of
        // the module linking text format.
        config.module_linking_enabled = false;
        Ok(())
    }) {
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
