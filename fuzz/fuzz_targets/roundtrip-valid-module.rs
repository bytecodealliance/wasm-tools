#![no_main]

use libfuzzer_sys::fuzz_target;
use wasm_smith::Module;

fuzz_target!(|module: Module| {
    let bytes = module.to_bytes();

    let wat_string = wasmprinter::print_bytes(&bytes).unwrap_or_else(|e| {
        fail(
            &bytes,
            &e,
            "failed first disassembly of Wasm into wat with `wasmprinter::print_bytes`",
        )
    });
    let wasm_bytes = wat::parse_str(&wat_string).unwrap_or_else(|e| {
        fail(
            &bytes,
            &e,
            "failed to assemble wat into Wasm with `wat::parse_str`",
        )
    });
    let wat_string2 = wasmprinter::print_bytes(&wasm_bytes).unwrap_or_else(|e| {
        fail(
            &bytes,
            &e,
            "failed second disassembly of Wasm into wat with `wasmprinter::print_bytes`",
        )
    });

    if wat_string != wat_string2 {
        fail(
            &bytes,
            &"first and second disassembly is not equal",
            "failed to roundtrip valid module",
        );
    }
});

fn fail(bytes: &[u8], error: &impl std::fmt::Display, msg: &str) -> ! {
    eprintln!("Writing test case to `test.wasm`.");
    std::fs::write("test.wasm", bytes).unwrap();
    panic!("{}: {}", msg, error)
}
