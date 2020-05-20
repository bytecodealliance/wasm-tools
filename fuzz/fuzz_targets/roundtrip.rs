#![no_main]

use libfuzzer_sys::*;
use std::str;

fuzz_target!(|data: &[u8]| {
    let string = match str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };
    if string.contains("binary") {
        return;
    }
    let wasm = match wat::parse_str(string) {
        Ok(bytes) => bytes,
        Err(_) => return,
    };
    if wasmparser::validate(&wasm, None).is_err() {
        return;
    }
    let string = match wasmprinter::print_bytes(&wasm) {
        Ok(s) => s,
        Err(_) => return,
    };
    assert_eq!(wasm, wat::parse_str(&string).unwrap());
});
