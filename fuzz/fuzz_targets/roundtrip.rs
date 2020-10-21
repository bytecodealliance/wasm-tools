#![no_main]

use libfuzzer_sys::*;
use std::str;

fuzz_target!(|data: &[u8]| {
    let string = match str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };
    // Weed out `(module binary ...)` because when we print the bytes and
    // convert it back to binary it's not guaranteed to be exactly the same.
    // (think of something like an over-long LEB encoding)
    if string.contains("binary") {
        return;
    }

    // Also weed out `@custom` custom sections since we don't print those right
    // now.
    if string.contains("@custom") {
        return;
    }
    let wasm = match wat::parse_str(string) {
        Ok(bytes) => bytes,
        Err(_) => return,
    };
    if wasmparser::validate(&wasm).is_err() {
        return;
    }
    let string = match wasmprinter::print_bytes(&wasm) {
        Ok(s) => s,
        Err(_) => return,
    };
    assert_eq!(wasm, wat::parse_str(&string).unwrap());
});
