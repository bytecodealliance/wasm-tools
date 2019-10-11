#![no_main]

use std::str;
use libfuzzer_sys::*;

fuzz_target!(|data: &[u8]| {
    let s = match str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };
    let mut l = wart::Lexer::new(s);
    while let Ok(_) = l.next() {
        // ...
    }
});
