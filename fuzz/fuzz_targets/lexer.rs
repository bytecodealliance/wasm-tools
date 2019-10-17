#![no_main]

use libfuzzer_sys::*;
use std::str;

fuzz_target!(|data: &[u8]| {
    let s = match str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };
    let mut l = wast::lexer::Lexer::new(s);
    while let Ok(Some(_)) = l.parse() {
        // ...
    }
});
