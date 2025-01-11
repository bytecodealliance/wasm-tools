#![no_main]

use libfuzzer_sys::*;
use std::str;

fuzz_target!(|data: &[u8]| {
    let s = match str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };
    let buf = match wast::parser::ParseBuffer::new(s) {
        Ok(b) => b,
        Err(_) => return,
    };
    drop(wast::parser::parse::<wast::Wast>(&buf));
});
