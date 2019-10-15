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
    let mut wat = match wast::parser::parse::<wast::ast::Wat>(&buf) {
        Ok(m) => m,
        Err(_) => return,
    };

    match wast::resolve::resolve(&mut wat.module) {
        Ok(()) => (),
        Err(_) => return,
    }
    wast::binary::encode(&wat.module);
});
