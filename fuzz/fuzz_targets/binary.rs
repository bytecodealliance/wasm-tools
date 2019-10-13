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
    let mut wat = match buf.parser().parse::<wast::ast::Wat>() {
        Ok(m) => m,
        Err(_) => return,
    };

    // make sure we parsed all the tokens
    if !buf.parser().is_empty() {
        return;
    }

    // ignore `quote` modules since they're only there for the test suite in
    // wabt to parse anyway
    if let wast::ast::ModuleKind::Quote(_) = wat.module.kind {
        return;
    }

    match wast::resolve::resolve(&mut wat.module) {
        Ok(()) => (),
        Err(_) => return,
    }
    wast::binary::encode(&wat.module);
});
