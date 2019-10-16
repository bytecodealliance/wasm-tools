#![no_main]

use libfuzzer_sys::*;
use std::process::Command;
use std::str;

fuzz_target!(|data: &[u8]| {
    let s = match str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };
    let binary = match wast::parse_str(s) {
        Ok(bytes) => bytes,
        Err(_) => return,
    };

    // FIXME(#8) needs some more work to enable
    if true {
        return;
    }

    let td = tempfile::TempDir::new().unwrap();
    let wat = td.path().join("foo.wat");
    let wasm = td.path().join("foo.wasm");
    std::fs::write(&wat, &s).unwrap();
    let output = Command::new("wat2wasm")
        .arg(&wat)
        .arg("-o")
        .arg(&wasm)
        .output()
        .unwrap();
    if output.status.success() {
        let wabt_bytes = std::fs::read(&wasm).unwrap();
        assert_eq!(binary, wabt_bytes);
    }
});
