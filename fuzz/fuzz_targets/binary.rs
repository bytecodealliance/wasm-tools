#![no_main]

use libfuzzer_sys::*;
use std::process::Command;
use std::str;

fuzz_target!(|data: &[u8]| {
    let s = match str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    let td = tempfile::TempDir::new().unwrap();
    let wat = td.path().join("foo.wat");
    let wasm = td.path().join("foo.wasm");
    std::fs::write(&wat, &s).unwrap();

    match wat::parse_str(s) {
        // If we succesfully parsed the binary, then we want to make sure that
        // `wabt` agrees on the binary encoding of the input wat file.
        Ok(binary) => {
            if wast_fuzz::wabt_may_disagree_on_binary(s) {
                return;
            }

            // Wabt looks to have lots of various subtle issues around non-MVP
            // features. For now lets just fuzz MVP features by avoiding passing
            // `--enable-all`, and then only if `wat2wasm` succeeds to we
            // compare the binary.
            let output = Command::new("wat2wasm")
                .arg(&wat)
                .arg("-o")
                .arg(&wasm)
                .output()
                .unwrap();
            if output.status.success() {
                let wabt_bytes = std::fs::read(&wasm).unwrap();
                // see comments in the test suite for why we remove the name
                // section
                assert_eq!(wast_fuzz::remove_name_section(&binary), wabt_bytes);
            }
        }

        // If we failed to parse the input wasm, then `wat2wasm` from `wabt`
        // should fail as well. Only enable implemented proposals though to
        // avoid where we fail to parse something because it's not supported but
        // wat2wasm supports it.
        Err(_) => {
            let output = Command::new("wat2wasm")
                .arg(&wat)
                .arg("--no-check")
                .arg("--enable-sign-extension")
                .arg("--enable-threads")
                .arg("--enable-multi-value")
                .arg("--enable-refrence-types")
                .arg("--enable-saturating-float-to-int")
                .arg("-o")
                .arg(&wasm)
                .output()
                .unwrap();
            assert!(!output.status.success());
        }
    }
});
