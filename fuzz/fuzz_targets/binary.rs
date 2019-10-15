#![no_main]

use libfuzzer_sys::*;
use std::str;

fuzz_target!(|data: &[u8]| {
    match str::from_utf8(data) {
        Ok(s) => drop(wast::parse_str(s)),
        Err(_) => {}
    }
});
