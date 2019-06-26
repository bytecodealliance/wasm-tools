#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate wasmparser;

use wasmparser::WasmDecoder;

fuzz_target!(|data: &[u8]| {
    let mut parser = wasmparser::ValidatingParser::new(data, None);
    loop {
        match *parser.read() {
            wasmparser::ParserState::Error(..) | wasmparser::ParserState::EndWasm => break,
            _ => (),
        }
    }
});
