#![no_main]

use libfuzzer_sys::*;
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
