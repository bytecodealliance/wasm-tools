extern crate wasmparser;

use std::io;
use std::io::prelude::*;
use std::fs::File;
use std::str;
use wasmparser::Parser;
use wasmparser::ParserState;

fn get_name(bytes: &[u8]) -> &str {
    str::from_utf8(bytes).ok().unwrap()
}

fn main() {
    let ref buf: Vec<u8> = read_wasm().unwrap();
    let mut parser = Parser::new(buf);
    loop {
        let state = parser.read();
        if state.is_none() {
            break;
        }
        match *state.unwrap() {
            ParserState::BeginWasm(_, _) => {
                println!("====== Module");
            }
            ParserState::ExportSectionEntry(field, ref ty, _) => {
                println!("  Export {} {:?}", get_name(field), ty);
            }
            ParserState::ImportSectionEntry(module, field, _) => {
                println!("  Import {}::{}", get_name(module), get_name(field))
            }
            _ => ( /* println!(" Other {:?}", state); */ ),
        }
    }
}

fn read_wasm() -> io::Result<Vec<u8>> {
    let mut data = Vec::new();
    let mut f = File::open("tests/spec.wasm")?;
    f.read_to_end(&mut data)?;
    Ok(data)
}
