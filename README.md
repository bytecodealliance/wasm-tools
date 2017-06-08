# Simple wasm parser in Rust

[![Build Status](https://travis-ci.org/yurydelendik/wasmparser.rs.svg?branch=master)](https://travis-ci.org/yurydelendik/wasmparser.rs)
[![crates.io link](https://img.shields.io/crates/v/wasmparser.svg)](https://crates.io/crates/wasmparser)

See also its sibling at https://github.com/wasdk/wasmparser

## Example

```rust
use wasmparser::Parser;
use wasmparser::ParserState;

fn get_name(bytes: &[u8]) -> &str {
  str::from_utf8(bytes).ok().unwrap()
}

fn main() {
  let ref buf: Vec<u8> = read_wasm_bytes();
  let mut parser = Parser::new(buf);
  loop {
    let state = parser.read();
    match *state {
        ParserState::BeginWasm { .. } => {
            println!("====== Module");
        }
        ParserState::ExportSectionEntry { field, ref kind, .. } => {
            println!("  Export {} {:?}", get_name(field), kind);
        }
        ParserState::ImportSectionEntry { module, field, .. } => {
            println!("  Import {}::{}", get_name(module), get_name(field))
        }
        ParserState::EndWasm => break,
        _ => ( /* println!(" Other {:?}", state) */ )
    }
  }
}
```
