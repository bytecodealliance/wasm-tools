//! Same as `binary.rs` fuzzing, but use `wasm-opt` with the input data to
//! generate a wasm program using `-ttf` instead of interpreting `data` as a
//! wasm program.
//!
//! Additionally assert that parsing succeeds since we should be able to parse
//! everything `wasm-opt` generates.

#![no_main]

use libfuzzer_sys::*;
use std::io::{Read, Write};
use std::process::{Command, Stdio};

fuzz_target!(|data: &[u8]| {
    let td = tempfile::TempDir::new().unwrap();
    std::fs::write(td.path().join("input"), data).unwrap();
    let mut cmd = Command::new("wasm-opt")
        .arg("-ttf")
        .arg("--emit-text")
        .arg(td.path().join("input"))
        .arg("-o")
        .arg("-")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    cmd.stdin.take().unwrap().write_all(data).unwrap();
    let mut s = String::new();
    cmd.stdout.take().unwrap().read_to_string(&mut s).unwrap();

    let wat = td.path().join("foo.wat");
    let wasm = td.path().join("foo.wasm");
    std::fs::write(&wat, &s).unwrap();

    let binary = wast::parse_str(&s).unwrap();
    let lexer = wast_parser::lexer::Lexer::new(&s);
    for token in lexer {
        let t = match token.unwrap() {
            wast_parser::lexer::Source::Token(t) => t,
            _ => continue,
        };
        match t {
            wast_parser::lexer::Token::Keyword(k) => {
                if k == "binary" {
                    return;
                }
            }
            wast_parser::lexer::Token::Float(f) => {
                if let wast_parser::lexer::FloatVal::Val { hex: true, .. } = f.val() {
                    return;
                }
            }
            _ => {}
        }
    }

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
        assert_eq!(remove_name_section(&binary), wabt_bytes);
    }
});

fn remove_name_section(bytes: &[u8]) -> Vec<u8> {
    use wasmparser::*;

    if let Ok(mut r) = ModuleReader::new(bytes) {
        loop {
            let start = r.current_position();
            if let Ok(s) = r.read() {
                match s.code {
                    SectionCode::Custom { name: "name", .. } => {
                        let mut bytes = bytes.to_vec();
                        bytes.drain(start..s.range().end);
                        return bytes;
                    }
                    _ => {}
                }
            } else {
                break;
            }
        }
    }
    return bytes.to_vec();
}
