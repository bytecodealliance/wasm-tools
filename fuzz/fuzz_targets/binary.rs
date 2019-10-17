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

    match wast::parse_str(s) {
        // If we succesfully parsed the binary, then we want to make sure that
        // `wabt` agrees on the binary encoding of the input wat file. There's a
        // few caveats though:
        //
        // * We ignore `(module binary ...)` modules since wabt postprocesses
        //   those but we don't
        //
        // * We ignore anything with a hex float literal. Our parsing is
        //   different than wabt's (#13) and it's not super interesting for us
        //   to fuzz those differences right now anyway.
        //
        // * Wabt looks to have lots of various subtle issues around non-MVP
        //   features. For now lets just fuzz MVP features by avoiding passing
        //   `--enable-all`, and then only if `wat2wasm` succeeds to we compare
        //   the binary.
        //
        // Ideally we'd just unconditionally pass everything to `wat2wasm` and
        // verify that it's the same, but for now that's producing a lot of
        // unintersting failures.
        Ok(binary) => {
            let lexer = wast_parser::lexer::Lexer::new(s);
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
        }

        // If we failed to parse the input wasm, then `wat2wasm` from `wabt`
        // should fail as well.
        Err(_) => {
            let output = Command::new("wat2wasm")
                .arg(&wat)
                .arg("--no-check")
                .arg("--enable-all")
                .arg("-o")
                .arg(&wasm)
                .output()
                .unwrap();
            assert!(!output.status.success());
        }
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
