use rayon::prelude::*;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use wast::ast::*;
use wast::lexer::Lexer;

#[test]
fn lex_wabt() {
    let tests = find_tests();

    tests.par_iter().for_each(|test| {
        let contents = std::fs::read_to_string(test).unwrap();
        if contents.contains(";; ERROR") {
            return;
        }
        let mut cur = contents.as_ptr();
        for token in Lexer::new(&contents) {
            let token = match token {
                Ok(t) => t,
                Err(e) => {
                    panic!("{}", render_error(test, &contents, e.line(), e.col(), &e));
                }
            };
            let source = token.src();
            assert_eq!(
                cur,
                source.as_ptr(),
                "tokenization missed a character before {:?}",
                token
            );
            cur = unsafe { cur.add(source.len()) };
        }
    })
}

fn find_tests() -> Vec<PathBuf> {
    let mut tests = Vec::new();
    if !Path::new("tests/wabt").exists() {
        panic!("submodules need to be checked out");
    }
    find_tests("tests/wabt/test/desugar".as_ref(), &mut tests);
    find_tests("tests/wabt/test/dump".as_ref(), &mut tests);
    find_tests("tests/wabt/test/interp".as_ref(), &mut tests);
    find_tests("tests/wabt/test/parse".as_ref(), &mut tests);
    find_tests("tests/wabt/test/roundtrip".as_ref(), &mut tests);
    find_tests("tests/wabt/test/spec".as_ref(), &mut tests);
    find_tests("tests/wabt/test/typecheck".as_ref(), &mut tests);
    find_tests("tests/wabt/third_party/testsuite".as_ref(), &mut tests);
    tests.sort();
    return tests;

    fn find_tests(path: &Path, tests: &mut Vec<PathBuf>) {
        for f in path.read_dir().unwrap() {
            let f = f.unwrap();
            if f.file_type().unwrap().is_dir() {
                find_tests(&f.path(), tests);
                continue;
            }

            match f.path().extension().and_then(|s| s.to_str()) {
                Some("txt") | Some("wast") => {}
                _ => continue,
            }
            tests.push(f.path());
        }
    }
}

#[test]
fn parse_wabt() {
    let mut tests = find_tests();
    tests.sort_by_key(|k| std::fs::read(&k).map(|b| b.len()).unwrap_or(0));
    tests.reverse();

    let failed = tests
        .iter()
        .filter_map(|test| {
            let contents = std::fs::read_to_string(&test).unwrap();
            if skip_test(&test, &contents) {
                return None;
            }

            // Lex everything into a `ParseBuffer`...
            let buf = match wast::parser::ParseBuffer::new(&contents) {
                Ok(b) => b,
                Err(e) => {
                    return Some(render_error(&test, &contents, e.line(), e.col(), &e));
                }
            };

            // ... then parse as a `*.wast` file, handling `*.txt` vs `*.wast`
            // and various test suite directives in wabt ...
            let wast = contents.contains("TOOL: wast2json")
                || test.display().to_string().ends_with(".wast");
            let result = if wast {
                buf.parser().parse::<Wast>()
            } else {
                buf.parser().parse::<Wat>().map(|wat| Wast {
                    directives: vec![WastDirective::Module(wat.module)],
                })
            };
            let directives = match result {
                Ok(wast) => wast.directives,
                Err(e) => {
                    return Some(render_error(&test, &contents, e.line(), e.col(), &e));
                }
            };

            // ... and ensure that we actually parsed everything ...
            if !buf.parser().is_empty() {
                return Some(format!("failed to parse all of {:?}", test));
            }

            // ... and then test our binary emission/resolution for all modules
            // found, ensuring that it matches wabt's
            let mut modules = 0;
            for directive in directives {
                let mut module = match directive {
                    WastDirective::Module(m) => m,
                    _ => continue,
                };
                match wast::resolve::resolve(&mut module) {
                    Ok(()) => {}
                    Err(e) => {
                        return Some(render_error(&test, &contents, e.line(), e.col(), &e));
                    }
                }

                let actual = wast::binary::encode(&module);
                if let Some(expected) = wat2wasm(&test, modules) {
                    if let Some(msg) = binary_compare(&test, &actual, &expected) {
                        return Some(msg);
                    }
                }

                modules += 1;
            }

            None
        })
        .collect::<Vec<_>>();

    if !failed.is_empty() {
        for msg in failed.iter() {
            println!("{}", msg);
        }

        panic!("{} tests failed", failed.len())
    }
}

fn binary_compare(test: &Path, actual: &[u8], expected: &[u8]) -> Option<String> {
    use wasmparser::*;

    if actual == expected {
        return None;
    }

    let difference = actual.iter().enumerate()
        .zip(expected)
        .find(|((_, actual), expected)| actual != expected);
    let pos = match difference {
        Some(((pos, _), _)) => format!("at byte {} ({0:#x})", pos),
        None => format!("by being too small"),
    };
    let mut msg = format!(
        "
error: actual wasm differs {pos} from expected wasm
      --> {file}
",
        pos = pos,
        file = test.display(),
    );

    if let Some(((pos, _), _)) = difference {
        msg.push_str(&format!("  {:4} |   {:#04x}\n", pos - 2, actual[pos - 2]));
        msg.push_str(&format!("  {:4} |   {:#04x}\n", pos - 1, actual[pos - 1]));
        msg.push_str(&format!("  {:4} | - {:#04x}\n", pos, expected[pos]));
        msg.push_str(&format!("       | + {:#04x}\n", actual[pos]));
    }

    let mut actual_parser = Parser::new(actual);
    let mut expected_parser = Parser::new(expected);

    let mut differences = 0;
    while differences < 5 {
        let actual_state = match read_state(&mut actual_parser) {
            Some(s) => s,
            None => break,
        };
        let expected_state = match read_state(&mut expected_parser) {
            Some(s) => s,
            None => break,
        };

        if actual_state == expected_state {
            if differences > 0 {
                msg.push_str(&format!("       |   ...\n"));
            }
            continue;
        }

        if differences == 0 {
            msg.push_str("\n\n");
        }
        msg.push_str(&format!("       | - {}\n", expected_state));
        msg.push_str(&format!("       | + {}\n", actual_state));
        differences += 1;
    }

    return Some(msg);

    fn read_state<'a, 'b>(parser: &'b mut Parser<'a>) -> Option<String> {
        loop {
            match parser.read() {
                // ParserState::BeginSection { code: SectionCode::DataCount, .. } => {}
                // ParserState::DataCountSectionEntry(_) => {}
                ParserState::Error(_) |
                ParserState::EndWasm => break None,
                other => break Some(format!("{:?}", other)),
            }
        }
    }
}

fn wat2wasm(test: &Path, module: usize) -> Option<Vec<u8>> {
    if test.to_str().unwrap().ends_with(".wast") {
        let td = tempfile::TempDir::new().unwrap();
        let result = Command::new("wast2json")
            .arg(test)
            .arg("--enable-all")
            .arg("--no-check")
            .arg("-o")
            .arg(td.path().join("foo.json"))
            .output()
            .expect("failed to spawn `wat2wasm`");
        if !result.status.success() {
            // TODO: handle this case better
            return None;
        }
        let json = fs::read_to_string(td.path().join("foo.json")).unwrap();
        let json: serde_json::Value = serde_json::from_str(&json).unwrap();
        let commands = json["commands"].as_array().unwrap();
        let module = commands
            .iter()
            .filter_map(|m| {
                if m["type"] == "module" {
                    Some(td.path().join(m["filename"].as_str().unwrap()))
                } else {
                    None
                }
            })
            .skip(module)
            .next()
            .expect("failed to find right module");
        Some(fs::read(module).unwrap())
    } else {
        assert_eq!(module, 0);
        let f = tempfile::NamedTempFile::new().unwrap();
        let result = Command::new("wat2wasm")
            .arg(test)
            .arg("--enable-all")
            .arg("--no-check")
            .arg("-o")
            .arg(f.path())
            .output()
            .expect("failed to spawn `wat2wasm`");
        if result.status.success() {
            Some(fs::read(f.path()).unwrap())
        } else {
            // TODO: handle this case better
            None
        }
    }
}

fn skip_test(test: &Path, contents: &str) -> bool {
    // This is something that doesn't seem worth supporting at this
    // time, `*.wast` files with inline modules (although we do support
    // it for `*.wat` files.
    if test.ends_with("inline-module.wast") {
        return true;
    }

    // This test still uses a bunch of old names and I don't feel like
    // typing them all out at this time, so just skip it. We get some
    // testing from wabt's test suite anyway.
    if test.ends_with("threads/atomic.wast") {
        return true;
    }

    // TODO: need to fix this test, how in the world is `if` supposed to
    // be parsed anyway?
    if test.ends_with("dump/br-loop-inner.txt") {
        return true;
    }

    // Skip tests that are supposed to fail
    if contents.contains(";; ERROR") {
        return true;
    }
    // Tests that have a different input
    if contents.contains("STDIN_FILE") {
        return true;
    }
    // Some exception-handling tests don't use `--enable-exceptions` since
    // `run-objdump` enables everything
    if contents.contains("run-objdump") && contents.contains("(event") {
        return true;
    }
    // contains weird stuff at the end of the file we don't parse
    if contents.contains("TOOL: run-objdump-spec") {
        return true;
    }

    // Skip tests that exercise unimplemented proposals
    if contents.contains("--enable-exceptions") {
        return true;
    }
    if contents.contains("--enable-all") {
        return true;
    }
    if contents.contains("--enable-annotations") {
        return true;
    }
    if contents.contains("--enable-simd") {
        return true;
    }
    if contents.contains("--enable-tail-call") {
        return true;
    }
    false
}

fn render_error(
    file: &Path,
    contents: &str,
    line: usize,
    col: usize,
    err: &dyn fmt::Display,
) -> String {
    format!(
        "
error: {err}
     --> {file}:{line}:{col}
      |
 {line:4} | {text}
      | {marker:>0$}
",
        col + 1,
        file = file.display(),
        line = line + 1,
        col = col + 1,
        err = err,
        text = contents.lines().nth(line).unwrap_or(""),
        marker = "^",
    )
}
