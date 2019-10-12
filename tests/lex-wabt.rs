use rayon::prelude::*;
use std::fmt;
use std::path::{Path, PathBuf};
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
    let tests = find_tests();

    let failed = tests
        .par_iter()
        .filter_map(|test| {
            // This is something that doesn't seem worth supporting at this
            // time, `*.wast` files with inline modules (although we do support
            // it for `*.wat` files.
            if test.ends_with("inline-module.wast") {
                return None;
            }

            // This test still uses a bunch of old names and I don't feel like
            // typing them all out at this time, so just skip it. We get some
            // testing from wabt's test suite anyway.
            if test.ends_with("threads/atomic.wast") {
                return None;
            }

            let contents = std::fs::read_to_string(&test).unwrap();
            // Skip tests that are supposed to fail
            if contents.contains(";; ERROR") {
                return None;
            }
            // Tests that have a different input
            if contents.contains("STDIN_FILE") {
                return None;
            }
            // Some exception-handling tests don't use `--enable-exceptions` since
            // `run-objdump` enables everything
            if contents.contains("run-objdump") && contents.contains("(event") {
                return None;
            }
            // contains weird stuff at the end of the file we don't parse
            if contents.contains("TOOL: run-objdump-spec") {
                return None;
            }

            // Skip tests that exercise unimplemented proposals
            if contents.contains("--enable-exceptions") {
                return None;
            }
            if contents.contains("--enable-all") {
                return None;
            }
            if contents.contains("--enable-annotations") {
                return None;
            }
            if contents.contains("--enable-simd") {
                return None;
            }
            if contents.contains("--enable-tail-call") {
                return None;
            }

            let buf = match wast::parser::ParseBuffer::new(&contents) {
                Ok(b) => b,
                Err(e) => {
                    return Some(render_error(&test, &contents, e.line(), e.col(), &e));
                }
            };
            let wast = contents.contains("TOOL: wast2json")
                || test.display().to_string().ends_with(".wast");
            let result = if wast {
                buf.parser().parse::<wast::ast::Wast>().err()
            } else {
                buf.parser().parse::<wast::ast::Wat>().err()
            };
            if let Some(e) = result {
                return Some(render_error(&test, &contents, e.line(), e.col(), &e));
            };
            if !buf.parser().is_empty() {
                return Some(format!("failed to parse all of {:?}", test));
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
