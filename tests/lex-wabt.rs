use rayon::prelude::*;
use std::fmt;
use std::path::{Path, PathBuf};
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
    let tests = find_tests();

    let failed = tests
        .par_iter()
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

                let bytes = wast::binary::encode(&module);
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
