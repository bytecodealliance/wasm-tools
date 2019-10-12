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
                    render_error(test, &contents, e.line(), e.col(), &e);
                    panic!()
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
    tests.sort();
    return tests;

    fn find_tests(path: &Path, tests: &mut Vec<PathBuf>) {
        for f in path.read_dir().unwrap() {
            let f = f.unwrap();
            if f.file_type().unwrap().is_dir() {
                find_tests(&f.path(), tests);
                continue;
            }

            if f.path().extension().and_then(|s| s.to_str()) != Some("txt") {
                continue;
            }
            tests.push(f.path());
        }
    }
}

#[test]
fn parse_wabt() {
    let tests = find_tests();

    let mut failed = 0;
    for test in tests {
        let contents = std::fs::read_to_string(&test).unwrap();
        // Skip tests that are supposed to fail
        if contents.contains(";; ERROR") {
            continue;
        }
        // Tests that have a different input
        if contents.contains("STDIN_FILE") {
            continue;
        }
        // Some exception-handling tests don't use `--enable-exceptions` since
        // `run-objdump` enables everything
        if contents.contains("run-objdump") && contents.contains("(event") {
            continue;
        }
        // contains weird stuff at the end of the file we don't parse
        if contents.contains("TOOL: run-objdump-spec") {
            continue;
        }
        // Skip wast2json tests since we don't parse those right now
        if contents.contains("TOOL: wast2json") {
            continue;
        }

        // Skip tests that exercise unimplemented proposals
        if contents.contains("--enable-exceptions") {
            continue;
        }
        if contents.contains("--enable-all") {
            continue;
        }
        if contents.contains("--enable-annotations") {
            continue;
        }
        if contents.contains("--enable-simd") {
            continue;
        }
        if contents.contains("--enable-tail-call") {
            continue;
        }

        let buf = match wast::parser::ParseBuffer::new(&contents) {
            Ok(b) => b,
            Err(e) => {
                render_error(&test, &contents, e.line(), e.col(), &e);
                failed += 1;
                continue;
            }
        };
        if let Err(e) = buf.parser().parse::<wast::ast::File>() {
            render_error(&test, &contents, e.line(), e.col(), &e);
            failed += 1;
            continue;
        };
        if !buf.parser().is_empty() {
            failed += 1;
            eprintln!("failed to parse all of {:?}", test);
        }
    }
    if failed > 0 {
        panic!("{} tests failed", failed)
    }
}

fn render_error(file: &Path, contents: &str, line: usize, col: usize, err: &dyn fmt::Display) {
    eprintln!("");
    eprintln!("error: {}", err);
    eprintln!("     --> {}:{}:{}", file.display(), line + 1, col + 1);
    eprintln!("      |");
    eprintln!(
        " {:4} | {}",
        line + 1,
        contents.lines().nth(line).unwrap_or("")
    );
    eprintln!("      | {1:>0$}", col + 1, "^");
    eprintln!("");
}
