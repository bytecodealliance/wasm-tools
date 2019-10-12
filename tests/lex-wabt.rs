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
                Err(e) => render_error(test, &contents, e.line(), e.col(), &e),
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

    tests.iter().for_each(|test| {
        let contents = std::fs::read_to_string(test).unwrap();
        if contents.contains(";; ERROR") {
            return;
        }
        if contents.contains("--enable-exceptions") {
            return;
        }
        let buf = match wast::parser::ParseBuffer::new(&contents) {
            Ok(b) => b,
            Err(e) => render_error(test, &contents, e.line(), e.col(), &e),
        };
        if let Err(e) = buf.parser().parse::<wast::ast::File>() {
            render_error(test, &contents, e.line(), e.col(), &e);
        };
        if !buf.parser().is_empty() {
            panic!("failed to parse all of {:?}", test);
        }
    })
}

fn render_error(file: &Path, contents: &str, line: usize, col: usize, err: &dyn fmt::Display) -> ! {
    eprintln!("");
    eprintln!("error: {}", err);
    eprintln!("     --> {}:{}:{}", file.display(), line + 1, col + 1);
    eprintln!("      |");
    eprintln!(" {:4} | {}", line + 1, contents.lines().nth(line).unwrap());
    eprintln!("      | {1:>0$}", col + 1, "^");
    eprintln!("");
    panic!("{}", err);
}
