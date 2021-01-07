//! A test suite to parse everything in `dump` and assert that it matches
//! the `*.dump` file it generates.
//!
//! Use `BLESS=1` in the environment to auto-update `*.err` files. Be sure to
//! look at the diff!

use anyhow::{bail, Context, Result};
use rayon::prelude::*;
use std::env;
use std::path::{Path, PathBuf};

fn main() {
    let mut tests = Vec::new();
    find_tests("tests/dump".as_ref(), &mut tests);
    let filter = std::env::args().nth(1);

    let bless = env::var("BLESS").is_ok();
    let tests = tests
        .iter()
        .filter(|test| {
            if let Some(filter) = &filter {
                if let Some(s) = test.file_name().and_then(|s| s.to_str()) {
                    if !s.contains(filter) {
                        return false;
                    }
                }
            }
            true
        })
        .collect::<Vec<_>>();

    println!("running {} tests\n", tests.len());

    let errors = tests
        .par_iter()
        .filter_map(|test| run_test(test, bless).err())
        .collect::<Vec<_>>();

    if !errors.is_empty() {
        for msg in errors.iter() {
            eprintln!("{:?}", msg);
        }

        panic!("{} tests failed", errors.len())
    }

    println!("test result: ok. {} passed\n", tests.len());
}

fn run_test(test: &Path, bless: bool) -> Result<()> {
    let wasm = wat::parse_file(test)?;
    let assert = test.with_extension("wat.dump");
    let dump =
        wasmparser_dump::dump_wasm(&wasm).with_context(|| format!("failed to dump {:?}", test))?;
    if bless {
        std::fs::write(assert, &dump)?;
        return Ok(());
    }

    // Ignore CRLF line ending and force always `\n`
    let assert = std::fs::read_to_string(assert)
        .unwrap_or(String::new())
        .replace("\r\n", "\n");

    let mut bad = false;
    let mut result = String::new();
    for diff in diff::lines(&assert, &dump) {
        match diff {
            diff::Result::Left(s) => {
                bad = true;
                result.push_str("-");
                result.push_str(s);
            }
            diff::Result::Right(s) => {
                bad = true;
                result.push_str("+");
                result.push_str(s);
            }
            diff::Result::Both(s, _) => {
                result.push_str(" ");
                result.push_str(s);
            }
        }
        result.push_str("\n");
    }
    if bad {
        bail!(
            "expected != actual for test `{}`\n\n{}",
            test.display(),
            result
        );
    } else {
        Ok(())
    }
}

fn find_tests(path: &Path, tests: &mut Vec<PathBuf>) {
    for f in path.read_dir().unwrap() {
        let f = f.unwrap();
        if f.file_type().unwrap().is_dir() {
            find_tests(&f.path(), tests);
            continue;
        }
        match f.path().extension().and_then(|s| s.to_str()) {
            Some("wat") => {}
            _ => continue,
        }
        tests.push(f.path());
    }
}
