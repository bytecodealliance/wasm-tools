//! You can run this test suite with:
//!
//!     cargo test --test all
//!
//! An argument can be passed as well to filter, based on filename, which test
//! to run
//!
//!     cargo test --test all foo.wit

use anyhow::{bail, Context, Result};
use pretty_assertions::StrComparison;
use rayon::prelude::*;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::str;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use wit_parser::*;

fn main() {
    env_logger::init();
    let tests = find_tests();
    let filter = std::env::args().nth(1);

    let tests = tests
        .par_iter()
        .filter_map(|test| {
            if let Some(filter) = &filter {
                if let Some(s) = test.to_str() {
                    if !s.contains(filter) {
                        return None;
                    }
                }
            }
            Some(test)
        })
        .collect::<Vec<_>>();

    println!("running {} test files\n", tests.len());

    let ntests = AtomicUsize::new(0);
    let errors = tests
        .par_iter()
        .filter_map(|test| {
            Runner { ntests: &ntests }
                .run(test)
                .context(format!("test {:?} failed", test))
                .err()
        })
        .collect::<Vec<_>>();

    if !errors.is_empty() {
        for msg in errors.iter() {
            eprintln!("{:?}", msg);
        }

        panic!("{} tests failed", errors.len())
    }

    println!(
        "test result: ok. {} directives passed\n",
        ntests.load(SeqCst)
    );
}

/// Recursively finds all tests in a whitelisted set of directories which we
/// then load up and test in parallel.
fn find_tests() -> Vec<PathBuf> {
    let mut tests = Vec::new();
    find_tests("tests/ui".as_ref(), &mut tests);
    find_tests("tests/ui/parse-fail".as_ref(), &mut tests);
    tests.sort();
    return tests;

    fn find_tests(path: &Path, tests: &mut Vec<PathBuf>) {
        for f in path.read_dir().unwrap() {
            let f = f.unwrap();
            let path = f.path();
            if path.file_name().unwrap().to_str().unwrap() == "parse-fail" {
                continue;
            }
            if f.file_type().unwrap().is_dir() {
                tests.push(path);
                continue;
            }

            match path.extension().and_then(|s| s.to_str()) {
                Some("md") => {}
                Some("wit") => {}
                _ => continue,
            }
            tests.push(path);
        }
    }
}

struct Runner<'a> {
    ntests: &'a AtomicUsize,
}

impl Runner<'_> {
    fn run(&mut self, test: &Path) -> Result<()> {
        let mut resolve = Resolve::new();
        let result = if test.is_dir() {
            resolve.push_dir(test).map(|(id, _)| id)
        } else {
            UnresolvedPackage::parse_file(test).and_then(|p| resolve.push(p))
        };

        let result = if test.iter().any(|s| s == "parse-fail") {
            match result {
                Ok(_) => bail!("expected test to not parse but it did"),
                Err(mut e) => {
                    if let Some(err) = e.downcast_mut::<io::Error>() {
                        *err = io::Error::new(
                            io::ErrorKind::Other,
                            "some generic platform-agnostic error message",
                        );
                    }
                    format!("{:?}", e)
                }
            }
        } else {
            result?;
            // format json string to human readable
            let json_result = serde_json::to_string_pretty(&resolve)?;
            // "foo.wit" => "foo.wit.json"
            self.read_or_write_to_file(test, &json_result, "json")?;
            return Ok(());
        };

        // "foo.wit" => "foo.wit.result"
        // "foo.wit.md" => "foo.wit.md.result"
        self.read_or_write_to_file(test, &result, "result")?;
        return Ok(());
    }

    fn read_or_write_to_file(
        &mut self,
        test: &Path,
        result: &str,
        extension: &str,
    ) -> Result<(), anyhow::Error> {
        let result_file = if test.extension() == Some(OsStr::new("md"))
            && test
                .file_stem()
                .and_then(|path| Path::new(path).extension())
                == Some(OsStr::new("wit"))
        {
            test.with_extension(format!("md.{extension}"))
        } else {
            test.with_extension(format!("wit.{extension}"))
        };
        if env::var_os("BLESS").is_some() {
            let normalized = normalize(&result, extension);
            fs::write(&result_file, normalized)?;
        } else {
            let expected = fs::read_to_string(&result_file).context(format!(
                "failed to read test expectation file {:?}\nthis can be fixed with BLESS=1",
                result_file
            ))?;
            let expected = normalize(&expected, extension);
            let result = normalize(&result, extension);
            if expected != result {
                bail!(
                    "failed test: result is not as expected:{}",
                    StrComparison::new(&expected, &result),
                );
            }
        }
        self.bump_ntests();
        Ok(())
    }

    fn bump_ntests(&self) {
        self.ntests.fetch_add(1, SeqCst);
    }
}

fn normalize(s: &str, extension: &str) -> String {
    match extension {
        // .result files have error messages with paths, so normalize Windows \ separators to /
        "result" => s.replace("\\", "/").replace("\r\n", "\n"),

        // .json files escape strings with \, so leave them alone
        _ => s.replace("\r\n", "\n"),
    }
}
