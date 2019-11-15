use anyhow::{bail, Context};
use rayon::prelude::*;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use wast::parser::ParseBuffer;
use wast::*;

fn main() {
    let tests = find_tests();
    let filter = std::env::args().nth(1);

    let mut tests = tests
        .par_iter()
        .filter_map(|test| {
            if let Some(filter) = &filter {
                if let Some(s) = test.file_name().and_then(|s| s.to_str()) {
                    if !s.contains(filter) {
                        return None;
                    }
                }
            }
            let contents = std::fs::read_to_string(test).unwrap();
            if skip_test(&test, &contents) {
                None
            } else {
                Some((test, contents))
            }
        })
        .collect::<Vec<_>>();
    tests.sort_by_key(|p| p.1.len());
    tests.reverse();

    println!("running {} tests\n", tests.len());

    let errors = tests
        .par_iter()
        .filter_map(|(test, contents)| run_test(test, contents).err())
        .collect::<Vec<_>>();

    if !errors.is_empty() {
        for msg in errors.iter() {
            eprintln!("{:?}", msg);
        }

        panic!("{} tests failed", errors.len())
    }

    println!("test result: ok. {} passed\n", tests.len());
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
    find_tests("tests/regression".as_ref(), &mut tests);
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
                Some("txt") | Some("wast") | Some("wat") => {}
                _ => continue,
            }
            tests.push(f.path());
        }
    }
}

fn skip_test(test: &Path, contents: &str) -> bool {
    // Skip tests that are supposed to fail
    if contents.contains(";; ERROR") {
        return true;
    }
    // These tests are acually ones that run with the `*.wast` files from the
    // official test suite, and we slurp those up elsewhere anyway.
    if contents.contains("STDIN_FILE") {
        return true;
    }
    // Skip tests that exercise unimplemented proposals
    if contents.contains("--enable-exceptions") || test.ends_with("all-features.txt") {
        return true;
    }
    if contents.contains("--enable-tail-call") {
        return true;
    }
    if contents.contains("--enable-threads") {
        return true;
    }
    if contents.contains("--enable-simd") {
        return true;
    }
    if contents.contains("--enable-annotations") {
        return true;
    }
    // Some exception-handling tests don't use `--enable-exceptions` since
    // `run-objdump` enables everything
    if contents.contains("run-objdump") && contents.contains("(event") {
        return true;
    }

    // Skip some intentionally valid tests sinc `wasm2wat` can't work on the
    // resulting binary, even with `--no-check`.
    if let Some(name) = test.file_name().and_then(|s| s.to_str()) {
        if name.starts_with("invalid-elem-segment") || name.starts_with("invalid-data-segment") {
            return true;
        }
    }

    // This just looks sort of buggy in wabt
    if test.ends_with("dump/table-multi.txt") {
        return true;
    }

    false
}

fn run_test(test: &Path, contents: &str) -> anyhow::Result<()> {
    let wast = contents.contains("TOOL: wast2json")
        || contents.contains("TOOL: run-objdump-spec")
        || test.display().to_string().ends_with(".wast");
    if wast {
        test_wast(test, contents)
    } else {
        let binary = wat::parse_file(test)?;
        test_binary(test, &binary)
    }
}

fn test_wast(test: &Path, contents: &str) -> anyhow::Result<()> {
    macro_rules! adjust {
        ($e:expr) => {{
            let mut e = wast::Error::from($e);
            e.set_path(test);
            e.set_text(contents);
            e
        }};
    }
    let buf = ParseBuffer::new(contents).map_err(|e| adjust!(e))?;
    let wast = parser::parse::<Wast>(&buf).map_err(|e| adjust!(e))?;

    let results = wast
        .directives
        .into_par_iter()
        .map(|directive| -> anyhow::Result<()> {
            match directive {
                WastDirective::Module(mut module)
                | WastDirective::AssertUnlinkable { mut module, .. } => {
                    let binary = module.encode().map_err(|e| adjust!(e))?;
                    test_binary(test, &binary)?;
                }
                _ => {}
            }

            Ok(())
        })
        .collect::<Vec<_>>();

    let errors = results
        .into_iter()
        .filter_map(|e| e.err())
        .collect::<Vec<_>>();
    if errors.is_empty() {
        return Ok(());
    }
    let mut s = format!("{} test failures in {}:", errors.len(), test.display());
    for error in errors {
        s.push_str("\n\t");
        s.push_str(&format!("{:?}", error).replace("\n", "\n\t"));
    }
    bail!("{}", s)
}

fn test_binary(path: &Path, contents: &[u8]) -> anyhow::Result<()> {
    let actual = wasmprinter::print_bytes(contents)
        .context(format!("rust failed to print `{}`", path.display()))?;
    let expected =
        wasm2wat(contents).context(format!("failed to run `wasm2wat` on `{}`", path.display()))?;

    let actual = normalize(&actual);
    let expected = normalize(&expected);

    fn normalize(s: &str) -> String {
        let mut s = s.trim().to_string();

        // We seem to have different decimal float printing than wabt, and a
        // hand-check seems to show that they're equivalent just different
        // renderings. To paper over these inconsequential differences delete
        // these comments.
        while let Some(i) = s.find("(;=") {
            let end = s[i..].find(";)").unwrap();
            s.drain(i..end + i + 2);
        }

        // Currently `wabt` seems to accidentally insert a newline after
        // `ref.func`, but we don't do that, so normalize wabt's output to not
        // have a newline.
        let needle = "ref.func\n";
        while let Some(i) = s.find(needle) {
            let len = s[i + needle.len()..]
                .chars()
                .take_while(|c| c.is_whitespace())
                .count();
            s.drain(i + needle.len() - 1..i + needle.len() - 1 + len);
        }
        return s;
    }

    let mut bad = false;
    let mut result = String::new();
    for diff in diff::lines(&expected, &actual) {
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
            path.display(),
            result
        );
    } else {
        Ok(())
    }
}

fn wasm2wat(contents: &[u8]) -> anyhow::Result<String> {
    let f = tempfile::TempDir::new().unwrap();
    let wasm = f.path().join("wasm");
    let wat = f.path().join("wat");
    fs::write(&wasm, contents).context("failed to write wasm file")?;
    let result = Command::new("wasm2wat")
        .arg(&wasm)
        .arg("--enable-all")
        .arg("--no-check")
        .arg("-o")
        .arg(&wat)
        .output()
        .expect("failed to spawn `wasm2wat`");
    if result.status.success() {
        fs::read_to_string(&wat).context("failed to read wat file")
    } else {
        bail!(
            "failed to run wasm2wat: {}\n\n    {}",
            result.status,
            String::from_utf8_lossy(&result.stderr).replace("\n", "\n    "),
        )
    }
}
