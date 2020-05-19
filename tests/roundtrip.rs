//! Finds as many tests as we can in the `wabt` submodule and does a few things:
//!
//! * First, asserts that we can parse and encode them all to binary.
//! * Next uses `wat2wasm` to encode to binary.
//! * Finally, asserts that the two binary encodings are byte-for-byte the same.
//!
//! This also has support for handling `*.wast` files from the official test
//! suite which involve parsing as a wast file and handling assertions. Also has
//! rudimentary support for running some of the assertions.

use anyhow::{bail, Context, Result};
use rayon::prelude::*;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use wast::parser::ParseBuffer;
use wast::*;

fn main() {
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
            let contents = std::fs::read(test).unwrap();
            if skip_test(&test, &contents) {
                None
            } else {
                Some((test, contents))
            }
        })
        .collect::<Vec<_>>();

    println!("running {} test files\n", tests.len());

    let state = TestState::default();
    let errors = tests
        .par_iter()
        .filter_map(|(test, contents)| state.run_test(test, contents).err())
        .collect::<Vec<_>>();

    if !errors.is_empty() {
        for msg in errors.iter() {
            eprintln!("{:?}", msg);
        }

        panic!("{} tests failed", errors.len())
    }

    println!(
        "test result: ok. {} directives passed\n",
        state.ntests.load(SeqCst)
    );
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
    find_tests("tests/local".as_ref(), &mut tests);
    find_tests("tests/testsuite".as_ref(), &mut tests);
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
                Some("txt") | Some("wast") | Some("wat") | Some("wasm") => {}
                _ => continue,
            }
            tests.push(f.path());
        }
    }
}

fn skip_test(test: &Path, contents: &[u8]) -> bool {
    // We've made the opinionated decision that well-known annotations like
    // `@custom` and `@name` must be well-formed. This test, however, uses
    // `@custom` in ways the spec doesn't specify, so we skip it.
    if test.ends_with("test/parse/annotations.txt") {
        return true;
    }

    // Waiting for wabt to remove subtyping from reference-types, updating the
    // test syntax
    if test.ends_with("reference-types.txt")
        || test.ends_with("all-features.txt")
        || test.ends_with("all-features.txt")
        || test.ends_with("bulk-memory-named.txt")
        || test.ends_with("reference-types-named.txt")
        || test.ends_with("table-grow.txt")
        || test.ends_with("result-exnref.txt")
        || test.ends_with("global-exnref.txt")
        || test.ends_with("global.txt")
        || test.ends_with("bulk-memory.txt")
    {
        return true;
    }

    if let Ok(contents) = str::from_utf8(contents) {
        // Skip tests that are supposed to fail
        if contents.contains(";; ERROR") {
            return true;
        }
        // These tests are acually ones that run with the `*.wast` files from the
        // official test suite, and we slurp those up elsewhere anyway.
        if contents.contains("STDIN_FILE") {
            return true;
        }
    }

    false
}

#[derive(Default)]
struct TestState {
    ntests: AtomicUsize,
}

struct Wast2Json {
    _td: tempfile::TempDir,
    modules: Vec<PathBuf>,
}

impl TestState {
    fn run_test(&self, test: &Path, contents: &[u8]) -> Result<()> {
        let result = match test.extension().and_then(|s| s.to_str()) {
            Some("wat") => self.test_wat(test),
            Some("wasm") => self.test_wasm(test, contents),
            Some("wast") => self.test_wast(test, contents),
            Some("txt") => match str::from_utf8(contents) {
                Ok(s) if s.contains("TOOL: wast2json") || s.contains("TOOL: run-objdump-spec") => {
                    self.test_wast(test, contents)
                }
                _ => self.test_wat(test),
            },
            _ => bail!("unknown file extension {:?}", test),
        };
        result.with_context(|| format!("failed test: {}", test.display()))
    }

    fn test_wat(&self, test: &Path) -> Result<()> {
        let binary = wat::parse_file(test)?;
        self.bump_ntests(); // tested the parse

        // Currently our encoding of tests two tests differs from wabt, but
        // they're invalid anyway so it's not that worrisome.
        if !test.ends_with("invalid-data-segment-offset.txt")
            && !test.ends_with("invalid-elem-segment-offset.txt")
        {
            // If enabled, assert `wabt` generates the same binary encoding
            if let Some(expected) = self.wat2wasm(&test)? {
                self.binary_compare(&binary, &expected)?;
            }
        }
        self.test_wasm(test, &binary)?;
        Ok(())
    }

    fn test_wasm(&self, test: &Path, contents: &[u8]) -> Result<()> {
        // TODO
        drop((test, contents));
        Ok(())
    }

    fn test_wast(&self, test: &Path, contents: &[u8]) -> Result<()> {
        let contents = str::from_utf8(contents)?;
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
        self.bump_ntests();
        let json = self.wast2json(&test)?;

        // Pair each `Module` directive with the result of wast2json's output
        // `*.wasm` file, and then execute each test in parallel.
        let mut modules = 0;
        let directives = wast
            .directives
            .into_iter()
            .map(|directive| match directive {
                WastDirective::Module(_) => {
                    modules += 1;
                    (directive, json.as_ref().map(|j| &j.modules[modules - 1]))
                }
                other => (other, None),
            })
            .collect::<Vec<_>>();

        let errors = directives
            .into_par_iter()
            .filter_map(|(directive, expected)| {
                let (line, col) = directive.span().linecol_in(contents);
                self.test_wast_directive(test, directive, expected)
                    .with_context(|| {
                        format!(
                            "failed directive on {}:{}:{}",
                            test.display(),
                            line + 1,
                            col + 1
                        )
                    })
                    .err()
            })
            .collect::<Vec<_>>();
        if errors.is_empty() {
            return Ok(());
        }
        let mut s = format!("{} test failures in {}:", errors.len(), test.display());
        for error in errors {
            s.push_str("\n\n\t--------------------------------\n\n\t");
            s.push_str(&format!("{:?}", error).replace("\n", "\n\t"));
        }
        bail!("{}", s)
    }

    fn test_wast_directive(
        &self,
        test: &Path,
        directive: WastDirective,
        expected: Option<&PathBuf>,
    ) -> Result<()> {
        match directive {
            WastDirective::Module(mut module) => {
                let actual = module.encode()?;
                self.bump_ntests(); // testing encode
                self.test_wasm(test, &actual)?;
                match module.kind {
                    ModuleKind::Text(_) => {
                        if let Some(expected) = &expected {
                            let expected = fs::read(expected)?;
                            self.binary_compare(&actual, &expected)?;
                        }
                    }
                    // Skip these for the same reason we skip
                    // `module/binary-module.txt` in `binary_compare` below.
                    // TODO
                    ModuleKind::Binary(_) => {}
                }
            }

            WastDirective::QuoteModule { source, span: _ } => {
                self.parse_quote_module(test, &source)?;
            }

            WastDirective::AssertMalformed {
                span: _,
                module: QuoteModule::Quote(source),
                message,
            } => {
                let result = self.parse_quote_module(test, &source);
                match result {
                    Ok(()) => bail!(
                        "parsed successfully but should have failed with: {}",
                        message,
                    ),
                    Err(e) => {
                        if error_matches(&e.to_string(), message) {
                            self.bump_ntests();
                            return Ok(());
                        }
                        bail!("bad error: {}\nshould have failed with: {:?}", e, message);
                    }
                }
            }

            _ => {}
        }
        Ok(())
    }

    fn parse_quote_module(&self, test: &Path, source: &[&[u8]]) -> Result<()> {
        let mut ret = String::new();
        for src in source {
            match str::from_utf8(src) {
                Ok(s) => ret.push_str(s),
                Err(_) => bail!("malformed UTF-8 encoding"),
            }
            ret.push_str(" ");
        }
        let buf = ParseBuffer::new(&ret)?;
        let mut wat = parser::parse::<Wat>(&buf)?;
        self.bump_ntests();
        let binary = wat.module.encode()?;
        self.bump_ntests();
        self.test_wasm(test, &binary)?;
        Ok(())
    }

    fn binary_compare(&self, actual: &[u8], expected: &[u8]) -> Result<()> {
        use wasmparser::*;

        // I tried for a bit but honestly couldn't figure out a great way to match
        // wabt's encoding of the name section. Just remove it from our asserted
        // sections and don't compare against wabt's.
        let actual = remove_name_section(actual);

        if actual == expected {
            self.bump_ntests();
            return Ok(());
        }

        let difference = actual
            .iter()
            .enumerate()
            .zip(expected)
            .find(|((_, actual), expected)| actual != expected);
        let pos = match difference {
            Some(((pos, _), _)) => format!("at byte {} ({0:#x})", pos),
            None => format!("by being too small"),
        };
        let mut msg = format!("error: actual wasm differs {} from expected wasm\n", pos);

        if let Some(((pos, _), _)) = difference {
            msg.push_str(&format!("  {:4} |   {:#04x}\n", pos - 2, actual[pos - 2]));
            msg.push_str(&format!("  {:4} |   {:#04x}\n", pos - 1, actual[pos - 1]));
            msg.push_str(&format!("  {:4} | - {:#04x}\n", pos, expected[pos]));
            msg.push_str(&format!("       | + {:#04x}\n", actual[pos]));
        }

        let mut actual_parser = Parser::new(&actual);
        let mut expected_parser = Parser::new(&expected);

        let mut differences = 0;
        let mut last_dots = false;
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
                if differences > 0 && !last_dots {
                    msg.push_str(&format!("       |   ...\n"));
                    last_dots = true;
                }
                continue;
            }
            last_dots = false;

            if differences == 0 {
                msg.push_str("\n\n");
            }
            msg.push_str(&format!(
                "  {:4} | - {}\n",
                expected_parser.current_position(),
                expected_state
            ));
            msg.push_str(&format!(
                "  {:4} | + {}\n",
                actual_parser.current_position(),
                actual_state
            ));
            differences += 1;
        }

        bail!("{}", msg);

        fn read_state<'a, 'b>(parser: &'b mut Parser<'a>) -> Option<String> {
            loop {
                match parser.read() {
                    // ParserState::BeginSection { code: SectionCode::DataCount, .. } => {}
                    // ParserState::DataCountSectionEntry(_) => {}
                    ParserState::Error(_) | ParserState::EndWasm => break None,
                    other => break Some(format!("{:?}", other)),
                }
            }
        }

        fn remove_name_section(bytes: &[u8]) -> Vec<u8> {
            let mut r = ModuleReader::new(bytes).expect("should produce valid header");
            while !r.eof() {
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
                }
            }
            return bytes.to_vec();
        }
    }

    fn bump_ntests(&self) {
        self.ntests.fetch_add(1, SeqCst);
    }

    fn wat2wasm(&self, test: &Path) -> Result<Option<Vec<u8>>> {
        let f = tempfile::NamedTempFile::new()?;
        let result = Command::new("wat2wasm")
            .arg(test)
            .arg("--enable-all")
            .arg("--no-check")
            .arg("-o")
            .arg(f.path())
            .output()
            .context("failed to spawn `wat2wasm`")?;
        Ok(if result.status.success() {
            Some(fs::read(f.path())?)
        } else {
            // TODO: handle this case better
            None
        })
    }

    fn wast2json(&self, test: &Path) -> Result<Option<Wast2Json>> {
        // Right now wabt infinite loops on this test.
        if test.ends_with("testsuite/proposals/annotations/annotations.wast") {
            return Ok(None);
        }
        let td = tempfile::TempDir::new()?;
        let result = Command::new("wast2json")
            .arg(test)
            .arg("--enable-all")
            .arg("--no-check")
            .arg("-o")
            .arg(td.path().join("foo.json"))
            .output()
            .context("failed to spawn `wat2wasm`")?;
        if !result.status.success() {
            // TODO: handle this case better
            return Ok(None);
        }
        let json = fs::read_to_string(td.path().join("foo.json"))?;
        let json: serde_json::Value = serde_json::from_str(&json)?;
        let commands = json["commands"].as_array().unwrap();
        let modules = commands
            .iter()
            .filter_map(|m| {
                if m["type"] == "module" {
                    Some(td.path().join(m["filename"].as_str().unwrap()))
                } else {
                    None
                }
            })
            .collect();
        Ok(Some(Wast2Json { _td: td, modules }))
    }
}
fn error_matches(error: &str, message: &str) -> bool {
    if error.contains(message) {
        return true;
    }
    if message == "unknown operator"
        || message == "unexpected token"
        || message == "wrong number of lane literals"
        || message == "type mismatch"
        || message == "malformed lane index"
        || message == "expected i8 literal"
        || message == "invalid lane length"
        || message == "unclosed annotation"
        || message == "malformed annotation id"
        || message == "alignment must be a power of two"
        || message == "i32 constant out of range"
    {
        return error.contains("expected ")
            || error.contains("constant out of range")
            || error.contains("extra tokens remaining");
    }

    if message == "illegal character" {
        return error.contains("unexpected character");
    }

    if message == "unclosed string" {
        return error.contains("unexpected end-of-file");
    }

    if message == "invalid UTF-8 encoding" {
        return error.contains("malformed UTF-8 encoding");
    }

    if message == "duplicate identifier" {
        return error.contains("duplicate") && error.contains("identifier");
    }

    return false;
}
