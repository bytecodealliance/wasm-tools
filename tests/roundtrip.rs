//! Finds as many tests as we can checked into this repository, and then runs a
//! bunch of assertions over them.
//!
//! * For `*.wast` files, parses them and runs as many directives as we can.
//! * For `*.wast` files, parse them with `wat`, parse that with `wasmparser`,
//!   and make sure `wasmprinter` + `wat` produces the same bytes.
//! * For `*.wasm`, assert they're either valid or invalid depending on their
//!   path name,
//!
//! The goal here is to make adding tests very easy. It should be as simple as
//! dropping tests into the `tests/local` directory or updating one of the
//! `tests/wabt` or `tests/testsuite` submodules. The `wabt` submodule is
//! intended to pull in a number of tests that wabt itself uses. The `testsuite`
//! submodule is the upstream git repository of the spec tests, including
//! proposals.
//!
//! You can run this test suite with:
//!
//!     cargo test --test roundtrip
//!
//! An argument can be passed as well to filter, based on filename, which test
//! to run
//!
//!     cargo test --test roundtrip local/ref.wat

use anyhow::{bail, Context, Result};
use rayon::prelude::*;
use std::path::{Path, PathBuf};
use std::str;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use wasmparser::*;
use wast::lexer::Lexer;
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

/// Recursively finds all tests in a whitelisted set of directories which we
/// then load up and test in parallel.
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

/// Returns whether an entire test file is skipped.
///
/// Note that this is used to skip tests for all crates, not just one at a
/// time. There's further filters applied while testing.
fn skip_test(test: &Path, contents: &[u8]) -> bool {
    let broken = &[
        // We've made the opinionated decision that well-known annotations like
        // `@custom` and `@name` must be well-formed. This test, however, uses
        // `@custom` in ways the spec doesn't specify, so we skip it.
        "test/parse/annotations.txt",
        // this has syntax of an element segment `(elem $e 0)` which isn't used
        // anywhere else, and I'm not entirely certain if it's vaild, and for
        // now I don't feel like filing an issue or adding parsing for this.
        "roundtrip/table-init-index.txt",
        // I don't really have any idea what's going on with the expected syntax
        // errors and expected error messages in these tests. They seem like
        // they're from left field considering other conventions, so let's just
        // ignore these until the proposal is further along.
        "exception-handling/try_delegate.wast",
        "exception-handling/try_catch.wast",
        "exception-handling/throw.wast",
    ];
    if broken.iter().any(|x| test.ends_with(x)) {
        return true;
    }

    // TODO: the gc proposal isn't implemented yet
    if test.iter().any(|p| p == "gc") {
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

impl TestState {
    fn run_test(&self, test: &Path, contents: &[u8]) -> Result<()> {
        let result = match test.extension().and_then(|s| s.to_str()) {
            Some("wat") => self.test_wat(test, contents),
            Some("wasm") => self.test_wasm(test, contents, false),
            Some("wast") => self.test_wast(test, contents),
            Some("txt") => match str::from_utf8(contents) {
                Ok(s) if s.contains("TOOL: wast2json") || s.contains("TOOL: run-objdump-spec") => {
                    self.test_wast(test, contents)
                }
                _ => self.test_wat(test, contents),
            },
            _ => bail!("unknown file extension {:?}", test),
        };
        result.with_context(|| format!("failed test: {}", test.display()))
    }

    fn test_wat(&self, test: &Path, contents: &[u8]) -> Result<()> {
        // First up test that we can parse the file and convert it to a binary
        // wasm file.
        let binary = wat::parse_file(test)?;
        self.bump_ntests();

        let contents = str::from_utf8(contents)?;

        // Finally we test that this is indeed a valid wasm file. Note,
        // however, that wasmparser doesn't implement all features that wabt
        // does, so we skip some tests here too.
        //
        // TODO: implement function-references in wasmparser
        // TODO: implement gc types in wasmparser
        if !contents.contains("--enable-function-references")
            && !contents.contains("--enable-gc")
            && !contents.contains("--no-check")
            // intentionally invalid wasm files
            && !contents.contains(";; TOOL: wat-desugar")
            && !test.ends_with("dump/import.txt")
        {
            self.test_wasm(test, &binary, true)
                .context("failed testing the binary output of `wat`")?;
        }
        Ok(())
    }

    fn test_wasm(&self, test: &Path, contents: &[u8], test_roundtrip: bool) -> Result<()> {
        if test.iter().any(|t| t == "invalid") {
            self.test_wasm_invalid(test, contents)?;
            return Ok(());
        }

        self.test_wasm_valid(test, contents)
            .context("wasm isn't valid")?;

        // Test that we can print these bytes, and if available make sure it
        // matches wabt.
        let string = wasmprinter::print_bytes(contents).context("failed to print wasm")?;
        self.bump_ntests();

        // If we can, convert the string back to bytes and assert it has the
        // same binary representation.
        if test_roundtrip {
            let binary2 =
                wat::parse_str(&string).context("failed to parse `wat` from `wasmprinter`")?;
            self.bump_ntests();
            self.binary_compare(&binary2, contents, false)
                .context("failed to compare original `wat` with roundtrip `wat`")?;
        }

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
        let mut lexer = Lexer::new(contents);
        lexer.allow_confusing_unicode(test.ends_with("names.wast"));
        let buf = ParseBuffer::new_with_lexer(lexer).map_err(|e| adjust!(e))?;
        let wast = parser::parse::<Wast>(&buf).map_err(|e| adjust!(e))?;
        self.bump_ntests();

        let errors = wast
            .directives
            .into_par_iter()
            .filter_map(|directive| {
                let (line, col) = directive.span().linecol_in(contents);
                self.test_wast_directive(test, directive)
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
        for mut error in errors {
            if let Some(err) = error.downcast_mut::<wast::Error>() {
                err.set_path(test);
                err.set_text(contents);
            }
            s.push_str("\n\n\t--------------------------------\n\n\t");
            s.push_str(&format!("{:?}", error).replace("\n", "\n\t"));
        }
        bail!("{}", s)
    }

    fn test_wast_directive(&self, test: &Path, directive: WastDirective) -> Result<()> {
        // Only test parsing and encoding of modules that wabt doesn't support
        let skip_verify = test.iter().any(|t| t == "function-references");

        match directive {
            WastDirective::Module(mut module) => {
                let actual = module.encode()?;
                self.bump_ntests(); // testing encode
                if skip_verify {
                    return Ok(());
                }
                let test_roundtrip = match module.kind {
                    ModuleKind::Text(_) => true,

                    // Don't test the wasmprinter round trip since these bytes
                    // may not be in their canonical form (didn't come from teh
                    // `wat` crate).
                    //
                    // Additionally don't test against the expected value since
                    // the encoding here is trivial and otherwise this disagrees
                    // with wabt which does further parsing.
                    ModuleKind::Binary(_) => false,
                };
                self.test_wasm(test, &actual, test_roundtrip)
                    .context("failed testing wasm binary produced by `wast`")?;
            }

            WastDirective::QuoteModule { source, span: _ } => {
                if skip_verify {
                    return Ok(());
                }
                self.test_quote_module(test, &source)?;
            }

            WastDirective::AssertMalformed {
                span: _,
                module: QuoteModule::Quote(source),
                message,
            } => {
                if skip_verify {
                    return Ok(());
                }
                let result = self.test_quote_module(test, &source);
                match result {
                    Ok(_) => bail!(
                        "parsed successfully but should have failed with: {}",
                        message,
                    ),
                    Err(e) => {
                        if error_matches(&format!("{:?}", e), message) {
                            self.bump_ntests();
                            return Ok(());
                        }
                        bail!("bad error: {:?}\nshould have failed with: {:?}", e, message);
                    }
                }
            }
            WastDirective::AssertInvalid {
                module,
                message,
                span: _,
            } => {
                self.bump_ntests();
                if skip_verify {
                    return Ok(());
                }

                // Our error for these tests is happening as a parser error of
                // the text file, not a validation error of the binary. It's not
                // really worth it contorting ourselves to have the exact same
                // location of error, so skip these tests.
                if message == "memory size must be at most 65536 pages (4GiB)" {
                    return Ok(());
                }

                let wasm = match module {
                    QuoteModule::Module(mut m) => m.encode()?,
                    QuoteModule::Quote(list) => self.parse_quote_module(test, &list)?,
                };
                let e = self.test_wasm_invalid(test, &wasm)?;
                if !error_matches(e.message(), message) {
                    bail!(
                        "expected \"{spec}\", got \"{actual}\"",
                        spec = message,
                        actual = e,
                    );
                }
            }

            WastDirective::AssertMalformed {
                module: QuoteModule::Module(mut module),
                ..
            } => {
                let wasm = module.encode()?;
                self.bump_ntests();
                if skip_verify {
                    return Ok(());
                }
                let e = self.test_wasm_invalid(test, &wasm)?;
                // TODO: Check the assert_malformed message
                drop(e);
            }

            _ => {}
        }
        Ok(())
    }

    fn test_wasm_valid(&self, test: &Path, contents: &[u8]) -> Result<()> {
        self.wasmparser_validator_for(test).validate_all(contents)?;
        self.bump_ntests();
        Ok(())
    }

    fn test_wasm_invalid(&self, test: &Path, contents: &[u8]) -> Result<BinaryReaderError> {
        match self.test_wasm_valid(test, contents) {
            Ok(()) => bail!("no wasm validation error found"),
            Err(e) => {
                let err = e.downcast()?;
                self.bump_ntests();
                Ok(err)
            }
        }
    }

    fn parse_quote_module(&self, test: &Path, source: &[&[u8]]) -> Result<Vec<u8>> {
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

        // TODO: when memory64 merges into the proper spec then this should be
        // removed since it will presumably no longer be a text-format error but
        // rather a validation error. Currently all non-memory64 proposals
        // assert that this offset is a text-parser error, whereas with memory64
        // support that error is deferred until later.
        if ret.contains("offset=4294967296") && !test.iter().any(|t| t == "memory64") {
            bail!("i32 constant out of bounds");
        }
        Ok(wat.module.encode()?)
    }

    fn test_quote_module(&self, test: &Path, source: &[&[u8]]) -> Result<()> {
        let wasm = self.parse_quote_module(test, source)?;
        self.bump_ntests();
        self.test_wasm(test, &wasm, true)?;
        Ok(())
    }

    /// Compare the `actual` and `expected`, asserting that they are the same.
    ///
    /// If they are not equal this attempts to produce as nice of an error
    /// message as it can to help narrow down on where the differences lie.
    fn binary_compare(&self, actual: &[u8], expected: &[u8], expected_is_wabt: bool) -> Result<()> {
        use wasmparser::*;

        // I tried for a bit but honestly couldn't figure out a great way to match
        // wabt's encoding of the name section. Just remove it from our asserted
        // sections and don't compare against wabt's.
        let actual = if expected_is_wabt {
            remove_name_section(actual)
        } else {
            actual.to_vec()
        };

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

        if let Ok(actual) = wasmparser_dump::dump_wasm(&actual) {
            if let Ok(expected) = wasmparser_dump::dump_wasm(&expected) {
                let mut actual = actual.lines();
                let mut expected = expected.lines();
                let mut differences = 0;
                let mut last_dots = false;
                while differences < 5 {
                    let actual_state = match actual.next() {
                        Some(s) => s,
                        None => break,
                    };
                    let expected_state = match expected.next() {
                        Some(s) => s,
                        None => break,
                    };

                    if actual_state == expected_state {
                        if differences > 0 && !last_dots {
                            msg.push_str(&format!(" ...\n"));
                            last_dots = true;
                        }
                        continue;
                    }
                    last_dots = false;

                    if differences == 0 {
                        msg.push_str("\n\n");
                    }
                    msg.push_str(&format!("- {}\n", expected_state));
                    msg.push_str(&format!("+ {}\n", actual_state));
                    differences += 1;
                }
            }
        }

        bail!("{}", msg);

        fn remove_name_section(bytes: &[u8]) -> Vec<u8> {
            let mut p = Parser::new(0);
            let mut offset = 0;
            loop {
                let start = offset;
                let payload = match p.parse(&bytes[offset..], true) {
                    Ok(Chunk::Parsed { consumed, payload }) => {
                        offset += consumed;
                        payload
                    }
                    _ => break,
                };
                match payload {
                    Payload::CustomSection { name: "name", .. } => {
                        let mut bytes = bytes.to_vec();
                        bytes.drain(start..offset);
                        return bytes;
                    }
                    Payload::End => break,
                    _ => {}
                }
            }
            return bytes.to_vec();
        }
    }

    fn wasmparser_validator_for(&self, test: &Path) -> Validator {
        let mut features = WasmFeatures {
            threads: true,
            reference_types: true,
            simd: true,
            relaxed_simd: true,
            exceptions: true,
            bulk_memory: true,
            tail_call: true,
            module_linking: false,
            deterministic_only: false,
            multi_value: true,
            multi_memory: true,
            memory64: true,
            extended_const: true,
        };
        for part in test.iter().filter_map(|t| t.to_str()) {
            match part {
                "testsuite" => features = WasmFeatures::default(),
                "missing-features" => {
                    features = WasmFeatures::default();
                    features.simd = false;
                    features.reference_types = false;
                }
                "threads" => {
                    features.threads = true;
                    features.bulk_memory = false;
                    features.reference_types = false;
                }
                "simd" => features.simd = true,
                "exception-handling" => features.exceptions = true,
                "tail-call" => features.tail_call = true,
                "memory64" => features.memory64 = true,
                "module-linking" => features.module_linking = true,
                "multi-memory" => features.multi_memory = true,
                "extended-const" => features.extended_const = true,
                _ => {}
            }
        }
        let mut ret = Validator::new();
        ret.wasm_features(features);
        return ret;
    }

    fn bump_ntests(&self) {
        self.ntests.fetch_add(1, SeqCst);
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

    if message == "unknown memory" {
        return error.contains("no linear memories are present");
    }

    if message == "bad magic" {
        return error.contains("Bad magic number");
    }

    return false;
}
