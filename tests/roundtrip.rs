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
//! dropping tests into the `tests/local` directory or updating the
//! `tests/testsuite` submodule which is the upstream git repository of the
//! spec tests, including proposals.
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
use wast::core::{Module, ModuleKind};
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
    if !Path::new("tests/testsuite").exists() {
        panic!("submodules need to be checked out");
    }
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
                Some("wast") | Some("wat") => {}
                Some("wasm") => panic!(
                    "use `*.wat` or `*.wast` instead of binaries: {:?}",
                    f.path()
                ),
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

    // todo!("component-model")
    if test.to_str().unwrap().contains("component-model") {
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
            Some("wat") => self.test_wat(test),
            Some("wast") => self.test_wast(test, contents),
            _ => bail!("unknown file extension {:?}", test),
        };
        result.with_context(|| format!("failed test: {}", test.display()))
    }

    fn test_wat(&self, test: &Path) -> Result<()> {
        // First up test that we can parse the file and convert it to a binary
        // wasm file.
        let binary = wat::parse_file(test)?;
        self.bump_ntests();
        self.test_wasm(test, &binary, true)
            .context("failed testing the binary output of `wat`")?;
        Ok(())
    }

    fn test_wasm(&self, test: &Path, contents: &[u8], test_roundtrip: bool) -> Result<()> {
        self.test_wasm_valid(test, contents)
            .context("wasm isn't valid")?;

        // Test that we can print these bytes.
        let string = wasmprinter::print_bytes(contents).context("failed to print wasm")?;
        self.bump_ntests();

        // If we can, convert the string back to bytes and assert it has the
        // same binary representation.
        if test_roundtrip {
            let binary2 =
                wat::parse_str(&string).context("failed to parse `wat` from `wasmprinter`")?;
            self.bump_ntests();
            self.binary_compare(&binary2, contents)
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
        // Only test parsing and encoding of modules which wasmparser doesn't
        // support test (basically just test `wast`, nothing else)
        let skip_verify = test.iter().any(|t| t == "function-references" || t == "gc");

        match directive {
            WastDirective::Wat(mut module) => {
                let actual = module.encode()?;
                self.bump_ntests(); // testing encode
                if skip_verify {
                    return Ok(());
                }
                let test_roundtrip = match module {
                    // Don't test the wasmprinter round trip since these bytes
                    // may not be in their canonical form (didn't come from the
                    // `wat` crate).
                    QuoteWat::Wat(Wat {
                        module:
                            Module {
                                kind: ModuleKind::Binary(_),
                                ..
                            },
                        ..
                    }) => false,

                    _ => true,
                };
                self.test_wasm(test, &actual, test_roundtrip)
                    .context("failed testing wasm binary produced by `wast`")?;
            }

            WastDirective::AssertMalformed {
                span: _,
                mut module,
                message,
            }
            | WastDirective::AssertInvalid {
                mut module,
                message,
                span: _,
            } => {
                let result = module.encode().map_err(|e| e.into()).and_then(|wasm| {
                    // TODO: when memory64 merges into the proper spec then this
                    // should be removed since it will presumably no longer be a
                    // text-format error but rather a validation error.
                    // Currently all non-memory64 proposals assert that this
                    // offset is a text-parser error, whereas with memory64
                    // support that error is deferred until later.
                    if !test.iter().any(|t| t == "memory64") {
                        if let QuoteWat::Quote(_, src) = module {
                            if src
                                .iter()
                                .filter_map(|(_, s)| str::from_utf8(s).ok())
                                .any(|s| s.contains("offset=4294967296"))
                            {
                                bail!("i32 constant out of bounds");
                            }
                        }
                    }

                    self.test_wasm_valid(test, &wasm)
                });
                if skip_verify {
                    return Ok(());
                }
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

            // This test suite doesn't actually execute any wasm code, so ignore
            // all of these assertions.
            WastDirective::Register { .. }
            | WastDirective::Invoke(_)
            | WastDirective::AssertTrap { .. }
            | WastDirective::AssertReturn { .. }
            | WastDirective::AssertExhaustion { .. }
            | WastDirective::AssertUnlinkable { .. }
            | WastDirective::AssertException { .. } => {}
        }
        Ok(())
    }

    fn test_wasm_valid(&self, test: &Path, contents: &[u8]) -> Result<()> {
        self.wasmparser_validator_for(test).validate_all(contents)?;
        self.bump_ntests();
        Ok(())
    }

    /// Compare the `actual` and `expected`, asserting that they are the same.
    ///
    /// If they are not equal this attempts to produce as nice of an error
    /// message as it can to help narrow down on where the differences lie.
    fn binary_compare(&self, actual: &[u8], expected: &[u8]) -> Result<()> {
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
            component_model: false,
            deterministic_only: false,
            multi_value: true,
            multi_memory: true,
            memory64: true,
            extended_const: true,
            saturating_float_to_int: true,
            sign_extension: true,
            mutable_global: true,
        };
        for part in test.iter().filter_map(|t| t.to_str()) {
            match part {
                "testsuite" => features = WasmFeatures::default(),
                "missing-features" => {
                    features = WasmFeatures::default();
                    features.simd = false;
                    features.reference_types = false;
                    features.multi_value = false;
                    features.sign_extension = false;
                    features.saturating_float_to_int = false;
                    features.mutable_global = false;
                    features.bulk_memory = false;
                }
                "threads" => {
                    features.threads = true;
                    features.bulk_memory = false;
                    features.reference_types = false;
                }
                "simd" => features.simd = true,
                "exception-handling" => features.exceptions = true,
                "tail-call" => features.tail_call = true,
                "memory64" => {
                    features.memory64 = true;
                    features.reference_types = false;
                }
                "component-model" => features.component_model = true,
                "multi-memory" => features.multi_memory = true,
                "extended-const" => features.extended_const = true,
                _ => {}
            }
        }
        Validator::new_with_features(features)
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

    if message == "malformed UTF-8 encoding" {
        return error.contains("invalid UTF-8 encoding");
    }

    if message == "duplicate identifier" {
        return error.contains("duplicate") && error.contains("identifier");
    }

    if message == "unknown memory" {
        return error.contains("no linear memories are present");
    }

    // wasmparser differentiates these cases, the spec interpreter apparently
    // doesn't
    if message == "function and code section have inconsistent lengths" {
        return error.contains("code section without function section");
    }

    // This test in binary.wast uses a section id implemented by other
    // proposals, so it's valid from wasmparser's point of view
    if message == "malformed section id" {
        return error.contains("unexpected end-of-file");
    }

    // The spec interpreter will apparently read beyond the limits of a section
    // as defined by its size to parse a function, wasmparser doesn't do that.
    // That means that the error message here is legitimately different.
    if message == "section size mismatch" {
        return error.contains("control frames remain at end of function");
    }

    if message == "malformed import kind" {
        return error.contains("invalid external kind")
            // wasmparser understands more import kinds than the default spec
            // interpreter
            || error.contains("unexpected end-of-file");
    }

    if message == "integer representation too long" {
        // wasmparser implements more features than the default spec
        // interpreter, so these error looks different.
        return error.contains("invalid memory limits flags")
            || error.contains("invalid table resizable limits flags")
            // different error message for types
            || error.contains("invalid leading byte")
            // the spec interpreter will read past section boundaries when
            // decoding, wasmparser won't, producing different errors.
            || error.contains("unexpected end-of-file")
            || error.contains("malformed section id");
    }

    if message == "integer too large" {
        // wasmparser implements more features than the default spec
        // interpreter, so these error looks different.
        return error.contains("threads must be enabled for shared memories")
            || error.contains("invalid table resizable limits flags")
            // honestly this feels like the spec interpreter is just weird
            || error.contains("unexpected end-of-file")
            // This mostly comes from the memory64/binary-leb128.wast test file
            // which I think is largely busted as it looks like a bunch of lebs
            // were inflated to a larger size while not updating the binary
            // encoding of the size of the section.
            || error.contains("invalid var_u32: integer representation too long")
            || error.contains("malformed section id");
    }

    // wasmparser blames a truncated file here, the spec interpreter blames the
    // section counts/lengths.
    if message == "length out of bounds" || message == "unexpected end of section or function" {
        return error.contains("unexpected end-of-file")
            || error.contains("control frames remain at end of function");
    }

    // this feels like a busted test in the spec suite
    if message == "unexpected end" {
        return error.contains("type index out of bounds");
    }

    if message == "unexpected content after last section" {
        return error.contains("section out of order");
    }

    if message == "malformed limits flags" {
        return error.contains("invalid memory limits flags");
    }

    if message == "zero flag expected" {
        return error.contains("zero byte expected")
            // wasmparser defers some of these errors to validation
            || error.contains("trailing bytes at end of section");
    }

    if message == "junk after last section" {
        return error.contains("section out of order");
    }

    // Our error for these tests is happening as a parser error of
    // the text file, not a validation error of the binary.
    if message == "memory size must be at most 65536 pages (4GiB)" {
        return error.contains("invalid u32 number: constant out of range");
    }

    return false;
}
