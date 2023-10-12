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
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::str;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use wasmparser::*;
use wast::core::{Module, ModuleKind};
use wast::lexer::Lexer;
use wast::parser::ParseBuffer;
use wast::{parser, QuoteWat, Wast, WastDirective, Wat};

fn main() {
    let tests = find_tests();
    let filter = std::env::args().nth(1);
    let bless = std::env::var_os("BLESS").is_some();
    if bless {
        drop(std::fs::remove_dir_all("tests/snapshots"));
    }

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
        .filter_map(|(test, contents)| {
            let start = std::time::Instant::now();
            let result = state.run_test(test, contents).err();
            if start.elapsed().as_secs() > 2 {
                println!("{test:?} SLOW");
            }
            result
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
        state.ntests.load(SeqCst)
    );
}

/// Recursively finds all tests in a whitelisted set of directories which we
/// then load up and test in parallel.
fn find_tests() -> Vec<PathBuf> {
    let mut tests = Vec::new();
    let test_suite = Path::new("tests/testsuite");
    if !test_suite.exists()
        || std::fs::read_dir(test_suite)
            .map(|mut d| d.next().is_none())
            .unwrap_or(true)
    {
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
        // This is an empty file which currently doesn't parse
        "multi-memory/memory_copy1.wast",
    ];
    let test_path = test.to_str().unwrap().replace("\\", "/"); // for windows paths
    if broken.iter().any(|x| test_path.contains(x)) {
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

fn skip_validation(test: &Path) -> bool {
    let broken = &[
        "gc/gc-array.wat",
        "gc/gc-rec-sub.wat",
        "gc/gc-struct.wat",
        "/proposals/gc/array.wast",
        "/proposals/gc/array_copy.wast",
        "/proposals/gc/array_fill.wast",
        "/proposals/gc/array_init_data.wast",
        "/proposals/gc/array_init_elem.wast",
        "/proposals/gc/binary-gc.wast",
        "/proposals/gc/br_on_cast.wast",
        "/proposals/gc/br_on_cast_fail.wast",
        "/proposals/gc/data.wast",
        "/proposals/gc/elem.wast",
        "/proposals/gc/extern.wast",
        "/proposals/gc/global.wast",
        "/proposals/gc/i31.wast",
        "/proposals/gc/ref_cast.wast",
        "/proposals/gc/ref_eq.wast",
        "/proposals/gc/ref_test.wast",
        "/proposals/gc/struct.wast",
        "/proposals/gc/type-equivalence.wast",
        "/proposals/gc/type-rec.wast",
        "/proposals/gc/type-subtyping.wast",
        "/exnref/exnref.wast",
        "/exnref/throw_ref.wast",
        "/exnref/try_table.wast",
    ];
    let test_path = test.to_str().unwrap().replace("\\", "/"); // for windows paths
    if broken.iter().any(|x| test_path.contains(x)) {
        return true;
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

        if skip_validation(test) {
            return Ok(());
        }

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
        // Snapshot these bytes.
        self.snapshot("print", test, &string)
            .context("failed to validate the `print` snapshot")?;
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

        // Test that the `wasmprinter`-printed bytes have "pretty" whitespace
        // which means that all whitespace is either categorized as leading
        // whitespace or a single space. Examples of "bad whitespace" are:
        //
        // * trailing whitespace at the end of a line
        // * two spaces in a row
        //
        // Both of these cases indicate possible bugs in `wasmprinter` itself
        // which while they don't actually affect the meaning they do "affect"
        // humans reading the output.
        for token in wast::lexer::Lexer::new(&string)
            .allow_confusing_unicode(true)
            .iter(0)
        {
            let token = token?;
            let ws = match token.kind {
                wast::lexer::TokenKind::Whitespace => token.src(&string),
                _ => continue,
            };
            if ws.starts_with("\n") || ws == " " {
                continue;
            }
            let offset = ws.as_ptr() as usize - string.as_ptr() as usize;
            let span = wast::token::Span::from_offset(offset);
            let msg = format!("found non-one-length whitespace in `wasmprinter` output: {ws:?}");
            let mut err = wast::Error::new(span, msg);
            err.set_text(&string);
            return Err(err.into());
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
            .enumerate()
            .filter_map(|(index, directive)| {
                let span = directive.span();
                self.test_wast_directive(test, directive, index)
                    .with_context(|| {
                        let (line, col) = span.linecol_in(contents);
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

    fn test_wast_directive(&self, test: &Path, directive: WastDirective, idx: usize) -> Result<()> {
        match directive {
            WastDirective::Wat(mut module) => {
                let actual = module.encode()?;
                self.bump_ntests(); // testing encode

                if skip_validation(test) {
                    // Verify that we can parse the wat, but otherwise do nothing.
                    return Ok(());
                }

                let test_roundtrip = match module {
                    // Don't test the wasmprinter round trip since these bytes
                    // may not be in their canonical form (didn't come from the
                    // `wat` crate).
                    QuoteWat::Wat(Wat::Module(Module {
                        kind: ModuleKind::Binary(_),
                        ..
                    })) => false,

                    _ => true,
                };

                let mut test_path = test.to_path_buf();
                test_path.push(idx.to_string());

                self.test_wasm(&test_path, &actual, test_roundtrip)
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
                if skip_validation(test) {
                    return Ok(());
                }

                let result = module.encode().map_err(|e| e.into()).and_then(|wasm| {
                    // TODO: when memory64 merges into the proper spec then this
                    // should be removed since it will presumably no longer be a
                    // text-format error but rather a validation error.
                    // Currently all non-memory64 proposals assert that this
                    // offset is a text-parser error, whereas with memory64
                    // support that error is deferred until later.
                    if !test.iter().any(|t| t == "memory64") {
                        if let QuoteWat::QuoteModule(_, src) = module {
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
                match result {
                    Ok(_) => bail!(
                        "encoded and validated successfully but should have failed with: {}",
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

            WastDirective::Thread(thread) => {
                for (i, directive) in thread.directives.into_iter().enumerate() {
                    self.test_wast_directive(test, directive, idx * 1000 + i)?;
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
            | WastDirective::AssertException { .. }
            | WastDirective::Wait { .. } => {}
        }
        Ok(())
    }

    fn test_wasm_valid(&self, test: &Path, contents: &[u8]) -> Result<()> {
        self.wasmparser_validator_for(test).validate_all(contents)?;
        self.bump_ntests();
        Ok(())
    }

    /// Compare the test result with a snapshot stored in the repository.
    ///
    /// Works great for tools like wasmprinter for which having a nice overview of what effect the
    /// changes cause.
    fn snapshot(&self, kind: &str, path: &Path, contents: &str) -> Result<()> {
        let contents = contents.replace("\r\n", "\n");
        let bless = std::env::var_os("BLESS").is_some();
        let snapshot_dir = ["tests", "snapshots"]
            .into_iter()
            .collect::<std::path::PathBuf>();
        let test_name = path
            .iter()
            .skip_while(|&c| c != std::ffi::OsStr::new("tests"))
            .skip(1)
            .collect::<std::path::PathBuf>();
        let mut snapshot_name = test_name.into_os_string();
        snapshot_name.push(".");
        snapshot_name.push(kind);
        let snapshot_path = snapshot_dir.join(snapshot_name);
        if bless {
            std::fs::create_dir_all(snapshot_path.parent().unwrap()).with_context(|| {
                format!("could not create the snapshot dir {:?}", snapshot_path)
            })?;
            std::fs::write(&snapshot_path, contents).with_context(|| {
                format!("could not write out the snapshot to {:?}", snapshot_path)
            })?;
        } else {
            let snapshot = std::fs::read(snapshot_path)
                .context("could not read the snapshot, try `env BLESS=1`")?;
            let snapshot =
                std::str::from_utf8(&snapshot).context("can't decode snapshot as utf-8")?;
            // Handle git possibly doing some newline shenanigans on windows.
            let snapshot = snapshot.replace("\r\n", "\n");
            if snapshot != contents {
                let mut result = String::with_capacity(snapshot.len());
                for diff in diff::lines(&snapshot, &contents) {
                    match diff {
                        diff::Result::Left(s) => {
                            result.push_str("-");
                            result.push_str(s);
                        }
                        diff::Result::Right(s) => {
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
                anyhow::bail!(
                    "snapshot does not match the expected result, try `env BLESS=1`\n{}",
                    result
                );
            }
        }
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

        if let Ok(actual) = self.dump(&actual) {
            if let Ok(expected) = self.dump(&expected) {
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

    fn dump(&self, bytes: &[u8]) -> Result<String> {
        let mut dump = Command::new(env!("CARGO_BIN_EXE_wasm-tools"))
            .arg("dump")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;
        dump.stdin.take().unwrap().write_all(bytes)?;
        let mut stdout = String::new();
        dump.stdout.take().unwrap().read_to_string(&mut stdout)?;
        if dump.wait()?.success() {
            bail!("dump subcommand failed");
        }
        Ok(stdout)
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
            floats: true,
            multi_value: true,
            multi_memory: true,
            memory64: true,
            extended_const: true,
            saturating_float_to_int: true,
            sign_extension: true,
            mutable_global: true,
            function_references: true,
            memory_control: true,
            gc: true,
            component_model_values: true,
        };
        for part in test.iter().filter_map(|t| t.to_str()) {
            match part {
                "testsuite" => {
                    features = WasmFeatures::default();

                    // NB: when these proposals are merged upstream in the spec
                    // repo then this should be removed. Currently this hasn't
                    // happened so this is required to get tests passing for
                    // when these proposals are enabled by default.
                    features.multi_memory = false;
                    features.threads = false;
                }
                "missing-features" => {
                    features = WasmFeatures::default();
                    features.simd = false;
                    features.reference_types = false;
                    features.multi_value = false;
                    features.sign_extension = false;
                    features.saturating_float_to_int = false;
                    features.mutable_global = false;
                    features.bulk_memory = false;
                    features.function_references = false;
                    features.gc = false;
                    features.component_model_values = false;
                }
                "floats-disabled.wast" => features.floats = false,
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
                "function-references" => features.function_references = true,
                "relaxed-simd" => features.relaxed_simd = true,
                "reference-types" => features.reference_types = true,
                "gc" => {
                    features.function_references = true;
                    features.gc = true;
                }
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
        return error.contains("invalid leading byte")
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
            || error.contains("control frames remain at end of function")
            // This is the same case as "unexpected end" (below) but in
            // function-references fsr it includes "of section or function"
            || error.contains("type index out of bounds");
    }

    // binary.wast includes a test in which a 0b (End) is eaten by a botched
    // br_table.  The test assumes that the parser (not the validator) errors on
    // a missing End before failing to validate the botched instruction.  However
    // wasmparser fails to validate the botched instruction first
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

    if message == "illegal opcode" {
        // The test suite includes "bad opcodes" that later became valid opcodes
        // (0xd4, function references proposal). However, they are still not
        // constant expressions, so we can sidestep by checking for that error
        // instead
        return error.contains("constant expression required")
            // The test suite contains a test with a global section where the
            // init expression is truncated and doesn't have an "end"
            // instruction. That's reported with wasmparser as end-of-file
            // because the end of the section was reached while the spec
            // interpreter says "illegal opcode".
            || error.contains("unexpected end-of-file");
    }
    if message == "unknown global" {
        return error.contains("global.get of locally defined global");
    }

    if message == "immutable global" {
        return error.contains("global is immutable");
    }

    if message == "sub type" {
        return error.contains("subtype");
    }

    return false;
}
