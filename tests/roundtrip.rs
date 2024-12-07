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

use anyhow::{anyhow, bail, Context, Result};
use libtest_mimic::{Arguments, FormatSetting, Trial};
use rayon::prelude::*;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::str;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use std::sync::Arc;
use wasm_encoder::reencode::{Reencode, ReencodeComponent, RoundtripReencoder};
use wasmparser::*;
use wasmprinter::PrintFmtWrite;
use wast::component::{Component, ComponentKind};
use wast::core::{Module, ModuleKind};
use wast::lexer::Lexer;
use wast::parser::ParseBuffer;
use wast::{parser, QuoteWat, Wast, WastDirective, Wat};

fn main() {
    env_logger::init();

    let tests = find_tests();
    let bless = std::env::var_os("BLESS").is_some();

    let state = Arc::new(TestState::default());
    let mut trials = Vec::new();
    for test in tests {
        let contents = std::fs::read(&test).unwrap();
        let skip = skip_test(&test, &contents);
        let trial = Trial::test(format!("{test:?}"), {
            let state = state.clone();
            move || {
                state
                    .run_test(&test, &contents)
                    .map_err(|e| format!("{e:?}").into())
            }
        })
        .with_ignored_flag(skip);
        trials.push(trial);
    }

    let mut args = Arguments::from_args();
    if args.format.is_none() {
        args.format = Some(FormatSetting::Terse);
    }
    if cfg!(target_family = "wasm") && !cfg!(target_feature = "atomics") {
        args.test_threads = Some(1);
    }
    if bless && !args.list {
        drop(std::fs::remove_dir_all("tests/snapshots"));
    }
    libtest_mimic::run(&args, trials).exit();
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
    let _ = contents;
    test.iter().any(|p| p == "exception-handling") && test.iter().any(|p| p == "legacy")
}

fn skip_validation(_test: &Path) -> bool {
    false
}

#[derive(Default)]
struct TestState {
    ntests: AtomicUsize,
}

impl TestState {
    fn run_test(&self, test: &Path, contents: &[u8]) -> Result<()> {
        let result =
            match std::panic::catch_unwind(|| match test.extension().and_then(|s| s.to_str()) {
                Some("wat") => self.test_wat(test),
                Some("wast") => self.test_wast(test, contents),
                _ => bail!("unknown file extension {:?}", test),
            }) {
                Ok(result) => result,
                Err(e) => Err(anyhow!("panicked: {e:?}")),
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

        // Test that we can print these bytes with instructions in folded form.
        let mut test_folded = true;
        let mut snapshot_folded = false;
        for part in test.iter().filter_map(|t| t.to_str()) {
            match part {
                "legacy-exceptions" => test_folded = false,
                "folding" => snapshot_folded = true,
                _ => (),
            }
        }

        let mut folded_string = String::new();
        if test_folded {
            let mut folding_printer = wasmprinter::Config::new();
            folding_printer.fold_instructions(true);
            folding_printer
                .print(contents, &mut PrintFmtWrite(&mut folded_string))
                .context("failed to print wasm in folded form")?;
            self.bump_ntests();

            if snapshot_folded {
                self.snapshot("print-folded", test, &folded_string)
                    .context("failed to validate the `print-folded` snapshot")?;
                self.bump_ntests();
            }
        }

        // If we can, convert the string back to bytes and assert it has the
        // same binary representation.
        if test_roundtrip {
            let binary2 =
                wat::parse_str(&string).context("failed to parse `wat` from `wasmprinter`")?;
            self.bump_ntests();
            self.binary_compare(&binary2, contents)
                .context("failed to compare original `wat` with roundtrip `wat`")?;

            if test_folded {
                let binary2f = wat::parse_str(&folded_string)
                    .context("failed to parse folded `wat` from `wasmprinter`")?;
                self.bump_ntests();
                self.binary_compare(&binary2f, contents)
                    .context("failed to compare original `wat` with roundtrip folded `wat`")?;
            }

            if wasmparser::Parser::is_component(contents) {
                let mut reencode = Default::default();
                RoundtripReencoder
                    .parse_component(&mut reencode, wasmparser::Parser::new(0), contents)
                    .context("failed to reencode module")?;
                self.binary_compare(&reencode.finish(), contents)
                    .context("failed to compare reencoded module with original encoding")?;
            } else {
                let mut reencode = Default::default();
                RoundtripReencoder
                    .parse_core_module(&mut reencode, wasmparser::Parser::new(0), contents)
                    .context("failed to reencode module")?;
                self.binary_compare(&reencode.finish(), contents)
                    .context("failed to compare reencoded module with original encoding")?;
            }
        }

        self.test_pretty_whitespace(&string)?;
        self.test_pretty_whitespace(&folded_string)
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
    fn test_pretty_whitespace(&self, string: &str) -> Result<()> {
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
        self.test_json_from_wast(test)
            .context("failed to run `json-from-wast` cli subcommand")?;
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
            WastDirective::Module(mut module) | WastDirective::ModuleDefinition(mut module) => {
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
                    }))
                    | QuoteWat::Wat(Wat::Component(Component {
                        kind: ComponentKind::Binary(_),
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
                        if error_matches(test, &format!("{:?}", e), message) {
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
            WastDirective::ModuleInstance { .. }
            | WastDirective::Register { .. }
            | WastDirective::Invoke(_)
            | WastDirective::AssertTrap { .. }
            | WastDirective::AssertReturn { .. }
            | WastDirective::AssertExhaustion { .. }
            | WastDirective::AssertUnlinkable { .. }
            | WastDirective::AssertException { .. }
            | WastDirective::AssertSuspension { .. }
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
                anyhow::bail!(
                    "snapshot does not match the expected result, try `env BLESS=1`\n{}",
                    pretty_assertions::StrComparison::new(&snapshot, &contents)
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

        if log::log_enabled!(log::Level::Debug) {
            static COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
            let i = COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel);

            let expected_path = format!("expected{i}.wasm");
            log::debug!("Writing expected Wasm to: {expected_path}");
            let _ = std::fs::write(expected_path, expected);

            let actual_path = format!("actual{i}.wasm");
            log::debug!("Writing actual Wasm to: {actual_path}");
            let _ = std::fs::write(actual_path, actual);
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
            } else {
                msg.push_str("\nfailed to dump expected");
            }
        } else {
            msg.push_str("\nfailed to dump actual");
        }

        bail!("{}", msg);
    }

    fn dump(&self, bytes: &[u8]) -> Result<String> {
        let mut dump = self
            .wasm_tools()
            .arg("dump")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;
        dump.stdin.take().unwrap().write_all(bytes)?;
        let mut stdout = String::new();
        dump.stdout.take().unwrap().read_to_string(&mut stdout)?;
        let status = dump.wait()?;
        if !status.success() {
            bail!("dump subcommand failed: {status}");
        }
        Ok(stdout)
    }

    fn test_json_from_wast(&self, path: &Path) -> Result<()> {
        // This has an `assert_invalid` which should be `assert_malformed`, so
        // skip it.
        if path.ends_with("gc-subtypes-invalid.wast") {
            return Ok(());
        }

        // No processes on wasm
        if cfg!(target_family = "wasm") {
            return Ok(());
        }

        // Generate the same output on windows and unix
        let path = path.to_str().unwrap().replace("\\", "/");

        let mut cmd = self.wasm_tools();
        let td = tempfile::TempDir::new()?;
        cmd.arg("json-from-wast")
            .arg(&path)
            .arg("--pretty")
            .arg("--wasm-dir")
            .arg(td.path());
        let output = cmd.output()?;
        let stdout = String::from_utf8_lossy(&output.stdout);
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            bail!("failed to run {cmd:?}\nstdout: {stdout}\nstderr: {stderr}");
        }
        self.snapshot("json", path.as_ref(), &stdout)
            .context("failed to validate the `json-from-wast` snapshot")?;
        Ok(())
    }

    fn wasm_tools(&self) -> Command {
        Command::new(env!("CARGO_BIN_EXE_wasm-tools"))
    }

    fn wasmparser_validator_for(&self, test: &Path) -> Validator {
        let mut features = WasmFeatures::all()
            & !WasmFeatures::SHARED_EVERYTHING_THREADS
            & !WasmFeatures::COMPONENT_MODEL
            & !WasmFeatures::COMPONENT_MODEL_NESTED_NAMES
            & !WasmFeatures::COMPONENT_MODEL_MORE_FLAGS
            & !WasmFeatures::COMPONENT_MODEL_MULTIPLE_RETURNS
            & !WasmFeatures::LEGACY_EXCEPTIONS;
        for part in test.iter().filter_map(|t| t.to_str()) {
            match part {
                "testsuite" => {
                    features = WasmFeatures::WASM2
                        | WasmFeatures::TAIL_CALL
                        | WasmFeatures::EXTENDED_CONST;
                }
                "missing-features" => {
                    features =
                        WasmFeatures::empty() | WasmFeatures::FLOATS | WasmFeatures::GC_TYPES;
                }
                "floats-disabled.wast" => features.remove(WasmFeatures::FLOATS),
                "gc-types-disabled.wast" => features.remove(WasmFeatures::GC_TYPES),
                "threads" => {
                    features.insert(WasmFeatures::THREADS);
                    features.remove(WasmFeatures::BULK_MEMORY);
                    features.remove(WasmFeatures::REFERENCE_TYPES);
                }
                "simd" => features.insert(WasmFeatures::SIMD),
                "exception-handling" => features.insert(WasmFeatures::EXCEPTIONS),
                "legacy-exceptions" => features.insert(WasmFeatures::LEGACY_EXCEPTIONS),
                "tail-call" => features.insert(WasmFeatures::TAIL_CALL),
                "memory64" => features.insert(WasmFeatures::MEMORY64 | WasmFeatures::WASM3),
                "component-model" => features.insert(WasmFeatures::COMPONENT_MODEL),
                "shared-everything-threads" => {
                    features.insert(WasmFeatures::COMPONENT_MODEL);
                    features.insert(WasmFeatures::SHARED_EVERYTHING_THREADS);
                }
                "multi-memory" => features.insert(WasmFeatures::MULTI_MEMORY),
                "extended-const" => features.insert(WasmFeatures::EXTENDED_CONST),
                "function-references" => features.insert(WasmFeatures::FUNCTION_REFERENCES),
                "relaxed-simd" => features.insert(WasmFeatures::RELAXED_SIMD),
                "reference-types" => features.insert(WasmFeatures::REFERENCE_TYPES),
                "gc" => {
                    features.insert(WasmFeatures::FUNCTION_REFERENCES);
                    features.insert(WasmFeatures::REFERENCE_TYPES);
                    features.insert(WasmFeatures::GC);
                }
                "custom-page-sizes" => {
                    features.insert(WasmFeatures::CUSTOM_PAGE_SIZES);
                    features.insert(WasmFeatures::MULTI_MEMORY);
                }
                "import-extended.wast" => {
                    features.insert(WasmFeatures::COMPONENT_MODEL_NESTED_NAMES);
                }
                "more-flags.wast" => {
                    features.insert(WasmFeatures::COMPONENT_MODEL_MORE_FLAGS);
                }
                "multiple-returns.wast" => {
                    features.insert(WasmFeatures::COMPONENT_MODEL_MULTIPLE_RETURNS);
                }
                "stack-switching" => {
                    features.insert(WasmFeatures::STACK_SWITCHING);
                }
                "wide-arithmetic" => {
                    features.insert(WasmFeatures::WIDE_ARITHMETIC);
                }
                "component-model-async" => {
                    features.insert(WasmFeatures::COMPONENT_MODEL);
                    features.insert(WasmFeatures::COMPONENT_MODEL_ASYNC);
                }
                _ => {}
            }
        }
        log::debug!("features for {} = {features:#?}", test.display());
        Validator::new_with_features(features)
    }

    fn bump_ntests(&self) {
        self.ntests.fetch_add(1, SeqCst);
    }
}

fn error_matches(test: &Path, error: &str, message: &str) -> bool {
    if error.contains(message) {
        return true;
    }

    // we are in control over all tests in `tests/local/*` so all the error
    // messages there should exactly match the `assert_invalid` or such
    if test.starts_with("tests/local") {
        return false;
    }

    // Historically wasm-tools tried to match the upstream error message. This
    // generally led to a large sequence of matches here which is not easy to
    // maintain and is particularly difficult when test suites and proposals
    // conflict with each other (e.g. one asserts one error message and another
    // asserts a different error message). Overall we didn't benefit a whole lot
    // from trying to match errors so just assume the error is roughly the same
    // and otherwise don't try to match it.
    true
}
