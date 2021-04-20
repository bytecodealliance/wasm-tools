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
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use wasmparser::*;
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
        // anywhere else, and I'm not entirely certain if it's vaild, and for now I
        // don't feel like filing an issue or adding parsing for this.
        "roundtrip/table-init-index.txt",
        "dump/reference-types.txt",
        "interp/reference-types.txt",
        "expr/reference-types.txt",
        // This test is skipped for now due to a delegate printing bug in wabt.
        "parse/expr/try-delegate.txt",
        // Skipped until (WebAssembly/wabt#1605) is merged.
        "typecheck/delegate.txt",
        // Usage of `assert_invalid` which should be `assert_malformed`
        "testsuite/proposals/memory64/memory.wast",
        "testsuite/proposals/memory64/address.wast",
    ];
    if broken.iter().any(|x| test.ends_with(x)) {
        return true;
    }

    // FIXME(WebAssembly/wabt#1404) - wast2json infinite loops here on macos
    if test.ends_with("annotations.wast") {
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
    wabt_available: AtomicUsize,
}

struct Wast2Json {
    _td: tempfile::TempDir,
    modules: Vec<PathBuf>,
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

        // Next up, if enabled, we execute `wat2wasm` to make sure `wat
        // `produces the same binary encoding.
        //
        // Currently our encoding of tests two tests differs from wabt, but
        // they're invalid anyway so it's not that worrisome.
        if !test.ends_with("invalid-data-segment-offset.txt")
            && !test.ends_with("invalid-elem-segment-offset.txt")
        {
            if let Some(expected) = self.wat2wasm(&test)? {
                self.binary_compare(&binary, &expected, true)
                    .context("`wat` doesn't match wabt's `wat2wasm`")?;
            }
        }

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
        if !test.ends_with("local/reloc.wasm")
            // FIXME(WebAssembly/wabt#1447)
            && !test.ends_with("bulk-memory-operations/binary.wast")
            && !test.ends_with("reference-types/binary.wast")
            && !test.ends_with("exception-handling/binary.wast")

            // not implemented in wabt
            && !test.iter().any(|t| t == "module-linking")
            && !test.ends_with("multi-memory.wast")
            && !test.ends_with("multi-memory64.wast")

            // FIXME(WebAssembly/wabt#1649)
            && !test.ends_with("local/simd.wat")
            && !test.ends_with("dump/simd-store-lane.txt")
            && !test.ends_with("dump/simd-load-lane.txt")
            && !test.ends_with("simd_load8_lane.wast")
            && !test.ends_with("simd_load16_lane.wast")
            && !test.ends_with("simd_load32_lane.wast")
            && !test.ends_with("simd_load64_lane.wast")
            && !test.ends_with("simd_store8_lane.wast")
            && !test.ends_with("simd_store16_lane.wast")
            && !test.ends_with("simd_store32_lane.wast")
            && !test.ends_with("simd_store64_lane.wast")

            // FIXME wabt doesn't print conflict or empty names in the same way
            // that we do.
            && !test.ends_with("local/names.wast")
        {
            if let Some(expected) = self.wasm2wat(contents)? {
                self.string_compare(&string, &expected)
                    .context("`wasmprinter` disagrees with `wabt`")?;
            }
        }

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

    fn test_wast_directive(
        &self,
        test: &Path,
        directive: WastDirective,
        expected: Option<&PathBuf>,
    ) -> Result<()> {
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
                    ModuleKind::Text(_) => {
                        if let Some(expected) = &expected {
                            // TODO: waiting on wabt to sync with the upstream
                            // simd spec to agree on encodings.
                            if !test.iter().any(|t| t == "simd") {
                                let expected = fs::read(expected)?;
                                self.binary_compare(&actual, &expected, true)
                                    .context("`wat` doesn't match output of wabt")?;
                            }
                        }
                        true
                    }

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
                self.parse_quote_module(test, &source)?;
            }

            WastDirective::AssertMalformed {
                span: _,
                module: QuoteModule::Quote(source),
                message,
            } => {
                if skip_verify {
                    return Ok(());
                }
                let result = self.parse_quote_module(test, &source);
                match result {
                    Ok(()) => bail!(
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
                mut module,
                message,
                span: _,
            } => {
                let wasm = module.encode()?;
                self.bump_ntests();
                if skip_verify {
                    return Ok(());
                }
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

    fn string_compare(&self, actual: &str, expected: &str) -> Result<()> {
        let actual = normalize(&actual);
        let expected = normalize(&expected);

        fn normalize(s: &str) -> String {
            let mut s = s.trim().to_string();

            // We seem to have different decimal float printing than wabt, and a
            // hand-check seems to show that they're equivalent just different
            // renderings. To paper over these inconsequential differences delete
            // these comments.
            while let Some(i) = s.find(" (;=") {
                let end = s[i..].find(";)").unwrap();
                s.drain(i..end + i + 2);
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
            bail!("expected != actual\n\n{}", result);
        } else {
            Ok(())
        }
    }

    /// Parses a quoted module, then asserts that it's valid.
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
        self.test_wasm(test, &binary, true)?;
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
            exceptions: true,
            bulk_memory: true,
            tail_call: true,
            module_linking: false,
            deterministic_only: false,
            multi_value: true,
            multi_memory: true,
            memory64: true,
        };
        for part in test.iter().filter_map(|t| t.to_str()) {
            match part {
                "testsuite" | "wasmtime905.wast" | "missing-features" => {
                    features = WasmFeatures::default();
                    features.bulk_memory = false;
                    features.reference_types = false;
                }
                "threads" => features.threads = true,
                "simd" => features.simd = true,
                "reference-types" => {
                    features.bulk_memory = true;
                    features.reference_types = true;
                }
                "exception-handling" => {
                    features.bulk_memory = true;
                    features.reference_types = true;
                    features.exceptions = true;
                }
                "bulk-memory-operations" => features.bulk_memory = true,
                "tail-call" => features.tail_call = true,
                "memory64" => {
                    features.memory64 = true;
                    features.bulk_memory = true;
                }
                "module-linking" => features.module_linking = true,
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

    fn wat2wasm(&self, test: &Path) -> Result<Option<Vec<u8>>> {
        if !self.wabt_available()? {
            return Ok(None);
        }
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

    fn wasm2wat(&self, contents: &[u8]) -> Result<Option<String>> {
        if !self.wabt_available()? {
            return Ok(None);
        }
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
            .context("failed to spawn `wasm2wat`")?;
        if result.status.success() {
            Ok(Some(
                fs::read_to_string(&wat).context("failed to read wat file")?,
            ))
        } else {
            bail!(
                "failed to run wasm2wat: {}\n\n    {}",
                result.status,
                String::from_utf8_lossy(&result.stderr).replace("\n", "\n    "),
            )
        }
    }

    fn wast2json(&self, test: &Path) -> Result<Option<Wast2Json>> {
        if !self.wabt_available()? {
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

    fn wabt_available(&self) -> Result<bool> {
        // Check if we've cached whether wabt is available...
        match self.wabt_available.load(SeqCst) {
            1 => return Ok(false),
            2 => return Ok(true),
            _ => {}
        }

        // ... otherwise figure it out ourselves and try to be the singular
        // thread which flags whether wabt is here or not.
        let available = Command::new("wasm2wat").arg("--version").output().is_ok() as usize + 1;
        if self
            .wabt_available
            .compare_exchange(0, available, SeqCst, SeqCst)
            .is_ok()
        {
            // If we were the singular thread to indicate whether we know wabt
            // is available or not, then we also return an error if it's
            // supposed to be available and it's not.
            if available == 1 && env::var("SKIP_WABT").is_err() {
                bail!(
                    "\
                        failed to locate `wabt` tools as a reference to run tests \
                        against; you either install wabt from the `tests/wabt` \
                        directory or set the SKIP_WABT=1 env var to fix this
                    "
                )
            }
        }
        Ok(available == 2)
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
