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
            Some("wat") => self.test_wat(test, contents),
            Some("wasm") => self.test_wasm(test, contents),
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
                self.binary_compare(&binary, &expected)?;
            }
        }

        let contents = str::from_utf8(contents)?;

        // Finally we test that this is indeed a valid wasm file. Note,
        // however, that wasmparser doesn't implement all features that wabt
        // does, so we skip some tests here too.
        if !contents.contains("--enable-tail-call")
            && !contents.contains("--enable-exceptions")
            && !contents.contains("--enable-gc")
            && !contents.contains("--no-check")
            && !test.ends_with("dump/event.txt")
            && !test.ends_with("dump/import.txt")
        {
            self.test_wasm(test, &binary)?;
        }
        Ok(())
    }

    fn test_wasm(&self, test: &Path, contents: &[u8]) -> Result<()> {
        if test.iter().any(|t| t == "invalid") {
            self.test_wasm_invalid(test, contents)?;
        } else {
            self.test_wasm_valid(test, contents)?;
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
                self.test_wast_directive(test, line, directive, expected)
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
        line: usize,
        directive: WastDirective,
        expected: Option<&PathBuf>,
    ) -> Result<()> {
        let wasmparser_disabled = self.wasmparser_disabled(test, line + 1);
        match directive {
            WastDirective::Module(mut module) => {
                let actual = module.encode()?;
                self.bump_ntests(); // testing encode
                if !wasmparser_disabled {
                    self.test_wasm(test, &actual)?;
                }
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
            WastDirective::AssertInvalid {
                mut module,
                message,
                span: _,
            } => {
                if wasmparser_disabled {
                    return Ok(());
                }
                let wasm = module.encode()?;
                self.bump_ntests();
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
                if wasmparser_disabled {
                    return Ok(());
                }
                let wasm = module.encode()?;
                self.bump_ntests();
                let e = self.test_wasm_invalid(test, &wasm)?;
                // TODO: Check the assert_malformed message
                drop(e);
            }

            _ => {}
        }
        Ok(())
    }

    fn test_wasm_valid(&self, test: &Path, contents: &[u8]) -> Result<()> {
        const MAX: usize = 100000000;

        // First test the wasm simply parses.
        let mut parser = Parser::new(contents);
        let mut iter = MAX;
        loop {
            let state = parser.read();
            match state {
                ParserState::EndWasm => break,
                ParserState::Error(err) => return Err(err.clone().into()),
                _ => (),
            }
            iter -= 1;
            if iter == 0 {
                bail!("Max iterations exceeded");
            }
        }
        self.bump_ntests();

        // Then test that it validates as well.
        let config = self.wasmparser_config_for(test);
        let mut parser = ValidatingParser::new(contents, Some(config));
        iter = MAX;
        loop {
            let state = parser.read();
            match state {
                ParserState::EndWasm => break,
                ParserState::Error(err) => return Err(err.clone().into()),
                _ => (),
            }
            iter -= 1;
            if iter == 0 {
                bail!("Max iterations exceeded");
            }
        }
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

    fn wasmparser_disabled(&self, test: &Path, line: usize) -> bool {
        // tail calls aren't implemented yet in wasmparser
        if test.iter().any(|t| t == "tail-call") {
            return true;
        }

        // Waiting for WebAssembly/reference-types#76 to get resolved
        if test.ends_with("reference-types/ref_func.wast") {
            return line == 108 || line == 112;
        }

        // Waiting for WebAssembly/reference-types#76 to get resolved
        if test.ends_with("reference-types/ref_func.wast") {
            return line == 108 || line == 112;
        }

        // these tests still use the old binary encoding, need to be
        // updated for the new one
        if test.ends_with("simd_const.wast") {
            return line == 1566
                || line == 1583
                || line == 1600
                || line == 1617
                || line == 1634
                || line == 1651;
        }

        false
    }

    fn wasmparser_config_for(&self, test: &Path) -> ValidatingParserConfig {
        let mut config = ValidatingParserConfig {
            operator_config: OperatorValidatorConfig {
                enable_threads: true,
                enable_reference_types: true,
                enable_simd: true,
                enable_bulk_memory: true,
                enable_multi_value: true,
            },
        };
        for part in test.iter().filter_map(|t| t.to_str()) {
            match part {
                "testsuite" | "wasmtime905.wast" => {
                    config.operator_config.enable_threads = false;
                    config.operator_config.enable_reference_types = false;
                    config.operator_config.enable_simd = false;
                    config.operator_config.enable_bulk_memory = false;
                }
                "threads" => config.operator_config.enable_threads = true,
                "simd" => config.operator_config.enable_simd = true,
                "reference-types" => {
                    config.operator_config.enable_bulk_memory = true;
                    config.operator_config.enable_reference_types = true;
                }
                "bulk-memory-operations" => {
                    config.operator_config.enable_bulk_memory = true;
                }
                _ => {}
            }
        }
        return config;
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

    if message == "unknown memory" {
        return error.contains("no linear memories are present");
    }

    if message == "bad magic" {
        return error.contains("Bad magic number");
    }

    if message == "unknown function 1" {
        return error.contains("unknown function");
    }

    return false;
}
