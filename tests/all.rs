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
use std::str;
use wasmparser::{BinaryReaderError, OperatorValidatorConfig};
use wasmparser::{Parser, ParserState, WasmDecoder};
use wasmparser::{ValidatingParser, ValidatingParserConfig};

#[derive(PartialEq, PartialOrd, Eq, Ord)]
enum Test {
    Wasm,
    InvalidWasm,
    NotDeterministic,
    Wast,
}

fn main() {
    let tests = find_tests();
    let filter = std::env::args().nth(1);

    let tests = tests
        .par_iter()
        .filter_map(|(kind, test)| {
            if let Some(filter) = &filter {
                if let Some(s) = test.to_str() {
                    if !s.contains(filter) {
                        return None;
                    }
                }
            }
            let contents = fs::read(test).unwrap();
            Some((kind, test, contents))
        })
        .collect::<Vec<_>>();

    println!("running {} test files\n", tests.len());

    let ntests = tests.len();
    let errors = tests
        .par_iter()
        .filter_map(|(kind, test, contents)| run_test(kind, test, contents).err())
        .collect::<Vec<_>>();

    if !errors.is_empty() {
        for msg in errors.iter() {
            eprintln!("{:?}", msg);
        }

        panic!("{} tests failed", errors.len())
    }

    println!("test result: ok. {} tests passed\n", ntests,);
}

fn run_test(kind: &Test, test: &Path, contents: &[u8]) -> Result<()> {
    let everything = ValidatingParserConfig {
        operator_config: OperatorValidatorConfig {
            enable_threads: true,
            enable_reference_types: true,
            enable_simd: true,
            enable_bulk_memory: true,
            enable_multi_value: true,

            #[cfg(feature = "deterministic")]
            deterministic_only: true,
        },
    };
    match kind {
        Test::Wasm => valid_wasm(contents, everything)
            .context(format!("failed to validate: {}", test.display()))?,
        Test::InvalidWasm => {
            invalid_wasm(contents, everything)
                .context(format!("validated when it shouldn't: {}", test.display()))?;
        }
        Test::NotDeterministic => validate_not_deterministic(contents, everything).context(
            format!("should not validate as deterministic: {}", test.display()),
        )?,
        Test::Wast => {
            run_wast(test, contents).context(format!("failed to run: {}", test.display()))?
        }
    }
    Ok(())
}

fn valid_wasm(contents: &[u8], config: ValidatingParserConfig) -> Result<()> {
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

    // Then test that it validates as well.
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
    Ok(())
}

fn invalid_wasm(contents: &[u8], config: ValidatingParserConfig) -> Result<BinaryReaderError> {
    match valid_wasm(contents, config) {
        Ok(()) => bail!("no wasm validation error found"),
        Err(e) => e.downcast(),
    }
}

fn validate_not_deterministic(wasm: &[u8], mut _config: ValidatingParserConfig) -> Result<()> {
    #[cfg(feature = "deterministic")]
    {
        _config.operator_config.deterministic_only = false;
    }
    let mut parser = ValidatingParser::new(wasm, Some(_config));
    let mut error = false;

    loop {
        let state = parser.read();
        if let ParserState::Error(_) = *state {
            error = true;
            break;
        }
        if let ParserState::EndWasm = *state {
            break;
        }
    }

    if !error {
        bail!("expected an error but no error found");
    }
    Ok(())
}

fn run_wast(filename: &Path, wast: &[u8]) -> Result<()> {
    let contents = str::from_utf8(wast)?;
    let buf = wast::parser::ParseBuffer::new(&contents).map_err(|mut e| {
        e.set_path(filename);
        e
    })?;
    let wast = wast::parser::parse::<wast::Wast>(&buf).map_err(|mut e| {
        e.set_path(filename);
        e
    })?;

    let mut config = ValidatingParserConfig {
        operator_config: OperatorValidatorConfig {
            enable_threads: false,
            enable_reference_types: false,
            enable_simd: false,
            enable_bulk_memory: false,
            enable_multi_value: false,

            #[cfg(feature = "deterministic")]
            deterministic_only: true,
        },
    };
    for part in filename {
        match part.to_str().unwrap() {
            "multi-value" => config.operator_config.enable_multi_value = true,
            "bulk-memory-operations" => config.operator_config.enable_bulk_memory = true,
            "threads" => config.operator_config.enable_threads = true,
            "simd" => {
                config.operator_config.enable_reference_types = true;
                config.operator_config.enable_simd = true;
            }
            "reference-types" => {
                config.operator_config.enable_bulk_memory = true;
                config.operator_config.enable_reference_types = true;
            }
            _ => {}
        }
    }
    if contents.contains("--enable-multi-value") {
        config.operator_config.enable_multi_value = true;
    }

    let errors = wast
        .directives
        .into_par_iter()
        .filter_map(|directive| {
            let (line, col) = directive.span().linecol_in(&contents);
            if skip_wast_test(filename, line + 1) {
                return None;
            }
            run_directive(directive, &config)
                .context(format!(
                    "{}:{}:{}: wast directive failed",
                    filename.display(),
                    line + 1,
                    col + 1
                ))
                .err()
        })
        .collect::<Vec<_>>();
    if errors.len() > 0 {
        let mut error = String::new();
        for e in errors {
            error.push_str(&format!("{:?}\n", e));
        }
        bail!("{}", error)
    }
    Ok(())
}

fn skip_wast_test(filename: &Path, line: usize) -> bool {
    if filename.ends_with("simd_const.wast") {
        // these tests still use the old binary encoding, need to be
        // updated for the new one
        return line == 1566
            || line == 1583
            || line == 1600
            || line == 1617
            || line == 1634
            || line == 1651;
    }

    // uses an old error message and we're waiting for repo to update
    if filename.ends_with("reference-types/memory_init.wast") {
        return line == 188;
    }

    // TODO: need to implement these
    if filename.ends_with("reference-types/select.wast")
        || filename.ends_with("reference-types/table_init.wast")
        || filename.ends_with("reference-types/br_table.wast")
        || filename.ends_with("reference-types/binary.wast")
        || filename.ends_with("reference-types/ref_func.wast")
    {
        return true;
    }

    false
}

fn run_directive(directive: wast::WastDirective, config: &ValidatingParserConfig) -> Result<()> {
    use wast::WastDirective::*;
    match directive {
        Module(mut module) | AssertUnlinkable { mut module, .. } => {
            let wasm = module.encode()?;
            valid_wasm(&wasm, config.clone())?;
        }
        AssertInvalid {
            mut module,
            message,
            span: _,
        } => {
            let e = invalid_wasm(&module.encode()?, config.clone())?;
            if !e.message().contains(message) {
                bail!(
                    "expected \"{spec}\", got \"{actual}\"",
                    spec = message,
                    actual = e,
                );
            }
        }
        AssertMalformed {
            module: wast::QuoteModule::Module(mut module),
            ..
        } => {
            let e = invalid_wasm(&module.encode()?, config.clone())?;
            // TODO: Check the assert_malformed message
            drop(e);
        }

        AssertMalformed {
            module: wast::QuoteModule::Quote(_),
            ..
        }
        | Register { .. }
        | QuoteModule { .. }
        | Invoke { .. }
        | AssertTrap { .. }
        | AssertReturn { .. }
        | AssertExhaustion { .. } => {}
    }
    Ok(())
}

fn find_tests() -> Vec<(Test, PathBuf)> {
    let mut tests = Vec::new();

    // Slurp up all files in `tests/*.wasm` and assert they're valid. For some
    // also assert they're not deterministic if they have floating ops in them.
    for entry in fs::read_dir("tests").unwrap() {
        let path = entry.unwrap().path();
        if path.extension() == Some("wasm".as_ref()) {
            tests.push((Test::Wasm, path.clone()));
        }
        if let Some(s) = path.file_name().and_then(|s| s.to_str()) {
            if cfg!(feature = "deterministic")
                && (s.starts_with("float_exprs.")
                    || s.starts_with("float_memory.")
                    || s.starts_with("float_misc."))
            {
                tests.push((Test::NotDeterministic, path.clone()));
            }
        }
    }

    // Slurp up all of `tests/invalid/*.wasm` and assert they're invalid.
    for entry in fs::read_dir("tests/invalid").unwrap() {
        let path = entry.unwrap().path();
        if path.extension() == Some("wasm".as_ref()) {
            tests.push((Test::InvalidWasm, path));
        }
    }

    // Test a mess of `*.wast` files by running them entirely.
    let mut push_wast = |dir: &str| {
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.extension() == Some("wast".as_ref()) {
                tests.push((Test::Wast, path));
            }
        }
    };
    push_wast("tests/wast");
    push_wast("testsuite");
    push_wast("testsuite/proposals/multi-value");
    push_wast("testsuite/proposals/bulk-memory-operations");
    push_wast("testsuite/proposals/mutable-global");
    push_wast("testsuite/proposals/sign-extension-ops");
    push_wast("testsuite/proposals/nontrapping-float-to-int-conversions");
    push_wast("testsuite/proposals/threads");
    push_wast("testsuite/proposals/simd");
    push_wast("testsuite/proposals/reference-types");

    return tests;
}
