use crate::validate::CliFeatures;
use anyhow::{bail, Context, Result};
use clap::Parser;
use rayon::prelude::*;
use std::fs;
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::str;
use wasm_encoder::reencode::{Reencode, ReencodeComponent, RoundtripReencoder};
use wasmprinter::PrintFmtWrite;
use wast::component::{Component, ComponentKind};
use wast::core::{Module, ModuleKind};
use wast::lexer::Lexer;
use wast::parser::{self, ParseBuffer};
use wast::{QuoteWat, Wast, WastDirective, Wat};

/// A subcommand to test `*.wast` files short of executing WebAssembly code.
///
/// This subcommand can be used to validate the `*.wast` test format used by the
/// WebAssembly spec interpreter. This command takes a single test case as input
/// and it will validate as much as it can about it including:
///
/// * The syntax of the test is valid.
/// * Modules that are declared to be valid are indeed valid.
/// * Modules that are declared to be invalid are indeed invalid.
///
/// This subcommand does not actually execute any WebAssembly code, so the
/// `*.wast` test is not guaranteed to pass in a real runtime if passed to a
/// runtime. This can be used as a quick check to double-check that a file is
/// almost all valid, however.
///
/// The `wasm-tools` project itself also heavily relies on this subcommand for
/// internal testing, so many tests for `wasm-tools` use this subcommand as a
/// way to write tests.
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    /// Input file to process.
    ///
    /// If not provided or if this is `-` then stdin is read entirely and
    /// processed.
    input: Option<PathBuf>,

    /// Whether to ignore the error message expectation in `(assert_invalid ..)`
    /// directives.
    ///
    /// When enabled this will only assert that modules produce an error, not
    /// that the error produced by wasm-tools matches that written in the test
    /// itself.
    #[clap(long)]
    ignore_error_messages: bool,

    #[clap(flatten)]
    features: CliFeatures,

    /// Whether or not "confusing unicode characters" are allowed to be present
    /// in `*.wast` files.
    ///
    /// The default for this is `false`.
    #[clap(long)]
    allow_confusing_unicode: bool,

    /// Perform extra internal assertions in `wasm-tools` itself over the
    /// provided test.
    ///
    /// This is used within the testing of `wasm-tools` itself to perform more
    /// tests by default such as asserting that the text format round-trips
    /// where possible. Asserts can be listed here to perform extra checks when
    /// validating the `*.wast` input.
    #[clap(long, short, value_parser = parse_asserts)]
    assert: Vec<Vec<Assert>>,

    /// Directory to place snapshots in with `--assert snapshot-*` flags.
    #[clap(long)]
    snapshot: Option<PathBuf>,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    pub fn run(&self) -> Result<()> {
        let (test, contents) = self.read_wast()?;
        let contents = contents.as_str();
        macro_rules! adjust {
            ($e:expr) => {{
                let mut e = wast::Error::from($e);
                e.set_path(test);
                e.set_text(contents);
                e
            }};
        }
        let mut lexer = Lexer::new(contents);
        lexer.allow_confusing_unicode(self.allow_confusing_unicode);
        let buf = ParseBuffer::new_with_lexer(lexer).map_err(|e| adjust!(e))?;
        let wast = parser::parse::<Wast>(&buf).map_err(|e| adjust!(e))?;

        if let Some(path) = &self.input {
            self.test_json_from_wast(path)
                .context("failed to run `json-from-wast` cli subcommand")?;
        }

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
            s.push_str("\n\n--------------------------------\n\n");
            s.push_str(&format!("{:?}", error));
        }
        bail!("{}", s)
    }

    fn read_wast(&self) -> Result<(&Path, String)> {
        if let Some(path) = &self.input {
            if path != Path::new("-") {
                let contents =
                    fs::read_to_string(path).with_context(|| format!("failed to read {path:?}"))?;
                return Ok((path, contents));
            }
        }
        let mut stdin = String::new();
        std::io::stdin()
            .read_to_string(&mut stdin)
            .context("failed to read <stdin>")?;
        Ok((Path::new("<stdin>"), stdin))
    }

    fn test_wast_directive(&self, test: &Path, directive: WastDirective, idx: usize) -> Result<()> {
        match directive {
            WastDirective::Module(mut module) | WastDirective::ModuleDefinition(mut module) => {
                let actual = module.encode()?;

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
                let result = module
                    .encode()
                    .map_err(|e| e.into())
                    .and_then(|wasm| self.test_wasm_valid(test, &wasm));
                match result {
                    Ok(_) => bail!(
                        "encoded and validated successfully but should have failed with: {}",
                        message,
                    ),
                    Err(e) => {
                        if self.error_matches(test, &format!("{e:?}"), message) {
                            return Ok(());
                        }
                        bail!(
                            "bad error: {e:?}\n\
                            should have failed with: {message:?}\n\
                            suppress this failure with `--ignore-error-messages`",
                        );
                    }
                }
            }

            WastDirective::Thread(thread) => {
                for (i, directive) in thread.directives.into_iter().enumerate() {
                    self.test_wast_directive(test, directive, idx * 1000 + i)?;
                }
            }

            // This command doesn't actually execute any wasm code, so ignore
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

    /// Tests whether `assert` is enabled.
    fn assert(&self, assert: Assert) -> bool {
        self.assert
            .iter()
            .flat_map(|v| v)
            .any(|a| match (*a, assert) {
                // if explicitly requested, always enable
                (a, b) if a == b => true,
                // exclude some assertions from the default
                (Assert::Default, Assert::SnapshotFolded) => false,
                // otherwise allow all other assertions when default is enabled
                (Assert::Default, _) => true,
                _ => false,
            })
    }

    /// Returns whether `error`, from the validator, matches the expected error
    /// `message`.
    fn error_matches(&self, _test: &Path, error: &str, message: &str) -> bool {
        error.contains(message) || self.ignore_error_messages
    }

    /// Performs tests about the wasm binary `contents` associated with `test`.
    ///
    /// If `test_roundtrip` is `false` then it will always skip the roundtrip
    /// tests.
    fn test_wasm(&self, test: &Path, contents: &[u8], test_roundtrip: bool) -> Result<()> {
        self.test_wasm_valid(test, contents)
            .context("wasm isn't valid")?;

        // Test that we can print these bytes.
        let string = wasmprinter::print_bytes(contents).context("failed to print wasm")?;
        if self.assert(Assert::SnapshotPrint) {
            self.snapshot("print", test, &string)
                .context("failed to validate the `print` snapshot")?;
        }

        // Test that we can print these bytes with instructions in folded form.
        //
        // Note though that the folded form doesn't support legacy exceptions,
        // so if that's enabled disable testing automatically.
        let mut folded_string = String::new();
        let test_folded = !self.features.features().legacy_exceptions();
        if test_folded {
            let mut folding_printer = wasmprinter::Config::new();
            folding_printer.fold_instructions(true);
            folding_printer
                .print(contents, &mut PrintFmtWrite(&mut folded_string))
                .context("failed to print wasm in folded form")?;

            if self.assert(Assert::SnapshotFolded) {
                self.snapshot("print-folded", test, &folded_string)
                    .context("failed to validate the `print-folded` snapshot")?;
            }
        }

        // If we can, convert the string back to bytes and assert it has the
        // same binary representation.
        if test_roundtrip && self.assert(Assert::Roundtrip) {
            let binary2 =
                wat::parse_str(&string).context("failed to parse `wat` from `wasmprinter`")?;
            self.binary_compare(&binary2, contents)
                .context("failed to compare original `wat` with roundtrip `wat`")?;

            if test_folded {
                let binary2f = wat::parse_str(&folded_string)
                    .context("failed to parse folded `wat` from `wasmprinter`")?;
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

        if self.assert(Assert::PrettyWhitespace) {
            self.test_pretty_whitespace(&string)?;
            self.test_pretty_whitespace(&folded_string)?;
        }

        Ok(())
    }

    /// Tests that `contents` is valid wasm binary with respect to to
    /// `self.features`.
    fn test_wasm_valid(&self, _test: &Path, contents: &[u8]) -> Result<()> {
        let mut validator = wasmparser::Validator::new_with_features(self.features.features());
        validator.validate_all(contents)?;
        Ok(())
    }

    /// Test that the `wasmprinter`-printed bytes have "pretty" whitespace
    /// which means that all whitespace is either categorized as leading
    /// whitespace or a single space. Examples of "bad whitespace" are:
    ///
    /// * trailing whitespace at the end of a line
    /// * two spaces in a row
    ///
    /// Both of these cases indicate possible bugs in `wasmprinter` itself
    /// which while they don't actually affect the meaning they do "affect"
    /// humans reading the output.
    fn test_pretty_whitespace(&self, string: &str) -> Result<()> {
        for token in wast::lexer::Lexer::new(&string)
            .allow_confusing_unicode(self.allow_confusing_unicode)
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

    /// Compare the `actual` and `expected`, asserting that they are the same.
    ///
    /// If they are not equal this attempts to produce as nice of an error
    /// message as it can to help narrow down on where the differences lie.
    fn binary_compare(&self, actual: &[u8], expected: &[u8]) -> Result<()> {
        if actual == expected {
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

    /// Run `wasm-tools dump` over the `bytes` provided.
    fn dump(&self, bytes: &[u8]) -> Result<String> {
        let mut dump = self
            .wasm_tools()?
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

    /// Performs a snapshot of `json-from-wast`.
    fn test_json_from_wast(&self, path: &Path) -> Result<()> {
        if !self.assert(Assert::SnapshotJson) {
            return Ok(());
        }

        // Generate the same output on windows and unix
        let path = path.to_str().unwrap().replace("\\", "/");

        let mut cmd = self.wasm_tools()?;
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

    /// Returns a `Command` pointing to this `wasm-tools` binary itself.
    fn wasm_tools(&self) -> Result<Command> {
        Ok(Command::new(std::env::current_exe()?))
    }

    /// Compare the test result with a snapshot stored in the repository.
    ///
    /// Works great for tools like wasmprinter for which having a nice overview of what effect the
    /// changes cause.
    fn snapshot(&self, kind: &str, path: &Path, contents: &str) -> Result<()> {
        let contents = contents.replace("\r\n", "\n");
        let bless = std::env::var_os("BLESS").is_some();
        let snapshot_dir = self.snapshot_dir()?;
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

    /// Returns the `--snapshot` directory, or an error if it's not configured.
    fn snapshot_dir(&self) -> Result<&Path> {
        self.snapshot.as_deref().with_context(|| {
            format!("must pass `--snapshot <dir>` to indicate where to place snapshots")
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Assert {
    Default,
    Roundtrip,
    PrettyWhitespace,
    SnapshotPrint,
    SnapshotJson,
    SnapshotFolded,
}

fn parse_asserts(arg: &str) -> Result<Vec<Assert>> {
    let mut ret = Vec::new();

    for part in arg.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
        match part {
            "default" => ret.push(Assert::Default),
            "roundtrip" => ret.push(Assert::Roundtrip),
            "pretty-whitespace" => ret.push(Assert::PrettyWhitespace),
            "snapshot-print" => ret.push(Assert::SnapshotPrint),
            "snapshot-json" => ret.push(Assert::SnapshotJson),
            "snapshot-folded" => ret.push(Assert::SnapshotFolded),
            other => bail!("unknown assertion: {other}"),
        }
    }

    Ok(ret)
}
