//! A test suite to test the `wasm-tools` CLI itself.
//!
//! This test suite will look for `*.wat` files in the `tests/cli/**` directory,
//! recursively. Each wat file must have a directive of the form:
//!
//!     ;; RUN: ...
//!
//! where `...` is a space-separate set of command to pass to the `wasm-tools`
//! CLI. The `%` argument is replaced with the path to the current file. For
//! example:
//!
//!     ;; RUN: dump %
//!
//! would execute `wasm-tools dump the-current-file.wat`. The `cli` directory
//! additionally contains `*.stdout` and `*.stderr` files to assert the output
//! of the subcommand. Files are not present if the stdout/stderr are empty.
//!
//! This also supports a limited form of piping along the lines of:
//!
//!     ;; RUN: strip % | objdump
//!
//! where a `|` will execute the first subcommand and pipe its stdout into the
//! stdin of the next command.
//!
//! Use `BLESS=1` in the environment to auto-update expectation files. Be sure
//! to look at the diff!

use anyhow::{anyhow, bail, Context, Result};
use pretty_assertions::StrComparison;
use rayon::prelude::*;
use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};

fn main() {
    // This test suite can't run on wasm since it involves spawning
    // subprocesses.
    if cfg!(target_family = "wasm") {
        return;
    }

    let mut tests = Vec::new();
    find_tests("tests/cli".as_ref(), &mut tests);
    let filter = std::env::args().nth(1);

    let bless = env::var("BLESS").is_ok();
    let tests = tests
        .iter()
        .filter(|test| {
            if let Some(filter) = &filter {
                if let Some(s) = test.file_name().and_then(|s| s.to_str()) {
                    if !s.contains(filter) {
                        return false;
                    }
                }
            }
            true
        })
        .collect::<Vec<_>>();

    println!("running {} tests\n", tests.len());

    let errors = tests
        .par_iter()
        .filter_map(|test| {
            run_test(test, bless)
                .with_context(|| format!("failed test {test:?}"))
                .err()
        })
        .collect::<Vec<_>>();

    if !errors.is_empty() {
        for msg in errors.iter() {
            eprintln!("{:?}", msg);
        }

        panic!("{} tests failed", errors.len())
    }

    println!("test result: ok. {} passed\n", tests.len());
}

fn wasm_tools_exe() -> Command {
    Command::new(env!("CARGO_BIN_EXE_wasm-tools"))
}

fn run_test(test: &Path, bless: bool) -> Result<()> {
    let contents = std::fs::read_to_string(test)?;
    let line = contents
        .lines()
        .filter_map(|l| l.strip_prefix(";; RUN: ").or(l.strip_prefix("// RUN: ")))
        .next()
        .ok_or_else(|| anyhow!("no line found with `;; RUN: ` directive"))?;

    let mut cmd = wasm_tools_exe();
    let mut stdin = None;
    for arg in line.split_whitespace() {
        if arg == "|" {
            let output = execute(&mut cmd, stdin.as_deref())?;
            stdin = Some(output.stdout);
            cmd = wasm_tools_exe();
        } else if arg == "%" {
            cmd.arg(test);
        } else {
            cmd.arg(arg);
        }
    }

    let output = execute(&mut cmd, stdin.as_deref())?;
    let extension = test.extension().unwrap().to_str().unwrap();
    assert_output(
        bless,
        &output.stdout,
        &test.with_extension(&format!("{extension}.stdout")),
    )
    .context("failed to check stdout expectation (auto-update with BLESS=1)")?;
    assert_output(
        bless,
        &output.stderr,
        &test.with_extension(&format!("{extension}.stderr")),
    )
    .context("failed to check stderr expectation (auto-update with BLESS=1)")?;
    Ok(())
}

fn execute(cmd: &mut Command, stdin: Option<&[u8]>) -> Result<Output> {
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    let mut p = cmd
        .spawn()
        .with_context(|| format!("failed to spawn {cmd:?}"))?;

    let mut io = p.stdin.take().unwrap();
    if let Some(stdin) = stdin {
        io.write_all(stdin)?;
    }
    drop(io);

    let output = p.wait_with_output()?;

    if !output.status.success() {
        bail!(
            "{cmd:?} failed:
            status: {}
            stdout: {}
            stderr: {}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(output)
}

fn assert_output(bless: bool, output: &[u8], path: &Path) -> Result<()> {
    if bless {
        if output.is_empty() {
            drop(std::fs::remove_file(path));
        } else {
            std::fs::write(path, output).with_context(|| format!("failed to write {path:?}"))?;
        }
        return Ok(());
    }

    if output.is_empty() {
        if path.exists() {
            bail!("command had no output but {path:?} exists");
        } else {
            Ok(())
        }
    } else {
        let output = std::str::from_utf8(output)?;
        let contents = std::fs::read_to_string(path)
            .with_context(|| format!("failed to read {path:?}"))?
            .replace("\r\n", "\n");
        if output != contents {
            bail!(
                "failed test: result is not as expected:{}",
                StrComparison::new(&contents, &output),
            );
        }
        Ok(())
    }
}

fn find_tests(path: &Path, tests: &mut Vec<PathBuf>) {
    for f in path.read_dir().unwrap() {
        let f = f.unwrap();
        if f.file_type().unwrap().is_dir() {
            find_tests(&f.path(), tests);
            continue;
        }
        match f.path().extension().and_then(|s| s.to_str()) {
            Some("wat") | Some("wit") => {}
            _ => continue,
        }
        tests.push(f.path());
    }
}
