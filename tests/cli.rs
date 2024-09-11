//! A test suite to test the `wasm-tools` CLI itself.
//!
//! This test suite will look for `*.wat` and `*.wit` files in the
//! `tests/cli/**` directory, recursively. For more information about supported
//! directives and features of this test suite see the `tests/cli/readme.wat`
//! file which has an explanatory comment at the top for what's going on.

use anyhow::{bail, Context, Result};
use indexmap::IndexMap;
use libtest_mimic::{Arguments, Trial};
use pretty_assertions::StrComparison;
use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use tempfile::TempDir;

fn main() {
    let mut tests = Vec::new();
    find_tests("tests/cli".as_ref(), &mut tests);
    let bless = env::var("BLESS").is_ok();

    let mut trials = Vec::new();
    for test in tests {
        let trial = Trial::test(format!("{test:?}"), move || {
            run_test(&test, bless)
                .with_context(|| format!("failed test {test:?}"))
                .map_err(|e| format!("{e:?}").into())
        })
        // This test suite can't run on wasm since it involves spawning
        // subprocesses.
        .with_ignored_flag(cfg!(target_family = "wasm"));
        trials.push(trial);
    }

    let mut args = Arguments::from_args();
    if cfg!(target_family = "wasm") && !cfg!(target_feature = "atomics") {
        args.test_threads = Some(1);
    }
    libtest_mimic::run(&args, trials).exit();
}

fn run_test(test: &Path, bless: bool) -> Result<()> {
    let contents = std::fs::read_to_string(test)?;

    let mut directives = contents
        .lines()
        .enumerate()
        .filter(|(_, l)| !l.is_empty())
        .filter_map(|(i, l)| {
            l.strip_prefix("// ")
                .or(l.strip_prefix(";; "))
                .map(|l| (i + 1, l))
        });

    let mut commands = IndexMap::new();

    while let Some((i, line)) = directives.next() {
        let run = line.strip_prefix("RUN");
        let fail = line.strip_prefix("FAIL");
        let (directive, should_fail) = match run.map(|l| (l, false)).or(fail.map(|l| (l, true))) {
            Some(pair) => pair,
            None => continue,
        };
        let (cmd, name) = match directive.strip_prefix("[") {
            Some(prefix) => match prefix.find("]:") {
                Some(i) => (&prefix[i + 2..], &prefix[..i]),
                None => bail!("line {i}: failed to find `]:` after `[`"),
            },
            None => match directive.strip_prefix(":") {
                Some(cmd) => (cmd, ""),
                None => bail!("line {i}: failed to find `:` after `RUN` or `FAIL`"),
            },
        };
        let mut cmd = cmd.to_string();
        while cmd.ends_with("\\") {
            cmd.pop();
            match directives.next() {
                Some((_, line)) => cmd.push_str(line),
                None => bail!("line {i}: directive ends in `\\` but nothing on next line"),
            }
        }

        match commands.insert(name, (cmd, should_fail, i)) {
            Some(_) => bail!("line {i}: duplicate directive named {name:?}"),
            None => {}
        }
    }

    if commands.is_empty() {
        bail!("failed to find `// RUN: ...` or `// FAIL: ...` at the top of this file");
    }
    let exe = Path::new(env!("CARGO_BIN_EXE_wasm-tools"));
    let tempdir = TempDir::new_in(exe.parent().unwrap())?;
    for (name, (line, should_fail, i)) in commands {
        run_test_directive(test, &name, &line, bless, should_fail, exe, &tempdir).with_context(
            || {
                let kind = if should_fail { "FAIL" } else { "RUN" };
                format!("failed {kind} directive `{name}` on line {i}")
            },
        )?;
    }
    Ok(())
}

fn run_test_directive(
    test: &Path,
    name: &str,
    line: &str,
    bless: bool,
    should_fail: bool,
    exe: &Path,
    tempdir: &TempDir,
) -> Result<()> {
    let mut cmd = Command::new(exe);
    let mut stdin = None;
    for arg in line.split_whitespace() {
        let arg = arg.replace("%tmpdir", tempdir.path().to_str().unwrap());
        if arg == "|" {
            let output = execute(&mut cmd, stdin.as_deref(), false)?;
            stdin = Some(output.stdout);
            cmd = Command::new(exe);
        } else if arg == "%" {
            cmd.arg(test);
        } else {
            cmd.arg(arg);
        }
    }

    let output = execute(&mut cmd, stdin.as_deref(), should_fail)?;
    let extension = test.extension().unwrap().to_str().unwrap();
    let extension = if name.is_empty() {
        extension.to_string()
    } else {
        format!("{extension}.{name}")
    };
    assert_output(
        bless,
        &output.stdout,
        &test.with_extension(&format!("{extension}.stdout")),
        &tempdir,
    )
    .context("failed to check stdout expectation (auto-update with BLESS=1)")?;
    assert_output(
        bless,
        &output.stderr,
        &test.with_extension(&format!("{extension}.stderr")),
        &tempdir,
    )
    .context("failed to check stderr expectation (auto-update with BLESS=1)")?;
    Ok(())
}

fn execute(cmd: &mut Command, stdin: Option<&[u8]>, should_fail: bool) -> Result<Output> {
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

    let mut failure = None;
    match output.status.code() {
        Some(0) => {
            if should_fail {
                failure = Some("succeeded instead of failed");
            }
        }
        Some(1) | Some(2) => {
            if !should_fail {
                failure = Some("failed");
            }
        }
        _ => failure = Some("unknown exit code"),
    }
    if let Some(msg) = failure {
        bail!(
            "{cmd:?} {msg}:
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

fn assert_output(bless: bool, output: &[u8], path: &Path, tempdir: &TempDir) -> Result<()> {
    let tempdir = tempdir.path().to_str().unwrap();
    // sanitize the output to be consistent across platforms and handle per-test
    // differences such as `%tmpdir`.
    let output = String::from_utf8_lossy(output)
        .replace(tempdir, "%tmpdir")
        .replace("\\", "/");

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
