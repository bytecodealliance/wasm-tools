//! A fuzzing test that generates a programs using `wasm-opt -ttf --emit-text`
//! and then compares our serialization with wabt. If a failure happens the test
//! is printed, and you can copy it to `tests/regression/*.wat` in the top-level
//! directory and then run `cargo test` to get a more descriptive error
//! message.

use std::io::Read;
use std::path::Path;
use std::time::{Duration, Instant};
use std::process::{Command, Stdio};

#[test]
fn fuzz() {
    let time_limit = std::env::var("FUZZ_TIME_LIMIT")
        .map(|s| s.parse().unwrap())
        .unwrap_or(5);
    let time_limit = Duration::from_secs(time_limit);
    let now = Instant::now();
    let td = tempfile::TempDir::new().unwrap();
    while now.elapsed() < time_limit {
        run_iter(td.path());
    }
}

fn run_iter(td: &Path) {
    let mut data = [0; 128];
    getrandom::getrandom(&mut data).unwrap();
    std::fs::write(td.join("input"), &data[..]).unwrap();
    let mut cmd = Command::new("wasm-opt")
        .arg("-ttf")
        .arg("--emit-text")
        .arg(td.join("input"))
        .arg("-o")
        .arg("-")
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let mut s = String::new();
    cmd.stdout.take().unwrap().read_to_string(&mut s).unwrap();

    if let Err(e) = attempt_roundtrip(td, &s) {
        panic!("failed to parse!\n\n{}\n\n{}", s, e);
    }
}

fn attempt_roundtrip(td: &Path, wat: &str) -> anyhow::Result<()> {
    let binary = wat::parse_str(wat)?;
    if wast_fuzz::wabt_may_disagree_on_binary(wat) {
        return Ok(());
    }

    let wat_file = td.join("foo.wat");
    let wasm = td.join("foo.wasm");
    std::fs::write(&wat_file, wat)?;

    let output = Command::new("wat2wasm")
        .arg(&wat_file)
        .arg("-o")
        .arg(&wasm)
        .output()?;
    if output.status.success() {
        let wabt_bytes = std::fs::read(&wasm)?;
        // see comments in the test suite for why we remove the name
        // section
        if wast_fuzz::remove_name_section(&binary) != wabt_bytes {
            anyhow::bail!("binary encoding differs from wabt");
        }
    }

    Ok(())
}
