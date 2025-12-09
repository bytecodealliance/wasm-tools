use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    let wasi_sdk_path = PathBuf::from(std::env::var("WASI_SDK_PATH").expect(
        "
The $WASI_SDK_PATH environment variable isn't set and thus these tests can't be
built. If you're testing the entier workspace pass `--exclude wit-dylib` to
Cargo to avoid testing this crate, and otherwise you can install the sdk through
https://github.com/webassembly/wasi-sdk

            ",
    ));
    let target = "wasm32-wasip2";
    let upcase = target.to_uppercase().replace("-", "_");
    let mut cargo = Command::new("cargo");
    let debug = std::env::var("OPT_LEVEL").unwrap() == "0";
    cargo
        .arg("build")
        .arg("--target")
        .arg(target)
        .arg("--package=test-programs")
        .env("CARGO_TARGET_DIR", &out_dir)
        .env(
            format!("CARGO_TARGET_{upcase}_RUSTFLAGS"),
            "-Clink-self-contained=n \
            -Clink-arg=-Wl,--skip-wit-component,--no-entry,--export=cabi_realloc \
            -Clink-arg=-shared",
        )
        .env(
            format!("CARGO_TARGET_{upcase}_LINKER"),
            wasi_sdk_path.join("bin/clang"),
        )
        .env_remove("CARGO_ENCODED_RUSTFLAGS");
    if !debug {
        cargo.arg("--release");
    }
    eprintln!("running {cargo:?}");
    let status = cargo.status().unwrap();
    assert!(status.success());

    let meta = cargo_metadata::MetadataCommand::new().exec().unwrap();
    let targets = meta
        .packages
        .iter()
        .find(|p| p.name == "test-programs")
        .unwrap()
        .targets
        .iter()
        .filter(move |t| t.kind == &[cargo_metadata::TargetKind::Bin])
        .map(|t| &t.name)
        .collect::<Vec<_>>();

    let mut output = String::new();

    let libc_so = wasi_sdk_path
        .join("share/wasi-sysroot/lib")
        .join(target)
        .join("libc.so");

    output.push_str(&format!("pub const LIBC_SO: &str = {libc_so:?};\n"));
    output.push_str("pub const TESTS: &[(&str, &str, &str)] = &[\n");

    let cwd = std::env::current_dir().unwrap();

    let mut deps = HashSet::new();
    let mut roundtrip = None;
    let artifact_dir = if debug { "debug" } else { "release" };
    for target_name in targets {
        if target_name.ends_with("_callee") {
            continue;
        }
        let name = target_name.strip_suffix("_caller").unwrap();
        let callee = format!("{name}_callee");
        let caller = out_dir
            .join(target)
            .join(artifact_dir)
            .join(format!("{target_name}.wasm"));
        let callee = out_dir
            .join(target)
            .join(artifact_dir)
            .join(format!("{callee}.wasm"));
        read_deps_of(&mut deps, &caller);
        read_deps_of(&mut deps, &callee);
        if name == "roundtrip" {
            roundtrip = Some((caller, callee));
            continue;
        }
        let wit_file = cwd.join("..").join("src/bin").join(format!("{name}.wit"));
        assert!(caller.exists());
        assert!(callee.exists());
        assert!(wit_file.exists());

        output.push_str(&format!(
            "({:?}, {:?}, {:?}),\n",
            caller.to_str().unwrap(),
            callee.to_str().unwrap(),
            wit_file.to_str().unwrap(),
        ));
    }

    output.push_str("];\n");

    let (caller, callee) = roundtrip.unwrap();
    output.push_str(&format!(
        "
            pub const ROUNDTRIP_CALLER: &str = {caller:?};
            pub const ROUNDTRIP_CALLEE: &str = {callee:?};
        ",
    ));
    std::fs::write(out_dir.join("out.rs"), &output).unwrap();
}

/// Helper function to read the `*.d` file that corresponds to `artifact`, an
/// artifact of a Cargo compilation.
///
/// This function will "parse" the makefile-based dep-info format to learn about
/// what files each binary depended on to ensure that this build script reruns
/// if any of these files change.
///
/// See
/// <https://doc.rust-lang.org/nightly/cargo/reference/build-cache.html#dep-info-files>
/// for more info.
fn read_deps_of(deps: &mut HashSet<String>, artifact: &Path) {
    let deps_file = artifact.with_extension("d");
    let contents = std::fs::read_to_string(&deps_file).expect("failed to read deps file");
    for line in contents.lines() {
        let Some(pos) = line.find(": ") else {
            continue;
        };
        let line = &line[pos + 2..];
        let mut parts = line.split_whitespace();
        while let Some(part) = parts.next() {
            let mut file = part.to_string();
            while file.ends_with('\\') {
                file.pop();
                file.push(' ');
                file.push_str(parts.next().unwrap());
            }
            if !deps.contains(&file) {
                println!("cargo:rerun-if-changed={file}");
                deps.insert(file);
            }
        }
    }
}
