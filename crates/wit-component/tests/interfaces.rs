use anyhow::{Context, Result};
use pretty_assertions::assert_eq;
use std::fs;
use std::path::Path;
use wit_component::DocumentPrinter;
use wit_parser::{Resolve, UnresolvedPackage};

/// Tests the encoding of a WIT package as a WebAssembly binary.
///
/// This test looks in the `interfaces/` directory for test cases. Each test
/// case is a `*.wit` file or a folder which contains `*.wit` files as part of a
/// multi-file package. Each tests `foo.wit` is accompanied with a `foo.wat` for
/// the WebAssembly text format encoding of the package. Additionally each test
/// has a `foo.print.wit` which is the machine-printed version of the WIT
/// document as decoded from the binary encoded interface.
///
/// Run the test with the environment variable `BLESS` set to update
/// the baseline files.
#[test]
fn interface_encoding() -> Result<()> {
    for entry in fs::read_dir("tests/interfaces")? {
        let path = entry?.path();
        let name = match path.file_name().and_then(|s| s.to_str()) {
            Some(s) => s,
            None => continue,
        };
        let is_dir = path.is_dir();
        let is_test = is_dir || name.ends_with(".wit");
        if is_test {
            run_test(&path, is_dir).context(format!("failed test `{}`", path.display()))?;
        }
    }

    Ok(())
}

fn run_test(path: &Path, is_dir: bool) -> Result<()> {
    println!("running test at {path:?}");
    let mut resolve = Resolve::new();
    let package = if is_dir {
        resolve.push_dir(path)?.0
    } else {
        resolve.push(UnresolvedPackage::parse_file(path)?, &Default::default())?
    };

    let features = wasmparser::WasmFeatures {
        component_model: true,
        ..Default::default()
    };

    // First convert the WIT package to a binary WebAssembly output, then
    // convert that binary wasm to textual wasm, then assert it matches the
    // expectation.
    let wasm = wit_component::encode(&resolve, package)?;
    let wat = wasmprinter::print_bytes(&wasm)?;
    assert_output(&path.with_extension("wat"), &wat)?;
    wasmparser::Validator::new_with_features(features)
        .validate_all(&wasm)
        .context("failed to validate wasm output")?;

    // Next decode a fresh WIT package from the WebAssembly generated. Print
    // this package's documents and assert they all match the expectations.
    let name = &resolve.packages[package].name;
    let decoded = wit_component::decode(name, &wasm)?;
    let resolve = decoded.resolve();

    for (id, pkg) in resolve.packages.iter() {
        for (name, doc) in pkg.documents.iter() {
            let root = if id == decoded.package() {
                path.to_path_buf()
            } else {
                path.join("deps").join(&pkg.name)
            };
            let expected = if is_dir {
                root.join(format!("{name}.wit.print"))
            } else {
                root.with_extension("wit.print")
            };
            let output = DocumentPrinter::default().print(&resolve, *doc)?;
            assert_output(&expected, &output)?;
        }
    }

    // Finally convert the decoded package to wasm again and make sure it
    // matches the prior wasm.
    let wasm2 = wit_component::encode(resolve, decoded.package())?;
    if wasm != wasm2 {
        let wat2 = wasmprinter::print_bytes(&wasm)?;
        assert_eq!(wat, wat2, "document did not roundtrip correctly");
    }

    Ok(())
}

fn assert_output(expected: &Path, actual: &str) -> Result<()> {
    if std::env::var_os("BLESS").is_some() {
        fs::write(&expected, actual).with_context(|| format!("failed to write {expected:?}"))?;
    } else {
        assert_eq!(
            fs::read_to_string(&expected)
                .with_context(|| format!("failed to read {expected:?}"))?
                .replace("\r\n", "\n"),
            actual,
            "expectation `{}` did not match actual",
            expected.display(),
        );
    }
    Ok(())
}
