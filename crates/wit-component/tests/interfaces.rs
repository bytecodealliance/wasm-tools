use anyhow::{Context, Result};
use pretty_assertions::assert_eq;
use std::fs;
use std::path::Path;
use wit_component::{ComponentEncoder, StringEncoding};
use wit_parser::Document;

/// Tests the encoding of the "types only" mode of `wit-component`.
///
/// This test looks in the `interfaces/` directory for test cases in a similar
/// format to those in the `components/` where there's a bunch of:
///
/// * `import-*.wit`
/// * `export-*.wit`
/// * `default.wit`
///
/// Where these represent the "world" of a component. Eventually this should
/// probably become just one file. For now though this suffices and plumbs
/// through the respective arguments of `wit-component`. The `*.wit` files are
/// encoded in "types only" mode and verified against `types_only.wat` and then
/// additionally encoded in normal mode with a dummy module to verify that works
/// as well.
///
/// Run the test with the environment variable `BLESS` set to update
/// the wat baseline file.
#[test]
fn interface_encoding() -> Result<()> {
    for entry in fs::read_dir("tests/interfaces")? {
        let path = entry?.path();
        if !path.is_dir() {
            continue;
        }
        run_test(&path).context(format!("failed test `{}`", path.display()))?;
    }

    Ok(())
}

fn run_test(path: &Path) -> Result<()> {
    let test_case = path.file_stem().unwrap().to_str().unwrap();
    println!("test {test_case}");
    let wit_path = path.join("world.wit");
    let doc = Document::parse_file(&wit_path)?;
    let world = &doc.worlds[doc.default_world()?];
    let world_name = world.name.clone();

    let assert_output = |wasm: &[u8], wat: &Path| -> Result<()> {
        let output = wasmprinter::print_bytes(wasm)?;

        if std::env::var_os("BLESS").is_some() {
            fs::write(wat, output)?;
        } else {
            assert_eq!(
                fs::read_to_string(wat)?.replace("\r\n", "\n").trim(),
                output.trim(),
                "encoding of `{test_case}` did not match the expected wat file `{}`",
                wat.display(),
            );
        }

        let (decoded_doc, decoded_world) = wit_component::decode_world(&world_name, wasm)
            .context(format!("failed to decode bytes for test `{test_case}`"))?;

        if test_case == "empty" {
            return Ok(());
        }

        let decoded_world = &decoded_doc.worlds[decoded_world];
        assert_eq!(decoded_world.imports.len(), world.imports.len());
        assert_eq!(decoded_world.exports.len(), world.exports.len());
        assert_eq!(decoded_world.default.is_some(), world.default.is_some());

        assert_wit(&wit_path, &decoded_doc)?;
        Ok(())
    };

    // Test a types-only component. This ensures that in "types only" mode we
    // can recover all the original `*.wit` interfaces from the generated
    // artifact.

    println!("testing types only");
    let bytes = ComponentEncoder::default()
        .types_only(true)
        .validate(true)
        .document(doc.clone(), StringEncoding::UTF8)?
        .encode()
        .with_context(|| {
            format!("failed to encode a types-only component for test case `{test_case}`")
        })?;
    assert_output(&bytes, &path.join("types_only.wat"))?;

    // Test a full component with a dummy module as the implementation. This
    // tests a different path through `wit-component` to ensure that we can
    // recover the original `*.wit` interfaces from the component output.

    println!("test dummy module");
    let module = test_helpers::dummy_module(&doc);
    let bytes = ComponentEncoder::default()
        .module(&module)?
        .validate(true)
        .document(doc.clone(), StringEncoding::UTF8)?
        .encode()
        .with_context(|| format!("failed to encode a component for test case `{test_case}`"))?;
    assert_output(&bytes, &path.join("component.wat"))?;

    Ok(())
}

fn assert_wit(wit_path: &Path, doc: &Document) -> Result<()> {
    let mut printer = wit_component::DocumentPrinter::default();
    let output = printer.print(doc).context("failed to print interface")?;

    if std::env::var_os("BLESS").is_some() {
        fs::write(&wit_path, output)?;
    } else {
        assert_eq!(
            fs::read_to_string(&wit_path)?.replace("\r\n", "\n"),
            output,
            "encoding of wit file `{}` did not match the the decoded interface",
            wit_path.display(),
        );
    }
    Ok(())
}
