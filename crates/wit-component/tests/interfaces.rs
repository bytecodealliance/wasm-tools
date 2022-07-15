use anyhow::{Context, Result};
use pretty_assertions::assert_eq;
use std::fs;
use wit_component::InterfaceEncoder;
use wit_parser::Interface;

/// Tests the encoding of individual interface files.
///
/// This test looks in the `interfaces/` directory for test cases.
///
/// Each test case is a directory containing a `<testcase>.wit` file
/// and an expected `<testcase>.wat` file.
///
/// The test encodes the wit file, prints the resulting component, and
/// compares the output to the wat file.
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

        let test_case = path.file_stem().unwrap().to_str().unwrap();
        let wit_path = path.join(test_case).with_extension("wit");

        let interface = Interface::parse_file(&wit_path)?;

        let encoder = InterfaceEncoder::new(&interface).validate(true);

        let bytes = encoder.encode().with_context(|| {
            format!(
                "failed to encode a component from interface `{}` for test case `{}`",
                wit_path.display(),
                test_case,
            )
        })?;

        let output = wasmprinter::print_bytes(&bytes)?;
        let wat_path = wit_path.with_extension("wat");

        if std::env::var_os("BLESS").is_some() {
            fs::write(&wat_path, output)?;
        } else {
            assert_eq!(
                fs::read_to_string(&wat_path)?.replace("\r\n", "\n"),
                output,
                "encoding of wit file `{}` did not match the expected wat file `{}` for test case `{}`",
                wit_path.display(),
                wat_path.display(),
                test_case
            );
        }
    }

    Ok(())
}
