use anyhow::{bail, Context, Result};
use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use wasm_compose::{composer::ComponentComposer, config::Config};
use wasmparser::{Validator, WasmFeatures};

fn read_config(dir: impl AsRef<Path>) -> Result<Config> {
    let dir = dir.as_ref();

    let path = dir.join("wasm-compose.yml");
    if path.is_file() {
        return Config::from_file(path);
    }

    let path = dir.join("wasm-compose.yaml");
    if path.is_file() {
        return Config::from_file(path);
    }

    Config::from_file(dir.join("wasm-compose.toml"))
}

/// Tests the composing of components.
///
/// This test looks in the `compositions/` directory for test cases.
///
/// The expected input files for a test case are:
///
/// * [required] `wasm-compose.toml` - contains the composition configuration.
/// * [optional] `*.wat` - represents a component imported for the composition.
///
/// And the output files are one of the following:
///
/// * `composed.wat` - the composed component if the composition is expected to succeed.
/// * `error.txt` - the expected error message if the composition is expected to fail.
///
/// The test composes a component based on the input files. If the encoding succeeds,
/// it expects the output to match `composed.wat`. If the encoding fails, it expects
/// the output to match `error.txt`.
///
/// Run the test with the environment variable `BLESS` set to update
/// either `composed.wat` or `error.txt` depending on the outcome of the composition.
#[test]
fn component_composing() -> Result<()> {
    for entry in fs::read_dir("tests/compositions")? {
        let path = entry?.path();
        if !path.is_dir() {
            continue;
        }

        let test_case = path.file_stem().unwrap().to_str().unwrap();
        let component_path = path.join("composed.wat");
        let error_path = path.join("error.txt");

        let config = read_config(&path)?;
        let composer = ComponentComposer::new(&config);

        let r = composer.compose(false);
        let (output, baseline_path) = if error_path.is_file() {
            match r {
                Ok(_) => bail!("composition should fail for test case `{}`", test_case),
                Err(e) => (
                    format!("{:?}", e).replace('\\', "/").replace(
                        "The system cannot find the file specified.",
                        "No such file or directory",
                    ),
                    &error_path,
                ),
            }
        } else {
            let bytes =
                r.with_context(|| format!("failed to encode for test case `{}`", test_case))?;

            Validator::new_with_features(WasmFeatures {
                component_model: true,
                ..Default::default()
            })
            .validate_all(&bytes)
            .with_context(|| {
                format!(
                    "failed to validate component bytes for test case `{}`",
                    test_case
                )
            })?;

            (
                wasmprinter::print_bytes(&bytes).with_context(|| {
                    format!(
                        "failed to print component bytes for test case `{}`",
                        test_case
                    )
                })?,
                &component_path,
            )
        };

        if std::env::var_os("BLESS").is_some() {
            fs::write(&baseline_path, output)?;
        } else {
            assert_eq!(
                fs::read_to_string(&baseline_path)
                    .with_context(|| format!(
                        "failed to read component baseline `{}`",
                        baseline_path.display()
                    ))?
                    .replace("\r\n", "\n"),
                output,
                "failed baseline comparison for test case `{}` ({})",
                test_case,
                baseline_path.display(),
            );
        }
    }

    Ok(())
}
