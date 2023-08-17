use anyhow::{bail, Context, Result};
use pretty_assertions::assert_eq;
use std::fs;
use std::io;
use wasm_compose::{composer::ComponentComposer, config::Config};
use wasmparser::{Validator, WasmFeatures};

/// Tests the composing of components.
///
/// This test looks in the `compositions/` directory for test cases.
///
/// The expected input files for a test case are:
/// * [required] `root.wat` - the root component being composed.
/// * [optional] `config.yml` - contains the composition configuration.
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
        println!("================ {test_case:30} ===============");
        let root_path = path.join("root.wat");
        let output_path = path.join("composed.wat");
        let error_path = path.join("error.txt");
        let config_path = path.join("config.yml");
        let config = if config_path.is_file() {
            Config::from_file(config_path)?
        } else {
            Config {
                dir: path.clone(),
                ..Default::default()
            }
        };
        let composer = ComponentComposer::new(&root_path, &config);

        let r = composer.compose();
        let (output, baseline_path) = if error_path.is_file() {
            match r {
                Ok(_) => bail!("composition should fail for test case `{}`", test_case),
                Err(e) => {
                    let mut err = format!("{e}");
                    let causes = e.chain();
                    let ncauses = causes.len();
                    if ncauses > 1 {
                        err.push_str("\n\nCaused by:");
                    }
                    for (i, cause) in causes.skip(1).enumerate() {
                        err.push_str("\n    ");
                        if ncauses > 2 {
                            err.push_str(&format!("{i}: "))
                        }
                        match cause.downcast_ref::<io::Error>() {
                            Some(_) => {
                                err.push_str("platform-specific error");
                            }
                            None => {
                                err.push_str(&cause.to_string());
                            }
                        }
                    }
                    (err.replace('\\', "/"), &error_path)
                }
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

            wit_component::decode(&bytes).with_context(|| {
                format!(
                    "failed to decode WIT from component bytes for test case `{}`",
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
                &output_path,
            )
        };

        if std::env::var_os("BLESS").is_some() {
            fs::write(baseline_path, output + "\n")?;
        } else {
            assert_eq!(
                fs::read_to_string(baseline_path)
                    .with_context(|| format!(
                        "failed to read component baseline `{}`",
                        baseline_path.display()
                    ))?
                    .replace("\r\n", "\n")
                    .trim(),
                output,
                "failed baseline comparison for test case `{}` ({})",
                test_case,
                baseline_path.display(),
            );
        }
    }

    Ok(())
}
