use anyhow::{bail, Context, Result};
use std::{fs, path::Path};
use wit_parser::{Resolve, UnresolvedPackageGroup, WorldId};

/// Tests whether a component targets a world.
///
/// This test looks in the `targets/` directory for test cases.
///
/// The expected input files for a test case are:
///
/// * [required] `test.wat` -- contains the component to test
/// encoded as a component produced via the `embed` and `new`
/// subcommands of `wasm-tools component`.
/// * [required] `test.wit` -- WIT package describing the target
/// world to use when checking conformance.
///
/// And the output file is:
/// * `error.txt` - the expected error message if the synthetic
/// component constructed for testing is a invalid component encoding.
/// NOTE: an invalid encoding here indicates that the targets check has failed.
///
/// Run the test with the environment variable `BLESS` set to update `error.txt`.
///
/// Each test is effectively executing as:
/// ```wasm-tools component targets -w foobar test.wit test.wat```
#[test]
fn targets() -> Result<()> {
    drop(env_logger::try_init());

    for entry in fs::read_dir("tests/targets")? {
        let path = entry?.path();
        if !path.is_dir() {
            continue;
        }

        let test_case = path.file_stem().unwrap().to_str().unwrap();
        println!("testing {test_case}");

        let (resolve, world) = load_test_wit(&path)?;

        let component = wat::parse_file(path.join("test.wat"))
            .with_context(|| "failed to parse component WAT".to_string())?;

        match wit_component::targets(&resolve, world, &component) {
            Ok(_) => {
                assert!(
                    !test_case.starts_with("error-"),
                    "should have failed targets check"
                );
            }
            Err(e) => {
                assert!(test_case.starts_with("error-"), "{e:?}");
                assert_output(&path.join("error.txt"), &format!("{e:?}"))?;
            }
        }
    }

    Ok(())
}

fn assert_output(expected: &Path, actual: &str) -> Result<()> {
    if std::env::var_os("BLESS").is_some() {
        fs::create_dir_all(expected.parent().unwrap())?;
        fs::write(expected, actual).with_context(|| format!("failed to write {expected:?}"))?;
    } else {
        assert_eq!(
            fs::read_to_string(expected)
                .with_context(|| format!("failed to read {expected:?}"))?
                .replace("\r\n", "\n"),
            actual,
            "expectation `{}` did not match actual",
            expected.display(),
        );
    }
    Ok(())
}

fn load_test_wit(path: &Path) -> Result<(Resolve, WorldId)> {
    const TEST_TARGET_WORLD_ID: &str = "foobar";

    let test_wit_path = path.join("test.wit");
    let UnresolvedPackageGroup {
        mut packages,
        source_map,
    } = UnresolvedPackageGroup::parse_file(&test_wit_path)
        .context("failed to parse WIT package")?;
    if packages.is_empty() {
        bail!("Files were completely empty - are you sure these are the files you're looking for?")
    }
    if packages.len() > 1 {
        bail!("Multi-package targeting tests are not yet supported.")
    }

    let mut resolve = Resolve::default();
    let package_id = resolve.push(packages.remove(0), &source_map)?;

    let world_id = resolve
        .select_world(package_id, Some(TEST_TARGET_WORLD_ID))
        .with_context(|| "failed to select world from package".to_string())?;

    Ok((resolve, world_id))
}
