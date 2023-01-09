use anyhow::{anyhow, bail, Context, Result};
use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use wasm_encoder::{Encode, Section};
use wit_component::{ComponentEncoder, StringEncoding};
use wit_parser::{Resolve, UnresolvedPackage};

/// Tests the encoding of components.
///
/// This test looks in the `components/` directory for test cases.
///
/// The expected input files for a test case are:
///
/// * [required] `module.wat` - contains the core module definition to be
///   encoded as a component.
/// * [required] `module.wit` - WIT package describing the interface of
///   `module.wat`. Must have a `default world`
/// * [optional] `adapt-$name.wat` - optional adapter for the module name
///   `$name`, can be specified for multiple `$name`s
/// * [optional] `adapt-$name.wit` - required for each `*.wat` adapter to
///   describe imports/exports of the adapter.
///
/// And the output files are one of the following:
///
/// * `component.wat` - the expected encoded component in text format if the
///   encoding is expected to succeed.
/// * `error.txt` - the expected error message if the encoding is expected to
///   fail.
///
/// The test encodes a component based on the input files. If the encoding
/// succeeds, it expects the output to match `component.wat`. If the encoding
/// fails, it expects the output to match `error.txt`.
///
/// Run the test with the environment variable `BLESS` set to update
/// either `component.wat` or `error.txt` depending on the outcome of the encoding.
#[test]
fn component_encoding_via_flags() -> Result<()> {
    drop(env_logger::try_init());

    for entry in fs::read_dir("tests/components")? {
        let path = entry?.path();
        if !path.is_dir() {
            continue;
        }

        let test_case = path.file_stem().unwrap().to_str().unwrap();
        println!("testing {test_case}");

        let module_path = path.join("module.wat");
        let component_path = path.join("component.wat");
        let error_path = path.join("error.txt");
        let module = read_core_module(&module_path)?;
        let mut encoder = ComponentEncoder::default().module(&module)?.validate(true);
        encoder = add_adapters(encoder, &path)?;
        assert_output(test_case, &encoder, &component_path, &error_path)?;
    }

    Ok(())
}

fn add_adapters(mut encoder: ComponentEncoder, path: &Path) -> Result<ComponentEncoder> {
    for adapter in glob::glob(path.join("adapt-*.wat").to_str().unwrap())? {
        let adapter = adapter?;
        let wasm = read_core_module(&adapter)?;
        let stem = adapter.file_stem().unwrap().to_str().unwrap();
        let name = stem.trim_start_matches("adapt-");
        encoder = encoder.adapter(&name, &wasm)?;
    }
    Ok(encoder)
}

/// Parses the core wasm module at `path`, expected as a `*.wat` file.
///
/// Additionally expects a sibling `*.wit` file which will be used to encode
/// metadata into the binary returned here.
fn read_core_module(path: &Path) -> Result<Vec<u8>> {
    let mut wasm = wat::parse_file(path)?;
    let interface = path.with_extension("wit");
    let mut resolve = Resolve::default();
    let pkg = resolve.push(
        UnresolvedPackage::parse_file(&interface)?,
        &Default::default(),
    )?;
    let doc = *resolve.packages[pkg].documents.iter().next().unwrap().1;
    let doc = &resolve.documents[doc];
    let world = doc
        .default_world
        .ok_or_else(|| anyhow!("no default world specified"))?;
    let encoded = wit_component::metadata::encode(&resolve, world, StringEncoding::UTF8)?;

    let section = wasm_encoder::CustomSection {
        name: "component-type",
        data: &encoded,
    };
    wasm.push(section.id());
    section.encode(&mut wasm);
    Ok(wasm)
}

fn assert_output(
    test_case: &str,
    encoder: &ComponentEncoder,
    component_path: &Path,
    error_path: &Path,
) -> Result<()> {
    let r = encoder.encode();

    let (output, baseline_path) = if error_path.is_file() {
        match r {
            Ok(_) => bail!("encoding should fail for test case `{}`", test_case),
            Err(e) => (format!("{e:?}"), &error_path),
        }
    } else {
        (
            wasmprinter::print_bytes(
                &r.with_context(|| format!("failed to encode for test case `{}`", test_case))?,
            )
            .with_context(|| {
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
            fs::read_to_string(&baseline_path)?
                .replace("\r\n", "\n")
                .trim(),
            output.trim(),
            "failed baseline comparison for test case `{}` ({})",
            test_case,
            baseline_path.display(),
        );
    }
    Ok(())
}
