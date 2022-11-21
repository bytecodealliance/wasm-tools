use anyhow::{bail, Context, Result};
use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use wasm_encoder::{Encode, Section};
use wit_component::{ComponentEncoder, StringEncoding};
use wit_parser::World;

fn read_adapters(dir: &Path) -> Result<Vec<(String, Vec<u8>, World)>> {
    glob::glob(dir.join("adapt-*.wat").to_str().unwrap())?
        .map(|p| {
            let p = p?;
            let adapter =
                wat::parse_file(&p).with_context(|| format!("expected file `{}`", p.display()))?;
            let stem = p.file_stem().unwrap().to_str().unwrap();
            let world = read_world(dir, &format!("{stem}-"))?;
            Ok((
                stem.trim_start_matches("adapt-").to_string(),
                adapter,
                world,
            ))
        })
        .collect::<Result<_>>()
}

/// Tests the encoding of components.
///
/// This test looks in the `components/` directory for test cases.
///
/// The expected input files for a test case are:
///
/// * [required] `module.wat` - contains the core module definition to be encoded
///   as a component.
/// * [optional] `default.wit` - represents the component's default interface.
/// * [optional] `export-<name>.wit` - represents an interface exported by the component.
/// * [optional] `import-<name>.wit` - represents an interface imported by the component.
///
/// And the output files are one of the following:
///
/// * `component.wat` - the expected encoded component in text format if the encoding
///   is expected to succeed.
/// * `error.txt` - the expected error message if the encoding is expected to fail.
///
/// The test encodes a component based on the input files. If the encoding succeeds,
/// it expects the output to match `component.wat`. If the encoding fails, it expects
/// the output to match `error.txt`.
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

        let module = wat::parse_file(&module_path)
            .with_context(|| format!("expected file `{}`", module_path.display()))?;

        let mut encoder = ComponentEncoder::default()
            .module(&module)?
            .validate(true)
            .world(read_world(&path, "")?, StringEncoding::UTF8)?;
        encoder = add_adapters(encoder, &path)?;

        assert_output(test_case, &encoder, &component_path, &error_path)?;
    }

    Ok(())
}

/// Tests the encoding of components.
///
/// This test looks in the `components/` directory for test cases. It parses
/// the inputs to the test out of that directly exactly like
/// `component_encoding_via_flags` does in this same file.
///
/// Rather than pass the default interface, imports, and exports directly to
/// the `ComponentEncoder`, this test encodes those Interfaces as component
/// types in custom sections of the wasm Module.
///
/// This simulates the flow that toolchains which don't yet know how to
/// emit a Component will emit a canonical ABI Module containing these custom sections,
/// and those will then be translated by wit-component to a Component without
/// needing the wit files passed in as well.
#[test]
fn component_encoding_via_custom_sections() -> Result<()> {
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

        let mut module = wat::parse_file(&module_path)
            .with_context(|| format!("expected file `{}`", module_path.display()))?;

        // Create the `component-type` custom section which will encode all of
        // the type information about imported/exported functions.
        let world = read_world(&path, "")?;
        let contents = wit_component::metadata::encode(&world, StringEncoding::UTF8);
        let section = wasm_encoder::CustomSection {
            name: "component-type",
            data: &contents,
        };
        module.push(section.id());
        section.encode(&mut module);

        // Now parse run the `module` alone through the encoder without extra
        // information about interfaces to ensure it still works as before.
        let mut encoder = ComponentEncoder::default().module(&module)?.validate(true);
        encoder = add_adapters(encoder, &path)?;
        assert_output(test_case, &encoder, &component_path, &error_path)?;
    }

    Ok(())
}

fn read_world(path: &Path, prefix: &str) -> Result<World> {
    let world_path = path.join(&format!("{prefix}world.wit"));
    World::parse_file(&world_path)
}

fn add_adapters(mut encoder: ComponentEncoder, path: &Path) -> Result<ComponentEncoder> {
    for (name, mut wasm, interfaces) in read_adapters(path)? {
        // Create a `component-type` custom section by slurping up `imports` as
        // a "world" and encoding it.
        let contents = wit_component::metadata::encode(&interfaces, StringEncoding::UTF8);
        let section = wasm_encoder::CustomSection {
            name: "component-type",
            data: &contents,
        };
        wasm.push(section.id());
        section.encode(&mut wasm);

        // Then register our new wasm blob which has the necessary custom
        // section.
        encoder = encoder.adapter(&name, &wasm)?;
    }
    Ok(encoder)
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
            Err(e) => (e.to_string(), &error_path),
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
            fs::read_to_string(&baseline_path)?.replace("\r\n", "\n"),
            output,
            "failed baseline comparison for test case `{}` ({})",
            test_case,
            baseline_path.display(),
        );
    }
    Ok(())
}
