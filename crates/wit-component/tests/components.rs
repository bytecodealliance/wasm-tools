use anyhow::{bail, Context, Result};
use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use wasm_encoder::{Encode, Section};
use wit_component::ComponentEncoder;
use wit_parser::Interface;

fn read_interface(path: &Path) -> Result<Interface> {
    wit_parser::Interface::parse_file(&path)
        .with_context(|| format!("failed to parse interface file `{}`", path.display()))
}

fn read_interfaces(dir: &Path, pattern: &str) -> Result<Vec<Interface>> {
    glob::glob(dir.join(pattern).to_str().unwrap())?
        .map(|p| {
            let p = p?;
            let mut i = read_interface(&p)?;
            i.name = p
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .trim_start_matches("import-")
                .trim_start_matches("export-")
                .to_string();
            Ok(i)
        })
        .collect::<Result<_>>()
}

fn read_adapters(dir: &Path) -> Result<Vec<(String, Vec<u8>, Vec<Interface>)>> {
    glob::glob(dir.join("adapt-*.wat").to_str().unwrap())?
        .map(|p| {
            let p = p?;
            let adapter =
                wat::parse_file(&p).with_context(|| format!("expected file `{}`", p.display()))?;
            let stem = p.file_stem().unwrap().to_str().unwrap();
            let glob = format!("{stem}-import-*.wit");
            let imports = glob::glob(dir.join(&glob).to_str().unwrap())?
                .map(|path| {
                    let path = path?;
                    let mut i = read_interface(&path)?;
                    i.name = path
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .trim_start_matches(stem)
                        .trim_start_matches("-import-")
                        .to_string();
                    Ok(i)
                })
                .collect::<Result<Vec<_>>>()?;
            Ok((
                stem.trim_start_matches("adapt-").to_string(),
                adapter,
                imports,
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

        let module_path = path.join("module.wat");
        let interface_path = path.join("default.wit");
        let component_path = path.join("component.wat");
        let error_path = path.join("error.txt");

        let module = wat::parse_file(&module_path)
            .with_context(|| format!("expected file `{}`", module_path.display()))?;
        let interface = interface_path
            .is_file()
            .then(|| read_interface(&interface_path))
            .transpose()?;
        let imports = read_interfaces(&path, "import-*.wit")?;
        let exports = read_interfaces(&path, "export-*.wit")?;

        let mut encoder = ComponentEncoder::default()
            .module(&module)?
            .imports(imports)?
            .exports(exports)?
            .validate(true);
        encoder = add_adapters(encoder, &path)?;

        if let Some(interface) = interface {
            encoder = encoder.interface(interface)?;
        }

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

        let module_path = path.join("module.wat");
        let interface_path = path.join("default.wit");
        let component_path = path.join("component.wat");
        let error_path = path.join("error.txt");

        let mut module = wat::parse_file(&module_path)
            .with_context(|| format!("expected file `{}`", module_path.display()))?;

        // Use a first `encoder` to encode all the `*.wit` interface information
        // into a "types only" component which is then placed into a custom
        // section.
        // of passing them to the ComponentEncoder explicitly.
        let mut encoder = ComponentEncoder::default().types_only(true).validate(true);
        encoder = encoder.imports(read_interfaces(&path, "import-*.wit")?)?;
        encoder = encoder.exports(read_interfaces(&path, "export-*.wit")?)?;
        if interface_path.is_file() {
            encoder = encoder.interface(read_interface(&interface_path)?)?;
        }
        let contents = encoder.encode()?;
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

fn add_adapters(mut encoder: ComponentEncoder, path: &Path) -> Result<ComponentEncoder> {
    for (name, mut wasm, imports) in read_adapters(path)? {
        // Create a `component-type` custom section by slurping up `imports` as
        // a "world" and encoding it.
        let mut types_encoder = ComponentEncoder::default().types_only(true).validate(true);
        types_encoder = types_encoder.imports(imports)?;
        let contents = types_encoder.encode()?;
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
