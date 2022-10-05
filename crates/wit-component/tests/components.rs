use anyhow::{bail, Context, Result};
use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use wit_component::{ComponentEncoder, InterfaceEncoder};
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

fn read_adapters(dir: &Path) -> Result<Vec<(String, Vec<u8>, Interface)>> {
    glob::glob(dir.join("adapt-*.wat").to_str().unwrap())?
        .map(|p| {
            let p = p?;
            let adapter =
                wat::parse_file(&p).with_context(|| format!("expected file `{}`", p.display()))?;
            let stem = p.file_stem().unwrap().to_str().unwrap();
            let glob = format!("{stem}-import-*.wit");
            let wit = match glob::glob(dir.join(&glob).to_str().unwrap())?.next() {
                Some(path) => path?,
                None => bail!("failed to find `{glob}` match"),
            };
            let mut i = read_interface(&wit)?;
            i.name = wit
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .trim_start_matches(stem)
                .trim_start_matches("-import-")
                .to_string();
            Ok((stem.trim_start_matches("adapt-").to_string(), adapter, i))
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
        let adapters = read_adapters(&path)?;

        let mut encoder = ComponentEncoder::default()
            .module(&module)?
            .imports(&imports)
            .exports(&exports)
            .validate(true);

        for (name, wasm, interface) in adapters.iter() {
            encoder = encoder.adapter(name, wasm, interface);
        }

        if let Some(interface) = &interface {
            encoder = encoder.interface(interface);
        }

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
    use wasm_encoder::{Encode, Section};

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

        fn encode_interface(i: &Interface, module: &mut Vec<u8>, kind: &str) -> Result<()> {
            let name = &format!("component-type:{}:{}", kind, i.name);
            let contents = InterfaceEncoder::new(&i).validate(true).encode()?;
            let section = wasm_encoder::CustomSection {
                name,
                data: &contents,
            };
            module.push(section.id());
            section.encode(module);
            Ok(())
        }

        // Encode the interface, exports, and imports into the module, instead of
        // passing them to the ComponentEncoder explicitly.
        if interface_path.is_file() {
            let i = read_interface(&interface_path)?;
            encode_interface(&i, &mut module, "export")?;
        }
        for i in read_interfaces(&path, "import-*.wit")? {
            encode_interface(&i, &mut module, "import")?;
        }
        for i in read_interfaces(&path, "export-*.wit")? {
            encode_interface(&i, &mut module, "export-instance")?;
        }
        //
        let adapters = read_adapters(&path)?;

        let mut encoder = ComponentEncoder::default().module(&module)?.validate(true);

        for (name, wasm, interface) in adapters.iter() {
            encoder = encoder.adapter(name, wasm, interface);
        }

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
    }

    Ok(())
}
