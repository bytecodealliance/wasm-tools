use anyhow::Result;
use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use wasm_encoder::{Encode, Section};
use wit_component::{ComponentEncoder, DecodedWasm, DocumentPrinter, StringEncoding};
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
/// * `component.wit` - if `component.wat` exists this is the inferred interface
///   of the component.
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
        let module = read_core_module(&module_path)?;
        let mut encoder = ComponentEncoder::default().module(&module)?.validate(true);
        encoder = add_adapters(encoder, &path)?;
        let component_path = path.join("component.wat");
        let component_wit_path = path.join("component.wit");
        let error_path = path.join("error.txt");

        let bytes = match encoder.encode() {
            Ok(bytes) => bytes,
            Err(err) => {
                assert_output(&format!("{err:?}"), &error_path)?;
                continue;
            }
        };

        let wat = wasmprinter::print_bytes(&bytes)?;
        assert_output(&wat, &component_path)?;
        let (doc, resolve) = match wit_component::decode("component", &bytes)? {
            DecodedWasm::WitPackage(..) => unreachable!(),
            DecodedWasm::Component(resolve, world) => (resolve.worlds[world].document, resolve),
        };
        let wit = DocumentPrinter::default().print(&resolve, doc)?;
        assert_output(&wit, &component_wit_path)?;

        // Check that the producer data got piped through properly
        let metadata = wasm_metadata::Metadata::from_binary(&bytes)?;
        match metadata {
            // Depends on the ComponentEncoder always putting the first module as the 0th child:
            wasm_metadata::Metadata::Component { children, .. } => match children[0].as_ref() {
                wasm_metadata::Metadata::Module { producers, .. } => {
                    let producers = producers.as_ref().expect("child module has producers");
                    let processed_by = producers
                        .get("processed-by")
                        .expect("child has processed-by section");
                    assert_eq!(
                        processed_by
                            .get("wit-component")
                            .expect("wit-component producer present"),
                        env!("CARGO_PKG_VERSION")
                    );
                    assert_eq!(
                        processed_by
                            .get("my-fake-bindgen")
                            .expect("added bindgen field present"),
                        "123.45"
                    );
                }
                _ => panic!("expected child to be a module"),
            },
            _ => panic!("expected top level metadata of component"),
        }
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
    let world = resolve.select_world(pkg, None)?;

    // Add this producer data to the wit-component metadata so we can make sure it gets through the
    // translation:
    let mut producers = wasm_metadata::Producers::empty();
    producers.add("processed-by", "my-fake-bindgen", "123.45");

    let encoded =
        wit_component::metadata::encode(&resolve, world, StringEncoding::UTF8, Some(&producers))?;

    let section = wasm_encoder::CustomSection {
        name: "component-type",
        data: &encoded,
    };
    wasm.push(section.id());
    section.encode(&mut wasm);
    Ok(wasm)
}

fn assert_output(contents: &str, path: &Path) -> Result<()> {
    let contents = contents.replace("\r\n", "\n");
    if std::env::var_os("BLESS").is_some() {
        fs::write(path, contents)?;
    } else {
        match fs::read_to_string(path) {
            Ok(expected) => {
                assert_eq!(
                    expected.replace("\r\n", "\n").trim(),
                    contents.trim(),
                    "failed baseline comparison ({})",
                    path.display(),
                );
            }
            Err(_) => {
                panic!("expected {path:?} to contain\n{contents}");
            }
        }
    }
    Ok(())
}
