use anyhow::{Context, Result, bail};
use libtest_mimic::{Arguments, Trial};
use pretty_assertions::assert_eq;
use serde_derive::Deserialize;
use std::{borrow::Cow, fs, path::Path};
use wasm_encoder::{Encode, Section};
use wasm_metadata::{Metadata, Payload};
use wasmparser::{Parser, Validator, WasmFeatures};
use wit_component::{ComponentEncoder, DecodedWasm, Linker, StringEncoding, WitPrinter};
use wit_parser::{PackageId, Resolve, UnresolvedPackageGroup};

/// Tests the encoding of components.
///
/// This test looks in the `components/` directory for test cases.
///
/// The expected input files for a test case are:
///
/// * [required] `module.wat` *or* some combination of `lib-$name.wat` and
///   `dlopen-lib-$name.wat` - contains the core module definition(s) to be
///   encoded as a component.  If one or more `lib-$name.wat` and/or
///   `dlopen-lib-$name.wat` files exist, they will be linked using `Linker`
///   such that the `lib-` ones are not `dlopen`-able but the `dlopen-lib-` ones
///   are.
/// * [required] `module.wit` *or* `lib-$name.wat` and `dlopen-lib-$name.wat`
///   corresponding to the WAT files above - WIT package(s) describing the
///   interfaces of the `module.wat` or `lib-$name.wat` and
///   `dlopen-lib-$name.wat` files. Must have a `default world`
/// * [optional] `adapt-$name.wat` - optional adapter for the module name
///   `$name`, can be specified for multiple `$name`s.  Alternatively, if $name
///   doesn't work as part of a filename (e.g. contains forward slashes), it may
///   be specified on the first line of the file with the prefix `;; module name:
///   `, e.g. `;; module name: wasi:cli/environment@0.2.0`.
/// * [optional] `adapt-$name.wit` - required for each `*.wat` adapter to
///   describe imports/exports of the adapter.
///
/// Additionally each test may specify configuration options which tune how the
/// component is encoded. Configuration is specified with comments at the top of
/// any one of a test's input `*.wat` files where each configuration line is
/// prefixed with `;;!`. The contents of all such lines are concatenated
/// together, after stripping the `;;!` prefix, and the result is deserialized
/// as TOML. For example:
///
/// ```text
/// ;;! return-call-ref = true
///
/// (module ...)
/// ```
///
/// Configuration lines may be interleaved with other leading comments, such as
/// the `;; module name: ...` directive above, but they must all appear before
/// the first non-comment line of the file. At most one input file per test may
/// specify configuration.
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
fn main() -> Result<()> {
    drop(env_logger::try_init());

    let mut trials = Vec::new();
    for entry in fs::read_dir("tests/components")? {
        let path = entry?.path();
        if !path.is_dir() {
            continue;
        }

        trials.push(Trial::test(path.to_str().unwrap().to_string(), move || {
            run_test(&path).map_err(|e| format!("{e:?}").into())
        }));
    }

    let mut args = Arguments::from_args();
    if cfg!(target_family = "wasm") && !cfg!(target_feature = "atomics") {
        args.test_threads = Some(1);
    }
    libtest_mimic::run(&args, trials).exit();
}

fn run_test(path: &Path) -> Result<()> {
    let test_case = path.file_stem().unwrap().to_str().unwrap();
    let config = read_config(path)
        .with_context(|| format!("failed to read test configuration in {path:?}"))?;
    let mut resolve = Resolve::default();
    let (pkg_id, _) = resolve.push_dir(&path)?;

    // If this test case contained multiple packages, create separate sub-directories for
    // each.
    let path = path.to_path_buf();

    let module_path = path.join("module.wat");
    let adapters = glob::glob(path.join("adapt-*.wat").to_str().unwrap())?;
    let result = if module_path.is_file() {
        if config.stub_missing_functions || config.use_built_in_libdl {
            bail!(
                "the `stub-missing-functions` and `use-built-in-libdl` options \
                 are only supported for tests which link libraries"
            );
        }
        let mut encoder = ComponentEncoder::default();
        (|| -> Result<_> {
            let module = read_core_module(&module_path, &resolve, pkg_id)
                .with_context(|| format!("failed to read core module at {module_path:?}"))?;
            encoder
                .debug_names(true)
                .shim_return_call_ref(config.return_call_ref)
                .realloc_via_memory_grow(config.realloc_via_memory_grow)
                .emit_canonical_names(config.merge_imports_based_on_canonical_version)
                .module(&module)?;
            for adapter in adapters {
                let (name, wasm) = read_name_and_module("adapt-", &adapter?, &resolve, pkg_id)?;
                encoder.adapter(&name, &wasm)?;
            }
            encoder.encode()
        })()
    } else {
        let mut libs = glob::glob(path.join("lib-*.wat").to_str().unwrap())?
            .map(|path| Ok(("lib-", path?, false)))
            .chain(
                glob::glob(path.join("dlopen-lib-*.wat").to_str().unwrap())?
                    .map(|path| Ok(("dlopen-lib-", path?, true))),
            )
            .collect::<Result<Vec<_>>>()?;

        // Sort list to ensure deterministic order, which determines priority in cases of duplicate symbols:
        libs.sort_by(|(_, a, _), (_, b, _)| a.cmp(b));

        let mut linker = Linker::default();
        linker
            .stub_missing_functions(config.stub_missing_functions)
            .use_built_in_libdl(config.use_built_in_libdl)
            .encoder()
            .validate(false)
            .debug_names(true)
            .shim_return_call_ref(config.return_call_ref)
            .realloc_via_memory_grow(config.realloc_via_memory_grow);

        (|| -> Result<_> {
            for (prefix, path, dl_openable) in libs {
                let (name, wasm) = read_name_and_module(prefix, &path, &resolve, pkg_id)?;
                linker.library(&name, &wasm, dl_openable)?;
            }
            for path in adapters {
                let (name, wasm) = read_name_and_module("adapt-", &path?, &resolve, pkg_id)?;
                linker.encoder().adapter(&name, &wasm)?;
            }

            linker.encode()
        })()
    };
    let component_path = path.join("component.wat");
    let component_wit_path = path.join("component.wit.print");
    let error_path = path.join("error.txt");

    let bytes = match result {
        Ok(bytes) => {
            if test_case.starts_with("error-") {
                bail!("expected an error but got success");
            }
            bytes
        }
        Err(err) => {
            if !test_case.starts_with("error-") {
                return Err(err);
            }
            assert_output(&format!("{err:#}"), &error_path)?;
            return Ok(());
        }
    };

    let wat = wasmprinter::print_bytes(&bytes).context("failed to print bytes")?;
    assert_output(&wat, &component_path)?;

    Validator::new_with_features(WasmFeatures::all())
        .validate_all(&bytes)
        .context("failed to validate component output")?;

    let mut parser = Parser::new(0);
    parser.set_features(WasmFeatures::all());
    let (pkg, resolve) = match wit_component::decode_reader(bytes.as_slice())
        .context("failed to decode resolve")?
    {
        DecodedWasm::WitPackage(..) => unreachable!(),
        DecodedWasm::Component(resolve, world) => (resolve.worlds[world].package.unwrap(), resolve),
    };
    let mut printer = WitPrinter::default();
    printer
        .print(&resolve, pkg, &[])
        .context("failed to print WIT")?;
    let wit = printer.output.to_string();
    assert_output(&wit, &component_wit_path)?;

    UnresolvedPackageGroup::parse(&component_wit_path, &wit)
        .map_err(|(map, e)| anyhow::anyhow!("{}", e.render(&map)))
        .context("failed to parse printed WIT")?;

    // Check that the producer data got piped through properly
    match Payload::from_binary(&bytes).unwrap() {
        // Depends on the ComponentEncoder always putting the first module as the 0th child:
        Payload::Component { children, .. } => match &children[0] {
            Payload::Module(Metadata { producers, .. }) => {
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
                if module_path.is_file() {
                    assert_eq!(
                        processed_by
                            .get("my-fake-bindgen")
                            .expect("added bindgen field present"),
                        "123.45"
                    );
                } else {
                    // Otherwise, we used `Linker`, which synthesizes the
                    // "main" module and thus won't have `my-fake-bindgen`
                }
            }
            _ => panic!("expected child to be a module"),
        },
        _ => panic!("expected top level metadata of component"),
    }

    Ok(())
}

/// Configuration for a test which tunes how its component is encoded.
#[derive(Default, Deserialize)]
#[serde(default, deny_unknown_fields, rename_all = "kebab-case")]
struct Config {
    stub_missing_functions: bool,
    use_built_in_libdl: bool,
    return_call_ref: bool,
    realloc_via_memory_grow: bool,
    merge_imports_based_on_canonical_version: bool,
}

/// Reads the configuration for the test located at `path`.
///
/// All input `*.wat` files for this test are searched for configuration and at
/// most one of them may specify it. If none do then a default configuration is
/// returned.
fn read_config(path: &Path) -> Result<Config> {
    let mut files = glob::glob(path.join("*.wat").to_str().unwrap())?
        .map(|p| Ok(p?))
        .collect::<Result<Vec<_>>>()?;
    files.sort();

    let mut found = None;
    for file in files {
        if file.file_name().and_then(|s| s.to_str()) == Some("component.wat") {
            continue;
        }
        let contents = fs::read_to_string(&file)?;
        let Some(config) = extract_config(&contents) else {
            continue;
        };
        if let Some((prev, _)) = &found {
            bail!(
                "test configuration is specified in both {prev:?} and {file:?}, \
                 but at most one file per test may specify configuration"
            );
        }
        found = Some((file, config));
    }

    let Some((file, config)) = found else {
        return Ok(Config::default());
    };
    return toml::from_str(&config)
        .with_context(|| format!("failed to parse configuration in {file:?}"));

    fn extract_config(contents: &str) -> Option<String> {
        let mut config = String::new();
        for line in contents.lines() {
            let line = line.trim();
            if let Some(rest) = line.strip_prefix(";;!") {
                config.push_str(rest);
                config.push('\n');
            } else if line.is_empty() || line.starts_with(";;") {
                continue;
            } else {
                break;
            }
        }
        if config.is_empty() {
            None
        } else {
            Some(config)
        }
    }
}

fn read_name_and_module(
    prefix: &str,
    path: &Path,
    resolve: &Resolve,
    pkg: PackageId,
) -> Result<(String, Vec<u8>)> {
    let wasm = read_core_module(path, resolve, pkg)
        .with_context(|| format!("failed to read core module at {path:?}"))?;
    let stem = path.file_stem().unwrap().to_str().unwrap();
    let contents = fs::read_to_string(path)?;
    let name = if let Some(name) = contents
        .lines()
        .map(|line| line.trim())
        .take_while(|line| line.is_empty() || line.starts_with(";;"))
        .find_map(|line| line.strip_prefix(";; module name: "))
    {
        name.to_owned()
    } else {
        stem.trim_start_matches(prefix).to_owned()
    };
    Ok((name, wasm))
}

/// Parses the core wasm module at `path`, expected as a `*.wat` file.
///
/// The `resolve` and `pkg` are the parsed WIT package from this test's
/// directory and the `path`'s filename is used to find a WIT document of the
/// corresponding name which should have a world that `path` ascribes to.
fn read_core_module(path: &Path, resolve: &Resolve, pkg: PackageId) -> Result<Vec<u8>> {
    let mut wasm = wat::parse_file(path)?;
    let name = path.file_stem().and_then(|s| s.to_str()).unwrap();
    let world = resolve
        .select_world(&[pkg], Some(name))
        .context("failed to select a world")?;

    // Add this producer data to the wit-component metadata so we can make sure it gets through the
    // translation:
    let mut producers = wasm_metadata::Producers::empty();
    producers.add("processed-by", "my-fake-bindgen", "123.45");

    let encoded =
        wit_component::metadata::encode(resolve, world, StringEncoding::UTF8, Some(&producers))?;

    let section = wasm_encoder::CustomSection {
        name: "component-type".into(),
        data: Cow::Borrowed(&encoded),
    };
    wasm.push(section.id());
    section.encode(&mut wasm);
    Ok(wasm)
}

fn assert_output(contents: &str, path: &Path) -> Result<()> {
    let contents = contents.replace("\r\n", "\n").replace(
        concat!("\"", env!("CARGO_PKG_VERSION"), "\""),
        "\"$CARGO_PKG_VERSION\"",
    );
    if std::env::var_os("BLESS").is_some() {
        if let Ok(prev) = fs::read_to_string(path)
            && prev == contents
        {
            return Ok(());
        }
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
