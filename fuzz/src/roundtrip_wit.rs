use arbitrary::{Result, Unstructured};
use std::path::Path;
use wasmparser::WasmFeatures;
use wit_component::*;
use wit_parser::{Resolve, SourceMap};

pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    let wasm = u.arbitrary().and_then(|config| {
        log::debug!("config: {config:#?}");
        wit_smith::smith(&config, u)
    })?;
    write_file("doc1.wasm", &wasm);
    let (resolve, _pkg) = match wit_component::decode(&wasm).unwrap() {
        DecodedWasm::WitPackages(resolve, pkg) => (resolve, pkg),
        DecodedWasm::Component(..) => unreachable!(),
    };

    roundtrip_through_printing("doc1", &resolve, &wasm);

    let (resolve2, pkgs2) = match wit_component::decode(&wasm).unwrap() {
        DecodedWasm::WitPackages(resolve, pkgs) => (resolve, pkgs),
        DecodedWasm::Component(..) => unreachable!(),
    };

    // wit_smith returns WIT source with only a single package.
    if pkgs2.len() != 1 {
        panic!("rountrip WIT test smithed file with multiple packages")
    }

    let pkg2 = pkgs2[0];
    let wasm2 =
        wit_component::encode(Some(true), &resolve2, pkg2).expect("failed to encode WIT document");
    write_file("doc2.wasm", &wasm2);
    roundtrip_through_printing("doc2", &resolve2, &wasm2);

    if wasm != wasm2 {
        panic!("roundtrip wasm didn't match");
    }

    // If there's hundreds or thousands of worlds only work with the first few
    // to avoid timing out this fuzzer with asan enabled.
    let mut decoded_worlds = Vec::new();
    for (id, world) in resolve.worlds.iter().take(20) {
        log::debug!("testing world {}", world.name);
        let mut dummy = wit_component::dummy_module(&resolve, id);
        wit_component::embed_component_metadata(&mut dummy, &resolve, id, StringEncoding::UTF8)
            .unwrap();
        write_file("dummy.wasm", &dummy);

        let (_, decoded) = wit_component::metadata::decode(&dummy).unwrap();
        decoded_worlds.push(decoded.resolve);

        let wasm = wit_component::ComponentEncoder::default()
            .module(&dummy)
            .unwrap()
            .encode()
            .unwrap();
        write_file("dummy.component.wasm", &wasm);
        wasmparser::Validator::new_with_features(
            WasmFeatures::default() | WasmFeatures::COMPONENT_MODEL,
        )
        .validate_all(&wasm)
        .unwrap();

        wit_component::decode(&wasm).unwrap();
    }

    if decoded_worlds.len() < 2 {
        return Ok(());
    }

    log::debug!("merging worlds");
    let w1 = u.choose(&decoded_worlds)?;
    let w2 = u.choose(&decoded_worlds)?;
    let mut dst = w1.clone();
    dst.merge(w2.clone()).unwrap();
    dst.assert_valid();
    Ok(())
}

fn roundtrip_through_printing(file: &str, resolve: &Resolve, wasm: &[u8]) {
    // For all packages in `resolve` print them all to a string, then re-parse
    // them and insert them into a `new_resolve`.
    let mut new_resolve = Resolve::default();
    let mut last = None;
    for (id, pkg) in resolve.packages.iter() {
        let mut map = SourceMap::new();
        let pkg_name = &pkg.name;
        let doc = WitPrinter::default().print(resolve, &[id], false).unwrap();
        write_file(&format!("{file}-{pkg_name}.wit"), &doc);
        map.push(format!("{pkg_name}.wit").as_ref(), doc);
        let unresolved = map.parse().unwrap();
        let id = new_resolve.push_group(unresolved).unwrap();
        last = Some(id.last().unwrap().to_owned());
    }

    // Finally encode the `new_resolve` which should be the exact same as
    // before.
    let wasm2 = wit_component::encode(Some(true), &new_resolve, last.unwrap()).unwrap();
    write_file(&format!("{file}-reencoded.wasm"), &wasm2);
    if wasm != wasm2 {
        panic!("failed to roundtrip through text printing");
    }
}

fn write_file(path: &str, contents: impl AsRef<[u8]>) {
    if !log::log_enabled!(log::Level::Debug) {
        return;
    }
    log::debug!("writing file {path}");
    let contents = contents.as_ref();
    let path = Path::new(path);
    std::fs::write(path, contents).unwrap();
    if path.extension().and_then(|s| s.to_str()) == Some("wasm") {
        let path = path.with_extension("wat");
        log::debug!("writing file {}", path.display());
        std::fs::write(path, wasmprinter::print_bytes(&contents).unwrap()).unwrap();
    }
}
