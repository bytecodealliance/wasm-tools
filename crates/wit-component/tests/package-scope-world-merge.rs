//! Encoding of a world merged from two packages that each declare a
//! package-scope type under the same local name.

use anyhow::Result;
use wasmparser::WasmFeatures;
use wit_parser::{CloneMaps, Resolve};

/// A package-scope name is unique within its package, so merging the worlds
/// keeps the two `r` types distinct. Each is encoded as its own `eq` import
/// named by its fully-qualified name rather than being unified into one.
#[test]
fn same_local_type_name_keeps_qualified_imports() -> Result<()> {
    let mut resolve = Resolve::new();
    let (pkg1, _) = resolve.push_dir("tests/package-scope-world-merge")?;
    let pkg2 = resolve
        .packages
        .iter()
        .find_map(|(id, pkg)| (pkg.name.namespace == "a" && pkg.name.name == "b2").then_some(id))
        .unwrap();

    let w1 = resolve.packages[pkg1].worlds["w1"];
    let w2 = resolve.packages[pkg2].worlds["w2"];
    resolve.merge_worlds(w2, w1, &mut CloneMaps::default())?;

    let wasm = wit_component::encode(&resolve, pkg1)?;
    wasmparser::Validator::new_with_features(WasmFeatures::all()).validate_all(&wasm)?;
    let wat = wasmprinter::print_bytes(&wasm)?;
    assert!(
        wat.contains("(import \"a:b1/r\"") && wat.contains("(record (field \"a\" u32))"),
        "expected `a:b1/r` as a u32 record:\n{wat}"
    );
    assert!(
        wat.contains("(import \"a:b2/r\"") && wat.contains("(record (field \"a\" f32))"),
        "expected `a:b2/r` as an f32 record:\n{wat}"
    );
    assert!(
        wat.contains("\"f1\"") && wat.contains("\"f2\""),
        "merged world should export both functions:\n{wat}"
    );
    Ok(())
}
