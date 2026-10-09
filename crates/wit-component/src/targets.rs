use crate::encoding::{check_duplicate_canonical_names, encode_world};
use anyhow::{Context, Result, bail};
use wasm_encoder::{ComponentBuilder, ComponentExportKind, ComponentTypeRef};
use wasmparser::names::has_canonical_names;
use wasmparser::{ComponentTypeRef as ImportTypeRef, Parser, Payload, Validator, WasmFeatures};
use wit_parser::decoding::{DecodedWasm, decode};
use wit_parser::{Resolve, WorldId};

/// This function checks whether `component_to_test` correctly conforms to the world specified.
/// It does so by instantiating a generated component that imports a component instance with
/// the component type as described by the "target" world.
///
/// When `semver_compatible` is set, interfaces are matched by their canonical
/// names, so semver-compatible versions (e.g. `wasi:cli@0.2.3` and
/// `wasi:cli@0.2.0`) are matched with each other and then type-checked
/// structurally.
pub fn targets(
    resolve: &Resolve,
    world: WorldId,
    component_to_test: &[u8],
    semver_compatible: bool,
) -> Result<()> {
    let mut root_component = ComponentBuilder::default();

    // (1) Embed the component to test. With `semver_compatible`, the names in
    // the component itself may not be canonical, in which case we decode
    // its world, merge semver-compatible imports, and import a component whose
    // type is that world encoded with canonical names.
    let reencode = semver_compatible && !has_canonical_names(component_to_test)?;
    let component_to_test_idx = if reencode {
        check_imports_are_wit(component_to_test)?;
        let (mut test_resolve, test_world) = match decode(component_to_test)
            .context("failed to decode the WIT world of the component to test")?
        {
            DecodedWasm::Component(resolve, world) => (resolve, world),
            DecodedWasm::WitPackage(..) => bail!("expected a component, found a WIT package"),
        };
        test_resolve.merge_world_imports_based_on_semver(test_world)?;
        // The merge above already ensures imports have unique canonical names,
        // so this mainly catches semver-compatible duplicate exports.
        check_duplicate_canonical_names(&test_resolve, test_world)?;
        let component_ty = encode_world(&test_resolve, test_world, true)?;
        let component_ty_idx = root_component.type_component(None, &component_ty);
        root_component.import(
            "component-to-test",
            ComponentTypeRef::Component(component_ty_idx),
        )
    } else {
        root_component.component_raw(None, component_to_test)
    };

    // (2) Encode the world to a component type and embed a new component which
    // imports the encoded component type.
    let test_component_idx = {
        let component_ty = if semver_compatible {
            let mut resolve = resolve.clone();
            resolve.merge_world_imports_based_on_semver(world)?;
            check_duplicate_canonical_names(&resolve, world)?;
            encode_world(&resolve, world, true)?
        } else {
            encode_world(resolve, world, false)?
        };
        let mut component = ComponentBuilder::default();
        let component_ty_idx = component.type_component(None, &component_ty);
        component.import(
            &resolve.worlds[world].name,
            ComponentTypeRef::Component(component_ty_idx),
        );
        root_component.component(None, component)
    };

    // (3) Instantiate the component from (2) with the component to test from (1).
    let args: Vec<(String, ComponentExportKind, u32)> = vec![(
        resolve.worlds[world].name.clone(),
        ComponentExportKind::Component,
        component_to_test_idx,
    )];
    root_component.instantiate(None, test_component_idx, args);

    let bytes = root_component.finish();

    Validator::new_with_features(WasmFeatures::all())
        .validate_all(&bytes)
        .context("failed to validate encoded bytes")?;

    Ok(())
}

/// Decoding a component to WIT skips imports that aren't representable in WIT.
/// For example, importing a component, a core module, or a value.
fn check_imports_are_wit(component: &[u8]) -> Result<()> {
    let mut depth = 0;
    for payload in Parser::new(0).parse_all(component) {
        match payload? {
            Payload::ModuleSection { .. } | Payload::ComponentSection { .. } => depth += 1,
            Payload::End(_) => depth -= 1,
            Payload::ComponentImportSection(imports) if depth == 0 => {
                for import in imports {
                    let import = import?;
                    match import.ty {
                        ImportTypeRef::Instance(_)
                        | ImportTypeRef::Func(_)
                        | ImportTypeRef::Type(_) => {}
                        _ => bail!(
                            "import `{}` of the component to test cannot be represented in WIT",
                            import.name.name
                        ),
                    }
                }
            }
            _ => {}
        }
    }
    Ok(())
}
