use anyhow::{Context, Result, bail};
use pretty_assertions::assert_eq;
use wasmparser::{Validator, WasmFeatures};
use wit_component::{DecodedWasm, WitPrinter, decode};
use wit_parser::{InterfaceId, Type, TypeDefKind, World, WorldItem, WorldKey};

#[test]
fn two_implements_imports_share_an_instance_type() -> Result<()> {
    let wasm = wat::parse_str(
        r#"
        (component
          (type (instance
            (type (variant (case "other" string)))
            (export "error" (type (eq 0)))
            (type (func (result 1)))
            (export "open" (func (type 2)))
          ))
          (import "primary" (implements "a:b/store@0.1.0") (instance (type 0)))
          (import "backup" (implements "a:b/store@0.1.0") (instance (type 0)))
        )
        "#,
    )?;
    Validator::new_with_features(WasmFeatures::all())
        .validate_all(&wasm)
        .context("validation accepts this component")?;

    let decoded = decode(&wasm).context("decoding the component to WIT")?;
    let (resolve, world_id) = match decoded {
        DecodedWasm::Component(resolve, world) => (resolve, world),
        DecodedWasm::WitPackage(_, _) => bail!("expected a component, decoded a WIT package"),
    };

    let world = &resolve.worlds[world_id];
    let primary = interface_import(world, "primary")?;
    let backup = interface_import(world, "backup")?;
    assert_eq!(primary, backup, "both imports are the same interface");
    assert_eq!(world.imports.len(), 2);

    let interface = &resolve.interfaces[primary];
    assert_eq!(interface.name.as_deref(), Some("store"));
    let package = &resolve.packages[interface.package.context("interface has a package")?];
    assert_eq!(package.name.to_string(), "a:b@0.1.0");

    let error_id = *interface
        .types
        .get("error")
        .context("store exports the error type")?;
    match &resolve.types[error_id].kind {
        TypeDefKind::Variant(variant) => {
            assert_eq!(variant.cases.len(), 1);
            assert_eq!(variant.cases[0].name, "other");
            assert_eq!(variant.cases[0].ty, Some(Type::String));
        }
        other => bail!("error has kind {other:?}, expected a variant"),
    }
    let open = interface
        .functions
        .get("open")
        .context("store exports open")?;
    assert!(open.params.is_empty());
    assert_eq!(open.result, Some(Type::Id(error_id)));

    let mut printer = WitPrinter::default();
    let nested = resolve
        .packages
        .iter()
        .map(|(id, _)| id)
        .filter(|id| *id != resolve.worlds[world_id].package.unwrap())
        .collect::<Vec<_>>();
    printer
        .print(&resolve, resolve.worlds[world_id].package.unwrap(), &nested)
        .context("printing WIT")?;
    assert_eq!(
        printer.output.to_string(),
        "\
package root:component;

world root {
  import primary: a:b/store@0.1.0;
  import backup: a:b/store@0.1.0;
}
package a:b@0.1.0 {
  interface store {
    variant error {
      other(string),
    }

    open: func() -> error;
  }
}
"
    );
    Ok(())
}

fn interface_import(world: &World, name: &str) -> Result<InterfaceId> {
    match world.imports.get(&WorldKey::Name(name.to_string())) {
        Some(WorldItem::Interface { id, .. }) => Ok(*id),
        Some(_) => bail!("import `{name}` is not an interface"),
        None => bail!("missing import `{name}`"),
    }
}
