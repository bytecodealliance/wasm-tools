use anyhow::Result;
use wit_component::{is_wasm_binary_or_wat, parse_wit_from_path};

const EXAMPLE_MODULE_WAT: &str = r#"
(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    nop
  )
)
"#;

/// Ensure that parse_wit_from_path works with directories
#[test]
fn parse_wit_dir() -> Result<()> {
    drop(env_logger::try_init());

    let (resolver, package_id) = parse_wit_from_path("tests/wit/parse-dir/wit")?;
    assert!(resolver
        .select_world(package_id, "foo-world".into())
        .is_ok());

    Ok(())
}

/// Ensure that parse_wit_from_path works for a single file
#[test]
fn parse_wit_file() -> Result<()> {
    drop(env_logger::try_init());

    let (resolver, package_id) = parse_wit_from_path("tests/wit/parse-dir/wit/deps/bar/bar.wit")?;
    resolver.select_world(package_id, "bar-world".into())?;
    assert!(resolver
        .interfaces
        .iter()
        .any(|(_, iface)| iface.name == Some("bar".into())));

    Ok(())
}

/// Ensure that parse_with_from_path fails for missing paths
#[test]
fn parse_wit_missing_path() -> Result<()> {
    drop(env_logger::try_init());

    assert!(parse_wit_from_path("tests/nonexistent/path").is_err());

    Ok(())
}

/// Ensure that is_wasm_binary_or_wat works for binaries
#[test]
fn check_wasm_from_bytes() -> Result<()> {
    drop(env_logger::try_init());

    assert!(is_wasm_binary_or_wat(wat::parse_str(EXAMPLE_MODULE_WAT)?));

    Ok(())
}
