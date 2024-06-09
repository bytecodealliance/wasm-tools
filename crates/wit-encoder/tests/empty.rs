use pretty_assertions::assert_eq;
use wit_encoder::{Package, PackageName};

const PACKAGE: &str = include_str!("./empty.wit");

#[test]
fn concrete_types() {
    let package = Package::new(PackageName::new("foo", "empty", None));
    assert_eq!(package.to_string(), PACKAGE);
}
