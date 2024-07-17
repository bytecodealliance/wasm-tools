use pretty_assertions::assert_eq;
use wit_encoder::{Package, PackageName};

const PACKAGE: &str = indoc::indoc! {"
    package foo:empty;
"};

#[test]
fn concrete_types() {
    let package = Package::new(PackageName::new("foo", "empty", None));
    assert_eq!(package.to_string(), PACKAGE);
}
