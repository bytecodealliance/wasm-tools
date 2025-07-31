use pretty_assertions::assert_eq;
use wit_encoder::{Interface, NestedPackage, PackageName};

const PACKAGE: &str = indoc::indoc! {"
    package foo:bar {

      interface baz {}
    }
"};

#[test]
fn concrete_types() {
    let mut package = NestedPackage::new(PackageName::new("foo", "bar", None));

    package.interface(Interface::new("baz"));

    assert_eq!(package.to_string(), PACKAGE);
}
