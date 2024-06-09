use pretty_assertions::assert_eq;
use wit_encoder::{Package, PackageName, StandaloneFunc, World};

const PACKAGE: &str = include_str!("./import-export-overlap1.wit");

#[test]
fn concrete_types() {
    let mut package = Package::new(PackageName::new("foo", "foo", None));

    let mut world = World::new("foo");
    world.function_import(StandaloneFunc::new("a"));
    world.function_export(StandaloneFunc::new("a"));
    package.world(world);

    assert_eq!(package.to_string(), PACKAGE);
}
