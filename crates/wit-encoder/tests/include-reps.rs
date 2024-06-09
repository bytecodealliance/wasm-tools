use pretty_assertions::assert_eq;
use wit_encoder::{Interface, Package, PackageName, World};

const PACKAGE: &str = include_str!("./include-reps.wit");

#[test]
fn concrete_types() {
    let mut package = Package::new(PackageName::new("foo", "foo", None));

    package.interface(Interface::new("a"));
    package.interface(Interface::new("b"));

    let mut world = World::new("bar");
    world.named_interface_import("a");
    world.named_interface_export("b");
    package.world(world);

    let mut world = World::new("foo");
    world.include("bar");
    world.include("bar");
    world.include("bar");
    package.world(world);

    assert_eq!(package.to_string(), PACKAGE);
}
