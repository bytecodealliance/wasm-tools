use pretty_assertions::assert_eq;
use wit_encoder::{Include, Package, PackageName, StandaloneFunc, World};

const PACKAGE: &str = indoc::indoc! {"
    package foo:foo;

    world foo {
      import a: func();
    }

    world bar {
      import a: func();
    }

    world baz {
      include bar;
      include foo with { a as b };
    }
"};

#[test]
fn concrete_types() {
    let mut package = Package::new(PackageName::new("foo", "foo", None));

    let mut world = World::new("foo");
    world.function_import(StandaloneFunc::new("a"));
    package.world(world);

    let mut world = World::new("bar");
    world.function_import(StandaloneFunc::new("a"));
    package.world(world);

    let mut world = World::new("baz");
    world.include(Include::new("bar"));
    let mut include = Include::new("foo");
    include.with("a", "b");
    world.include(include);
    package.world(world);

    assert_eq!(package.to_string(), PACKAGE);
}
