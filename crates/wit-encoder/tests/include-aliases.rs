use pretty_assertions::assert_eq;
use wit_encoder::{Include, Package, PackageName, StandaloneFunc, World};

const PACKAGE: &str = indoc::indoc! {"
    package foo:foo;

    world foo {
      import a: func();
    }

    world bar {
      import a: func();
      import b: func();
    }

    world baz {
      import a: func();
      import b: func();
      import c: func();
    }

    world quux {
      include foo with { a as b };
      include bar with { a as b, b as c };
      include baz with { a as b, b as c, c as d };
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
    world.function_import(StandaloneFunc::new("b"));
    package.world(world);

    let mut world = World::new("baz");
    world.function_import(StandaloneFunc::new("a"));
    world.function_import(StandaloneFunc::new("b"));
    world.function_import(StandaloneFunc::new("c"));
    package.world(world);

    let mut world = World::new("quux");

    let mut include = Include::new("foo");
    include.with("a", "b");
    world.include(include);

    let mut include = Include::new("bar");
    include.with("a", "b");
    include.with("b", "c");
    world.include(include);

    let mut include = Include::new("baz");
    include.with("a", "b");
    include.with("b", "c");
    include.with("c", "d");
    world.include(include);

    package.world(world);

    assert_eq!(package.to_string(), PACKAGE);
}
