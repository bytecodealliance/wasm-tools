use pretty_assertions::assert_eq;
use wit_encoder::{Interface, Package, PackageName, ResourceFunc, TypeDef, Use, World};

const PACKAGE: &str = indoc::indoc! {"
    package foo:foo;

    interface foo {
      resource bar {
      }
    }

    interface bar {
      use foo.{ bar as foobar };
      resource baz {
      }
    }

    world baz {
      use bar.{ baz };
    }
"};

#[test]
fn concrete_types() {
    let mut package = Package::new(PackageName::new("foo", "foo", None));

    let mut interface = Interface::new("foo");
    interface.type_def(TypeDef::resource("bar", Vec::<ResourceFunc>::new()));
    package.interface(interface);

    let mut interface = Interface::new("bar");
    let mut use_ = Use::new("foo");
    use_.item("bar", Some("foobar"));
    interface.use_(use_);
    interface.type_def(TypeDef::resource("baz", Vec::<ResourceFunc>::new()));
    package.interface(interface);

    let mut world = World::new("baz");
    let mut use_ = Use::new("bar");
    use_.item("baz", None);
    world.use_(use_);
    package.world(world);

    assert_eq!(package.to_string(), PACKAGE);
}
