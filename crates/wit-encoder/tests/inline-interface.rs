use pretty_assertions::assert_eq;
use wit_encoder::{
    Field, Ident, Interface, Package, PackageName, StandaloneFunc, Type, TypeDef, World, WorldItem,
};

const PACKAGE: &str = indoc::indoc! {"
    package foo:bar;

    world the-world {
      export the-interface: interface {
        use types.{ the-type as the-import };
        the-function: func(the-parameter: the-import);
      }
    }

    interface types {
      record the-type {  }
    }
"};

#[test]
fn inline_interface() {
    let mut package = Package::new(PackageName::new("foo", "bar", None));
    let mut world = World::new("the-world");
    let mut inline_interface = Interface::new("the-interface");
    let mut types_interface = Interface::new("types");
    let mut function = StandaloneFunc::new("the-function", false);

    function.set_params(("the-parameter", Type::Named(Ident::from("the-import"))));
    inline_interface.function(function);
    inline_interface.use_type("types", "the-type", Some(Ident::from("the-import")));
    world.item(WorldItem::InlineInterfaceExport(inline_interface));
    package.world(world);

    types_interface.type_def(TypeDef::record("the-type", Vec::<Field>::new()));
    package.interface(types_interface);

    assert_eq!(package.to_string(), PACKAGE);
}
