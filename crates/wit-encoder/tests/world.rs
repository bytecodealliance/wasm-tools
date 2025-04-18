use pretty_assertions::assert_eq;
use wit_encoder::{Interface, StandaloneFunc, Type};

const PACKAGE: &str = indoc::indoc! {"
    package foo:functions;

    interface error-reporter {}

    world %world {
      /// inline interface
      export example: interface {
        /// func docs
        do-nothing: func();
      }
      /// scan stuff
      export scan: func() -> list<u8>;
      import error-reporter;
      import print: func(s: string);
    }
"};

#[test]
fn worlds() {
    let name = wit_encoder::PackageName::new("foo", "functions", None);
    let mut package = wit_encoder::Package::new(name);

    package.interface(wit_encoder::Interface::new("error-reporter"));

    package.world({
        let mut world = wit_encoder::World::new("world");
        world.inline_interface_export({
            let mut interface = Interface::new("example");
            interface.set_docs(Some("inline interface"));
            interface.function({
                let mut func = StandaloneFunc::new("do-nothing", false);
                func.set_docs(Some("func docs"));
                func
            });
            interface
        });
        world.function_export({
            let mut func = StandaloneFunc::new("scan", false);
            func.set_result(Some(Type::list(Type::U8)));
            func.set_docs(Some("scan stuff"));
            func
        });
        world.named_interface_import("error-reporter");
        world.function_import({
            let mut func: StandaloneFunc = StandaloneFunc::new("print", false);
            func.set_params(("s", Type::String));
            func
        });
        world
    });

    assert_eq!(PACKAGE, package.to_string());
}
