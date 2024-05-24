use pretty_assertions::assert_eq;
use wit_encoder::{Interface, StandaloneFunction, Type};

const PACKAGE: &str = "package foo:functions;

interface error-reporter {
}
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
";

#[test]
fn worlds() {
    let name = wit_encoder::PackageName::new("foo", "functions", None);
    let mut package = wit_encoder::Package::new(name);

    package.interface(wit_encoder::Interface::new("error-reporter"));

    package.world({
        let mut wold = wit_encoder::World::new("world");
        wold.inline_interface_export({
            let mut interface = Interface::new("example");
            interface.docs(Some("inline interface"));
            interface.function({
                let mut func = StandaloneFunction::new("do-nothing");
                func.docs(Some("func docs"));
                func
            });
            interface
        });
        wold.function_export({
            let mut func = StandaloneFunction::new("scan");
            func.results(Type::list(Type::U8));
            func.docs(Some("scan stuff"));
            func
        });
        wold.named_interface_import("error-reporter");
        wold.function_import({
            let mut func: StandaloneFunction = StandaloneFunction::new("print");
            func.params(("s", Type::String));
            func
        });
        wold
    });

    assert_eq!(PACKAGE, package.to_string());
}
