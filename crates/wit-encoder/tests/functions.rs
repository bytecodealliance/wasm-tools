use pretty_assertions::assert_eq;
use wit_encoder::{Params, Result_, Results, StandaloneFunc, Type};

const PACKAGE: &str = include_str!("./functions.wit");

#[test]
fn smoke() {
    let name = wit_encoder::PackageName::new("foo", "functions", None);
    let mut package = wit_encoder::Package::new(name);

    package.interface({
        let mut interface = wit_encoder::Interface::new("functions");
        interface.function(StandaloneFunc::new("f1"));
        interface.function({
            let mut func = StandaloneFunc::new("f2");
            func.params(Params::from_iter([("a", Type::U32)]));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f4");
            func.results(Results::anon(Type::U32));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f6");
            func.results(Results::anon(Type::tuple(vec![Type::U32, Type::U32])));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f7");
            func.params(Params::from_iter([("a", Type::F32), ("b", Type::F32)]));
            func.results(Type::tuple(vec![Type::U32, Type::U32]));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f8");
            func.params(Params::from_iter([("a", Type::option(Type::U32))]));
            func.results(Type::result(Result_::both(Type::U32, Type::F32)));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f9");
            func.results(Results::named(vec![("u", Type::U32), ("f", Type::F32)]));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f10");
            func.results(Results::named(vec![("u", Type::U32)]));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f9");
            func.results(Type::result(Result_::ok(Type::F32)));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f10");
            func.results(Type::result(Result_::err(Type::F32)));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f11");
            func.results(Type::result(Result_::empty()));
            func
        });
        interface
    });

    assert_eq!(package.to_string(), PACKAGE);
}
