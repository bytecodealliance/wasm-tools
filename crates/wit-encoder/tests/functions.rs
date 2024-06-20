use pretty_assertions::assert_eq;
use wit_encoder::{
    Interface, Package, PackageName, Params, Result_, Results, StandaloneFunc, Type,
};

const PACKAGE: &str = indoc::indoc! {"
    package foo:functions;

    interface functions {
      f1: func();
      f2: func(a: u32);
      f4: func() -> u32;
      f6: func() -> tuple<u32, u32>;
      f7: func(a: f32, b: f32) -> tuple<u32, u32>;
      f8: func(a: option<u32>) -> result<u32, f32>;
      f9: func() -> (u: u32, f: f32);
      f10: func() -> (u: u32);
      f11: func() -> result<f32>;
      f12: func() -> result<_, f32>;
      f13: func() -> result;
    }
"};

#[test]
fn concrete_types() {
    let name = PackageName::new("foo", "functions", None);
    let mut package = Package::new(name);

    package.interface({
        let mut interface = Interface::new("functions");
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
            let mut func = StandaloneFunc::new("f11");
            func.results(Type::result(Result_::ok(Type::F32)));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f12");
            func.results(Type::result(Result_::err(Type::F32)));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f13");
            func.results(Type::result(Result_::empty()));
            func
        });
        interface
    });

    assert_eq!(package.to_string(), PACKAGE);
}
