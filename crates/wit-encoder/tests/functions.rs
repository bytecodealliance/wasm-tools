use pretty_assertions::assert_eq;
use wit_encoder::{Params, Result_, Results, StandaloneFunc, Type};

const PACKAGE: &str = "package foo:functions;

interface functions {
    f1: func();
    f2: func(a: u32);
    f3: func() -> u32;
    /// this is a documentation comment
    /// for the f4 function
    f4: func() -> tuple<u32, u32>;
    f5: func(a: f32, b: f32) -> tuple<u32, u32>;
    f6: func(a: option<u32>) -> result<u32, f32>;
    f7: func() -> (u: u32, f: f32);
    f8: func() -> (u: u32);
    f9: func() -> result<f32>;
    f10: func() -> result<_, f32>;
    f11: func() -> result;
}
";

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
            let mut func = StandaloneFunc::new("f3");
            func.results(Type::U32);
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f4");
            func.results(Type::tuple(vec![Type::U32, Type::U32]));
            func.docs(Some("this is a documentation comment\nfor the f4 function"));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f5");
            func.params(Params::from_iter([("a", Type::F32), ("b", Type::F32)]));
            func.results(Type::tuple(vec![Type::U32, Type::U32]));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f6");
            func.params(Params::from_iter([("a", Type::option(Type::U32))]));
            func.results(Type::result(Result_::both(Type::U32, Type::F32)));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f7");
            func.results(Results::named(vec![("u", Type::U32), ("f", Type::F32)]));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f8");
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

    assert_eq!(PACKAGE, package.to_string());
}
