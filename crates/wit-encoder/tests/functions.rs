use pretty_assertions::assert_eq;
use wit_encoder::{Interface, Package, PackageName, Params, Result_, StandaloneFunc, Type};

const PACKAGE: &str = indoc::indoc! {"
    package foo:functions;

    interface functions {
      f1: func();
      f2: func(a: u32);
      f4: func() -> u32;
      f6: func() -> tuple<u32, u32>;
      f7: func(a: f32, b: f32) -> tuple<u32, u32>;
      f8: func(a: option<u32>) -> result<u32, f32>;
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
        interface.function(StandaloneFunc::new("f1", false));
        interface.function({
            let mut func = StandaloneFunc::new("f2", false);
            func.set_params(Params::from_iter([("a", Type::U32)]));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f4", false);
            func.set_result(Some(Type::U32));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f6", false);
            func.set_result(Some(Type::tuple(vec![Type::U32, Type::U32])));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f7", false);
            func.set_params(Params::from_iter([("a", Type::F32), ("b", Type::F32)]));
            func.set_result(Some(Type::tuple(vec![Type::U32, Type::U32])));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f8", false);
            func.set_params(Params::from_iter([("a", Type::option(Type::U32))]));
            func.set_result(Some(Type::result(Result_::both(Type::U32, Type::F32))));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f11", false);
            func.set_result(Some(Result_::ok(Type::F32).into()));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f12", false);
            func.set_result(Some(Result_::err(Type::F32).into()));
            func
        });
        interface.function({
            let mut func = StandaloneFunc::new("f13", false);
            func.set_result(Some(Result_::empty().into()));
            func
        });
        interface
    });

    assert_eq!(package.to_string(), PACKAGE);
}
