use wit_encoder::Function;

const PACKAGE: &str = "package foo:functions;

interface functions {
    f1: func();
    f2: func(a: u32);
    f3: func(a: u32,);
    f4: func() -> u32;
    f6: func() -> tuple<u32, u32>;
    f7: func(a: f32, b: f32) -> tuple<u32, u32>;
    f8: func(a: option<u32>) -> result<u32, f32>;
    f9: func() -> (u: u32, f: f32);
    f10: func() -> (u: u32);
    f11: func() -> ();
}
";

#[test]
fn smoke() {
    let name = wit_encoder::PackageName::new("foo", "functions", None);
    let mut package = wit_encoder::Package::new(name);

    let mut interface = wit_encoder::Interface::new();
    interface.function(Function::new_freestanding("f1"));
    package.interface(interface);
}
