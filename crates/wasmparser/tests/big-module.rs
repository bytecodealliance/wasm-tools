use std::borrow::Cow;
use wasm_encoder::*;

#[test]
fn big_type_indices() {
    const N: u32 = 100_000;
    let mut module = Module::new();
    let mut types = TypeSection::new();
    for _ in 0..N {
        types.ty().function([], []);
    }
    module.section(&types);
    let mut funcs = FunctionSection::new();
    funcs.function(N - 1);
    module.section(&funcs);

    let mut elems = ElementSection::new();
    elems.declared(Elements::Functions(Cow::Borrowed(&[0])));
    module.section(&elems);

    let mut code = CodeSection::new();
    let mut body = Function::new([]);
    body.instructions().ref_func(0);
    body.instructions().drop();
    body.instructions().end();
    code.function(&body);
    module.section(&code);

    let wasm = module.finish();

    wasmparser::Validator::default()
        .validate_all(&wasm)
        .unwrap();
}

#[test]
fn big_function_body() {
    let mut module = Module::new();

    let mut types = TypeSection::new();
    types.ty().function([], []);
    module.section(&types);
    let mut funcs = FunctionSection::new();
    funcs.function(0);
    module.section(&funcs);

    let mut code = CodeSection::new();
    let mut body = Function::new([]);
    // Function body larger than the 7_654_321-byte implementation
    // limit.
    for _ in 0..8_000_000 {
        body.instructions().unreachable();
    }
    body.instructions().end();
    code.function(&body);
    module.section(&code);

    let wasm = module.finish();

    let result = wasmparser::Validator::default().validate_all(&wasm);
    assert!(result.is_err());
}

#[test]
fn deeply_nested_component_defined_type() {
    fn build(n: u32) -> Vec<u8> {
        let mut types = ComponentTypeSection::new();
        types.defined_type().primitive(PrimitiveValType::Bool);
        for i in 1..n {
            types.defined_type().list(ComponentValType::Type(i - 1));
        }
        let mut exports = ComponentExportSection::new();
        exports.export("foo", ComponentExportKind::Type, n - 1, None);
        let mut component = Component::new();
        component.section(&types);
        component.section(&exports);
        component.finish()
    }

    let features = wasmparser::WasmFeatures::all();

    // Don't stack overflow please.
    assert!(
        wasmparser::Validator::new_with_features(features)
            .validate_all(&build(100_000))
            .is_err()
    );

    // But do allow somewhat deep types.
    assert!(
        wasmparser::Validator::new_with_features(features)
            .validate_all(&build(50))
            .is_ok()
    );
}

#[test]
fn deeply_nested_component_types() {
    use wasmparser::{Validator, WasmFeatures};

    fn nested_instance_types(n: u32) -> Vec<u8> {
        let mut ty = InstanceType::new();
        for _ in 1..n {
            let mut outer = InstanceType::new();
            outer.ty().instance(&ty);
            ty = outer;
        }
        let mut types = ComponentTypeSection::new();
        types.instance(&ty);
        let mut component = Component::new();
        component.section(&types);
        component.finish()
    }

    fn validate(wasm: &[u8]) -> wasmparser::Result<()> {
        Validator::new_with_features(WasmFeatures::all())
            .validate_all(wasm)
            .map(|_| ())
    }

    assert!(validate(&nested_instance_types(50)).is_ok());
    assert!(validate(&nested_instance_types(500)).is_err());
    assert!(validate(&nested_instance_types(5000)).is_err());
    assert!(validate(&nested_instance_types(50000)).is_err());
}
