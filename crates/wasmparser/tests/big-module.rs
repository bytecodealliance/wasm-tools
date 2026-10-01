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

#[test]
fn too_many_core_types_across_modules() {
    use wasmparser::{Validator, WasmFeatures};

    fn rec_group_module(n: u32, inner: impl Fn() -> CompositeInnerType) -> Module {
        let mut types = TypeSection::new();
        types.ty().rec((0..n).map(|_| SubType {
            is_final: true,
            supertype_idxs: Vec::new(),
            composite_type: CompositeType {
                inner: inner(),
                shared: false,
                descriptor: None,
                describes: None,
            },
        }));
        let mut module = Module::new();
        module.section(&types);
        module
    }

    fn validate(wasm: &[u8]) -> wasmparser::Result<()> {
        Validator::new_with_features(WasmFeatures::all())
            .validate_all(wasm)
            .map(|_| ())
    }

    // Each module is within the per-module type limit and validates on its
    // own ...
    const N: u32 = 600_000;
    let a = rec_group_module(N, || {
        CompositeInnerType::Struct(StructType {
            fields: Box::new([]),
        })
    });
    let mut b = rec_group_module(N, || CompositeInnerType::Func(FuncType::new([], [])));
    let mut funcs = FunctionSection::new();
    funcs.function(0);
    b.section(&funcs);
    let mut code = CodeSection::new();
    let mut body = Function::new([]);
    body.instructions()
        .ref_null(HeapType::Concrete(N - 1))
        .drop()
        .end();
    code.function(&body);
    b.section(&code);
    let x = a.clone().finish();
    validate(&x).unwrap();
    validate(&b.clone().finish()).unwrap();

    // ... but right now they can't validate together within the same component
    // so this should at least not panic.
    let mut component = Component::new();
    component.section(&ModuleSection(&a));
    component.section(&ModuleSection(&b));
    assert!(validate(&component.finish()).is_err());
}

#[test]
fn too_many_modules_and_components() {
    use wasmparser::Validator;

    fn many_nested_modules(components: usize, modules: usize) -> Vec<u8> {
        let mut inner = Component::new();
        for _ in 0..modules {
            inner.section(&ModuleSection(&Module::new()));
        }
        let mut outer = Component::new();
        for _ in 0..components {
            outer.section(&NestedComponentSection(&inner));
        }
        outer.finish()
    }

    fn deeply_nested_components(depth: usize) -> Vec<u8> {
        let mut component = Component::new();
        for _ in 0..depth {
            let mut outer = Component::new();
            outer.section(&NestedComponentSection(&component));
            component = outer;
        }
        component.finish()
    }

    fn validate(wasm: &[u8]) -> wasmparser::Result<()> {
        Validator::default().validate_all(wasm).map(|_| ())
    }

    // 3x300 = 900, ok, 300x300 = 90_000, bad
    assert!(validate(&many_nested_modules(3, 300)).is_ok());
    assert!(validate(&many_nested_modules(300, 300)).is_err());

    assert!(validate(&deeply_nested_components(100)).is_ok());
    assert!(validate(&deeply_nested_components(1000)).is_err());
}
