use wasm_mutate::WasmMutate;
use wasmparser::Validator;

// Copied from wasm-smith
fn validate(validator: &mut Validator, bytes: &[u8]) {
    let err = match validator.validate_all(bytes) {
        Ok(()) => return,
        Err(e) => e,
    };
    drop(std::fs::write("test.wasm", &bytes));
    if let Ok(text) = wasmprinter::print_bytes(bytes) {
        drop(std::fs::write("test.wat", &text));
    }

    panic!("wasm failed to validate {:?}", err);
}

#[test]
fn integration_test() {
    // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
    let wat = r#"
    (module
        
        (func (export "exported_func") (result i32)
            i32.const 42
        )
        (func (export "exported_func2") (result i32)
            i32.const 42
        )
    )
    "#;
    let original = &wat::parse_str(wat).unwrap();
    let mutator = WasmMutate::default();
    // seed is zero, which means first mutator

    let mutated = mutator.run(original).unwrap();
    // Down here is the validation for the correct mutation
    let mut validator = Validator::new();
    validate(&mut validator, &mutated);
}
