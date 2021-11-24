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
    let _ = env_logger::try_init();
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
    let mut mutator = WasmMutate::default();
    mutator.fuel(1000);
    mutator.seed(2);
    // seed is zero, which means first mutator
    let start = std::time::Instant::now();
    let it = mutator.run(original).unwrap();
    let mut count = 0;
    for mutated in it.take(1000) {
        // Down here is the validation for the correct mutation
        let mutated = mutated.unwrap();
        let text = wasmprinter::print_bytes(&mutated).unwrap();
        println!("{}", text);
        let mut validator = Validator::new();
        validate(&mut validator, &mutated);
        count += 1;
    }

    let elapsed = start.elapsed();
    println!(
        "Generate {} modules in {}.{:03} seconds",
        count,
        elapsed.as_secs(),
        elapsed.subsec_millis()
    );

    todo!();
}
