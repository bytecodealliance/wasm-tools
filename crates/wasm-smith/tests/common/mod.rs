use wasmparser::{Validator, types::Types};

pub fn validate(validator: &mut Validator, bytes: &[u8]) -> Types {
    let err = match validator.validate_all(bytes) {
        Ok(types) => return types,
        Err(e) => e,
    };
    eprintln!("Writing Wasm to `test.wasm`");
    drop(std::fs::write("test.wasm", &bytes));
    if let Ok(text) = wasmprinter::print_bytes(bytes) {
        eprintln!("Writing WAT to `test.wat`");
        drop(std::fs::write("test.wat", &text));
    }
    panic!("wasm failed to validate: {err}");
}
