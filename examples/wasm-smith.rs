use arbitrary::Unstructured;
use wasm_smith::{DefaultConfig, Module};

fn test_wasm_smith() {
    let seed = "W3B4553MB1Y!!!!!!!!!!!!!!!!!!!!!!!!!!";
    let mut u = Unstructured::new(seed.as_bytes());
    if let Ok(module) = Module::new(DefaultConfig::default(), &mut u) {
        let wasm_buffer = module.to_bytes();
    }
}

fn main() {
    test_wasm_smith();
}
