use wasmparser::Validator;

#[test]
fn simd_not_enabled() {
    let bytes = wat::parse_str("(module (func (param v128)))").unwrap();
    let mut v = Validator::new();
    v.wasm_simd(false);
    let result = v.validate_all(&bytes).unwrap_err();
    assert_eq!(result.offset(), 11);
}
