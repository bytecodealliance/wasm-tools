use wasmparser::*;

#[test]
fn simd_not_enabled() {
    let bytes = wat::parse_str("(module (func (param v128)))").unwrap();
    let mut v = Validator::new();
    v.wasm_simd(false);
    let result = v.validate_all(&bytes).unwrap_err();
    assert_eq!(result.offset(), 11);
}

#[test]
fn massive_data_count() {
    let mut v = Validator::new();
    assert!(v
        .data_count_section(0x0fffffff, &Range { start: 0, end: 0 })
        .is_err());
}

#[test]
fn module_linking_sections_out_of_order() {
    let mut v = Validator::new();
    v.wasm_module_linking(true);
    v.type_section(&TypeSectionReader::new(&[0], 0).unwrap())
        .unwrap();
    v.data_section(&DataSectionReader::new(&[0], 0).unwrap())
        .unwrap();
    assert!(v
        .type_section(&TypeSectionReader::new(&[0], 0).unwrap())
        .is_err())
}
