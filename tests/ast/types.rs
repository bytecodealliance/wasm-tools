use wart::ast::*;

#[test]
fn valtype() {
    assert_parses!("i32", ValType::I32);
    assert_parses!("i64", ValType::I64);
    assert_parses!("f32", ValType::F32);
    assert_parses!("f64", ValType::F64);
}

#[test]
fn globals() {
    assert_parses!(
        "i32",
        GlobalType {
            ty: ValType::I32,
            mutable: false
        }
    );
    assert_parses!(
        "(mut f32)",
        GlobalType {
            ty: ValType::F32,
            mutable: true
        }
    );
}

#[test]
fn functions() {
    assert_parses!(
        "func",
        FunctionType {
            params: Vec::new(),
            results: Vec::new(),
        }
    );
    assert_parses!(
        "func (param i32)",
        FunctionType {
            params: vec![ValType::I32],
            results: Vec::new(),
        }
    );
    assert_parses!(
        "func (param i32 i64) (param f32) (result f32 f64) (result i32)",
        FunctionType {
            params: vec![ValType::I32, ValType::I64, ValType::F32],
            results: vec![ValType::F32, ValType::F64, ValType::I32],
        }
    );

    assert_not_parses!(
        "func (result i32) (param i32)",
        FunctionType,
        "cannot list params after results"
    );

    assert_not_parses!("func x", FunctionType, "expected `(`");
    assert_not_parses!("func (", FunctionType, "expected keyword `result`2");
}
