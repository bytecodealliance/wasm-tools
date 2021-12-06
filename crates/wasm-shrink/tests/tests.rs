use anyhow::Result;
use wasm_shrink::WasmShrink;

fn wasm() -> Vec<u8> {
    let _ = env_logger::try_init();
    wat::parse_str(
        r#"
            (module
                (table 1 funcref)
                (memory 1)
                (func $a (param i32 i32) (result i32)
                    local.get 0
                    local.get 1
                    i32.add
                )
                (func (export "f") (param i32 i32) (result i32)
                    local.get 0
                    local.get 1
                    call $a
                )
            )
        "#,
    )
    .unwrap()
}

#[test]
fn shrink_to_empty_is_error() -> Result<()> {
    let result = WasmShrink::default().run(wasm(), |_| Ok(true), |_| Ok(()));
    assert!(result.is_err());
    let err_msg = result.err().unwrap().to_string();
    assert!(dbg!(err_msg).contains("empty Wasm module"));
    Ok(())
}

#[test]
fn shrink_to_empty_allowed() -> Result<()> {
    WasmShrink::default()
        .allow_empty(true)
        .run(wasm(), |_| Ok(true), |_| Ok(()))?;
    Ok(())
}

#[test]
fn smoke_test() -> Result<()> {
    let info = WasmShrink::default().attempts(100).run(
        wasm(),
        |wasm| {
            let wat = wasmprinter::print_bytes(&wasm)?;
            Ok(wat.contains("local.get"))
        },
        |_| Ok(()),
    )?;

    assert!(info.input_size > info.output_size);

    let wat = wasmprinter::print_bytes(&info.output)?;
    assert!(wat.contains("local.get"));

    wasmparser::validate(&info.output)?;
    Ok(())
}
