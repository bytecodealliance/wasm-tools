use arbitrary::{Result, Unstructured};
use wasm_encoder::reencode::{Reencode, RoundtripReencoder};

pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    let (module1, config) = super::generate_valid_module(u, |_, _| Ok(()))?;

    let mut module2 = Default::default();
    RoundtripReencoder
        .parse_core_module(&mut module2, wasmparser::Parser::new(0), &module1)
        .unwrap();

    let module2 = module2.finish();
    if module1 == module2 {
        return Ok(());
    }
    crate::log_wasm(&module1, &config);
    crate::log_wasm(&module2, &config);
    assert_eq!(module1, module2);

    Ok(())
}

#[test]
fn smoke() {
    super::test::test_n_times(100, run);
}
