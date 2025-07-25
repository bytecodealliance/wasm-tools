#![cfg(feature = "component-model")]

use arbitrary::{Arbitrary, Unstructured};
use rand::{RngCore, SeedableRng, rngs::SmallRng};
use wasm_smith::Component;

#[test]
#[ignore] // FIXME(#1000): need to update wasm-smith's support for components
fn smoke_test_component() {
    const NUM_RUNS: usize = 4096;

    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 1024];
    let mut ok_count = 0;

    for _ in 0..NUM_RUNS {
        rng.fill_bytes(&mut buf);
        let u = Unstructured::new(&buf);
        if let Ok(component) = Component::arbitrary_take_rest(u) {
            ok_count += 1;
            let component = component.to_bytes();

            let mut validator = wasmparser::Validator::new_with_features(
                wasmparser::WasmFeatures::default() | wasmparser::WasmFeatures::COMPONENT_MODEL,
            );
            if let Err(e) = validator.validate_all(&component) {
                std::fs::write("component.wasm", &component).unwrap();
                panic!(
                    "generated component should be valid; failing binary written \
                     to `component.wasm`. Error: {e}"
                );
            }
        }
    }

    println!(
        "Generated {} / {} ({:.02}%) arbitrary components okay",
        ok_count,
        NUM_RUNS,
        ok_count as f64 / NUM_RUNS as f64 * 100.0
    );
}
