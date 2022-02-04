use arbitrary::{Arbitrary, Unstructured};
use rand::{rngs::SmallRng, RngCore, SeedableRng};
use wasm_smith::Component;

#[test]
fn smoke_test_component() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 1024];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let u = Unstructured::new(&buf);
        if let Ok(component) = Component::arbitrary_take_rest(u) {
            // TODO: encode to binary and validate.
            drop(component);
        }
    }
}
