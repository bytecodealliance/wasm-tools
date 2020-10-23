use arbitrary::{Arbitrary, Unstructured};
use rand::{rngs::SmallRng, RngCore, SeedableRng};
use wasm_smith::{ConfiguredModule, Module, SwarmConfig};
use wasmparser::{Validator, WasmFeatures};

#[test]
fn smoke_test_module() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 1024];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let u = Unstructured::new(&buf);
        if let Ok(module) = Module::arbitrary_take_rest(u) {
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new();
            validator.wasm_features(WasmFeatures {
                multi_value: true,
                multi_memory: true,
                bulk_memory: true,
                reference_types: true,
                ..WasmFeatures::default()
            });

            assert!(validator.validate_all(&wasm_bytes).is_ok());
        }
    }
}

#[test]
fn smoke_test_ensure_termination() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 1024];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let u = Unstructured::new(&buf);
        if let Ok(mut module) = Module::arbitrary_take_rest(u) {
            module.ensure_termination(10);
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new();
            validator.wasm_features(WasmFeatures {
                multi_value: true,
                multi_memory: true,
                bulk_memory: true,
                reference_types: true,
                ..WasmFeatures::default()
            });

            assert!(validator.validate_all(&wasm_bytes).is_ok());
        }
    }
}

#[test]
fn smoke_test_swarm_config() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 1024];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let u = Unstructured::new(&buf);
        if let Ok(module) = ConfiguredModule::<SwarmConfig>::arbitrary_take_rest(u) {
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new();
            validator.wasm_features(WasmFeatures {
                multi_value: true,
                multi_memory: true,
                bulk_memory: true,
                reference_types: true,
                ..WasmFeatures::default()
            });

            assert!(validator.validate_all(&wasm_bytes).is_ok());
        }
    }
}
