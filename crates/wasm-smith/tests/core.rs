use arbitrary::{Arbitrary, Unstructured};
use rand::{RngCore, SeedableRng, rngs::SmallRng};
use wasm_smith::{Config, Module};
use wasmparser::{Validator, WasmFeatures};

mod common;
use common::validate;

#[test]
fn smoke_test_module() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let u = Unstructured::new(&buf);
        if let Ok(module) = Module::arbitrary_take_rest(u) {
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new_with_features(WasmFeatures::all());
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
fn smoke_test_ensure_termination() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let u = Unstructured::new(&buf);
        if let Ok(mut module) = Module::arbitrary_take_rest(u) {
            module.ensure_termination(10).unwrap();
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new_with_features(WasmFeatures::all());
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
fn smoke_test_swarm_config() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        if let Ok(config) = Config::arbitrary(&mut u) {
            if let Ok(module) = Module::new(config, &mut u) {
                let wasm_bytes = module.to_bytes();

                let mut validator = Validator::new_with_features(WasmFeatures::all());
                validate(&mut validator, &wasm_bytes);
            }
        }
    }
}

#[test]
fn multi_value_disabled() {
    let mut rng = SmallRng::seed_from_u64(42);
    let mut buf = vec![0; 2048];
    for _ in 0..10 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let mut cfg = Config::arbitrary(&mut u).unwrap();
        cfg.multi_value_enabled = false;
        if let Ok(module) = Module::new(cfg, &mut u) {
            let wasm_bytes = module.to_bytes();
            let mut features = WasmFeatures::all();
            features.remove(WasmFeatures::MULTI_VALUE);
            let mut validator = Validator::new_with_features(features);
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
#[cfg(feature = "wasmparser")]
fn smoke_can_smith_valid_webassembly_one_point_oh() {
    let mut rng = SmallRng::seed_from_u64(42);
    let mut buf = vec![0; 10240];
    for _ in 0..100 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let mut cfg = Config::arbitrary(&mut u).unwrap();
        cfg.sign_extension_ops_enabled = false;
        cfg.saturating_float_to_int_enabled = false;
        cfg.reference_types_enabled = false;
        cfg.multi_value_enabled = false;
        cfg.bulk_memory_enabled = false;
        cfg.simd_enabled = false;
        cfg.relaxed_simd_enabled = false;
        cfg.exceptions_enabled = false;
        cfg.memory64_enabled = false;
        cfg.reference_types_enabled = false;
        cfg.gc_enabled = false;
        cfg.extended_const_enabled = false;
        cfg.tail_call_enabled = false;
        cfg.threads_enabled = false;
        cfg.max_memories = 1;
        cfg.max_tables = 1;
        if let Ok(module) = Module::new(cfg, &mut u) {
            let wasm_bytes = module.to_bytes();
            // This table should set to `true` only features specified in wasm-core-1 spec.
            let mut validator = Validator::new_with_features(WasmFeatures::WASM1);
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
fn smoke_test_no_trapping_mode() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let mut cfg = Config::arbitrary(&mut u).unwrap();
        cfg.disallow_traps = true;
        if let Ok(module) = Module::new(cfg, &mut u) {
            let wasm_bytes = module.to_bytes();
            let mut validator = Validator::new_with_features(WasmFeatures::all());
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
fn smoke_test_disallow_floats() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let mut cfg = Config::arbitrary(&mut u).unwrap();
        cfg.allow_floats = false;
        if let Ok(module) = Module::new(cfg, &mut u) {
            let wasm_bytes = module.to_bytes();
            let mut features = WasmFeatures::all();
            features.remove(WasmFeatures::FLOATS);
            let mut validator = Validator::new_with_features(features);
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
fn smoke_test_reference_types() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let mut cfg = Config::arbitrary(&mut u).unwrap();
        cfg.reference_types_enabled = false;
        cfg.max_tables = 1;
        if let Ok(module) = Module::new(cfg, &mut u) {
            let wasm_bytes = module.to_bytes();
            let mut features = WasmFeatures::all();
            features.remove(WasmFeatures::REFERENCE_TYPES);
            let mut validator = Validator::new_with_features(features);
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
fn smoke_test_wasm_gc() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let config = Config {
            gc_enabled: true,
            reference_types_enabled: true,
            ..Config::default()
        };
        if let Ok(module) = Module::new(config, &mut u) {
            let wasm_bytes = module.to_bytes();
            let mut validator = Validator::new_with_features(WasmFeatures::all());
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
fn smoke_test_wasm_custom_page_sizes() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let config = Config {
            custom_page_sizes_enabled: true,
            ..Config::default()
        };
        if let Ok(module) = Module::new(config, &mut u) {
            let wasm_bytes = module.to_bytes();
            let mut validator = Validator::new_with_features(WasmFeatures::all());
            validate(&mut validator, &wasm_bytes);
        }
    }
}
