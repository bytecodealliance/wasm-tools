use std::rc::Rc;

use arbitrary::{Arbitrary, Unstructured};
use rand::{rngs::SmallRng, RngCore, SeedableRng};
use wasm_smith::{Config, ConfiguredModule, FuncType, Module, SwarmConfig, Type, ValType};
use wasmparser::{Validator, WasmFeatures};

fn wasm_features() -> WasmFeatures {
    WasmFeatures {
        multi_value: true,
        multi_memory: true,
        bulk_memory: true,
        reference_types: true,
        simd: true,
        memory64: true,
        ..WasmFeatures::default()
    }
}

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
            validator.wasm_features(wasm_features());
            validate(&mut validator, &wasm_bytes);
        }
    }
}


/// Provides configuration for wasm-smith generation. It uses preset values as the seed for the generated Wasm module
#[derive(Clone, Debug)]
pub struct InitialValuesConfig{
    tpes: Vec<Type>
}

impl InitialValuesConfig {
    /// Creates a new InitialValuesConfig with preset types
    pub fn new(tpes: Vec<Type>) -> Self {
        InitialValuesConfig {
            tpes: tpes
        }
    }
}

impl Config for InitialValuesConfig {
    fn initial_types(&self) -> Option<Vec<Type>> {
        Some(self.tpes.clone())
    }

    fn min_funcs(&self) -> usize {
        self.tpes.len()
    }

    fn max_funcs(&self) -> usize {
        self.tpes.len()
    }

    fn min_types(&self) -> usize {
        0
    }
    fn max_type_size(&self) -> u32 {
        0
    }

}

#[test]
fn smoke_test_module_with_initial() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 1024];

    rng.fill_bytes(&mut buf);
    let mut u = Unstructured::new(&buf);

    let initial_config = InitialValuesConfig::new(vec![
        wasm_smith::Type::Func(Rc::new(
            FuncType::new(
                vec![],
                vec![ValType::I32]
            )
        ))
    ]); // no functio defs
    let module = Module::new(initial_config, &mut u).unwrap();

    let with_initial  = module.to_bytes();

    let mut validator = Validator::new();
    validator.wasm_features(wasm_features());
    validate(&mut validator, &with_initial);
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
            validator.wasm_features(wasm_features());
            validate(&mut validator, &wasm_bytes);
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
            let module = module.module;
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new();
            let mut features = wasm_features();
            features.module_linking = module.config().module_linking_enabled();
            validator.wasm_features(features);
            validate(&mut validator, &wasm_bytes);
        }
    }
}

fn validate(validator: &mut Validator, bytes: &[u8]) {
    let err = match validator.validate_all(bytes) {
        Ok(()) => return,
        Err(e) => e,
    };
    drop(std::fs::write("test.wasm", &bytes));
    if let Ok(text) = wasmprinter::print_bytes(bytes) {
        drop(std::fs::write("test.wat", &text));
    }
    panic!("wasm failed to validate {:?}", err);
}
