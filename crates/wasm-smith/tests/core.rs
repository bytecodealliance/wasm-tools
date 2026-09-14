use arbitrary::{Arbitrary, Unstructured};
use rand::{Rng, SeedableRng, rngs::SmallRng};
use wasm_smith::{Config, Module};
use wasmparser::{Parser, Validator, WasmFeatures};

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
fn module_generation_terminates_with_empty_input() {
    let mut u = Unstructured::new(&[]);
    Module::new(Config::default(), &mut u).unwrap();
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

#[derive(Default)]
struct ImportStats {
    import_count: usize,
    has_single: bool,
    has_compact1: bool,
    has_compact2: bool,
    has_multi_compact1: bool,
    has_multi_compact2: bool,
    compact1_reaches_import_limit: bool,
    compact2_reaches_import_limit: bool,
}

fn inspect_imports(bytes: &[u8], max_imports: usize) -> ImportStats {
    enum ImportsKind {
        Single,
        Compact1,
        Compact2,
    }

    let mut validator = Validator::new_with_features(WasmFeatures::all());
    validate(&mut validator, bytes);

    let mut import_stats = ImportStats::default();
    for payload in Parser::new(0).parse_all(bytes) {
        let payload = payload.unwrap();
        let wasmparser::Payload::ImportSection(imports) = payload else {
            continue;
        };
        for imports in imports.into_iter_with_offsets() {
            let (_, imports) = imports.unwrap();
            let (count, kind) = match imports {
                wasmparser::Imports::Single(_, _) => {
                    import_stats.has_single = true;
                    (1, ImportsKind::Single)
                }
                wasmparser::Imports::Compact1 { items, .. } => {
                    let count = items.count() as usize;
                    import_stats.has_compact1 = true;
                    import_stats.has_multi_compact1 |= count >= 2;
                    (count, ImportsKind::Compact1)
                }
                wasmparser::Imports::Compact2 { names, .. } => {
                    let count = names.count() as usize;
                    import_stats.has_compact2 = true;
                    import_stats.has_multi_compact2 |= count >= 2;
                    (count, ImportsKind::Compact2)
                }
            };
            import_stats.import_count += count;
            if import_stats.import_count == max_imports {
                match kind {
                    ImportsKind::Compact1 => {
                        import_stats.compact1_reaches_import_limit = count >= 2;
                    }
                    ImportsKind::Compact2 => {
                        import_stats.compact2_reaches_import_limit = count >= 2;
                    }
                    ImportsKind::Single => {}
                }
            }
        }
    }
    import_stats
}

#[test]
fn compact_imports_disabled() {
    let mut rng = SmallRng::seed_from_u64(42);
    let mut buf = vec![0; 2048];
    let mut imports_seen = false;
    let mut config = Config::default();
    config.compact_imports_enabled = false;
    config.min_imports = 1;
    config.max_imports = 4;
    config.max_funcs = 100;
    config.max_globals = 100;
    config.max_tables = 100;
    config.max_memories = 100;
    config.max_tags = 100;

    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        if let Ok(module) = Module::new(config.clone(), &mut u) {
            let groups = inspect_imports(&module.to_bytes(), config.max_imports);
            assert!(groups.import_count <= config.max_imports);
            imports_seen |= groups.import_count > 0;
            assert!(!groups.has_compact1);
            assert!(!groups.has_compact2);
            assert!(groups.has_single || groups.import_count == 0);
        }
    }

    assert!(imports_seen);
}

#[test]
fn compact_imports_enabled() {
    let mut rng = SmallRng::seed_from_u64(42);
    let mut buf = vec![0; 2048];
    let mut compact1_seen = false;
    let mut compact2_seen = false;
    let mut compact1_reaches_import_limit = false;
    let mut compact2_reaches_import_limit = false;
    let mut config = Config::default();
    config.compact_imports_enabled = true;
    config.min_imports = 4;
    config.max_imports = 4;
    config.max_funcs = 100;
    config.max_globals = 100;
    config.max_tables = 100;
    config.max_memories = 100;
    config.max_tags = 100;

    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        if let Ok(module) = Module::new(config.clone(), &mut u) {
            let groups = inspect_imports(&module.to_bytes(), config.max_imports);
            assert!(groups.import_count <= config.max_imports);
            compact1_seen |= groups.has_multi_compact1;
            compact2_seen |= groups.has_multi_compact2;
            compact1_reaches_import_limit |= groups.compact1_reaches_import_limit;
            compact2_reaches_import_limit |= groups.compact2_reaches_import_limit;
        }
    }

    assert!(compact1_seen);
    assert!(compact2_seen);
    assert!(compact1_reaches_import_limit);
    assert!(compact2_reaches_import_limit);
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
        cfg.compact_imports_enabled = false;
        cfg.wide_arithmetic_enabled = false;
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
fn smoke_test_threads() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let config = Config {
            threads_enabled: true,
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
