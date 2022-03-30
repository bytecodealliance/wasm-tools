use arbitrary::{Arbitrary, Unstructured};
use rand::{rngs::SmallRng, RngCore, SeedableRng};
use std::{borrow::Cow, collections::HashMap};
use wasm_smith::{Config, ConfiguredModule, ImportType, Module, SwarmConfig, ValType};
use wasmparser::{Parser, TypeRef, Validator, WasmFeatures};

fn wasm_features() -> WasmFeatures {
    WasmFeatures {
        multi_memory: true,
        relaxed_simd: true,
        memory64: true,
        exceptions: true,
        ..WasmFeatures::default()
    }
}

#[test]
fn smoke_test_module() {
    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 2048];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let u = Unstructured::new(&buf);
        if let Ok(module) = Module::arbitrary_take_rest(u) {
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new_with_features(wasm_features());
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
            module.ensure_termination(10);
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new_with_features(wasm_features());
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
        let u = Unstructured::new(&buf);
        if let Ok(module) = ConfiguredModule::<SwarmConfig>::arbitrary_take_rest(u) {
            let module = module.module;
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new_with_features(wasm_features());
            validate(&mut validator, &wasm_bytes);
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
        let mut cfg = SwarmConfig::arbitrary(&mut u).unwrap();
        cfg.multi_value_enabled = false;
        if let Ok(module) = Module::new(cfg, &mut u) {
            let wasm_bytes = module.to_bytes();
            let mut features = wasm_features();
            features.multi_value = false;
            let mut validator = Validator::new_with_features(features);
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
fn smoke_can_smith_valid_webassembly_one_point_oh() {
    let mut rng = SmallRng::seed_from_u64(42);
    let mut buf = vec![0; 10240];
    for _ in 0..100 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let mut cfg = SwarmConfig::arbitrary(&mut u).unwrap();
        cfg.sign_extension_enabled = false;
        cfg.saturating_float_to_int_enabled = false;
        cfg.reference_types_enabled = false;
        cfg.multi_value_enabled = false;
        cfg.bulk_memory_enabled = false;
        cfg.simd_enabled = false;
        cfg.relaxed_simd_enabled = false;
        cfg.exceptions_enabled = false;
        cfg.memory64_enabled = false;
        cfg.max_memories = 1;
        cfg.max_tables = 1;
        if let Ok(module) = Module::new(cfg, &mut u) {
            let wasm_bytes = module.to_bytes();
            // This table should set to `true` only features specified in wasm-core-1 spec.
            let features = WasmFeatures {
                mutable_global: true, // available in 1.0
                saturating_float_to_int: false,
                sign_extension: false,
                reference_types: false,
                multi_value: false,
                bulk_memory: false,
                simd: false,
                relaxed_simd: false,
                threads: false,
                tail_call: false,
                deterministic_only: false,
                multi_memory: false,
                exceptions: false,
                memory64: false,
                extended_const: false,
                component_model: false,
            };
            let mut validator = Validator::new_with_features(features);
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct ImportConfig {
    exceptions: bool,
}

impl Config for ImportConfig {
    fn exceptions_enabled(&self) -> bool {
        self.exceptions
    }

    fn available_imports(&self) -> Option<Cow<'static, [wasm_smith::AvailableImport]>> {
        Some(
            vec![
                wasm_smith::AvailableImport {
                    module: "env".into(),
                    field: "ping".into(),
                    import_type: ImportType::function(&[ValType::I32], &[]),
                },
                wasm_smith::AvailableImport {
                    module: "env".into(),
                    field: "ping2".into(),
                    import_type: ImportType::function(&[ValType::I32], &[]),
                },
                wasm_smith::AvailableImport {
                    module: "env".into(),
                    field: "pingping2".into(),
                    import_type: ImportType::function(&[ValType::I32, ValType::I32], &[]),
                },
                wasm_smith::AvailableImport {
                    module: "env".into(),
                    field: "pong".into(),
                    import_type: ImportType::function(&[], &[ValType::I32]),
                },
                wasm_smith::AvailableImport {
                    module: "env".into(),
                    field: "pingpong".into(),
                    import_type: ImportType::function(&[ValType::I32], &[ValType::I32]),
                },
                wasm_smith::AvailableImport {
                    module: "env".into(),
                    field: "mem".into(),
                    import_type: ImportType::Memory {
                        minimum: 1,
                        maximum: Some(16),
                        memory64: false,
                    },
                },
                wasm_smith::AvailableImport {
                    module: "env".into(),
                    field: "tbl".into(),
                    import_type: ImportType::Table {
                        minimum: 0,
                        maximum: None,
                        element: ValType::FuncRef,
                    },
                },
                wasm_smith::AvailableImport {
                    module: "vars".into(),
                    field: "g".into(),
                    import_type: ImportType::Global {
                        mutable: false,
                        value: ValType::I32,
                    },
                },
                wasm_smith::AvailableImport {
                    module: "tags".into(),
                    field: "tag1".into(),
                    import_type: ImportType::tag(&[ValType::I32]),
                },
            ]
            .into(),
        )
    }
}

impl<'a> Arbitrary<'a> for ImportConfig {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(ImportConfig {
            exceptions: u.arbitrary()?,
        })
    }
}

#[test]
fn smoke_test_imports_config() {
    let mut n_partial = 0;
    let mut global_imports_seen = HashMap::<_, bool>::new();
    let mut rng = SmallRng::seed_from_u64(11);
    let mut buf = vec![0; 512];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);
        let config: ImportConfig = u.arbitrary().unwrap();
        if let Ok(module) = Module::new(config, &mut u) {
            let wasm_bytes = module.to_bytes();
            let mut validator = Validator::new_with_features(WasmFeatures {
                exceptions: config.exceptions_enabled(),
                ..wasm_features()
            });
            validate(&mut validator, &wasm_bytes);
            let available = config.available_imports().unwrap();
            let mut imports_seen = available
                .iter()
                .map(|i| ((&*i.module, &*i.field), (false, &i.import_type)))
                .collect::<HashMap<_, _>>();
            for payload in Parser::new(0).parse_all(&wasm_bytes) {
                let payload = payload.unwrap();
                if let wasmparser::Payload::ImportSection(mut rdr) = payload {
                    while let Ok(import) = rdr.read() {
                        let entry = imports_seen.get_mut(&(import.module, import.name));
                        match (entry, &import.ty) {
                            (Some((true, _)), _) => panic!("duplicate import of {:?}", import),
                            (Some((f, ImportType::Memory { .. })), TypeRef::Memory(_))
                            | (Some((f, ImportType::Global { .. })), TypeRef::Global(_))
                            | (Some((f, ImportType::Function { .. })), TypeRef::Func(_))
                            | (Some((f, ImportType::Table { .. })), TypeRef::Table(_))
                            | (Some((f, ImportType::Tag { .. })), TypeRef::Tag(_)) => *f = true,
                            (Some(_), _) => panic!("import type mismatch"),
                            (None, _) => panic!("import of an unknown entity: {:?}", import),
                        }
                    }
                }
            }
            for import in &available[..] {
                let (seen, _) = imports_seen[&(&*import.module, &*import.field)];
                let global_seen = global_imports_seen
                    .entry((import.module.to_string(), import.field.to_string()))
                    .or_default();
                *global_seen |= seen;
            }
            if !imports_seen.values().all(|v| v.0) {
                n_partial += 1;
            }
        }
    }
    assert!(global_imports_seen.values().all(|v| *v));
    assert!(n_partial > 0);
}

fn validate(validator: &mut Validator, bytes: &[u8]) {
    let err = match validator.validate_all(bytes) {
        Ok(_) => return,
        Err(e) => e,
    };
    drop(std::fs::write("test.wasm", &bytes));
    if let Ok(text) = wasmprinter::print_bytes(bytes) {
        drop(std::fs::write("test.wat", &text));
    }
    panic!("wasm failed to validate {:?}", err);
}
