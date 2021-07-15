use arbitrary::{Arbitrary, Unstructured};
use rand::{rngs::SmallRng, RngCore, SeedableRng};
use wasm_smith::{
    Config, ConfiguredModule, EntityDesc, FuncType, GlobalType, Import, Limits, MemoryType, Module,
    SwarmConfig, TableType, ValType,
};
use wasmparser::{ImportSectionEntryType, Parser, Validator, WasmFeatures};

fn wasm_features() -> WasmFeatures {
    WasmFeatures {
        multi_value: true,
        multi_memory: true,
        bulk_memory: true,
        reference_types: true,
        simd: true,
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
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new();
            let mut features = wasm_features();
            features.module_linking = module.config().module_linking_enabled();
            validator.wasm_features(features);
            validate(&mut validator, &wasm_bytes);
        }
    }
}

#[test]
fn smoke_test_imports_config() {
    #[derive(Clone, Copy)]
    struct ImportConfig;

    impl Config for ImportConfig {
        fn module_linking_enabled(&self) -> bool {
            true
        }

        fn max_modules(&self) -> usize {
            0
        }

        fn available_imports(&self) -> Option<Vec<Import>> {
            Some(vec![
                Import {
                    module: "env".to_string(),
                    name: Some("ping".to_string()),
                    desc: EntityDesc::Func(FuncType {
                        params: vec![ValType::I32],
                        results: vec![],
                    }),
                },
                Import {
                    module: "env".to_string(),
                    name: Some("pong".to_string()),
                    desc: EntityDesc::Func(FuncType {
                        params: vec![],
                        results: vec![ValType::I32],
                    }),
                },
                Import {
                    module: "env".to_string(),
                    name: Some("mem".to_string()),
                    desc: EntityDesc::Memory(MemoryType {
                        limits: Limits {
                            min: 1,
                            max: Some(16),
                        },
                    }),
                },
                Import {
                    module: "env".to_string(),
                    name: Some("tbl".to_string()),
                    desc: EntityDesc::Table(TableType {
                        limits: Limits {
                            min: 1,
                            max: Some(16),
                        },
                        elem_ty: ValType::FuncRef,
                    }),
                },
                Import {
                    module: "vars".to_string(),
                    name: Some("g".to_string()),
                    desc: EntityDesc::Global(GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                    }),
                },
            ])
        }
    }

    impl<'a> Arbitrary<'a> for ImportConfig {
        fn arbitrary(_: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
            Ok(ImportConfig)
        }
    }

    let mut n_pings = 0;
    let mut n_pongs = 0;
    let mut n_vars = 0;
    let mut n_mems = 0;
    let mut n_tbls = 0;

    let mut n_partial = 0;
    let mut n_full = 0;

    let mut rng = SmallRng::seed_from_u64(0);
    let mut buf = vec![0; 1024];
    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);

        let u = Unstructured::new(&buf);
        if let Ok(module) = ConfiguredModule::<ImportConfig>::arbitrary_take_rest(u) {
            let wasm_bytes = module.to_bytes();

            let mut validator = Validator::new();
            let mut features = wasm_features();
            features.module_linking = module.config().module_linking_enabled();
            validator.wasm_features(features);
            validate(&mut validator, &wasm_bytes);

            let mut n_imports = 0;
            for payload in Parser::new(0).parse_all(&wasm_bytes) {
                let payload = payload.unwrap();
                if let wasmparser::Payload::ImportSection(mut rdr) = payload {
                    while let Ok(import) = rdr.read() {
                        match import.ty {
                            ImportSectionEntryType::Function(_) => {
                                assert_eq!(import.module, "env");
                                match import.field {
                                    Some("ping") => n_pings += 1,
                                    Some("pong") => n_pongs += 1,
                                    other => panic!("unexpected import: {:?}", other),
                                }
                            }
                            ImportSectionEntryType::Table(_) => {
                                assert_eq!(import.module, "env");
                                assert_eq!(import.field.as_deref(), Some("tbl"));
                                n_tbls += 1;
                            }
                            ImportSectionEntryType::Memory(_) => {
                                assert_eq!(import.module, "env");
                                assert_eq!(import.field.as_deref(), Some("mem"));
                                n_mems += 1;
                            }
                            ImportSectionEntryType::Tag(_) => todo!(),
                            ImportSectionEntryType::Global(_) => {
                                assert_eq!(import.module, "vars", "{:?}", import);
                                assert_eq!(import.field.as_deref(), Some("g"));
                                n_vars += 1;
                            }
                            ImportSectionEntryType::Module(_) => todo!(),
                            ImportSectionEntryType::Instance(_) => todo!(),
                        }
                        n_imports += 1;
                    }
                }
            }
            if n_imports == 5 {
                n_full += 1
            } else {
                n_partial += 1;
            }
        }
    }

    assert!(
        n_pings > 0
            && n_pongs > 0
            && n_vars > 0
            && n_mems > 0
            && n_tbls > 0
            && n_full > 0
            && n_partial > 0
    );
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
