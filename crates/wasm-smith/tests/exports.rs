#![cfg(feature = "wasmparser")]

use arbitrary::Unstructured;
use rand::{RngCore, SeedableRng, rngs::SmallRng};
use wasm_smith::{Config, Module};
use wasmparser::{
    CompositeType, FuncType, GlobalType, MemoryType, Parser, TableType, Validator, WasmFeatures,
    types::EntityType,
};

mod common;
use common::validate;

#[derive(Debug, PartialEq)]
enum ExportType {
    Func(FuncType),
    Global(GlobalType),
    Memory(MemoryType),
    Table(TableType),
    Tag(FuncType),
}

#[test]
fn smoke_test_single_export() {
    let test = r#"
        (module
            (func (export "foo") (param i32) (result i64)
                unreachable
            )
        )
        "#;
    smoke_test_exports(test, 11)
}

#[test]
fn smoke_test_multiple_exports() {
    let test = r#"
        (module
            (func (export "a") (param i32) (result i64)
                unreachable
            )
            (func (export "b")
                unreachable
            )
            (func (export "c")
                unreachable
            )
        )
        "#;
    smoke_test_exports(test, 12)
}

#[test]
fn smoke_test_exported_global() {
    let test = r#"
        (module
            (func (export "a") (param i32 i32 f32 f64) (result f32)
                unreachable
            )
            (global (export "glob") f64 f64.const 0)
        )
        "#;
    smoke_test_exports(test, 20)
}

#[test]
fn smoke_test_export_with_imports() {
    let test = r#"
        (module
            (import "" "foo" (func (param i32)))
            (import "" "bar" (global (mut f32)))
            (import "" "mem" (memory 5))
            (import "" "baz" (table 1 10 funcref))
            (import "" "qux" (tag (param i32 f64)))
            (func (param i64) unreachable)
            (global i32 (i32.const 0))
            (memory 10)
            (table 10 (ref null any))
            (tag (param f32 v128))
            (export "a" (func 0))
            (export "b" (global 0))
            (export "c" (memory 0))
            (export "d" (table 0))
            (export "e" (tag 0))
            (export "f" (func 1))
            (export "g" (global 1))
            (export "h" (memory 1))
            (export "i" (table 1))
            (export "j" (tag 1))
        )
        "#;
    smoke_test_exports(test, 21)
}

#[test]
fn smoke_test_with_mutable_global_exports() {
    let test = r#"
        (module
            (global (export "1i32") (mut i32) (i32.const 0))
            (global (export "2i32") (mut i32) (i32.const 0))
            (global (export "1i64") (mut i64) (i64.const 0))
            (global (export "2i64") (mut i64) (i64.const 0))
            (global (export "3i32") (mut i32) (i32.const 0))
            (global (export "3i64") (mut i64) (i64.const 0))
            (global (export "4i32") i32 (i32.const 0))
            (global (export "4i64") i64 (i64.const 0))
        )"#;
    smoke_test_exports(test, 22)
}

#[test]
fn smoke_test_all_exports() {
    let test = r#"
        (module
            (func (export "foo") (param i32) (result i64) unreachable)
            (global (export "bar") f32 f32.const 0)
            (memory (export "baz") 1 10)
            (table (export "qux") 5 10 (ref null extern))
            (tag (export "quux") (param f32))
        )
        "#;
    smoke_test_exports(test, 23);
}

fn get_exports(features: WasmFeatures, module: &[u8]) -> Vec<(String, ExportType)> {
    let mut validator = Validator::new_with_features(features);
    let types = validate(&mut validator, module);
    let types = types.as_ref();
    let mut exports = vec![];
    for payload in Parser::new(0).parse_all(module) {
        let payload = payload.unwrap();
        if let wasmparser::Payload::ExportSection(rdr) = payload {
            for export in rdr {
                let export = export.unwrap();
                match types.entity_type_from_export(&export).unwrap() {
                    EntityType::Func(core_id) => {
                        let sub_type = types.get(core_id).expect("Failed to lookup core id");
                        assert!(sub_type.is_final);
                        assert!(sub_type.supertype_idx.is_none());
                        let CompositeType {
                            inner: wasmparser::CompositeInnerType::Func(func_type),
                            ..
                        } = &sub_type.composite_type
                        else {
                            panic!("Expected Func CompositeType, but found {sub_type:?}");
                        };
                        exports
                            .push((export.name.to_string(), ExportType::Func(func_type.clone())));
                    }
                    EntityType::Global(global_type) => {
                        exports.push((export.name.to_string(), ExportType::Global(global_type)));
                    }
                    EntityType::Memory(memory_type) => {
                        exports.push((export.name.to_string(), ExportType::Memory(memory_type)));
                    }
                    EntityType::Table(table_type) => {
                        exports.push((export.name.to_string(), ExportType::Table(table_type)));
                    }
                    EntityType::Tag(core_id) => {
                        let sub_type = types.get(core_id).expect("Failed to lookup core id");
                        assert!(sub_type.is_final);
                        assert!(sub_type.supertype_idx.is_none());
                        let CompositeType {
                            inner: wasmparser::CompositeInnerType::Func(func_type),
                            ..
                        } = &sub_type.composite_type
                        else {
                            panic!("Expected Func CompositeType, but found {sub_type:?}");
                        };
                        assert!(func_type.results().is_empty());
                        exports.push((export.name.to_string(), ExportType::Tag(func_type.clone())));
                    }
                    EntityType::FuncExact(_) => panic!("Unexpected func_exact in export"),
                }
            }
        }
    }
    exports
}

fn smoke_test_exports(exports_test_case: &str, seed: u64) {
    let mut rng = SmallRng::seed_from_u64(seed);
    let mut buf = vec![0; 512];
    let wasm = wat::parse_str(exports_test_case).unwrap();
    let expected_exports = get_exports(WasmFeatures::default(), &wasm);

    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);

        // Enable all standardized proposals.
        let mut config = Config::default();
        config.max_memories = u.int_in_range(2..=5).unwrap();
        config.exports = Some(wasm.clone());

        let features = config.features();
        let module = Module::new(config, &mut u).unwrap();
        let wasm_bytes = module.to_bytes();

        let generated_exports = get_exports(features, &wasm_bytes);
        assert_eq!(expected_exports, generated_exports);
    }
}
