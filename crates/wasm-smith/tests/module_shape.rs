#![cfg(feature = "wasmparser")]

use arbitrary::Unstructured;
use rand::{RngCore, SeedableRng, rngs::SmallRng};
use wasm_smith::{Config, Module};
use wasmparser::{Parser, Validator, WasmFeatures, types::EntityType};

mod common;
use common::validate;

#[derive(Debug, PartialEq)]
struct WasmExport(String, EntityType);
#[derive(Debug, PartialEq)]
struct WasmImport(String, String, EntityType);

#[test]
fn smoke_test_module_shape_with_gc_types() {
    let test = r#"
        (module
            (rec
                (type $t0 (func (param i32 (ref null $t1)) (result (ref null $t0))))
                (type $t1 (func (param i64 (ref null $t0))))
            )
            (rec
                (type $t2 (sub (struct (field i8))))
                (type $t3 (sub $t2 (struct (field i8) (field i16))))
                (type $t4 (sub final $t3 (struct (field i8) (field i16) (field (ref null $t0)))))
            )
            (rec
                (type $t5 (sub (array (ref null $t2))))
                (type $t6 (sub final $t5 (array (ref null $t3))))
            )
            (import "env" "fun" (func (type $t0)))
            (import "env" "tbl" (table i32 1 10 (ref null $t2)))
            (import "env" "mem" (memory 5 10))
            (import "env" "glb" (global (ref null $t4)))
            (import "env" "tag" (tag (type $t1)))
            (func (export "f") (type $t0) unreachable)
            (table (export "t") i64 10 100 (ref null $t2) struct.new_default $t2)
            (memory (export "m") 10 15)
            (export "tt" (tag 0))
            (export "g" (global 0))
        )
        "#;
    smoke_test_imports_exports(test, 42);
}

#[test]
fn smoke_test_module_shape_without_gc_types() {
    let test = r#"
        (module
            (import "env" "fun" (func (param i32)))
            (import "env" "tag" (tag (param externref)))
            (import "env" "mem" (memory 1 10))
            (import "env" "tbl" (table i64 10 10 funcref))
            (import "env" "glb" (global (mut v128)))
            (func (export "f") (param i32) (result i64) unreachable)
            (memory (export "m") 10)
            (global (export "g") f64 f64.const 0.0)
            (table (export "t") 15 20 externref)
            (export "tt" (tag 0))
        )
        "#;
    smoke_test_imports_exports(test, 43);
}

fn get_imports_exports(
    features: WasmFeatures,
    module: &[u8],
) -> (Vec<WasmImport>, Vec<WasmExport>) {
    let mut validator = Validator::new_with_features(features);
    let types = validate(&mut validator, module);
    let types = types.as_ref();
    let mut imports = vec![];
    let mut exports = vec![];
    for payload in Parser::new(0).parse_all(module) {
        let payload = payload.unwrap();
        match payload {
            wasmparser::Payload::ImportSection(ir) => {
                for import in ir.into_imports() {
                    let import = import.unwrap();
                    imports.push(WasmImport(
                        import.module.to_string(),
                        import.name.to_string(),
                        types.entity_type_from_import(&import).unwrap(),
                    ));
                }
            }
            wasmparser::Payload::ExportSection(er) => {
                for export in er {
                    let export = export.unwrap();
                    exports.push(WasmExport(
                        export.name.to_string(),
                        types.entity_type_from_export(&export).unwrap(),
                    ));
                }
            }
            _ => {}
        }
    }
    (imports, exports)
}

fn smoke_test_imports_exports(module_shape_test_case: &str, seed: u64) {
    let mut rng = SmallRng::seed_from_u64(seed);
    let mut buf = vec![0; 512];
    let wasm = wat::parse_str(module_shape_test_case).unwrap();
    let (expected_imports, expected_exports) = get_imports_exports(WasmFeatures::default(), &wasm);

    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);

        // Enable all standardized proposals.
        let mut config = Config::default();
        config.max_memories = u.int_in_range(2..=5).unwrap();
        config.module_shape = Some(wasm.clone());

        let features = config.features();
        let module = Module::new(config, &mut u).unwrap();
        let wasm_bytes = module.to_bytes();

        // The generated and expected imports/exports should share the same CoreTypeId, since
        // we copy all types, imports, and exports from the module-shape module into the target
        // module at the start of generation.
        let (generated_imports, generated_exports) = get_imports_exports(features, &wasm_bytes);
        assert_eq!(expected_imports, generated_imports);
        assert_eq!(expected_exports, generated_exports);
    }
}
