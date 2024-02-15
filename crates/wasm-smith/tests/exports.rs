use arbitrary::{Arbitrary, Unstructured};
use rand::{rngs::SmallRng, RngCore, SeedableRng};
use wasm_smith::{Config, Module};
use wasmparser::{ExternalKind, FuncType, GlobalType, Parser, TypeRef, ValType, Validator};

mod common;
use common::{parser_features_from_config, validate};

#[derive(Debug)]
enum ExportType {
    Func(FuncType),
    Global(GlobalType),
}

struct TestCase {
    export_module: &'static str,
    expected_exports: Vec<(&'static str, ExportType)>,
}

fn make_global(name: &'static str, ty: ValType, mutable: bool) -> (&'static str, ExportType) {
    (
        name,
        ExportType::Global(GlobalType {
            content_type: ty,
            mutable,
        }),
    )
}

#[test]
fn smoke_test_single_export() {
    let test = TestCase {
        export_module: r#"
        (module
        	(func (export "foo") (param i32) (result i64)
        		unreachable
        	)
        )
        "#,
        expected_exports: vec![(
            "foo",
            ExportType::Func(FuncType::new([ValType::I32], [ValType::I64])),
        )],
    };
    smoke_test_exports(test, 11)
}

#[test]
fn smoke_test_multiple_exports() {
    let test = TestCase {
        export_module: r#"
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
        "#,
        expected_exports: vec![
            (
                "a",
                ExportType::Func(FuncType::new([ValType::I32], [ValType::I64])),
            ),
            ("b", ExportType::Func(FuncType::new([], []))),
            ("c", ExportType::Func(FuncType::new([], []))),
        ],
    };
    smoke_test_exports(test, 12)
}

#[test]
fn smoke_test_exported_global() {
    let test = TestCase {
        export_module: r#"
        (module
        	(func (export "a") (param i32 i32 f32 f64) (result f32)
        		unreachable
        	)
            (global (export "glob") f64)
        )
        "#,
        expected_exports: vec![
            (
                "a",
                ExportType::Func(FuncType::new(
                    [ValType::I32, ValType::I32, ValType::F32, ValType::F64],
                    [ValType::F32],
                )),
            ),
            make_global("glob", ValType::F64, false),
        ],
    };
    smoke_test_exports(test, 20)
}

#[test]
fn smoke_test_export_with_imports() {
    let test = TestCase {
        export_module: r#"
        (module
            (import "" "foo" (func (param i32)))
            (import "" "bar" (global (mut f32)))
            (func (param i64) unreachable)
            (global i32)
            (export "a" (func 0))
            (export "b" (global 0))
            (export "c" (func 1))
            (export "d" (global 1))
        )
            "#,
        expected_exports: vec![
            ("a", ExportType::Func(FuncType::new([ValType::I32], []))),
            make_global("b", ValType::F32, true),
            ("c", ExportType::Func(FuncType::new([ValType::I64], []))),
            make_global("d", ValType::I32, false),
        ],
    };
    smoke_test_exports(test, 21)
}

#[test]
fn smoke_test_with_mutable_global_exports() {
    let test = TestCase {
        export_module: r#"
        (module
            (global (export "1i32") (mut i32))
            (global (export "2i32") (mut i32))
            (global (export "1i64") (mut i64))
            (global (export "2i64") (mut i64))
            (global (export "3i32") (mut i32))
            (global (export "3i64") (mut i64))
            (global (export "4i32") i32)
            (global (export "4i64") i64)
        )"#,
        expected_exports: vec![
            make_global("1i32", ValType::I32, true),
            make_global("2i32", ValType::I32, true),
            make_global("1i64", ValType::I64, true),
            make_global("2i64", ValType::I64, true),
            make_global("3i32", ValType::I32, true),
            make_global("3i64", ValType::I64, true),
            make_global("4i32", ValType::I32, false),
            make_global("4i64", ValType::I64, false),
        ],
    };
    smoke_test_exports(test, 22)
}

fn smoke_test_exports(exports_test_case: TestCase, seed: u64) {
    let mut rng = SmallRng::seed_from_u64(seed);
    let mut buf = vec![0; 512];

    for _ in 0..1024 {
        rng.fill_bytes(&mut buf);
        let mut u = Unstructured::new(&buf);

        let mut config = Config::arbitrary(&mut u).expect("arbitrary config");
        config.exports = Some(wat::parse_str(&exports_test_case.export_module).unwrap());

        let features = parser_features_from_config(&config);
        let module = Module::new(config, &mut u).unwrap();

        let wasm_bytes = module.to_bytes();
        let mut validator = Validator::new_with_features(features);
        validate(&mut validator, &wasm_bytes);

        let mut types = vec![];
        let mut func_imports = vec![];
        let mut global_imports = vec![];
        let mut funcs = vec![];
        let mut globals = vec![];
        let mut exports = vec![];

        for payload in Parser::new(0).parse_all(&wasm_bytes) {
            let payload = payload.unwrap();
            if let wasmparser::Payload::TypeSection(rdr) = payload {
                // Gather the signature types to later check function types
                // against.
                for ty in rdr.into_iter_err_on_gc_types() {
                    types.push(ty.unwrap());
                }
            } else if let wasmparser::Payload::FunctionSection(rdr) = payload {
                funcs = rdr.into_iter().collect::<Result<_, _>>().unwrap();
            } else if let wasmparser::Payload::ImportSection(rdr) = payload {
                for import in rdr {
                    let import = import.expect("Failed to read import");
                    match import.ty {
                        TypeRef::Func(i) => func_imports.push(i),
                        TypeRef::Global(g) => global_imports.push(g),
                        TypeRef::Table(_) | TypeRef::Memory(_) | TypeRef::Tag(_) => {}
                    }
                }
            } else if let wasmparser::Payload::GlobalSection(rdr) = payload {
                for global in rdr {
                    let global = global.expect("Failed to read global");
                    globals.push(global.ty);
                }
            } else if let wasmparser::Payload::ExportSection(rdr) = payload {
                exports = rdr.into_iter().collect::<Result<_, _>>().unwrap();
            }
        }

        assert_eq!(
            exports_test_case.expected_exports.len(),
            exports.len(),
            "Expected exports {:?} but got {:?}",
            exports_test_case.expected_exports,
            exports
        );
        for ((name, ty), export) in exports_test_case
            .expected_exports
            .iter()
            .zip(exports.iter())
        {
            assert_eq!(name, &export.name);
            match ty {
                ExportType::Func(func_ty) => {
                    assert_eq!(ExternalKind::Func, export.kind);
                    let index = export.index as usize;
                    let type_index = if index < func_imports.len() {
                        func_imports[index]
                    } else {
                        funcs[index - func_imports.len()]
                    };
                    let export_ty = &types[type_index as usize];
                    assert_eq!(func_ty, export_ty);
                }
                ExportType::Global(global_ty) => {
                    assert_eq!(ExternalKind::Global, export.kind);
                    let index = export.index as usize;
                    let export_ty = if index < global_imports.len() {
                        global_imports[index]
                    } else {
                        globals[index - global_imports.len()]
                    };
                    assert_eq!(global_ty, &export_ty);
                }
            }
        }
    }
}
