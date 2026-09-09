use {
    anyhow::{Context, Result},
    wit_component::StringEncoding,
    wit_parser::Resolve,
};

const FOO: &str = r#"
(module
  (@dylink.0
    (mem-info (memory 4 4))
    (needed "libc.so")
  )
  (type (func))
  (type (func (param i32) (result i32)))
  (import "env" "memory" (memory 1))
  (import "env" "__indirect_function_table" (table 0 funcref))
  (import "env" "__stack_pointer" (global $__stack_pointer (mut i32)))
  (import "env" "__memory_base" (global $__memory_base i32))
  (import "env" "__table_base" (global $__table_base i32))
  (import "env" "malloc" (func $malloc (type 1)))
  (import "env" "abort" (func $abort (type 0)))
  (import "GOT.mem" "um" (global $um (mut i32)))
  (import "test:test/test" "bar" (func $bar (type 1)))
  (func $__wasm_call_ctors (type 0))
  (func $__wasm_apply_data_relocs (type 0))
  (func $foo (type 1) (param i32) (result i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    global.set $__stack_pointer

    i32.const 4
    call $malloc

    i32.const 0
    i32.eq
    if
      call $abort
      unreachable
    end

    local.get 0
    global.get $um
    i32.load offset=16
    i32.add
    i32.const 42
    i32.add

    call $bar

    global.get $__stack_pointer
    i32.const 16
    i32.add
    global.set $__stack_pointer
  )
  (global i32 i32.const 0)
  (export "__wasm_call_ctors" (func $__wasm_call_ctors))
  (export "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs))
  (export "foo" (func $foo))
  (export "well" (global 4))
  (data $.data (global.get $__memory_base) "\04\00\00\00")
)
"#;

const BAR: &str = r#"
(module
  (@dylink.0
    (mem-info (memory 20 4))
    (needed "libfoo.so")
  )
  (type (func (param i32) (result i32)))
  (type (func))
  (import "env" "memory" (memory 1))
  (import "env" "__indirect_function_table" (table 0 funcref))
  (import "env" "__memory_base" (global $__memory_base i32))
  (import "env" "__table_base" (global $__table_base i32))
  (import "env" "foo" (func $foo (type 0)))
  (import "GOT.mem" "well" (global $well (mut i32)))
  (func $__wasm_call_ctors (type 1))
  (func $__wasm_apply_data_relocs (type 1))
  (func $bar (type 0) (param i32) (result i32)
    local.get 0
    call $foo
    global.get $well
    i32.load
    i32.add
  )
  (global i32 i32.const 0)
  (export "__wasm_call_ctors" (func $__wasm_call_ctors))
  (export "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs))
  (export "test:test/test#bar" (func $bar))
  (export "um" (global 3))
  (data $.data (global.get $__memory_base) "\01\00\00\00\02\00\00\00\03\00\00\00\04\00\00\00\05\00\00\00")
)
"#;

const LIBC: &str = r#"
(module
  (@dylink.0)
  (type (func))
  (type (func (param i32) (result i32)))
  (import "GOT.mem" "__heap_base" (global $__heap_base (mut i32)))
  (import "GOT.mem" "__heap_end" (global $__heap_end (mut i32)))
  (global $heap (mut i32) i32.const 0)
  (func $start (type 0)
    global.get $__heap_base
    global.set $heap
  )
  (func $malloc (type 1) (param i32) (result i32)
    global.get $heap
    global.get $heap
    local.get 0
    i32.add
    global.set $heap
  )
  (func $abort (type 0)
    unreachable
  )
  (export "malloc" (func $malloc))
  (export "abort" (func $abort))
  (start $start)
)
"#;

const FOO_WIT: &str = r#"
package test:test;

interface test {
   bar: func(v: s32) -> s32;
}

world foo {
    import test;
}
"#;

const BAR_WIT: &str = r#"
package test:test;

interface test {
   bar: func(v: s32) -> s32;
}

world bar {
    import test;
    export test;
}
"#;

const INTRINSIC_WIT: &str = r#"
package test:test;

world intrinsic {
    export run: func();
}
"#;

fn encode(wat: &str, wit: Option<&str>) -> Result<Vec<u8>> {
    let mut module = wat::parse_str(wat)?;

    if let Some(wit) = wit {
        let mut resolve = Resolve::default();
        let pkg = resolve.push_str("test.wit", wit)?;
        let world = resolve.select_world(&[pkg], None)?;

        wit_component::embed_component_metadata(
            &mut module,
            &resolve,
            world,
            StringEncoding::UTF8,
        )?;
    }

    wasmparser::validate(&module)?;

    Ok(module)
}

fn link_intrinsic_wat(wat: &str, validate: bool) -> Result<Vec<u8>> {
    let module = encode(wat, Some(INTRINSIC_WIT))?;
    let mut linker = wit_component::Linker::default();
    linker.encoder().validate(validate);
    linker.library("app.wasm", &module, false)?;
    linker.encode()
}

fn link_intrinsic(namespace: &str, name: &str, signature: &str, validate: bool) -> Result<Vec<u8>> {
    link_intrinsic_wat(
        &format!(
            r#"
(module
  (@dylink.0)
  (import "{namespace}" "{name}" (func {signature}))
  (func (export "run"))
)
"#,
        ),
        validate,
    )
}

#[test]
fn linker_intrinsic_signatures() -> Result<()> {
    for (namespace, name, signature) in [
        ("env", "__wasm_get_stack_pointer", "(result i32)"),
        ("env", "__wasm_set_stack_pointer", "(param i32)"),
        ("env", "__wasm_get_tls_base", "(result i32)"),
        ("env", "__wasm_set_tls_base", "(param i32)"),
        (
            "$root",
            "[thread-new-indirect-v0]",
            "(param i32 i32) (result i32)",
        ),
    ] {
        link_intrinsic(namespace, name, signature, true)
            .with_context(|| format!("failed to link {namespace}.{name}"))?;
    }
    link_intrinsic_wat(
        r#"
(module
  (@dylink.0)
  (type (func))
  (type (func (result i32)))
  (import "env" "__wasm_get_stack_pointer" (func (type 1)))
  (func (export "run") (type 0))
)
"#,
        true,
    )
    .context("failed to link an intrinsic at a nonzero type index")?;
    Ok(())
}

#[test]
fn linker_intrinsic_signature_mismatches() {
    for (namespace, name, signature, expected, actual, validate) in [
        (
            "env",
            "__wasm_get_stack_pointer",
            "(param i64) (result i32)",
            "[] -> [I32]",
            "[I64] -> [I32]",
            false,
        ),
        (
            "env",
            "__wasm_set_stack_pointer",
            "(param i64)",
            "[I32] -> []",
            "[I64] -> []",
            true,
        ),
        (
            "env",
            "__wasm_get_tls_base",
            "",
            "[] -> [I32]",
            "[] -> []",
            false,
        ),
        (
            "env",
            "__wasm_set_tls_base",
            "(param i32) (result i64)",
            "[I32] -> []",
            "[I32] -> [I64]",
            false,
        ),
        (
            "$root",
            "[thread-new-indirect-v0]",
            "(param i32) (result i32)",
            "[I32, I32] -> [I32]",
            "[I32] -> [I32]",
            false,
        ),
        (
            "env",
            "__wasm_get_stack_pointer",
            "(result i64)",
            "[] -> [I32]",
            "[] -> [I64]",
            false,
        ),
        (
            "$root",
            "[thread-new-indirect-v0]",
            "(param i32 i32 i32) (result i32)",
            "[I32, I32] -> [I32]",
            "[I32, I32, I32] -> [I32]",
            false,
        ),
    ] {
        let error = link_intrinsic(namespace, name, signature, validate).unwrap_err();
        let error = format!("{error:#}");
        assert!(error.contains("failed to extract linking metadata from app.wasm"));
        assert!(error.contains(&format!("function `{namespace}.{name}`")));
        assert!(
            error.contains(&format!(
                "required linker ABI `{expected}` but found `{actual}`"
            )),
            "{error}"
        );
    }
}

#[test]
fn linker_intrinsic_rejects_wrong_kind_and_unsupported_type() {
    let error = link_intrinsic_wat(
        r#"
(module
  (@dylink.0)
  (import "$root" "[thread-new-indirect-v0]" (global i32))
  (func (export "run"))
)
"#,
        false,
    )
    .unwrap_err();
    assert!(
        format!("{error:#}")
            .contains("unexpected type for $root:[thread-new-indirect-v0]: Global(GlobalType")
    );

    let error =
        link_intrinsic("env", "__wasm_set_stack_pointer", "(param v128)", false).unwrap_err();
    let error = format!("{error:#}");
    assert!(error.contains("failed to read function type for `env.__wasm_set_stack_pointer`"));
    assert!(error.contains("V128 not yet supported"));
}

#[test]
fn linker_intrinsic_rejects_invalid_type_index() {
    use {
        std::borrow::Cow,
        wasm_encoder::{CustomSection, EntityType, ImportSection, Module, TypeSection},
    };

    let mut module = Module::new();
    module.section(&CustomSection {
        name: Cow::Borrowed("dylink.0"),
        data: Cow::Borrowed(&[]),
    });
    let mut types = TypeSection::new();
    types.ty().function([], []);
    module.section(&types);
    let mut imports = ImportSection::new();
    imports.import("env", "__wasm_get_stack_pointer", EntityType::Function(1));
    module.section(&imports);

    let module = module.finish();
    let mut linker = wit_component::Linker::default();
    linker.library("app.wasm", &module, false).unwrap();
    let error = linker.encode().unwrap_err();
    assert!(
        format!("{error:#}")
            .contains("invalid function type index 1 for `env.__wasm_get_stack_pointer`")
    );
}

#[test]
fn linker_intrinsic_requires_matching_namespace() {
    let error = link_intrinsic(
        "other",
        "__wasm_get_stack_pointer",
        "(param i64) (result i32)",
        false,
    )
    .unwrap_err();
    let error = format!("{error:#}");
    assert!(error.contains("module requires an import interface named `other`"));
    assert!(!error.contains("required linker ABI"));
}

#[test]
fn linking() -> Result<()> {
    let mut linker = wit_component::Linker::default();
    linker.encoder().validate(true);
    for (name, wat, wit) in [
        ("libfoo.so", FOO, Some(FOO_WIT)),
        ("libbar.so", BAR, Some(BAR_WIT)),
        ("libc.so", LIBC, None),
    ] {
        linker.library(
            name,
            &encode(wat, wit).with_context(|| name.to_owned())?,
            false,
        )?;
    }
    let component = linker.encode()?;

    #[cfg(target_family = "wasm")]
    {
        _ = component;
    }

    #[cfg(not(target_family = "wasm"))]
    {
        use {
            anyhow::anyhow,
            wasmtime::{
                Config, Engine, Store,
                component::{Component, Linker},
            },
        };

        let mut config = Config::new();
        config.wasm_component_model(true);

        let engine = Engine::new(&config)?;
        let mut linker = Linker::new(&engine);
        linker
            .instance("test:test/test")?
            .func_wrap("bar", |_store, (v,): (i32,)| Ok((v + 7,)))?;
        let mut store = Store::new(&engine, ());
        let instance = linker.instantiate(&mut store, &Component::new(&engine, &component)?)?;
        let func = instance
            .get_export_index(&mut store, None, "test:test/test")
            .and_then(|i| instance.get_export_index(&mut store, Some(&i), "bar"))
            .and_then(|f| {
                instance
                    .get_typed_func::<(i32,), (i32,)>(&mut store, &f)
                    .ok()
            })
            .ok_or_else(|| anyhow!("func `test:test/test/bar` not found"))?;

        assert_eq!(65, func.call(&mut store, (7,))?.0);
    }

    Ok(())
}

const GOT_IMPORT: &str = r#"
(module
  (@dylink.0
    (mem-info)
    (needed "libc.so")
    (import-info "env" "foobar" binding-weak undefined)
  )
  (type (;0;) (func (result i32)))
  (import "env" "foobar" (func (;0;) (type 0)))
  (import "GOT.func" "foobar" (global (;0;) (mut i32)))
  (export "foo" (func 1))
  (func (;1;) (type 0) (result i32)
    global.get 0
    i32.eqz
    if ;; label = @1
      i32.const 0
      return
    end
    call 0
  )
)
"#;

const GOT_IMPORT_WIT: &str = r#"
package test:test;

world bar {
    export foo: func() -> u32;
}
"#;

#[test]
fn linking_got_weak() -> Result<()> {
    let mut linker = wit_component::Linker::default();
    linker.encoder().validate(true);
    for (name, wat, wit) in [
        ("libfoo.so", GOT_IMPORT, Some(GOT_IMPORT_WIT)),
        ("libc.so", LIBC, None),
    ] {
        linker.library(
            name,
            &encode(wat, wit).with_context(|| name.to_owned())?,
            false,
        )?;
    }
    let component = linker.encode()?;

    #[cfg(target_family = "wasm")]
    {
        _ = component;
    }

    #[cfg(not(target_family = "wasm"))]
    {
        use {
            anyhow::anyhow,
            wasmtime::{
                Config, Engine, Store,
                component::{Component, Linker},
            },
        };

        let config = Config::new();
        let engine = Engine::new(&config)?;
        let mut linker = Linker::new(&engine);
        linker.instance("test:test/test")?;
        let mut store = Store::new(&engine, ());
        let instance = linker.instantiate(&mut store, &Component::new(&engine, &component)?)?;
        let func = instance
            .get_export(&mut store, None, "foo")
            .and_then(|(_, f)| instance.get_typed_func::<(), (u32,)>(&mut store, &f).ok())
            .ok_or_else(|| anyhow!("func `foo` not found"))?;

        assert_eq!(0, func.call(&mut store, ())?.0);
    }
    Ok(())
}
