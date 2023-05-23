use {
    anyhow::{Context, Result},
    std::{borrow::Cow, path::Path},
    wasm_encoder::{CustomSection, Encode as _, Module, RawSection},
    wasmparser::Parser,
    wit_component::StringEncoding,
    wit_parser::{Resolve, UnresolvedPackage},
};

const FOO: &str = r#"
(module
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

const WIT: &str = r#"
package test:test

interface test {
   bar: func(v: s32) -> s32
}

world bar {
    import test
    export test
}
"#;

struct MemInfo {
    memory_size: u32,
    memory_alignment: u32,
    table_size: u32,
    table_alignment: u32,
}

fn encode(
    wat: &str,
    mem_info: &MemInfo,
    needed_libs: &[&str],
    wit: Option<&str>,
) -> Result<Vec<u8>> {
    const WASM_DYLINK_MEM_INFO: u8 = 1;
    const WASM_DYLINK_NEEDED: u8 = 2;

    let module = wat::parse_str(wat)?;

    let mut mem_info_subsection = Vec::new();
    mem_info.memory_size.encode(&mut mem_info_subsection);
    mem_info.memory_alignment.encode(&mut mem_info_subsection);
    mem_info.table_size.encode(&mut mem_info_subsection);
    mem_info.table_alignment.encode(&mut mem_info_subsection);

    let mut needed_subsection = Vec::new();
    needed_libs.len().encode(&mut needed_subsection);
    for needed in needed_libs {
        needed.encode(&mut needed_subsection);
    }

    let mut dylink0 = Vec::new();
    dylink0.push(WASM_DYLINK_MEM_INFO);
    mem_info_subsection.encode(&mut dylink0);
    dylink0.push(WASM_DYLINK_NEEDED);
    needed_subsection.encode(&mut dylink0);

    let mut result = Module::new();

    result.section(&CustomSection {
        name: Cow::Borrowed("dylink.0"),
        data: Cow::Borrowed(&dylink0),
    });

    for payload in Parser::new(0).parse_all(&module) {
        if let Some((id, range)) = payload?.as_section() {
            result.section(&RawSection {
                id,
                data: &module[range],
            });
        }
    }

    if let Some(wit) = wit {
        let mut resolve = Resolve::default();
        let pkg = resolve.push(UnresolvedPackage::parse(Path::new("wit"), wit)?)?;
        let world = resolve.select_world(pkg, None)?;
        let component_type =
            wit_component::metadata::encode(&resolve, world, StringEncoding::UTF8, None)?;

        result.section(&CustomSection {
            name: Cow::Borrowed("component-type"),
            data: Cow::Borrowed(&component_type),
        });
    }

    let module = result.finish();

    wasmparser::validate(&module)?;

    Ok(module)
}

#[test]
fn linking() -> Result<()> {
    let component = [
        ("libfoo.so", FOO, 4, &["libc.so"] as &[_], None),
        ("libbar.so", BAR, 20, &["libfoo.so"] as &[_], Some(WIT)),
        ("libc.so", LIBC, 0, &[] as &[_], None),
    ]
    .into_iter()
    .try_fold(
        wit_component::Linker::default().validate(true),
        |linker, (name, wat, memory_size, needed_libs, wit)| {
            linker.library(
                name,
                &encode(
                    wat,
                    &MemInfo {
                        memory_size,
                        memory_alignment: 4,
                        table_size: 0,
                        table_alignment: 0,
                    },
                    needed_libs,
                    wit,
                )
                .with_context(|| name.to_owned())?,
                false,
            )
        },
    )?
    .encode()?;

    #[cfg(target_family = "wasm")]
    {
        _ = component;
    }

    #[cfg(not(target_family = "wasm"))]
    {
        use {
            anyhow::anyhow,
            wasmtime::{
                component::{Component, Linker},
                Config, Engine, Store,
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
            .exports(&mut store)
            .instance("test:test/test")
            .ok_or_else(|| anyhow!("instance `test:test/test` not found"))?
            .typed_func::<(i32,), (i32,)>("bar")?;

        assert_eq!(65, func.call(&mut store, (7,))?.0);
    }

    Ok(())
}
