(module
  (@dylink.0
    (mem-info (memory 4 4))
    (needed "c")
  )
  (type (func))
  (type (func (param i32) (result i32)))
  (type (func (param i64) (result i64)))
  (import "env" "memory" (memory i64 1))
  (import "env" "__indirect_function_table" (table i64 0 funcref))
  (import "env" "__stack_pointer" (global $__stack_pointer (mut i64)))
  (import "env" "__memory_base" (global $__memory_base i64))
  (import "env" "__table_base" (global $__table_base i64))
  (import "env" "malloc" (func $malloc (type 2)))
  (import "env" "abort" (func $abort (type 0)))
  (import "GOT.mem" "um" (global $um (mut i64)))
  (import "GOT.func" "baz" (global $baz (mut i64)))
  (import "test:test/test" "bar" (func $bar (type 1)))
  (func $__wasm_call_ctors (type 0))
  (func $__wasm_apply_data_relocs (type 0)
    i64.const 8
    global.get $__memory_base
    i64.add
    global.get $baz
    i64.store
  )
  (func $foo (type 1) (param i32) (result i32)
    global.get $__stack_pointer
    i64.const 16
    i64.sub
    global.set $__stack_pointer

    i64.const 4
    call $malloc

    i64.eqz
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

    global.get $baz
    call_indirect (type 1)

    call $bar

    global.get $__stack_pointer
    i64.const 16
    i64.add
    global.set $__stack_pointer
  )
  (global i64 i64.const 0)
  (export "__wasm_call_ctors" (func $__wasm_call_ctors))
  (export "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs))
  (export "foo" (func $foo))
  (export "well" (global 5))
  (data $.data (global.get $__memory_base) "\04\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")
)
