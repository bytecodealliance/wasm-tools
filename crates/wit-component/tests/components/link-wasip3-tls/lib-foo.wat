(module
  (@dylink.0
    (mem-info (memory 8 4))
    (needed "c")
    (needed "none")
  )
  (type $void (func))
  (type $get (func (result i32)))
  (type $set (func (param i32)))

  (import "env" "memory" (memory 1))
  (import "env" "__indirect_function_table" (table 0 funcref))
  (import "env" "__memory_base" (global $__memory_base i32))
  (import "env" "__table_base" (global $__table_base i32))
  (import "env" "__wasm_get_tls_base" (func $get_tls (type $get)))
  (import "env" "__wasm_set_tls_base" (func $set_tls (type $set)))
  (import "env" "abort" (func $abort (type $void)))
  (import "env" "none_helper" (func $none_helper (type $get)))

  (global $__tls_size i32 i32.const 100)
  (global $__tls_align i32 i32.const 4)
  (global (export "__wasm_library_tls_info") i32 i32.const 8)
  (func (export "__wasm_apply_data_relocs")
    (i32.store
      (i32.add
        (global.get $__memory_base)
        (i32.const 0))
      (i32.add
        (global.get $__table_base)
        (i32.const 0)))
    (i32.store
      (i32.add
        (global.get $__memory_base)
        (i32.const 4))
      (i32.add
        (global.get $__table_base)
        (i32.const 1)))
  )
  (func $__wasm_init_tls (param i32))
  (func $get_size_and_align (param i32) (result i32)
    (i32.store
      (local.get 0)
      (global.get $__tls_align))
    global.get $__tls_size
  )
  (elem (global.get $__table_base) func $get_size_and_align $__wasm_init_tls)

  (func (export "test:test/test#foo") (result i32)
    call $get_tls
    i32.load
    call $none_helper
    i32.add
  )
)
