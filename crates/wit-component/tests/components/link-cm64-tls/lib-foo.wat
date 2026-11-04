(module
  (@dylink.0
    (mem-info (memory 8 4))
    (needed "c")
    (needed "none")
  )
  (type $void (func))
  (type $get (func (result i64)))
  (type $set (func (param i64)))
  (type $get32 (func (result i32)))

  (import "env" "memory" (memory i64 1))
  (import "env" "__indirect_function_table" (table i64 0 funcref))
  (import "env" "__memory_base" (global $__memory_base i64))
  (import "env" "__table_base" (global $__table_base i64))
  (import "env" "__wasm_get_tls_base" (func $get_tls (type $get)))
  (import "env" "__wasm_set_tls_base" (func $set_tls (type $set)))
  (import "env" "abort" (func $abort (type $void)))
  (import "env" "none_helper" (func $none_helper (type $get32)))

  (global $__tls_size i64 i64.const 100)
  (global $__tls_align i64 i64.const 4)
  (global (export "__wasm_library_tls_info") i64 i64.const 8)
  (func (export "__wasm_apply_data_relocs"))
  (func $__wasm_init_tls (param i64))
  (func $get_size_and_align (param i64) (result i64)
    (i64.store
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
