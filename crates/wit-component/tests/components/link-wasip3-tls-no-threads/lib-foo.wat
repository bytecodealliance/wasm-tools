(module
  (@dylink.0
    (mem-info (memory 8 4))
    (needed "c")
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

  (global (export "__tls_size") i32 i32.const 32)
  (global (export "__tls_align") i32 i32.const 16)
  (func (export "__wasm_init_tls") (param i32) unreachable)

  (func $foo (result i32)
    call $get_tls
    i32.load
  )

  (export "test:test/test#foo" (func $foo))
)
