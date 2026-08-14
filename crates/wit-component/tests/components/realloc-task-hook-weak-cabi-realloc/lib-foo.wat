;; Same as the `link-weak-cabi-realloc` test except that `__wasm_task_hook` is
;; additionally exported here.
(module
  (@dylink.0
    (mem-info (memory 0 4))
  )
  (type (func (param i32 i32 i32 i32) (result i32)))
  (type (func (param i32)))
  (import "env" "cabi_realloc" (func $cabi_realloc.0 (type 0)))
  (func $cabi_realloc.1 (type 0)
    i32.const 0xf09f9880
  )
  (func $foo (param i32 i32))
  (func $hook (type 1) unreachable)
  (export "cabi_realloc" (func $cabi_realloc.1))
  (export "test:test/test#foo" (func $foo))
  (export "__wasm_task_hook" (func $hook))
)
