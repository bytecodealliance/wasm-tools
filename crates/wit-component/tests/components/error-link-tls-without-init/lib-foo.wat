;; A library with thread-local storage which doesn't export the
;; `__wasm_init_tls` that initializes it. Nothing could set this module's TLS up
;; on a spawned thread, so linking has to fail rather than silently leave it
;; uninitialized.
(module
  (@dylink.0
    (mem-info (memory 8 4))
  )
  (type $void (func))
  (type $get (func (result i32)))

  (global $__tls_size i32 i32.const 32)
  (global $__tls_align i32 i32.const 16)

  (func $foo (type $get) i32.const 7)

  (export "__tls_size" (global $__tls_size))
  (export "__tls_align" (global $__tls_align))
  (export "test:test/test#foo" (func $foo))
)
