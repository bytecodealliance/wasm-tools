(module
  (@dylink.0
    (mem-info (memory 0 4))
    (target-arch "wasm64")
  )
  (type (func (param i32) (result i32)))
  (func $f (type 0) (param i32) (result i32)
    unreachable
  )
  (global $what i64 i64.const 42)
  (export "test:test/dl#f" (func $f))
  (export "g" (func $f))
  (export "what" (global $what))
)
