(module
  (@dylink.0
    (mem-info (memory 0 4))
    (needed "bar")
  )
  (import "env" "memory" (memory 1))
  (import "env" "bar" (func $bar))
  (func (export "test:test/test#foo") call $bar)
)
