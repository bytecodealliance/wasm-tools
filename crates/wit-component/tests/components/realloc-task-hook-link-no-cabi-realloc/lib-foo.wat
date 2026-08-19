(module
  (@dylink.0
    (mem-info (memory 16 4))
    (needed "c")
  )
  (import "env" "memory" (memory 1))
  (import "test:test/test" "bar" (func $bar (param i32 i32 i32)))

  (func (export "run") (result i32) unreachable)

  (func (export "greet") (param i32 i32) (result i32) unreachable)
  (func (export "cabi_post_greet") (param i32) unreachable)
)
