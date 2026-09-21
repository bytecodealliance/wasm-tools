(module
  (import "new" "get-two" (func $get_two (param i32)))
  (import "env" "memory" (memory 0))

  (func (export "get_sum") (result i32)
    i32.const 0
  )
)
