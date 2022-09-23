(module
  (import "foo" "a: func(b: record { x: u8 }) -> ()" (func (param i32)))
  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  (func (export "a: func(b: record { x: u8 }) -> ()") (param i32) unreachable)
)
