(module
  (import "foo" "a" (func (param i32)))
  (memory (export "memory") 1)
  (func (export "canonical_abi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  (func (export "a") (param i32 i32) (result i32) unreachable)
  (func (export "bar#a") unreachable)
  (func (export "bar#b") (result i32) unreachable)
)