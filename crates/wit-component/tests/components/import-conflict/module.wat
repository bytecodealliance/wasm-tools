(module
  (import "foo" "a" (func))
  (import "bar" "a" (func (param i64 i32 i32)))
  (import "baz" "baz" (func (param i32 i32 i32)))
  (memory (export "memory") 1)
  (func (export "canonical_abi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)