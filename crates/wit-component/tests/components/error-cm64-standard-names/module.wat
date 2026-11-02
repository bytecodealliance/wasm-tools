(module
  (import "cm32p2|foo:foo/foo" "f" (func (param i64 i64)))
  (memory (export "memory") i64 1)
  (func (export "cabi_realloc") (param i64 i64 i64 i64) (result i64) unreachable)
)
