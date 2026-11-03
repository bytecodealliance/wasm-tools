;;! return-call-ref = true

(module
  (import "foo:foo/foo" "b" (func (param i64 i64)))
  (import "foo:foo/foo" "l" (func (param i64)))
  (import "foo:foo/foo" "m" (func (param i64)))
  (memory (export "memory") i64 1)
  (func (export "cabi_realloc") (param i64 i64 i64 i64) (result i64) unreachable)
)
