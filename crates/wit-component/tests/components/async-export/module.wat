(module
  (func (export "[async-stackful]foo") (param i32 i32) unreachable)
  (func (export "[async-stackful]foo:foo/bar#foo") (param i32 i32) unreachable)
  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)
