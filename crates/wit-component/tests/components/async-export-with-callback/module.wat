(module
  (func (export "[async]foo") (param i32 i32) (result i32) unreachable)
  (func (export "[callback][async]foo") (param i32 i32 i32 i32) (result i32) unreachable)
  (func (export "[async]foo:foo/bar#foo") (param i32 i32) (result i32) unreachable)
  (func (export "[callback][async]foo:foo/bar#foo") (param i32 i32 i32 i32) (result i32) unreachable)
  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)
