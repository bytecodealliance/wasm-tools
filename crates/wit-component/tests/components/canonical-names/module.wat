;;! emit-canonical-names = true

(module
  (import "a:b/c@0.1.1" "x" (func (param i32 i32)))
  (import "a:b/c@0.1.1" "y" (func))

  (func (export "a:b/c@0.1.0#x") (param i32 i32) unreachable)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  (memory (export "memory") 1)
)
