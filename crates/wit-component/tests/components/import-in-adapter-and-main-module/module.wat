(module
  (import "shared-dependency" "f1" (func))
  (import "shared-dependency" "f2" (func))

  (import "shared-dependency" "g1" (func (param i32)))
  (import "shared-dependency" "g2" (func (param i32)))

  (import "old" "adapter-f1" (func))

  (import "main-dep" "foo" (func (result i32)))

  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32)
    unreachable)
)
