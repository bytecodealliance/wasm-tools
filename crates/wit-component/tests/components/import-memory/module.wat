(module
  (import "env" "memory" (memory 1))
  (import "$root" "no-options" (func (param i32) (result i32)))
  (import "$root" "takes-string" (func (param i32 i32)))
  (import "$root" "returns-string" (func (param i32)))

  (func (export "run"))
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)
