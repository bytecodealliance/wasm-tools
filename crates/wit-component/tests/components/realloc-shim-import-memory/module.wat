(module
  (import "env" "memory" (memory 1))
  (import "$root" "returns-string" (func (param i32)))
  (import "$root" "returns-list" (func (param i32)))
  (import "$root" "returns-string2" (func (param i32 i32)))
  (import "$root" "takes-string" (func (param i32 i32)))
  (import "$root" "no-options" (func (param i32) (result i32)))

  (func (export "takes-and-returns") (param i32 i32) (result i32) unreachable)
  (func (export "run"))
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)
