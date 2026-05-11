(module
  (import "[export]$root" "[task-cancel]" (func))
  (import "[export]$root" "[task-return]foo" (func))
  (func (export "[async-lift]foo") (param i32 i32) (result i32) unreachable)
  (func (export "[callback][async-lift]foo") (param i32 i32 i32) (result i32) unreachable)
  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)
