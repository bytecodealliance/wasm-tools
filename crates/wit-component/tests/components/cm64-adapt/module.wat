(module
  (import "old" "get_sum" (func (result i32)))
  (import "old" "log" (func (param i64 i64)))
  (import "nostate" "get_sum" (func (result i32)))
  (memory (export "memory") i64 1)
  (func (export "cabi_realloc") (param i64 i64 i64 i64) (result i64) unreachable)
  ;; This is preferred over `cabi_realloc` for allocating adapters' stacks.
  (func (export "cabi_realloc_adapter") (param i64 i64 i64 i64) (result i64) unreachable)
)
