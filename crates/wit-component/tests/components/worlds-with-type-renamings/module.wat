(module
  (type (;0;) (func (param i32 i64 i32)))
  (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
  (import "foo:foo/i" "the-func" (func (;0;)))
  (func (export "foo:foo/i#the-func")
    unreachable
  )
  (func (;2;) (export "cabi_realloc") (param i32 i32 i32 i32) (result i32)
    unreachable
  )
  (memory (export "memory") 0)
)
