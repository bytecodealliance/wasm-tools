(module
  (type (;0;) (func (param i32 i64 i32)))
  (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
  (import "i1" "the-func" (func (;0;)))
  (func (export "i2#the-func")
    unreachable
  )
  (func (;2;) (export "cabi_realloc") (param i32 i32 i32 i32) (result i32)
    unreachable
  )
  (memory (export "memory") 0)
)
