(component
  (core module (;0;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (func (;0;) (type 0) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (memory (;0;) 0)
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 0))
  )
  (core instance (;0;) (instantiate 0))
  (alias core export 0 "memory" (core memory (;0;)))
  (alias core export 0 "cabi_realloc" (core func (;0;)))
)