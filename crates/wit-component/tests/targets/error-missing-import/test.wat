(component
  (type (;0;)
    (instance
      (type (;0;) (func))
      (export (;0;) "f" (func (type 0)))
    )
  )
  (import (interface "test:foo/foo") (instance (;0;) (type 0)))
  (core module (;0;)
    (type (;0;) (func))
    (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
    (import "test:foo/foo" "f" (func (;0;) (type 0)))
    (func (;1;) (type 1) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (memory (;0;) 0)
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 1))
    (@producers
      (processed-by "wit-component" "0.11.0")
    )
  )
  (alias export 0 "f" (func (;0;)))
  (core func (;0;) (canon lower (func 0)))
  (core instance (;0;)
    (export "f" (func 0))
  )
  (core instance (;1;) (instantiate 0
      (with "test:foo/foo" (instance 0))
    )
  )
  (@producers
    (processed-by "wit-component" "0.11.0")
  )
  (alias core export 1 "memory" (core memory (;0;)))
  (alias core export 1 "cabi_realloc" (core func (;1;)))
)