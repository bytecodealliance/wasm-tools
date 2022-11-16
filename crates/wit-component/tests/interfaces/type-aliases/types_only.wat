(component
  (type (;0;) (record))
  (type (;1;) (func (param "a" 0) (result 0)))
  (type (;2;)
    (instance
      (alias outer 1 0 (type (;0;)))
      (export "foo" (type (eq 0)))
      (export "bar" (type (eq 0)))
      (alias outer 1 1 (type (;1;)))
      (export "baz" (func (type 1)))
    )
  )
  (import "foo" (instance (;0;) (type 2)))
)