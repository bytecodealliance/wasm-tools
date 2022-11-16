(component
  (type (;0;) (func))
  (type (;1;)
    (instance
      (alias outer 1 0 (type (;0;)))
      (export "foo" (func (type 0)))
    )
  )
  (type (;2;)
    (instance
      (alias outer 1 0 (type (;0;)))
      (export "bar" (func (type 0)))
    )
  )
  (import "foo" (instance (;0;) (type 1)))
  (export "bar" (type 2))
)