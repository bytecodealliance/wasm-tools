(component
  (type (;0;)
    (instance
      (type (;0;) (func))
      (export (;0;) "foo" (func (type 0)))
    )
  )
  (import "foo" (instance (;0;) (type 0)))
  (type (;1;)
    (instance
      (type (;0;) (func))
      (export (;0;) "bar" (func (type 0)))
    )
  )
  (export (;2;) "bar" (type 1))
)