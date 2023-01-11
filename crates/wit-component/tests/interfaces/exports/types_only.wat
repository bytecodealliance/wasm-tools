(component
  (type (;0;)
    (instance
      (type (;0;) (record (field "a" u32)))
      (export "my-struct" (type (eq 0)))
      (type (;1;) (func (param "a" 0) (result string)))
      (export "my-function" (func (type 2)))
    )
  )
  (export (;1;) "foo" (type 0))
)