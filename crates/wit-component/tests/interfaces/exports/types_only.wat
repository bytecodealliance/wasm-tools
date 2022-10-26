(component
  (type (;0;) (record (field "a" u32)))
  (type (;1;) (func (param "a" 0) (result string)))
  (type (;2;)
    (instance
      (alias outer 1 0 (type (;0;)))
      (export "my-struct" (type (eq 0)))
      (alias outer 1 1 (type (;1;)))
      (export "my-function" (func (type 1)))
    )
  )
  (export "foo" (type 2))
)