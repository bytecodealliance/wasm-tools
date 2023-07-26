(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (record (field "a" u32)))
          (export (;1;) "my-struct" (type (eq 0)))
          (type (;2;) (func (param "a" 1) (result string)))
          (export (;0;) "my-function" (func (type 2)))
        )
      )
      (export (;0;) (interface "foo:foo/foo") (instance (type 0)))
      (type (;1;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (record (field "a" u32)))
              (export (;1;) "my-struct" (type (eq 0)))
              (type (;2;) (func (param "a" 1) (result string)))
              (export (;0;) "my-function" (func (type 2)))
            )
          )
          (export (;0;) (interface "foo:foo/foo") (instance (type 0)))
        )
      )
      (export (;0;) (interface "foo:foo/export-foo") (component (type 1)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)