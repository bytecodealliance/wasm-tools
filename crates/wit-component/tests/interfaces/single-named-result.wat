(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (func (result "a" u32)))
          (export (;0;) "a" (func (type 0)))
        )
      )
      (export (;0;) (interface "foo:foo/foo") (instance (type 0)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)