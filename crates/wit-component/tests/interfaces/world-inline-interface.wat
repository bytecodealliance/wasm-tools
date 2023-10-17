(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance)
          )
          (import "foo" (instance (;0;) (type 0)))
          (type (;1;)
            (instance)
          )
          (export (;1;) "bar" (instance (type 1)))
        )
      )
      (export (;1;) (interface "foo:foo/has-inline") (type (eq 0)))
    )
  )
  (export (;1;) "has-inline" (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)