(component
  (type (;0;)
    (component
      (type (;0;)
        (instance)
      )
      (export (;0;) (interface "foo:foo/bar") (instance (type 0)))
      (type (;1;)
        (instance)
      )
      (export (;1;) (interface "foo:foo/foo") (instance (type 1)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)