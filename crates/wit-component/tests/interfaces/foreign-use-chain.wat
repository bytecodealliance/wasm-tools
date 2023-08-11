(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) u8)
              (export (;1;) "the-type" (type (eq 0)))
            )
          )
          (import (interface "foo:bar/the-interface") (instance (;0;) (type 0)))
          (alias export 0 "the-type" (type (;1;)))
          (type (;2;)
            (instance
              (alias outer 1 1 (type (;0;)))
              (export (;1;) "bar" (type (eq 0)))
            )
          )
          (import (interface "foo:bar/bar") (instance (;1;) (type 2)))
        )
      )
      (export (;0;) (interface "foo:foo/foo") (component (type 0)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)