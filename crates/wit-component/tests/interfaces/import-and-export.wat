(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (func))
          (export (;0;) "foo" (func (type 0)))
        )
      )
      (export (;0;) (interface "foo:foo/foo") (instance (type 0)))
      (type (;1;)
        (instance
          (type (;0;) (func))
          (export (;0;) "bar" (func (type 0)))
        )
      )
      (export (;1;) (interface "foo:foo/bar") (instance (type 1)))
      (type (;2;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (func))
              (export (;0;) "foo" (func (type 0)))
            )
          )
          (import (interface "foo:foo/foo") (instance (;0;) (type 0)))
          (type (;1;)
            (instance
              (type (;0;) (func))
              (export (;0;) "bar" (func (type 0)))
            )
          )
          (export (;1;) (interface "foo:foo/bar") (instance (type 1)))
        )
      )
      (export (;0;) (interface "foo:foo/import-and-export") (component (type 2)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)