(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (func (param "arg" string)))
          (export (;0;) "log" (func (type 0)))
        )
      )
      (export (;0;) (interface "foo:foo/console") (instance (type 0)))
      (type (;1;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (func (param "arg" string)))
              (export (;0;) "log" (func (type 0)))
            )
          )
          (import (interface "foo:foo/console") (instance (;0;) (type 0)))
        )
      )
      (export (;0;) (interface "foo:foo/the-world") (component (type 1)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)