(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (enum "info" "debug"))
          (export (;1;) "level" (type (eq 0)))
        )
      )
      (export (;0;) (interface "foo:foo/types") (instance (type 0)))
    )
  )
  (export (;1;) "types" (type 0))
  (type (;2;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (enum "info" "debug"))
          (export (;1;) "level" (type (eq 0)))
        )
      )
      (import (interface "foo:foo/types") (instance (;0;) (type 0)))
      (alias export 0 "level" (type (;1;)))
      (type (;2;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (export (;1;) "level" (type (eq 0)))
          (type (;2;) (func (param "level" 1) (param "msg" string)))
          (export (;0;) "log" (func (type 2)))
        )
      )
      (export (;1;) (interface "foo:foo/console") (instance (type 2)))
    )
  )
  (export (;3;) "console" (type 2))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)