(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) u8)
          (export (;1;) "some-type" (type (eq 0)))
        )
      )
      (import (interface "foo:some-dep/types") (instance (;0;) (type 0)))
      (alias export 0 "some-type" (type (;1;)))
      (type (;2;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (export (;1;) "some-type" (type (eq 0)))
        )
      )
      (export (;1;) (interface "foo:foo/foo") (instance (type 2)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)