(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) u8)
          (export (;1;) "ty" (type (eq 0)))
        )
      )
      (import "foo:a/the-name" (instance (;0;) (type 0)))
      (type (;1;)
        (instance
          (type (;0;) u8)
          (export (;1;) "ty" (type (eq 0)))
        )
      )
      (import "foo:b/the-name" (instance (;1;) (type 1)))
      (alias export 0 "ty" (type (;2;)))
      (alias export 1 "ty" (type (;3;)))
      (type (;4;)
        (instance
          (alias outer 1 2 (type (;0;)))
          (export (;1;) "ty" (type (eq 0)))
          (alias outer 1 3 (type (;2;)))
          (export (;3;) "ty2" (type (eq 2)))
        )
      )
      (export (;2;) "foo:foo/a" (instance (type 4)))
    )
  )
  (export (;1;) "a" (type 0))
  (@custom "package-docs" "\01{}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
