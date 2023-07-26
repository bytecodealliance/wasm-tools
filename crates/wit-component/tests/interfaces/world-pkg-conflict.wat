(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) u32)
          (export (;1;) "t" (type (eq 0)))
        )
      )
      (export (;0;) (interface "foo:foo/a") (instance (type 0)))
      (alias export 0 "t" (type (;1;)))
      (type (;2;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (export (;1;) "t" (type (eq 0)))
        )
      )
      (export (;1;) (interface "foo:foo/foo") (instance (type 2)))
      (type (;3;)
        (component)
      )
      (export (;0;) (interface "foo:foo/c") (component (type 3)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)