(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (record (field "f" u8)))
              (export (;1;) "foo" (type (eq 0)))
              (export (;2;) "bar" (type (eq 1)))
            )
          )
          (export (;0;) (interface "foo:my-dep/my-interface") (instance (type 0)))
        )
      )
      (export (;0;) (interface "foo:foo/foo") (component (type 0)))
    )
  )
  (export (;1;) "foo" (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)