(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) u8)
              (export (;1;) "t" (type (eq 0)))
            )
          )
          (export (;0;) (interface "foo:the-dep/the-interface") (instance (type 0)))
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