(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;)
            (instance
              (type (;0;) string)
              (export (;1;) "t" (type (eq 0)))
            )
          )
          (export (;0;) "foo:other/i" (instance (type 0)))
        )
      )
      (export (;0;) "foo:bar/i1" (instance (type 0)))
    )
  )
  (export (;1;) "i1" (type 0))
  (@custom "package-docs" "\00{}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
