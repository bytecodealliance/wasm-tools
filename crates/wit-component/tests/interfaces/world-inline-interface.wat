(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance)
          )
          (import "foo" (instance (;0;) (type 0)))
          (type (;1;)
            (instance)
          )
          (export (;1;) "bar" (instance (type 1)))
        )
      )
      (export (;0;) "has-inline" "pkg:/world-inline-interface/has-inline" (component (type 0)))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
  (export (;1;) "world-inline-interface" "pkg:/world-inline-interface" (type 0))
)