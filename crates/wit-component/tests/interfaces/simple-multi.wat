(component
  (type (;0;)
    (component
      (type (;0;)
        (instance)
      )
      (export (;0;) "foo" "pkg:/foo/foo" (instance (type 0)))
    )
  )
  (export (;1;) "foo" "pkg:/foo" (type 0))
  (type (;2;)
    (component
      (type (;0;)
        (instance)
      )
      (export (;0;) "bar" "pkg:/bar/bar" (instance (type 0)))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
  (export (;3;) "bar" "pkg:/bar" (type 2))
)