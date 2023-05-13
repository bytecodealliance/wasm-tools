(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (record))
          (export (;1;) "the-type" (type (eq 0)))
        )
      )
      (export (;0;) (interface "foo/b") (instance (type 0)))
      (alias export 0 "the-type" (type (;1;)))
      (type (;2;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (export (;1;) "the-type" (type (eq 0)))
        )
      )
      (export (;1;) (interface "foo/b2") (instance (type 2)))
      (type (;3;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (export (;1;) "the-type" (type (eq 0)))
        )
      )
      (export (;2;) (interface "foo/a2") (instance (type 3)))
      (type (;4;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (export (;1;) "the-type" (type (eq 0)))
        )
      )
      (export (;3;) (interface "foo/a") (instance (type 4)))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
  (export (;1;) (interface "foo/wit") (type 0))
)