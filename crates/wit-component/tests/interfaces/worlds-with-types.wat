(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) u32)
          (export (;1;) "foo" (type (eq 0)))
        )
      )
      (export (;0;) "foo:foo/import-me" (instance (type 0)))
    )
  )
  (export (;1;) "import-me" (type 0))
  (type (;2;)
    (component
      (type (;0;)
        (component
          (type (;0;) (record (field "f" u8)))
          (import "foo" (type (;1;) (eq 0)))
          (import "bar" (type (;2;) (eq 1)))
          (type (;3;) (func (param "a" 1) (result 2)))
          (import "a" (func (;0;) (type 3)))
          (export (;1;) "b" (func (type 3)))
        )
      )
      (export (;0;) "foo:foo/simple" (component (type 0)))
    )
  )
  (export (;3;) "simple" (type 2))
  (type (;4;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) u32)
              (export (;1;) "foo" (type (eq 0)))
            )
          )
          (import "foo:foo/import-me" (instance (;0;) (type 0)))
          (alias export 0 "foo" (type (;1;)))
          (import "foo" (type (;2;) (eq 1)))
          (type (;3;) (func (param "a" 2)))
          (import "a" (func (;0;) (type 3)))
          (export (;1;) "b" (func (type 3)))
        )
      )
      (export (;0;) "foo:foo/with-imports" (component (type 0)))
    )
  )
  (export (;5;) "with-imports" (type 4))
  (@custom "package-docs" "\01{}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
