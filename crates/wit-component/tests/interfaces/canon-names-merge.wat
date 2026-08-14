(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) u32)
              (export (;1;) "my-type" (type (eq 0)))
              (type (;2;) (func (result 1)))
              (export (;0;) "my-func" (func (type 2)))
              (type (;3;) (func (param "x" 1) (result string)))
              (export (;1;) "added-func" (func (type 3)))
            )
          )
          (import "test:lib/types@1" (versionsuffix ".2.0") (instance (;0;) (type 0)))
        )
      )
      (export (;0;) "test:app/my-world@1.0.0" (component (type 0)))
    )
  )
  (export (;1;) "my-world" (type 0))
  (@custom "package-docs" "\01{}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
