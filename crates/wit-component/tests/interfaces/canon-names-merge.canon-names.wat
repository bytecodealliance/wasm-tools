(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (record (field "name" string)))
              (export (;1;) "info" (type (eq 0)))
              (type (;2;) (func (result 1)))
              (export (;0;) "get-info" (func (type 2)))
            )
          )
          (import "test:lib/api@1" (versionsuffix ".2.2") (instance (;0;) (type 0)))
        )
      )
      (export (;0;) "test:app/app@1.0.0" (component (type 0)))
    )
  )
  (export (;1;) "app" (type 0))
  (@custom "package-docs" "\01{}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
