(component
  (type (;0;) (record (field "x" u32) (field "y" u32)))
  (export (;1;) "point" (type 0))
  (type (;2;)
    (component
      (type (;0;) (record (field "x" u32) (field "y" u32)))
      (import "local:demo/point" (type (;1;) (eq 0)))
      (type (;2;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (type (;1;) (func (param "p" 0)))
          (export (;0;) "move-to" (func (type 1)))
        )
      )
      (export (;0;) "local:demo/api" (instance (type 2)))
    )
  )
  (export (;3;) "api" (type 2))
  (@custom "package-docs" "\01{}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
