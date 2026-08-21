(component
  (type (;0;) (record (field "x" u32) (field "y" u32)))
  (export (;1;) "point" (external-id "pkg-point") (type 0))
  (import "local:demo/point" (external-id "pkg-point") (type (;2;) (eq 1)))
  (type (;3;) (list 2))
  (export (;4;) "path" (external-id "pkg-path") (type 3))
  (import "local:demo/path" (external-id "pkg-path") (type (;5;) (eq 4)))
  (@custom "package-docs" "\01{}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
