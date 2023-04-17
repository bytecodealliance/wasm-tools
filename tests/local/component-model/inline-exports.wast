(component
  (type (export "foo") u8)
)

(assert_malformed
  (component quote "(type (component (type (export \"\") (func))))")
  "unexpected token")
