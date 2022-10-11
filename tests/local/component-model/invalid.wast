(assert_invalid
  (component
    (core type (module
      (import "" "" (func (type 1)))
    ))
    (type (func))
  )
  "type index out of bounds")

(assert_malformed
  (component quote
    "(export \"\" (func $foo))"
  )
  "unknown func")

(assert_malformed
  (component quote
    "(alias outer 100 $foo (type $foo))"
  )
  "outer count of `100` is too large")

(assert_malformed
  (component quote
    "(alias outer $nonexistent $foo (type $foo))"
  )
  "outer component `nonexistent` not found")
