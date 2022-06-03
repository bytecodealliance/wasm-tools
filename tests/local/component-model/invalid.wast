(assert_invalid
  (component
    (core type (module
      (import "" "" (func (type 1)))
    ))
    (type (func))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (import "" (func $f))
    (component
      (alias outer 1 $f (func $f))
    )
  )
  "outer aliases may only be made to types, modules, and components")

(assert_malformed
  (component quote
    "(export \"\" (func $foo))"
  )
  "failed to find func named")

(assert_malformed
  (component quote
    "(alias outer 100 $foo (func $foo))"
  )
  "component depth of `100` is too large")

(assert_malformed
  (component quote
    "(alias outer $nonexistent $foo (func $foo))"
  )
  "outer component `nonexistent` not found")
