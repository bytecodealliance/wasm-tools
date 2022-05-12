(assert_invalid
  (component
    (type (module
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
  "invalid leading byte")
