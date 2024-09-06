(assert_invalid
  (component
    (core type (func (param v128)))
  )
  "SIMD support is not enabled")

(assert_invalid
  (component
    (core type (func (param (ref 0))))
  )
  "unknown type 0: type index out of bounds")

(assert_invalid
  (component
    (core type (func))
    (core type (func (param (ref 0))))
  )
  "reference types support is not enabled")
