(assert_invalid
  (module
    (type (func (result externref)))
  )
  "gc types are disallowed")

(assert_invalid
  (module
    (type (func (result (ref any))))
  )
  "gc types are disallowed")

(module
  (table 1 funcref)
)

(module
  (type $t (func))
  (table 1 (ref null $t))
)

(assert_invalid
  (module
    (type (array i8))
  )
  "cannot define array types")

(assert_invalid
  (module
    (type (struct))
  )
  "cannot define struct types")
